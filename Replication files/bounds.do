******************************************
**This do file constructs bounds for attrition; i.e., imputes various simulated values to check that the main results on earnings in the first follow-up is not due to differentail rate of attrition by treatment status
***Prepared by Girum Abebe September 2023
************************************

clear
set more off
macro drop _all


clear
		
*Assign Global Path
	* User 1: Girum
	global ga 1

	* User 2: Nik
	global nik 0
		
	if $ga {
		 global directory		"C:\Users\wb536602\OneDrive - WBG\In the Bank\Projects\2020\CJC follow up"	
		 global datain 			"$directory\Analysis data"
		 global output			"$directory\Output"
		 }
		 
		 
		 
 ********************************
*Open the data
 ************************************


clear
use "$datain\CJC_wide_all2020.dta", clear 


**************************************************
*Table A1: Predicting attrition, OLS estimates 
**************************************************

*Make sure the treatment variable is also assigned for the second endline
count  if survey_FL==1
replace treatment=_treatment if  survey_FL==1 & _treatment!=.
tab treatment if survey_FL==1
tab treatment if survey_EL==1


tab panel_final //tracked at both endlines (balanced)

local control_variables_attrit "age_BL married_BL any_children_BL D_migrant_BL schooling_BL"

global interact ""
foreach x in `control_variables_attrit' {
    gen `x'INT=`x'*treatment
    global interact "$interact `x'INT"
}

xi, prefix(_W) i.woreda_BL



*****************
*Survival  in the first follow-up, and second follow-up surveys 
gen panel_EL=panel==1
gen panel_FL=panel_final==1
sum panel*

xi, prefix(_W) i.woreda_BL
local control_variables_attrit "age_BL married_BL any_children_BL D_migrant_BL schooling_BL"
foreach i in EL FL {
reg panel_`i' treatment 
outreg2 using attri.doc, append nolabel  bdec(1) sdec(1) 
reg panel_`i' treatment _W*
outreg2 using attri.doc, append nolabel  bdec(1) sdec(1) 
reg panel_`i' treatment `control_variables_attrit' _W*,  
outreg2 using attri.doc, append nolabel  bdec(1) sdec(1) 
reg panel_`i' treatment `control_variables_attrit' _W* $interact
outreg2 using attri.doc, append nolabel  bdec(1) sdec(1) 
}


*Compostion test:F-test on the coeffiencent between on treatment interacated with baseline covariates
local control_variables_attrit "age_BL married_BL any_children_BL D_migrant_BL schooling_BL"
reg panel_EL treatment `control_variables_attrit' $interact _W*
testparm *INT
reg panel_FL treatment `control_variables_attrit' $interact _W*
testparm *INT


*Construct upper and lower bounds for attrition 

reg earnings_4w_EL treatment earnings_4w_BL $balance

*relable to fit with earlier syntax 
gen control=treatment==0
gen monthly_wage=earnings_4w_EL
gen tg_2=treatment 

global balance "earnings_4w_BL age_BL married_BL schooling_BL _W*  any_children_BL D_migrant_BL"

exit 
*****************************************************************************************
*Predicting upper bound for earnings 
*Attritors from the treatment group are high performing and hence adjust for their missing value by using adding by higher values 
**************************************************************

* 1) EARNINGS UPPER BOUND FOR 2017  (Table A.28)
**# Bookmark #4
reg monthly_wage $balance  if control==1
predict predict_wage 
predict pred_wage_se ,stdp

gen monthly_wage_pred = monthly_wage
replace monthly_wage_pred = predict_wage if monthly_wage==.
  
gen monthly_wage_pred_se25 = monthly_wage
replace monthly_wage_pred_se25 = predict_wage-(0.25*pred_wage_se) if monthly_wage==.&control==1
replace monthly_wage_pred_se25 = predict_wage+(0.25*pred_wage_se) if monthly_wage==.&tg_2==1

gen monthly_wage_pred_se50 = monthly_wage
replace monthly_wage_pred_se50 = predict_wage-(0.50*pred_wage_se) if monthly_wage==.&control==1
replace monthly_wage_pred_se50 = predict_wage+(0.50*pred_wage_se) if monthly_wage==.&tg_2==1

**# Bookmark #8
gen monthly_wage_imp25_gened = monthly_wage
gen monthly_wage_imp50_gened =monthly_wage

sum monthly_wage_imp25_gened if control == 1 
replace monthly_wage_imp25_gened = r(mean)-0.25*r(sd) if monthly_wage==.&control == 1  

sum monthly_wage_imp25_gened if tg_2 == 1 
replace monthly_wage_imp25_gened = r(mean)+0.25*r(sd) if monthly_wage==.&tg_2 == 1 

sum monthly_wage_imp50_gened if control == 1  
replace monthly_wage_imp50_gened = r(mean)-0.5*r(sd) if monthly_wage==.&control == 1 

sum monthly_wage_imp50_gened if tg_2 == 1 
replace monthly_wage_imp50_gened = r(mean)+0.5*r(sd) if monthly_wage==.&tg_2 == 1 

**# Bookmark #9
gen monthly_wage_imp95 = monthly_wage
gen monthly_wage_manski = monthly_wage

sum monthly_wage_imp95 if control == 1 ,d
replace monthly_wage_imp95 = r(p5) if monthly_wage==.&control == 1 
replace monthly_wage_manski = r(min) if monthly_wage==.&control == 1 

sum monthly_wage_imp95 if tg_2 == 1,d
replace monthly_wage_imp95 =   r(p95)  if monthly_wage==.&tg_2 == 1 
replace monthly_wage_manski =  r(max) if monthly_wage==.&tg_2 == 1 

*recode bs_education (2/3=2) (4=3)
*keep if control==1|tg_2==1

label var monthly_wage_pred "Predicted earnings" 
label var monthly_wage_pred_se25 "Predicted earnings +/- 0.25 SDs" 
label var monthly_wage_pred_se50 "Predicted earnings +/- 0.5 SDs" 
label var monthly_wage_imp25_gened "Mean control earnings +/- 0.25 SDs" 
label var monthly_wage_imp50_gened "Mean control earnings +/- 0.5 SDs" 
label var monthly_wage_imp95 "95th / 5th percentile" 
label var monthly_wage_manski "Max/min" 


tabstat  monthly_wage_pred  monthly_wage_pred_se25 monthly_wage_pred_se50  monthly_wage_imp25_gened monthly_wage_imp50_gened monthly_wage_imp95   monthly_wage_manski, by(control) 


*Control mean 
tabout treatment  using "$output\WBER\table_bounding_upper.xls" ,cells(mean monthly_wage mean monthly_wage_pred mean monthly_wage_pred_se25 mean monthly_wage_pred_se50 mean monthly_wage_imp25_gened mean monthly_wage_imp50_gened mean monthly_wage_imp95 mean  monthly_wage_manski) format (0 1) clab(monthly_wage monthly_wage_pred monthly_wage_pred_se25 monthly_wage_pred_se50  monthly_wage_imp25_gened monthly_wage_imp50_gened monthly_wage_imp95 monthly_wage_manski ) sum oneway replace  

*Now run the regressions 
xi, prefix(_W) i.woreda_BL //woreda dummies 


**# Bookmark #7
reg monthly_wage treatment $balance _W*
outreg2 using attri.xls, replace nolabel  bdec(1) sdec(1) 
reg monthly_wage_pred treatment $balance _W*
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg  monthly_wage_pred_se25 treatment $balance _W*,  
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg  monthly_wage_pred_se50 treatment $balance _W*,  
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg monthly_wage_imp25_gened treatment $balance _W*
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg  monthly_wage_imp50_gened treatment $balance _W*,  
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg monthly_wage_imp95 treatment $balance _W*
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg monthly_wage_manski treatment $balance _W*
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 

**# Bookmark #10
drop predict_wage pred_wage_se monthly_wage_pred  monthly_wage_pred_se25 monthly_wage_pred_se50  monthly_wage_imp25_gened monthly_wage_imp50_gened monthly_wage_imp95   monthly_wage_manski // drop them to generate the lower bound with the same variable name



***********************************************************************************
*Predicting lower bound for earnings 
*********************************************************************************

* 2) EARNINGS Lower BOUND FOR 2017  (Table A.28)
**# Bookmark #4
reg monthly_wage $balance  if control==1
predict predict_wage 
predict pred_wage_se ,stdp

gen monthly_wage_pred = monthly_wage
replace monthly_wage_pred = predict_wage if monthly_wage==.
  
gen monthly_wage_pred_se25 = monthly_wage
replace monthly_wage_pred_se25 = predict_wage+(0.25*pred_wage_se) if monthly_wage==.&control==1
replace monthly_wage_pred_se25 = predict_wage-(0.25*pred_wage_se) if monthly_wage==.&tg_2==1

gen monthly_wage_pred_se50 = monthly_wage
replace monthly_wage_pred_se50 = predict_wage+(0.50*pred_wage_se) if monthly_wage==.&control==1
replace monthly_wage_pred_se50 = predict_wage-(0.50*pred_wage_se) if monthly_wage==.&tg_2==1

**# Bookmark #8
gen monthly_wage_imp25_gened = monthly_wage
gen monthly_wage_imp50_gened =monthly_wage

sum monthly_wage_imp25_gened if control == 1 
replace monthly_wage_imp25_gened = r(mean)+0.25*r(sd) if monthly_wage==.&control == 1  

sum monthly_wage_imp25_gened if tg_2 == 1 
replace monthly_wage_imp25_gened = r(mean)-0.25*r(sd) if monthly_wage==.&tg_2 == 1 

sum monthly_wage_imp50_gened if control == 1  
replace monthly_wage_imp50_gened = r(mean)+0.5*r(sd) if monthly_wage==.&control == 1 

sum monthly_wage_imp50_gened if tg_2 == 1 
replace monthly_wage_imp50_gened = r(mean)-0.5*r(sd) if monthly_wage==.&tg_2 == 1 

**# Bookmark #9

gen monthly_wage_imp95 = monthly_wage
gen monthly_wage_manski = monthly_wage

sum monthly_wage_imp95 if tg_2 == 1 ,d
replace monthly_wage_imp95 = r(p5) if monthly_wage==.&tg_2 == 1 
replace monthly_wage_manski = r(min) if monthly_wage==.&tg_2 == 1 

sum monthly_wage_imp95 if control == 1,d
replace monthly_wage_imp95 =   r(p95)  if monthly_wage==.&control == 1 
replace monthly_wage_manski =  r(max) if monthly_wage==.&control == 1 


*recode bs_education (2/3=2) (4=3)
*keep if control==1|tg_2==1

label var monthly_wage_pred "Predicted earnings" 
label var monthly_wage_pred_se25 "Predicted earnings +/- 0.25 SDs" 
label var monthly_wage_pred_se50 "Predicted earnings +/-0.5 SDs" 
label var monthly_wage_imp25_gened "Mean control earnings +/- 0.25 SDs" 
label var monthly_wage_imp50_gened "Mean control earnings +/- 0.5 SDs" 
label var monthly_wage_imp95 "95th / 5th percentile" 
label var monthly_wage_manski "Max/min" 


asdoc sum monthly_wage_pred  monthly_wage_pred_se25 monthly_wage_pred_se50  monthly_wage_imp25_gened monthly_wage_imp50_gened monthly_wage_imp95 monthly_wage_manski if control==1, replace 

*Control mean 
tabout treatment  using "$output\WBER\table_bounding_lower.xls" ,cells(mean monthly_wage mean monthly_wage_pred mean monthly_wage_pred_se25 mean monthly_wage_pred_se50 mean monthly_wage_imp25_gened mean monthly_wage_imp50_gened mean monthly_wage_imp95 mean  monthly_wage_manski) format (0 1) clab(monthly_wage monthly_wage_pred monthly_wage_pred_se25 monthly_wage_pred_se50  monthly_wage_imp25_gened monthly_wage_imp50_gened monthly_wage_imp95 monthly_wage_manski ) sum oneway replace  


*Now run the regressions 
xi, prefix(_W) i.woreda_BL //woreda dummies 

**# Bookmark #7
reg monthly_wage treatment $balance _W*
outreg2 using attri.xls, replace nolabel  bdec(1) sdec(1) 
reg monthly_wage_pred treatment $balance _W*
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg  monthly_wage_pred_se25 treatment $balance _W*,  
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg  monthly_wage_pred_se50 treatment $balance _W*,  
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg monthly_wage_imp25_gened treatment $balance _W*
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg  monthly_wage_imp50_gened treatment $balance _W*,  
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg monthly_wage_imp95 treatment $balance _W*
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 
reg monthly_wage_manski treatment $balance _W*
outreg2 using attri.xls, append nolabel  bdec(1) sdec(1) 

sum monthly_wage_imp95 if treatment==0 & monthly_wage==. // 96th percentlile wage for the control group

***************************************
****Table A.33: Lee Bounds on binary variables
**************************************

local  var	" currently_emplDV currently_self_emplDV currently_wageDV currently_factory empl_time6m laborhours7dw "

**# Bookmark #14
leebounds ever_factory_EL treatment   ,    cieffect 
outreg2 using attri2.xls, replace nolabel  bdec(2) sdec(2) 
leebounds currently_emplDV_EL treatment   ,    cieffect 
outreg2 using attri2.xls, append nolabel  bdec(2) sdec(2) 
leebounds currently_self_emplDV_EL treatment    ,    cieffect 
outreg2 using attri2.xls, append nolabel  bdec(2) sdec(2) 
leebounds currently_wageDV_EL treatment    ,    cieffect 
outreg2 using attri2.xls, append nolabel  bdec(2) sdec(2) 
leebounds currently_factory_EL treatment    ,    cieffect 
outreg2 using attri2.xls, append nolabel  bdec(2) sdec(2) 
leebounds empl_time6m_EL treatment    ,    cieffect 
outreg2 using attri2.xls, append nolabel  bdec(2) sdec(2) 
leebounds laborhours7dw_EL treatment    ,    cieffect 
outreg2 using attri2.xls, append nolabel  bdec(2) sdec(2) 


leebounds permanent_work tg_1   [pw=ed_weight] if control == 1 | tg_1 == 1  ,    cieffect 
leebounds written_agreement tg_1   [pw=ed_weight] if control == 1 | tg_1 == 1  ,    cieffect 


local outcome1 written_agreement
local outcome1 permanent_work

sum permanent_work tg_1 ed_weight control
