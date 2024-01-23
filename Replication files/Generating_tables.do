*Organizing the syntax for submission (based on the final table ; table_v1)

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

 use "$datain\CJC_wide_combined2020.dta", clear 

 exit 

*******************************************************************************************
**************************** Mean Comparisons - CJC data **********************************
*******************************************************************************************

* This do file generates correlates of decision to take up and mantain a job at Bole Lemi IP
   
*** FINDINS THAT ARE ONLY INCLUDED IN THE TEXT BUT NOT IN THE TABLES ***

* Very few respondents send money to relatives/remittences (around 4%) at endline
tab s5q9_EL
tab s5q9_FL

*Strongly balanced panel_final
drop if panel_final!=1


*************************************************************
  *** Table 1: Baseline balance ***
 ************************************************************
 
local DEMOGRAPHICS 		"age_BL married_BL any_children_BL D_migrant_BL"
local EDUCATION			"schooling_BL any_training_BL"
local ECONOMIC			"ever_emplDV_BL ever_factory_BL anyearnings_4w_BL D_rent_BL"

global topics      		"DEMOGRAPHICS EDUCATION ECONOMIC"


global vars ""
global varsestimation ""
foreach el of global topics {
    global vars "$vars `el' ``el''"
    global varsestimation "$varsestimation ``el''"
}

foreach var of global varsestimation {
	
	reg `var' treatment
	getreg treatment, name(d`var')
	
	sum `var' if treatment==1
    local sd1_`var'=r(sd)
	global sd1_`var'=r(sd)
	local Var1_`var'=r(Var)
    local mean1_`var'=r(mean)
	global mean1_`var'=r(mean)
	betaformat sd1_`var'
	betaformat mean1_`var'
        
    sum `var' if treatment==0
    local sd0_`var'=r(sd)
	global sd0_`var'=r(sd)
	local Var0_`var'=r(Var)
    local mean0_`var'=r(mean)
	global mean0_`var'=r(mean)
	betaformat sd0_`var'
	betaformat mean0_`var'
        
    global nd_`var'=(`mean1_`var''-`mean0_`var'')/(sqrt(`Var1_`var''+`Var0_`var''))
    betaformat nd_`var'

}

reg treatment $varsestimation
test $varsestimation // joint hypothesis that each coefficient obtained from a regression of treatment on all variables listed in Table 1 is equal to zero


*** Generating the output ***

gen topic=""
gen var=""
gen N=""
gen mean1=""
gen mean0=""
gen diff=""
gen norm_diff=""

local p=0
local i=1
foreach var of global vars {

    foreach x of global varsestimation {
    
        local p=0
        if "`x'"=="`var'" {
            local j=`i'+1
                
            qui replace var="`x'" in `i'
			
			qui replace N="${N_d`x'}" in `i'
			
			qui replace mean1="${mean1_`x'}" in `i'
            qui replace mean1="[${sd1_`x'}]" in `j'
			
			qui replace mean0="${mean0_`x'}" in `i'
            qui replace mean0="[${sd0_`x'}]" in `j'
			
			qui replace diff="${beta_d`x'}${pstar_d`x'}" in `i'
            qui replace diff="${se_d`x'}" in `j'
			
			qui replace norm_diff="${nd_`x'}" in `i'
           
			local i=`i'+2
            local p=1
            continue, break
        }
    }
    if `p'==0 {
        if `i'==1 local i=1
        else local i=`i'+2
        qui replace topic="`var'" in `i'
    }
}


*browse variables of interest 
br topic var N mean1 mean0 diff norm_diff
exit

*Text (experience)
exit



*************************************************************
  *** Table 2: Impacts on employment 
 ************************************************************
clear
use "$directory\Analysis data\CJC_final_working.dta", 

drop if panel_final==0  // drop all observations that do not appear in all the three waves
count
*Control_mean for columns 1 and 4
foreach i in EL FL {
foreach var in ever_factory_`i' currently_emplDV_`i' currently_self_emplDV_`i' currently_wageDV_`i' currently_factory_`i' empl_time6m_`i'  laborhours7dw_`i'  {
tabout treatment [aweight=iwght_`i'] if treatment==0 & survey_`i'==1 using "$output\JDE submission\table2_mean.xls" , cells(mean `var' sd `var') format (3 3) clab(MEAN_`var' SD_`var') sum oneway append 
}
}


************************
*Table 2 (with first endline-2017, columns 1-3)
************************
xi, prefix(_W) i.woreda_BL //woreda dummies 

*First "ever_factory employed"
reg ever_factory_EL treatment [pweight=iwght_EL]
outreg2 using ce.doc, replace nolabel  bdec(3) sdec(3)
reg ever_factory_EL treatment ever_factory_BL age_BL married_BL schooling_BL _W* eval_duration_EL any_children_BL D_migrant_BL  [pweight=iwght_EL]
outreg2 using ce.doc, append nolabel  bdec(3) sdec(3)

*Second current employment 
local  var	" currently_emplDV currently_self_emplDV currently_wageDV currently_factory empl_time6m laborhours7dw "

foreach var in `var' {
reg `var'_EL treatment [pweight=iwght_EL]
outreg2 using ce.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL treatment `var'_BL age_BL married_BL schooling_BL _W* eval_duration_EL any_children_BL D_migrant_BL  [pweight=iwght_EL]
outreg2 using ce.doc, append nolabel  bdec(3) sdec(3)
}

************************
*Table 2 (with second endline-2020: columns 4-6)
************************
*First "ever_factory employed"
reg ever_factory_FL treatment [pweight=iwght_FL]
outreg2 using ce.doc, replace nolabel  bdec(3) sdec(3)
reg ever_factory_FL treatment ever_factory_BL age_BL married_BL schooling_BL _W* eval_duration_FL month_dummy* any_children_BL D_migrant_BL [pweight=iwght_FL]
outreg2 using ce.doc, append nolabel  bdec(3) sdec(3)

*Second current employment 
local  var	"currently_emplDV currently_self_emplDV currently_wageDV currently_factory empl_time6m laborhours7dw "
foreach var in `var' {
reg `var'_FL treatment [pweight=iwght_FL]
outreg2 using ce.doc, append nolabel  bdec(3) sdec(3)
reg `var'_FL treatment `var'_BL age_BL married_BL schooling_BL _W* eval_duration_FL month_dummy*  any_children_BL D_migrant_BL [pweight=iwght_FL]
outreg2 using ce.doc, append nolabel  bdec(3) sdec(3)

}


*************************************************************
  *** Table 3: Impacts on Income, expenditure and savings   
 ************************************************************
clear
use "$directory\Analysis data\CJC_final_working.dta", 
drop if panel_final==0  // drop all observations that do not appear in all the three waves


**Control mean at follow-up

foreach i in EL FL {
foreach var in earnings_4w_`i'  foodexp_7d_`i' nfoodexp_1m_`i' savings_4w_`i'   {
tabout treatment [aweight=iwght_`i']  if treatment==0 & survey_`i'==1 using "$output\JDE submission\table4b_income_new.xls" , cells(mean `var' sd `var') format (0 0) clab(MEAN_`var' SD_`var') sum oneway append
}
}


************************
*Table 3 (with first endline-2017: columns 1-3)
************************
xi, prefix(_W) i.woreda_BL //woreda dummies 

reg earnings_4w_EL treatment [pweight=iwght_EL] 
outreg2 using er.doc, replace nolabel  bdec(3) sdec(3)
reg earnings_4w_EL  treatment earnings_4w_BL age_BL married_BL schooling_BL _W* eval_duration_EL any_children_BL D_migrant_BL [pweight=iwght_EL] 
outreg2 using er.doc, append nolabel  bdec(3) sdec(3)

local var " foodexp_7d nfoodexp_1m savings_4w "
foreach var in `var' {
reg `var'_EL treatment [pweight=iwght_EL] 
outreg2 using er.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL treatment `var'_BL age_BL married_BL schooling_BL _W* eval_duration_EL any_children_BL D_migrant_BL [pweight=iwght_EL] 
outreg2 using er.doc, append nolabel  bdec(3) sdec(3)
}


************************
*Table 3 (with second endline-2020: columns 4-6)
************************
reg earnings_4w_FL treatment [pweight=iwght_FL] 
outreg2 using er.doc, replace nolabel  bdec(3) sdec(3)
reg earnings_4w_FL  treatment earnings_4w_BL age_BL married_BL schooling_BL _W* eval_duration_FL month_dummy* any_children_BL D_migrant_BL [pweight=iwght_FL] 
outreg2 using er.doc, append nolabel  bdec(3) sdec(3)

local var " foodexp_7d nfoodexp_1m savings_4w "
foreach var in `var' {
reg `var'_FL treatment [pweight=iwght_FL] 
outreg2 using er.doc, append nolabel  bdec(3) sdec(3)
reg `var'_FL treatment `var'_BL age_BL married_BL schooling_BL _W* eval_duration_FL month_dummy* any_children_BL D_migrant_BL [pweight=iwght_FL] 
outreg2 using er.doc, append nolabel  bdec(3) sdec(3)
}

*Check earning growth between treatment and control 
gen earn_growth=100*(earnings_4w_FL-earnings_4w_EL)/(earnings_4w_EL)
ttest earn_growth, by (treatment)

reg earn_growth treatment [pweight=iwght_FL] 
reg earn_growth treatment earnings_4w_BL age_BL married_BL schooling_BL _W* eval_duration_FL month_dummy* any_children_BL D_migrant_BL [pweight=iwght_FL] 

*New 
*************************************************************
  *** Table 4: Impacts on  health expenditure and perception 
 ************************************************************
clear
use "$directory\Analysis data\CJC_final_working.dta", 
drop if panel_final==0  // drop all observations that do not appear in all the three waves


*First reverse the health dummy so that it is consistent with the standaredized strneousity scores

foreach i in BL EL FL {
gen bad_health_`i'=.
replace bad_health_`i'=1 if D_healthy_`i'==0
replace bad_health_`i'=0 if D_healthy_`i'==1
label var bad_health_`i' "Perceives factory work as unhealthy_`i'"
}

*First and Second endline control means
foreach i in EL FL {
foreach var in zhealth_`i' bad_health_`i' factjobqual_`i'  expfacinc_`i'  D_steadyin_`i' D_permanent_`i' {
tabout treatment [aweight=iwght_`i'] if treatment==0 & survey_`i'==1 using "$output\JDE submission\table4_new.xls" , cells(mean `var' sd `var') format (3 3) clab(MEAN_`var' SD_`var') sum oneway append
}
}


*First follow-up (columns 1-3)
xi, prefix(_W) i.woreda_BL

reg zhealth_EL treatment [pweight=iwght_EL] 
outreg2 using ep.doc, replace nolabel  bdec(3) sdec(3)

local var " zhealth bad_health factjobqual expfacinc D_steadyin D_permanent"
foreach var in `var' {
reg `var'_EL treatment [pweight=iwght_EL] 
outreg2 using ep.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL treatment `var'_BL age_BL married_BL schooling_BL _W* eval_duration_EL any_children_BL D_migrant_BL  [pweight=iwght_EL] 
outreg2 using ep.doc, append nolabel  bdec(3) sdec(3)
}


*Table 4d  (second endline)

* Second endline

reg zhealth_FL treatment [pweight=iwght_FL] 
outreg2 using epf.doc, replace nolabel  bdec(3) sdec(3)

local var "zhealth bad_health factjobqual expfacinc D_steadyin D_permanent"
foreach var in `var' {
reg `var'_FL treatment [pweight=iwght_FL] 
outreg2 using epf.doc, append nolabel  bdec(3) sdec(3)
reg `var'_FL treatment `var'_BL age_BL married_BL schooling_BL _W* eval_duration_FL month_dummy* any_children_BL D_migrant_BL [pweight=iwght_FL] 
outreg2 using epf.doc, append nolabel  bdec(3) sdec(3)
}


*Complier analysis to check why the health impact dissappers: Is this because the women with the negative impact left or because the negative impact vanished. 

*First check whether the health impact vary when conditioning on current factory employment 
local var "zhealth bad_health"
foreach var in `var' {
reg `var'_FL treatment `var'_BL age_BL married_BL schooling_BL _W* eval_duration_EL any_children_BL D_migrant_BL  [pweight=iwght_EL] if currently_factory_FL==1
}

sum zhealth_BL zhealth_EL zhealth_FL, d
sum bad_health_EL
sum bad_health_FL

*probability of working in the factory if you have a bad health in the midline 
reg currently_factory_FL bad_health_EL currently_factory_BL age_BL married_BL schooling_BL _W* eval_duration_FL month_dummy* any_children_BL D_migrant_BL [pweight=iwght_FL] 

reg currently_factory_FL bad_health_EL currently_factory_BL age_BL married_BL schooling_BL _W* eval_duration_FL month_dummy* any_children_BL D_migrant_BL [pweight=iwght_FL] 

*Correlation coefficient
sum bad_health_EL bad_health_FL if currently_factory_FL==1
sum bad_health_EL bad_health_FL if currently_factory_FL==0

ttest bad_health_EL, by (currently_factory_FL)

*current factory employment is not correlated with health status


****************************************************************************************************
* This section complies responses to the referees and editor's comments 
*************************************************************************************************
*For revision WBER (in text discussions)
gen without_work=s8q1e1_FL==2
ttest without_work if panel==1, by(treatment)  // 
sum without_work if treatment==1 & currently_factory_EL==1 & panel==1,   // where are those who quit their factory employment among the treatment
sum without_work if treatment==0 & currently_factory_EL==1 & panel==1 ,   // where are those who quit their factory employment among the treatment


*Kind of jobs workers do outside of factory at the endline
forvalues x=1(1)25 {
	gen work_`x'_FL=p0s9q1_`x'_FL==1
}

*Currently employed in this work
forvalues x=1(1)25 {
	gen currently_`x'_FL=s9q1e9_`x'_FL==1
}

*First identify main categories of employment (those jobs at least with five percent response)
sum work_*_FL,   // 

*Get those whose average is greater than 5 percent
forvalues x=1(1)25 {
egen average_work_`x'=mean(work_`x'_FL)
gen select_`x'=work_`x'_FL if average_work_`x'>0.05
}

*These are :work_6_FL, work_7_FL,  work_8_FL, work_10_FL ,work_12_FL, work_13_FL, work_18_FL , work_19_FL ,  work_20_FL,  work_21_FL,  work_22_FL,  work_23_FL
*From the questionnaire, we can relabel what these jobs are
gen food_drink_sales=work_6_FL
gen Shop_kiosk=work_7_FL
gen Other_petty_trading=work_8_FL
gen tailoring_weaving=work_10_FL
gen factory_worke=work_12_FL
gen Casual_labor=work_13_FL
gen domestic_work=work_18_FL
gen clerical_office_work=work_19_FL
gen family_business=work_20_FL
gen Waiter_restaurant=work_21_FL
gen teacher_health_worker=work_22_FL
gen other_job=work_23_FL

*What about the other categories 
gen psnp=work_1_FL
gen own_farming=work_2_FL
gen rented_farming=work_3_FL
gen animal_rearing=work_4_FL
gen farm_wage_labor=work_5_FL

gen artisian_work=work_9_FL //metalwork, carpentry and shoemaking 
gen other_craft_work=work_11_FL

gen mining=work_14_FL
gen driver_mechanic=work_15_FL
gen security_guard=work_16_FL
gen solider_police=work_17_FL



*Have they every done this jobs?
preserve
keep if panel==1
asdoc ttest food_drink_sales, by(treatment) cnames(Control, Treatment) replace 
asdoc ttest Shop_kiosk, by(treatment) rowappend 
asdoc ttest Other_petty_trading, by(treatment) rowappend 
asdoc ttest tailoring_weaving, by(treatment) rowappend 
asdoc ttest factory_worke, by(treatment) rowappend 

asdoc ttest Casual_labor, by(treatment) rowappend 
asdoc ttest domestic_work, by(treatment) rowappend 


asdoc ttest clerical_office_work, by(treatment) rowappend 
asdoc ttest family_business, by(treatment) rowappend 
asdoc ttest Waiter_restaurant, by(treatment) rowappend 
asdoc ttest teacher_health_worker, by(treatment) rowappend 
asdoc ttest psnp, by(treatment) rowappend 
asdoc ttest own_farming, by(treatment) rowappend 
asdoc ttest rented_farming, by(treatment) rowappend 
asdoc ttest animal_rearing, by(treatment) rowappend 
asdoc ttest farm_wage_labor, by(treatment) rowappend 
asdoc ttest artisian_work, by(treatment) rowappend 
asdoc ttest other_craft_work, by(treatment) rowappend 
asdoc ttest mining, by(treatment) rowappend 
asdoc ttest driver_mechanic, by(treatment) rowappend 
asdoc ttest security_guard, by(treatment) rowappend 
asdoc ttest solider_police, by(treatment) rowappend 
asdoc ttest other_job, by(treatment) rowappend 
restore 


*Are they currently employed in these jobs //work_6_FL, work_7_FL,  work_8_FL, work_10_FL ,work_12_FL, work_13_FL, work_18_FL , work_19_FL ,  work_20_FL,  work_21_FL,  work_22_FL,  work_23_FL
*Conditional
/*
preserve
keep if panel==1
asdoc ttest currently_6_FL if food_drink_sales==1, by(treatment) cnames(Control, Treatment) replace 
asdoc ttest currently_7_FL if Shop_kiosk==1, by(treatment) rowappend 
asdoc ttest currently_8_FL if Other_petty_trading==1, by(treatment) rowappend 
asdoc ttest currently_10_FL if tailoring_weaving==1, by(treatment) rowappend 
asdoc ttest  currently_12_FL if factory_worke==1, by(treatment) rowappend 

asdoc ttest currently_13_FL if Casual_labor==1, by(treatment) rowappend 
asdoc ttest currently_18_FL if domestic_work==1, by(treatment) rowappend 

asdoc ttest currently_19_FL    if clerical_office_work==1, by(treatment) rowappend
 
asdoc ttest currently_20_FL if family_business==1, by(treatment) rowappend 
asdoc ttest currently_21_FL if Waiter_restaurant==1 , by(treatment) rowappend 
asdoc ttest currently_22_FL if teacher_health_worker==1 , by(treatment) rowappend 

asdoc ttest currently_1_FL if psnp==1 , by(treatment) rowappend
asdoc ttest currently_2_FL if own_farming==1 , by(treatment) rowappend 
asdoc ttest currently_3_FL if rented_farming==1 , by(treatment) rowappend 
asdoc ttest currently_4_FL if animal_rearing==1 , by(treatment) rowappend 
asdoc ttest currently_5_FL if farm_wage_labor==1 , by(treatment) rowappend 
asdoc ttest currently_9_FL if artisian_work==1 , by(treatment) rowappend 
asdoc ttest currently_11_FL if other_craft_work==1 , by(treatment) rowappend 
asdoc ttest currently_14_FL if mining==1 , by(treatment) rowappend 
asdoc ttest currently_15_FL if driver_mechanic==1 , by(treatment) rowappend 
asdoc ttest currently_16_FL if security_guard==1 , by(treatment) rowappend 
*asdoc ttest currently_17_FL if solider_police==1, by(treatment) rowappend 
asdoc ttest currently_23_FL if other_job==1 , by(treatment) rowappend 
restore 
exit 
*/

*Unconditional 
preserve
keep if panel==1
asdoc ttest currently_6_FL , by(treatment) cnames(Control, Treatment) replace 
asdoc ttest currently_7_FL , by(treatment) rowappend 
asdoc ttest currently_8_FL , by(treatment) rowappend 
asdoc ttest currently_10_FL , by(treatment) rowappend 
asdoc ttest  currently_12_FL , by(treatment) rowappend 

asdoc ttest currently_13_FL , by(treatment) rowappend 
asdoc ttest currently_18_FL , by(treatment) rowappend 

asdoc ttest currently_19_FL   , by(treatment) rowappend
 
asdoc ttest currently_20_FL , by(treatment) rowappend 
asdoc ttest currently_21_FL , by(treatment) rowappend 
asdoc ttest currently_22_FL , by(treatment) rowappend 

asdoc ttest currently_1_FL , by(treatment) rowappend
asdoc ttest currently_2_FL , by(treatment) rowappend 
asdoc ttest currently_3_FL , by(treatment) rowappend 
asdoc ttest currently_4_FL  , by(treatment) rowappend 
asdoc ttest currently_5_FL  , by(treatment) rowappend 
asdoc ttest currently_9_FL , by(treatment) rowappend 
asdoc ttest currently_11_FL  , by(treatment) rowappend 
asdoc ttest currently_14_FL  , by(treatment) rowappend 
asdoc ttest currently_15_FL  , by(treatment) rowappend 
asdoc ttest currently_16_FL , by(treatment) rowappend 
asdoc ttest currently_17_FL , by(treatment) rowappend 
asdoc ttest currently_23_FL , by(treatment) rowappend 
restore 
exit 


*Worker's subjective value on permanent work: 
*We have the survey quesiton" "Suppose there are two job. One job is a permanent job and pays 1300 birr per month. The other job is a casual labor job and pays a different amount per month. How many birr per month on average would the casual labor job have to pay you in order for you to take it over the permanent job?"
gen casual_wage=s12q23e1_FL
replace casual_wage=. if casual_wage<0
sum casual_wage if panel==1 
gen perm_premium=(casual_wage-1300)/1300
sum perm_premium if panel==1, d

*job secuirty is an important feature of a job for workers. The premmum workers attach to permanent work is very large -- Workers report that they would prefer a casual job over permanent job only if the casusal job offers on average a wage equivalent 179 percent of the corresponding wage from a permanent work.  [workers are willing to accept a much lower wage to take a permanent job offers]



****Revision on turnover 
*Why do workers leave and where do they go to 
*Editor says Here, the opportunity should be (should have been?) present to better understand why there is so much turnover


*First why do people decline job offers at Bole Lemi 
BLinterviewIMP_FL // ever applied/interviewed at the park. 
tab BLinterviewIMP_FL
label list s2q4e14

tab s2q40e3_FL  // did not accept a job offer while being offered postions in bole lemi. 
tab BLjobaccepted_FL // this is the reversly coded acceptance rate, our focus is on those who were offered but did not accept (53%)


*Generate reasons for deciding not to take the position at Bole Lemi after the interview  
gen compensation=1 if [s2q40e14_1_FL==1 | s2q40e14_2_FL==1 | s2q40e14_3_FL==1 | s2q40e14_4_FL==2 | s2q40e14_1_FL==2 | s2q40e14_2_FL==2 | s2q40e14_3_FL==2 | s2q40e14_4_FL==1]
replace compensation=0 if compensation!=1 & s2q40e14_1_FL!=.
gen house_rent=1 if [s2q40e14_1_FL==3 | s2q40e14_2_FL==3 | s2q40e14_3_FL==3]
replace house_rent=0 if house_rent!=1 & s2q40e14_1_FL!=.
gen working_conditions=1 if inrange(s2q40e14_1_FL, 4, 6) | inrange(s2q40e14_2_FL, 4, 6) | inrange(s2q40e14_3_FL, 4, 6) | inrange(s2q40e14_4_FL, 4, 6) | inrange(s2q40e14_1_FL, 10, 12) | inrange(s2q40e14_2_FL, 10, 12) | inrange(s2q40e14_3_FL, 10, 12) | inrange(s2q40e14_4_FL, 10, 12) | s2q40e14_5_FL==10
replace working_conditions=0 if working_conditions!=1 & s2q40e14_1_FL!=.
gen health_safety=1 if [s2q40e14_1_FL==7 | s2q40e14_2_FL==7 | s2q40e14_3_FL==7 | s2q40e14_4_FL==7 | s2q40e14_5_FL==7 | s2q40e14_6_FL==7] | [s2q40e14_1_FL==13 | s2q40e14_2_FL==13 | s2q40e14_3_FL==13 | s2q40e14_4_FL==13]
replace health_safety=0 if health_safety!=1 & s2q40e14_1_FL!=.

*We merge travel time and transport into one indicators 
gen travel_time=1 if [s2q40e14_1_FL==8 | s2q40e14_2_FL==8 | s2q40e14_3_FL==8 | s2q40e14_4_FL==8 | s2q40e14_5_FL==8]
replace travel_time=0 if travel_time!=1 & s2q40e14_1_FL!=.

gen transport_options=1 if [s2q40e14_1_FL==9 | s2q40e14_2_FL==9 | s2q40e14_3_FL==9 | s2q40e14_4_FL==9 |s2q40e14_6_FL==9]
replace transport_options=0 if transport_options!=1 & s2q40e14_1_FL!=.


gen transport_distance=1 if [s2q40e14_1_FL==8 | s2q40e14_2_FL==8 | s2q40e14_3_FL==8 | s2q40e14_4_FL==8] | s2q40e14_5_FL==8 | [s2q40e14_1_FL==9 | s2q40e14_2_FL==9 | s2q40e14_3_FL==9 | s2q40e14_4_FL==9 | s2q40e14_5_FL==9]
replace transport_distance=0 if transport_distance!=1 & s2q40e14_1_FL!=.

gen family_reason=1 if s2q40e14_1_FL==14 | s2q40e14_2_FL==14 | s2q40e14_3_FL==14 | s2q40e14_4_FL==14 
replace family_reason=0 if family_reason!=1 & s2q40e14_1_FL!=.
gen interview=1 if  s2q40e14_1_FL==15 | s2q40e14_2_FL==15 | s2q40e14_3_FL==15 | s2q40e14_4_FL==15
replace interview=0 if interview!=1 & s2q40e14_1_FL!=.
gen found_job=1 if s2q40e14_1_FL==16 | s2q40e14_2_FL==16 | s2q40e14_3_FL==16 | s2q40e14_4_FL==16 | s2q40e14_6_FL==1
replace found_job=0 if found_job!=1 & s2q40e14_1_FL!=.
gen no_need_job=1 if s2q40e14_1_FL==17| s2q40e14_2_FL==17 | s2q40e14_3_FL==17 | s2q40e14_4_FL==17 
replace no_need_job=0 if no_need_job!=1 & s2q40e14_1_FL!=.

sum compensation house_rent working_conditions health_safety transport_distance family_reason interview found_job no_need_job

local var "compensation travel_time transport_options working_conditions health_safety house_rent family_reason interview found_job no_need_job"
foreach var in `var' {
tabout treatment  if panel==1 using "$output\WBER\table_quit_reason.xls" , cells(mean  `var' sd `var') format (2 2) clab(MEAN_`var' SD_`var') sum oneway append
}

local var "compensation travel_time transport_options working_conditions health_safety house_rent family_reason interview found_job no_need_job"
asdoc tabstat `var', stat(mean sd count)

*Grpah 
**# Bookmark #1
graph bar (mean) compensation  working_conditions travel_time transport_options  health_safety house_rent family_reason interview found_job no_need_job, ytitle(Share of workers) legend(order(1 "Compensation" 2 "Working conditions" 3 "Travel Time" 4 "Transport" 5 "Healty and Safety" 6 "House rent" 7 "Family objection" 6 "Did not like the interview" 8 "Found a new job" 9 "No need for a job"))
graph save "Graph" "$output\WBER\\quit_decide.gph", replace 

*Convert the graph to mono chrome
graph use "$output\WBER\\quit_decide.gph", scheme(s2mono)
graph save "$output\WBER\\quit_decide_mono.gph", replace


*This is a better graph 
local var "compensation  working_conditions travel_time transport_options  health_safety house_rent family_reason interview found_job no_need_job"
graph bar `var', asyvars showyvars leg(off) scheme(s1mono) ylab(0 "0" 0.1 "10" 0.2 "20" 0.3 "30" 0.4 "40" 0.5 "50" 0.6 "60") ytitle("Percent")  ylabel(,labsize(small)) bargap(10)  yvaroptions(relabel(1 "Compensation" 2 "Working conditions" 3 "Travel Time" 4 "Transport" 5 "Health and Safety" 6 "House rent" 7 "Family objection" 8 "Did not like the interview" 9 "Found a new job" 10 "No need for a job") label(angle(vertical))) 


*How long do workers work before they leave (subsample of quits)
gen months_tenure_EL=s2q40e12a_EL
tab2 treatment months_tenure_EL
gen days_tenure_EL=30*months_tenure_EL
replace days_tenure_EL=days_tenure_EL+ s2q40e12b_EL

gen months_tenure_FL=s2q40e12a_FL
replace months_tenure_FL=. if months_tenure_FL<0
tab2 treatment months_tenure_FL
gen days_tenure_FL=30*months_tenure_FL
replace days_tenure_FL=days_tenure_FL+ s2q40e12b_FL

**# Bookmark #3
sum months_tenure_EL months_tenure_FL, d
sum days_tenure_EL days_tenure_FL, d

*Comparisons with Blattman and Dercon (2022, pages 11-12)
tab months_tenure_EL if treatment==1, 
tab months_tenure_FL if treatment==1,

*Graph turnover 
hist days_tenure_EL , 
graph bar retention_3m , over(treat)
graph bar retention_6m , over(treat)

cdfplot days_tenure_EL if !missing(treatment), by(treatment) legend(order(1 "Treatment" 2 "Control")) xtitle("Tenure in days")
cdfplot days_tenure_FL if !missing(treatment), by(treatment) legend(order(1 "Treatment" 2 "Control")) xtitle("Tenure in days") 

cdfplot days_tenure_EL if !missing(treatment),  legend(order(1 "Treatment" 2 "Control")) xtitle("Tenure in days")
graph save "Graph" "$output\WBER\\turnover_EL.gph", replace 
cdfplot days_tenure_FL if !missing(treatment),  legend(order(1 "Treatment" 2 "Control")) xtitle("Tenure in days") 
graph save "Graph" "$output\WBER\\turnover_FL.gph", replace // this needs editing of the x axis for visualization

*Now turn the grpahs into monochrome 
foreach i in EL FL {
graph use "$output\WBER\\turnover_`i'.gph", scheme(s2mono)
graph save "$output\WBER\\turnover_`i'_mono.gph", replace

}


*Second where do workers go to after leaving the factories 
* We already show this in the new table of turnovers, now we look at why they leave factory jobs
**# Bookmark #10
tab currently_factory_EL 
tab currently_factory_FL
tab ever_factory_FL

**# Bookmark #12
**# Bookmark #14
**# Bookmark #18
label list s9q1e10
tab s9q1e10_12_FL
tab s9q1e10o_12_FL

gen reasons_quit=. 
replace reasons_quit=1  if s9q1e10_12_FL==3 | s9q1e10o_12_FL=="They couldn't pay our salary on time"
replace reasons_quit=2 if  s9q1e10_12_FL==1 | s9q1e10o_12_FL=="The Factory was closed" | s9q1e10o_12_FL=="The company was clossed."
replace reasons_quit=3 if  s9q1e10_12_FL==2 
replace reasons_quit=4 if  s9q1e10_12_FL==4 | s9q1e10o_12_FL=="Due to supervisor beheviour" | s9q1e10o_12_FL=="Feud  from worker" 
replace reasons_quit=5 if s9q1e10_12_FL==5
replace reasons_quit=6 if  s9q1e10_12_FL==6
replace reasons_quit=7 if s9q1e10_12_FL==8 | s9q1e10_12_FL==9 | s9q1e10o_12_FL=="Pergencey" | s9q1e10o_12_FL=="Pregnancy" | s9q1e10o_12_FL=="Because of pregnancy" | s9q1e10o_12_FL=="Due to pregnancy" 
replace reasons_quit=8 if  s9q1e10_12_FL==7  | s9q1e10o_12_FL=="My family requested that leave job."
replace reasons_quit=9 if  s9q1e10_12_FL==10  
replace reasons_quit=10 if  s9q1e10_12_FL==11  
replace reasons_quit=11 if  s9q1e10_12_FL==12 |  s9q1e10o_12_FL=="Long working hour"
replace reasons_quit=12 if s9q1e10o_12_FL=="Because of Migration" | s9q1e10o_12_FL=="I wanted  to change my living place." | s9q1e10o_12_FL=="Migrant"

label define quit_reasons 1 "Compensation" 2 "Job ended" 3 "To search better jobs" 4 "Unsatisfaction with the job" 5 "Fired" 6 "Own illness or disability" 7 "Marriage and Pregnancy" 8 "Child care and family responsiblity" 9 "Education/training" 10 "Workplace is too far" 11 "Workplace is physically strenous"  12 "Migration"
label values reasons_quit quit_reasons, 

quietly tab reasons_quit, gen (quit_dummy)

*Test the difference 
asdoc ttest quit_dummy1 , by(treatment) cnames(Control, Treatment) replace 
forvalue i=2(1)12 {
	asdoc ttest quit_dummy`i' , by(treatment) rowappend 
}
 

*Graph/table 
tab2 reasons_quit treatment, col nofreq

graph bar quit_dummy*, over(treatment)

graph bar quit_dummy*, asyvars showyvars leg(off) scheme(s1mono) ylab(0 "0" 0.2 "20" 0.4 "40" 0.6 "60" 0.8 "80") ytitle("Percent") bargap(10) yvaroptions(relabel(1 "Compensation" 2 "Job ended" 3 "To search better jobs" 4 "Unsatisfaction with the job" 5 "Fired" 6 "Own illness or disability" 7 "Marriage and Pregnancy" 8 "Child care and family responsiblity" 9 "Education/training" 10 "Workplace is too far" 11 "Workplace is physically strenous"  12 "Migration"))

*For ease of illustration we consider an abridged version 
**# Bookmark #21
graph bar quit_dummy*, asyvars showyvars leg(off) scheme(s1mono) ylab(0 "0" 0.1 "10" 0.2 "20" 0.3 "30" ) ytitle("Percent")  ylabel(,labsize(small)) bargap(10)  yvaroptions(relabel(1 "Compensation" 2 "Job ended" 3 "To search more" 4 "Dissatisfaction with job" 5 "Fired" 6 "Illness/disability" 7 "Marriage/Pregnancy" 8 "Childcare/family" 9 "Education/training" 10 "Distance" 11 "Strenouity"  12 "Migration") label(angle(vertical))) 


*-------------------------------------------------------------end ---------------------------------------


