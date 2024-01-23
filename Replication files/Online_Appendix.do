**********************************************
***Online Appendix 
**********************************************

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



*****************Define outcome variables*************************
*Survival  in the first follow-up, and second follow-up surveys 
gen panel_EL=panel==1
gen panel_FL=panel_final==1
sum panel*

*Produce attrition table in Table A1
xi, prefix(_W) i.woreda_BL
local control_variables_attrit "age_BL married_BL any_children_BL D_migrant_BL schooling_BL"
foreach i in EL FL {
reg panel_`i' treatment 
outreg2 using attri.doc, append nolabel  bdec(3) sdec(3) 
reg panel_`i' treatment _W*
outreg2 using attri.doc, append nolabel  bdec(3) sdec(3) 
reg panel_`i' treatment `control_variables_attrit' _W*,  
outreg2 using attri.doc, append nolabel  bdec(3) sdec(3) 
reg panel_`i' treatment `control_variables_attrit' _W* $interact
outreg2 using attri.doc, append nolabel  bdec(3) sdec(3) 
}


*Compostion test:F-test on the coeffiencent between on treatment interacated with baseline covariates
local control_variables_attrit "age_BL married_BL any_children_BL D_migrant_BL schooling_BL"
reg panel_EL treatment `control_variables_attrit' $interact _W*
testparm *INT
reg panel_FL treatment `control_variables_attrit' $interact _W*
testparm *INT



*/
****************************************************************
***Table A2: Selection into employment in factory jobs, OLS estimates 
****************************************************************

*** Table A2: Selection ***
*Use the restricted data set 

*Reporduce the 12 columns in Table 3 using outreg2
clear
use "$directory\Analysis data\CJC_wide_restricted2020.dta", clear

*Make sure the treatment variable is also assigned for the second endline
count  if survey_FL==1
replace treatment=_treatment if  survey_FL==1 & _treatment!=.
**# Bookmark #1
tab treatment if survey_FL==1

keep if treatment==1

*Assign macros 
global control_variables_select1 "treatment"
global control_variables_select2 	"age_BL married_BL any_children_BL D_migrant_BL "
*global control_variables_select3	"any_shock_BL"
global control_variables_select3	"schooling_BL any_training_BL"
global control_variables_select4	"ever_factory_BL anyearnings_4w_BL "
global control_variables_select 5    "factjobqual_BL D_healthy_BL D_permanent_BL"
global control_variables_select6 	"mob_score_BL empowerscore_BL"

xi, prefix(_W) i.woreda_BL

gen outcome1=BLinterview_EL
gen outcome2=BLjobaccepted_EL
gen outcome3=BLjobstillwork_FL

reg outcome1 $control_variables_select2  $control_variables_select3 _W*
outreg2 using t.doc, bdec(3) sdec(3) nolabel replace

forvalues i=1/3 {
reg outcome`i' $control_variables_select2  $control_variables_select3 _W*
outreg2 using t.doc, bdec(3) sdec(3) nolabel append
reg outcome`i' $control_variables_select2  $control_variables_select4 _W*
outreg2 using t.doc, bdec(3) sdec(3) nolabel append
reg outcome`i' $control_variables_select2  $control_variables_select5 _W*
outreg2 using t.doc, bdec(3) sdec(3) nolabel append
reg outcome`i' $control_variables_select2  $control_variables_select6 _W*
outreg2 using t.doc, bdec(3) sdec(3) nolabel append
}

drop outcome*

*******************************************************************
*key summary stat for text
********************************************************************
clear
use "$directory\Analysis data\CJC_final_working.dta", 
count
**# Bookmark #2
tab treatment 
tab treatment if panel==1
*sum BLinterview_EL if treatment==1 // share of treatment people who were interviwed 
sum BLinterview_EL if treatment==1 // share of treatment people who were interviwed  
sum BLjobacceptedIMP_EL if treatment==1 & BLinterview_EL==1 // conditonal on treatment and interview ever started the job


*Obtain the previous employment status // for text
clear
use "C:\Users\wb536602\OneDrive - WBG\In the Bank\Projects\2019\CJC paper\Data\Stata\CJC_baseline_Sec9.dta"

*Generate employment status of the sample in the baseline
count
tab s9q17
egen work=sum (s9q17), by (uid)  //total number of days across work activites in the last 7 days
replace work=0 if work==.  // no work are missing values in the data so replace them with zero days of work
collapse work, by(uid)
tab work

*/

****************************************************************************************************
*Constructing Index for Table A3
****************************************************************************************************
use "$directory\Analysis data\CJC_final_working.dta", clear

keep if panel_final==1
count

***************************************************
*      Employment index
***************************************************

local var "empl_time6m laborhours7dw searchactn "
foreach var in `var' {
sum `var'_BL  `var'_EL `var'_FL, detail 
gen `var'_dummy_EL=`var'_EL>r(p50) 
gen `var'_dummy_FL=`var'_BL>r(p50) 
gen `var'_dummy_BL=`var'_BL>r(p50) 
}

*generate index by horizontally summing up
*Baseline index
local var "ever_factory_BL currently_emplDV_BL currently_self_emplDV_BL currently_wageDV_BL currently_factory_BL empl_time6m_dummy_BL  laborhours7dw_dummy_BL"
egen new_empl_BL=rowtotal(`var')


local var "ever_factory_EL currently_emplDV_EL currently_self_emplDV_EL currently_wageDV_EL currently_factory_EL empl_time6m_dummy_EL  laborhours7dw_dummy_EL"
egen new_empl_EL=rowtotal(`var')

local var "ever_factory_FL currently_emplDV_FL currently_self_emplDV_FL currently_wageDV_FL currently_factory_FL empl_time6m_dummy_FL  laborhours7dw_dummy_FL"
egen new_empl_FL=rowtotal(`var')


/*
local var "ever_emplDV_EL  ever_self_emplDV_EL ever_wageDV_EL ever_factory_EL currently_emplDV_EL currently_self_emplDV_EL currently_wageDV_EL currently_factory_EL empl_time6m_dummy_EL  laborhours7dw_dummy_EL searchactD_EL searchactn_dummy_EL expinc30d_EL expinc12m_EL"
*Note that we do not have the offer variable from the baseline so we drop it here.
egen new_empl_EL=rowtotal (`var')  //employment index endline
*/

foreach i in BL EL FL {
sum new_empl_`i', d
gen new_empl_`i'_stan= (new_empl_`i'-r(mean))/r(sd)
}


**************************************************
*Index for earning and spending
***************************************************
local var " earnings_4w  foodexp_7d nfoodexp_1m  savings_4w"
foreach var in `var' {
foreach i in BL FL EL {
sum `var'_`i', detail 
gen `var'_dummy_`i'=`var'_`i'>r(p50) 
}
}

egen income_expe_BL=rowtotal(earnings_4w_dummy_BL foodexp_7d_dummy_BL nfoodexp_1m_dummy_BL  savings_4w_dummy_BL)
egen income_expe_EL=rowtotal( earnings_4w_dummy_EL foodexp_7d_dummy_EL nfoodexp_1m_dummy_EL  savings_4w_dummy_EL)
egen income_expe_FL=rowtotal( earnings_4w_dummy_FL foodexp_7d_dummy_FL nfoodexp_1m_dummy_FL  savings_4w_dummy_FL)

*Standardize
foreach i in BL EL FL {
sum income_expe_`i', d
gen income_expe_`i'_stan= (income_expe_`i'-r(mean))/r(sd)
}

***************************************************
*Index for health
***************************************************
*First reverse the health dummy so that it is consistent with the standaredized strneousity scores

foreach i in BL EL FL {
gen bad_health_`i'=.
replace bad_health_`i'=1 if D_healthy_`i'==0
replace bad_health_`i'=0 if D_healthy_`i'==1
label var bad_health_`i' "Perceives factory work as unhealthy_`i'"
}

*Standardize
foreach i in BL EL FL {
sum bad_health_`i', d
gen health_stan_`i'= (bad_health_`i'-r(mean))/r(sd)
}


*zhealth_`i' // is already a z score so no need to standaredized further 

*Combine the two health measures into one index

foreach i in BL EL FL {
	egen health_index_`i'_stan=rowmean(zhealth_`i'  health_stan_`i') // generate mean of the two into the index
}

sum health_index_*


*Index for expectations and perceptions
local var "expfacinc factjobqual D_steadyin D_permanent"
foreach var in `var' {
tab `var'_BL
tab `var'_EL
tab `var'_FL
}


*Some are not dummies so convert them to dummies
local var "expfacinc factjobqual"
foreach var in `var' {
sum `var'_BL, detail
gen `var'_dummy_BL=(`var'_BL>r(p50))
sum `var'_EL, detail
gen `var'_dummy_EL=(`var'_EL>r(p50))
sum `var'_FL, detail
gen `var'_dummy_FL=(`var'_FL>r(p50))
}

egen expectation_BL=rowtotal(expfacinc_dummy_BL factjobqual_dummy_BL D_healthy_BL D_steadyin_BL D_permanent_BL)
egen expectation_EL=rowtotal(expfacinc_dummy_EL factjobqual_dummy_EL D_healthy_EL D_steadyin_EL D_permanent_EL)
egen expectation_FL=rowtotal(expfacinc_dummy_FL factjobqual_dummy_FL D_healthy_FL D_steadyin_FL D_permanent_FL)

*Standardize
foreach i in BL EL FL {
sum expectation_`i', d
gen expectation_`i'_stan= (expectation_`i'-r(mean))/r(sd)
}

/*
*Index for empowerment
local var "ownbankacc mob_score contrause DM_solescore DM_jointscore empowerscore"
foreach var in `var' {
tab `var'_BL
tab `var'_EL
tab `var'_FL
}

*Some are not dummies so convert them to dummies
*Doing so results in some variables with zero scores as values appear to be concentrated in the median, for example, mob_score)
local var " mob_score  DM_solescore DM_jointscore empowerscore"
foreach var in `var' {
foreach i in BL EL FL {
sum `var'_`i' , detail
gen `var'_dummy_`i'=`var'_`i'>r(p50)
}
}

egen new_empower_BL=rowtotal( mob_score_dummy_BL DM_solescore_dummy_BL DM_jointscore_dummy_BL empowerscore_dummy_BL)
egen new_empower_EL=rowtotal( mob_score_dummy_EL DM_solescore_dummy_EL DM_jointscore_dummy_EL empowerscore_dummy_EL)
egen new_empower_FL=rowtotal( mob_score_dummy_FL DM_solescore_dummy_FL DM_jointscore_dummy_FL empowerscore_dummy_FL)


*Standardize
foreach i in BL EL FL {
sum new_empower_`i', d
gen new_empower_`i'_stan= (new_empower_`i'-r(mean))/r(sd)
}
*/



*Table A3 in the appendix 
*control mean (baseline)
local var "new_empl income_expe health_index  expectation "
foreach var in `var' {
sum `var'_BL_stan if treatment==0
}

*control mean (followup; columns 1 and 4 in the table)
local var "new_empl income_expe  health_index expectation "
foreach var in `var' {
sum `var'_EL_stan [aweight=iwght_EL]  if treatment==0
sum `var'_FL_stan [aweight=iwght_FL]   if treatment==0
}

exit 

*regression based on indexed outcome variables (First follow up 2017: Columns 1-3)
local var "new_empl income_expe health_index expectation "
reg new_empl_EL_stan treatment
outreg2 using t2.doc, replace nolabel  bdec(3) sdec(3)
foreach var in `var' {
reg `var'_EL_stan treatment
outreg2 using t2.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL_stan treatment `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration_EL any_children_BL D_migrant_BL
outreg2 using t2.doc, append nolabel  bdec(3) sdec(3)
}

*regression based on indexed outcome variables (second follow up 2020: Columns 4-6)
local var "new_empl income_expe health_index expectation "
reg new_empl_FL_stan treatment
outreg2 using tf1.doc, replace nolabel  bdec(3) sdec(3)

foreach var in `var' {
reg `var'_FL_stan treatment
outreg2 using tf1.doc, append nolabel  bdec(3) sdec(3)
reg `var'_FL_stan treatment `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration_FL any_children_BL D_migrant_BL
outreg2 using tf1.doc, append nolabel  bdec(3) sdec(3)
}


*******************************End*************************************************************************













*Heterogneity based on the indexed

*local  var	" currently_emplDV currently_self_emplDV currently_wageDV currently_factory empl_time6m laborhours7dw "

gen factory_exper_treat=ever_factory_BL*treatment 

*By factory experience 
local var "new_empl income_expe sternousity_index cog expectation new_empower "
reg new_empl_EL_stan factory_exper_treat treatment ever_factory_BL
outreg2 using t1.doc, replace nolabel  bdec(3) sdec(3)
foreach var in `var' {
reg `var'_EL_stan factory_exper_treat treatment ever_factory_BL
outreg2 using t1.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL_stan factory_exper_treat treatment ever_factory_BL `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration
outreg2 using t1.doc, append nolabel  bdec(3) sdec(3)
}












******************************END************************************************************************
*******************************************************************************************************
******

*Note to self (Heterogeneity and other tests to be done later)
********

* By baseline factory experience 



***********************************************************
*Table Y: Heterogeneity test using index variables 
gen rent_treat=D_rent_BL*treatment
gen married_treat=married_BL*treatment
gen migrant_treat=migrate_BL*treatment

gen factory_exper_treat=ever_factory_BL*treatment 

*By factory experience 
local var "new_empl income_expe sternousity_index cog expectation new_empower "
reg new_empl_EL_stan factory_exper_treat treatment ever_factory_BL
outreg2 using t1.doc, replace nolabel  bdec(3) sdec(3)
foreach var in `var' {
reg `var'_EL_stan factory_exper_treat treatment ever_factory_BL
outreg2 using t1.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL_stan factory_exper_treat treatment ever_factory_BL `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration
outreg2 using t1.doc, append nolabel  bdec(3) sdec(3)
}





*By rent first follow-up
local var "new_empl income_expe sternousity_index cog expectation new_empower "
reg new_empl_EL_stan rent_treat treatment D_rent_BL
outreg2 using t1.doc, replace nolabel  bdec(3) sdec(3)
foreach var in `var' {
reg `var'_EL_stan rent_treat treatment D_rent_BL
outreg2 using t1.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL_stan rent_treat treatment D_rent_BL `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration
outreg2 using t1.doc, append nolabel  bdec(3) sdec(3)
}


*By rent second follow-up
local var "new_empl income_expe sternousity_index cog expectation new_empower "
reg new_empl_FL_stan rent_treat treatment D_rent_BL
outreg2 using t1.doc, replace nolabel  bdec(3) sdec(3)
foreach var in `var' {
reg `var'_FL_stan rent_treat treatment D_rent_BL
outreg2 using t1.doc, append nolabel  bdec(3) sdec(3)
reg `var'_FL_stan rent_treat treatment D_rent_BL `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration
outreg2 using t1.doc, append nolabel  bdec(3) sdec(3)
}


*By marriage (first follow-up)
local var "new_empl income_expe sternousity_index cog expectation new_empower "
reg new_empl_EL_stan married_treat treatment married_BL
outreg2 using t4.doc, append replace  bdec(3) sdec(3)
foreach var in `var' {
reg `var'_EL_stan married_treat treatment married_BL
outreg2 using t4.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL_stan married_treat treatment married_BL `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration
outreg2 using t4.doc, append nolabel  bdec(3) sdec(3)
}

*By marriage (second follow-up)
local var "new_empl income_expe sternousity_index cog expectation new_empower "
reg new_empl_FL_stan married_treat treatment married_BL
outreg2 using t4.doc, append replace  bdec(3) sdec(3)
foreach var in `var' {
reg `var'_FL_stan married_treat treatment married_BL
outreg2 using t4.doc, append nolabel  bdec(3) sdec(3)
reg `var'_FL_stan married_treat treatment married_BL `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration
outreg2 using t4.doc, append nolabel  bdec(3) sdec(3)
}





*By migration status (first follow_up)
local var "new_empl income_expe sternousity_index cog expectation new_empower "
foreach var in `var' {
reg `var'_EL_stan migrant_treat treatment migrate_BL
outreg2 using t5.doc, append nolabel  bdec(3) sdec(3)
reg `var'_EL_stan migrant_treat treatment migrate_BL `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration
outreg2 using t5.doc, append nolabel  bdec(3) sdec(3)
}

*By migration status (secpnd follow_up)
local var "new_empl income_expe sternousity_index cog expectation new_empower "
foreach var in `var' {
reg `var'_FL_stan migrant_treat treatment migrate_BL
outreg2 using t5.doc, append nolabel  bdec(3) sdec(3)
reg `var'_FL_stan migrant_treat treatment migrate_BL `var'_BL_stan age_BL married_BL schooling_BL _W* eval_duration
outreg2 using t5.doc, append nolabel  bdec(3) sdec(3)
}


*********************************************************************
*Change in wage/earning expectations 
********************************************************************
sum s2q40e9_EL
sum s2q40e9w_EL
sum s2q40e11_EL
sum s2q40e11w_EL
sum expfacinc_BL
sum expfacincw_BL
sum expfacinc_EL
sum expfacincw_EL


local var " s2q40e9_EL s2q40e9w_EL s2q40e11_EL s2q40e11w_EL expfacinc_BL expfacincw_BL expfacinc_EL expfacincw_EL"
foreach var in `var' {
replace `var'=. if `var'<=0
}

gen starting_wage_EL=s2q40e9_EL
gen current_wage_EL=s2q40e11_EL
gen expected_wage_BL=expfacinc_BL
gen expected_wage_EL=expfacincw_EL

gen expectation_gap_BL=expected_wage_BL-starting_wage_EL
gen expectation_gap_EL=expfacincw_EL-current_wage_EL

sum expectation_gap_BL expectation_gap_EL, d
tabstat expectation_gap_BL expectation_gap_EL, stat(count mean)
tabstat expectation_gap_BL expectation_gap_EL if expectation_gap_BL>-600, stat(count mean)

*Notice outlier probably due to entry of additional zero so remove that in the graphing
*Graph: belief updating 
twoway kdensity expectation_gap_BL if expectation_gap_BL>-600  || kdensity expectation_gap_EL

twoway kdensity expectation_gap_BL if expectation_gap_BL>-600 &  currently_factory_EL==1 || kdensity expectation_gap_EL if currently_factory_EL==1

*by treatment status
twoway kdensity expectation_gap_BL if treatment==1 || kdensity expectation_gap_BL if treatment==0

twoway kdensity expectation_gap_EL if treatment==1

twoway kdensity expectation_gap_EL if treatment==0




tabstat  expectation_gap_BL expectation_gap_EL, by (currently_factory_EL)  stat(mean count) // clear evidence of narrowing expectation gap 

* How large are the gaps compared to actual wage rates
gen gap_actual_BL=starting_wage_EL/expectation_gap_BL
gen gap_actual_EL=current_wage_EL/expectation_gap_EL

tabstat  gap_actual_BL gap_actual_EL, by (currently_factory_EL)  stat(mean count) // clear evidence of narrowing expectation gap 




/*



*** Spider Graph ***

gen Health_Score=""
replace Health_Score="Walk 2km" in 1
replace Health_Score="Carry 20l" in 2
replace Health_Score="Daily activities" in 3
replace Health_Score="Work on feet" in 4
replace Health_Score="Stand at workbench" in 5
replace Health_Score="Stand up from chair" in 6
replace Health_Score="Stand up from floor" in 7
replace Health_Score="Bend down" in 8
replace Health_Score="Wipe floor" in 9
replace Health_Score="Read book" in 10

local outcome "Health_Score"

local i=1
foreach var in s2q41 s2q42 s2q43 s2q44 s2q45 s2q46 s2q47 s2q48 s2q49 s2q50 {
	gen health`i'_BL=`var'_BL
	gen health`i'_EL=`var'_EL
	recode health`i'_BL health`i'_BL (2=0)
	local i=`i'+1
}

gen zero=.
gen graphtreatment=.
gen cimax=.
gen cimin=.

local n=1
if "`outcome'"=="Health_Score" local m=10

while `n'<=`m' {

	reg health`n'_EL health`n'_BL treatment age_BL married_BL schooling_BL _W* eval_duration
	
	global cimin=_b[treatment] - invttail(e(df_r),0.025)*_se[treatment]
    global cimax=_b[treatment] + invttail(e(df_r),0.025)*_se[treatment]
    
    global treatment=_b[treatment]

    qui replace graphtreatment=$treatment in `n'
    qui replace cimax=$cimax in `n'
    qui replace cimin=$cimin in `n'
	
	replace zero=0 in `n'

    local n=`n'+1
}

drop if `outcome'==""
egen minr=rowmin(graphtreatment cimax cimin)
egen min=min(minr)
egen maxr=rowmax(graphtreatment cimax cimin)
egen max=max(maxr)
sum max
local maxhelp=r(mean)
local max=round(r(mean),0.1)
if `max'<`maxhelp' local max=`max'+0.1
sum min
local minhelp=r(mean)
local min=round(r(mean),0.1)
if `min'>`minhelp' local min=`min'+0.1

if abs(`max')>=abs(`min') local bound=`max'
if abs(`max')<abs(`min') local bound=`min'
local bound=abs(`bound')
replace min=0-`bound'
local min=0-`bound'
local max=0+`bound'


radar Health_Score graphtreatment zero cimax cimin, r(`min' `min' 0 `max') lc(red black blue blue) lp(solid solid dash dash) lw(thick medthick medium medium) labsize(*.7) note("") graphregion(color(white)) legend(label(1 ITT) label(3 95% Confidence Interval)) legend(order(1 3))





stop

   
