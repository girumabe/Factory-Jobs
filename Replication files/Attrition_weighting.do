*First draft August 11 2020
*By: Girum Abebe
**************************************************************
*This do file considers only the strongly balanced panel; i.e individuals who were surveyed in all the three rounds 

**We rerun the regressions by adjusting for weights for attrition

***We use monthly dummies for the endline to make sure that we are controlling Covid-19 and pre-Covid 19 time trend differences



******************************************************************
*Impact: on balanced panel (including those who showed up at the second enline)
*******************************************************************
clear
set more off
macro drop _all

		
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
		 
		 
		
use "$directory\Analysis data\CJC_wide_all2020.dta", 

*Make sure the treatment variable is also assigned for the second endline
count  if survey_FL==1
replace treatment=_treatment if  survey_FL==1 & _treatment!=.
tab treatment if survey_FL==1
tab panel_final
*drop if panel_final==0  // drop all observations that do not appear in all the three waves


tab interview_month_FL
quietly tab interview_month_FL, gen (month_dummy) // Covid-19 period dummies

rename empl_time12m_BL empl_time6m_BL      /* this needs to be done given that the recall period between baseline and endline varies */
replace empl_time6m_BL=0 if empl_time6m_BL==. 
gen currently_factory_BL=ever_factory_BL	/* seems necessary as directly comparable variable at baseline does not exist */
gen currently_emplDV_BL=ever_emplDV_BL
gen currently_self_emplDV_BL=ever_self_emplDV_BL
gen currently_wageDV_BL=ever_wageDV_BL
gen laborhours6m_BL=laborhours12m_BL
gen harassedjobDV_BL=1
gen skillsjobDV_BL=1
gen riskyjobDV_BL=1
gen notsafejobDV_BL=1
gen disagreejobDV_BL=1
gen expensesjobDV_BL=1
gen savejobDV_BL=1
xi, prefix(_W) i.woreda_BL

*Recode baseline missing values to zero as they indicate lack of experience 
foreach i in BL {
foreach var in ever_emplDV_`i'  ever_self_emplDV_`i' ever_wageDV_`i' ever_factory_`i' currently_emplDV_`i' currently_self_emplDV_`i' currently_wageDV_`i' currently_factory_`i' empl_time6m_`i'  laborhours7dw_`i'  searchactD_`i' searchactn_`i' expinc30d_`i' expinc12m_`i'  {
replace `var'=0 if `var'==.
}
}




/*************************Construct weights for attrition\*************************************************************************
*The idea is to introduce the inverse probability weights that attaches higher probability for observations that are likely to attrite (to compensate for their underreprsentaiton in the follow-up) and lower 
probablities for observations that are likely to be tracked in the follow-up surveys. 
This would mean the attrition weight will be the inverse of the probability of being tracked in the follow-up survey (or the probability of being the panel at EL and FL)

*Construct two sets of weights: 1) attrition from the first follow-up & 2) attrition from the second follow-up
*************************************************************************/


*%%%%%%%%%%%%%%%%%%%
*Define attrition 
*%%%%%%%%%%%%%%%%%%%
tab panel
tab panel_final

*For looping purpose rename the two variable to found in the first follow_up (EL) and second follow_up (FL)
rename  panel found_EL
rename  panel_final found_FL



/************************************
*   Caculate attrition probability weights as inverse of the probability of being found
*************************************
*Blattman and Dercon suggest to use a method called "Leave One Out" estimation (LOO). This is what they say:
            If you have an observation in a sample, and you use that same observation to create
			a p-hat (predicted probability of being found), this result will be 
			biased. To counteract this, you run the logit regression on every
			observation *but* the one you are trying to predict. Consequently, 
			we the estimation in a for loop, looping through all observations, 
			creating predicted probabilities for one observation at a time. 
*/



*As a benchmark, we start with a standared way of defining the weights
xi, prefix(_W) i.woreda_BL
local control_variables_attrit "age_BL married_BL any_children_BL D_migrant_BL schooling_BL"
foreach i in EL FL {
logit found_`i' treatment `control_variables_attrit' _W*,  
predict pf_`i'
sum pf_`i', d
replace pf_`i'=r(mean) if pf_`i'==. // replace with mean if missing
}

**********************************************************
*    Generate invesrse probability weights
*********************************************************

foreach i in EL FL {
gen temp_wght_`i'=1
replace temp_wght_`i'=1/ pf_`i' 
}

*&&&&&&&&
*NOW run the Leave one out estimation loop 


*Generate probabilities of being found in the two follow_ups
gen found_prob_EL=.
gen found_prob_FL=.

// creating a vector of unique ids
gen worker_id=_n
quietly levelsof worker_id, local(participants)

	
*Start the loop
xi, prefix(_W) i.woreda_BL
local control_variables_attrit "age_BL married_BL any_children_BL D_migrant_BL schooling_BL"

local j=1
foreach i in `participants' {
	display "Round `j'"
foreach t in EL FL {
		// Run the probit regression on everything *but* the observation we are predicting. 
         cap quietly logit found_`t' treatment `control_variables_attrit' _W* if (worker_id!=`i')
		 
       // Used the estimates from the probit regression to predict p-hat for everyone
       cap predict temp_prob_`t', pr asif // asif means stata does not produce missing values

		// Input the predicted probabilities into *just* the cjc_id that the for loop has selected
		cap replace found_prob_`t' = temp_prob_`t' if (worker_id == `i')

		// Drop the temprary variable that stored probabilities for everyone. 
		cap drop temp_prob_`t'    //
		local ++j
		}
}

**********************************************************
*   Now Generate invesrse probability weights for the two follow-ups
*********************************************************
foreach i in EL FL {
gen iwght_`i'=1/ found_prob_`i'  
}

sum iwght*


lab var iwght_EL "inverse weight for predicted probablity of attrition in the first follow-up"
lab var iwght_FL "inverse weight for predicted probablity of attrition in the second follow-up"


*First and Second endline control means
foreach i in EL FL {
foreach var in  ever_factory_`i' currently_emplDV_`i' currently_self_emplDV_`i' currently_wageDV_`i' currently_factory_`i' empl_time6m_`i'  laborhours7dw_`i'  {
tabout treatment [aweight=iwght_`i'] if treatment==0 & survey_`i'==1 using "$output\new_table1_mean.xls" , cells(mean `var' sd `var') format (3 3) clab(MEAN_`var' SD_`var') sum oneway append
}
}

*go back to the orignial names for consistency 
rename found_EL panel
rename found_FL panel_final

save "$directory\Analysis data\CJC_final_working.dta", replace
