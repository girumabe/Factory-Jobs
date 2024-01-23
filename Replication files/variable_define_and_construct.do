/* -----------------------------------------------------------------------------
	
	ETHIOPIA
	CJC endline July 2020 - Data preparation and merging
	
	This version: July 14, 2020
		
----------------------------------------------------------------------------- */

		
//	----------------------------------------------------------------------------		
//	Set paths
//	----------------------------------------------------------------------------
clear
		
	* User 1: Girum
	global ga 1

	* User 2: Nik
	global nik 0
		
	if $ga {
		 global directory		"C:\Users\wb536602\OneDrive - WBG\In the Bank\Projects\2020\CJC follow up"	
		 global datain 			"$directory\Original data"
		 global output			"$directory\Output"
		 }

		use "$datain\Job_Creation_4May2020_Cleaned_1", clear // load third wave data
		
		
		
*Generate variables for analysis
count		
duplicates report cjc_id //no id duplicates
tab cjc_id
drop if participate==2 // refused to interview

gen survey=1 if pull_id!=. //surveyed in the endline in 2020
label var survey "interviwed at the endline (FL) 2020"

*assert _svy_uid==cjc_id // one contradiction reported
*display cjc_id if _svy_uid~=cjc_id


*** Interview date
gen int interview_day=day(today)

gen int interview_month_int=month(today)
gen zero=0
egen interview_month= concat(zero interview_month_int)
 
gen int Interview_year=year(today)

replace Interview_year=2020 if Interview_year==2015

*concatinate the vairable to create a vairable matching the baeline and the follow-up
egen interview_date=concat (interview_day interview_month Interview_year)
destring interview_date, replace  


   
*** Woreda dummy
rename s2q12 woreda
recode woreda (14 17 18 19 22=99) (-99 -96 -77=.)
   

*After merging I found that five firms have been assigned wrong ids (see the name and phone matching descriped in lines 580 to 600)
   
replace cjc_id="G012G1" if cjc_id== "G012G3"    
replace cjc_id="E210G2" if cjc_id== "E210G1"    
replace cjc_id="E166G2" if cjc_id== "E166G1"    
replace cjc_id="E153G3" if cjc_id== "E153G1"    
replace cjc_id="E155G1" if cjc_id== "E111G1"    

         
*Generating variables consistent wiht the earlier waves 
**********		Formatting and Generating Variables for Analysis	  **********
********** In this survey wave, however, there is a slight change in the manner with which some of the questions are asked, for example, education quesitons refer to the last six months, so we create the variable spearately, sometimes using a different name and variable label"

*** Ever employed and total number of jobs 
forvalues i=1/25 {
gen    temp`i' = (p0s9q1_`i'==1)
bysort pull_id: egen job_`i' = max(temp`i')
drop   temp`i'
}
egen total_jobs=rowtotal(job_*)
tab total_jobs
gen ever_emplDV = total_jobs>0

*bysort pull_id: egen num_empl   = sum(job_*)
rename  total_jobs num_empl
tab ever_emplDV


*Current employment [currently_emplDV currently_self_emplDV currently_wageDV currently_factory empl_time6m laborhours7dw]
forvalues i=1/25 {
gen    temp`i' = (s9q1e9_`i'==1)
bysort pull_id: egen current_empt_`i' = max(temp`i')
drop   temp`i'
}
egen total_current_jobs=rowtotal(current_empt_*)
tab total_current_jobs
gen currently_emplDV = total_current_jobs>0 
tab currently_emplDV

*current self and wage
forvalues i=1/25 {
gen    current_self_job_`i' = (s9q1e9_`i'==1) & (s9q4_`i'==1  | s9q4_`i'==3 )
gen    current_wage_job_`i' = (s9q1e9_`i'==1) & (s9q4_`i'==2  | s9q4_`i'==3 )
}

egen num_current_self=rowtotal (current_self_job*)
gen currently_self_emplDV = num_current_self>0
egen num_current_wage=rowtotal (current_wage_job*)
gen currently_wageDV = num_current_wage>0

tab2 currently_self_emplDV currently_wageDV

*rename job12 factory_job
tab job_12
gen ever_factory=job_12

gen currently_factory=s9q1e9_12==1
tab currently_factory


*** Ever self-employed and total self-employed jobs as well as wage job

forvalues i=1/19 {
gen    self_job_`i' = (s9q4_`i'==1  | s9q4_`i'==3 )
gen    wage_job_`i' = (s9q4_`i'==2  | s9q4_`i'==3 )
}

egen num_self_empl=rowtotal (self_job*)
gen ever_self_emplDV = num_self_empl>0
egen num_wage=rowtotal (wage_job*)
gen ever_wageDV = num_wage>0

tab2 ever_wageDV ever_self_emplDV



*** How long respondent has done job
*** modifying the "-22" code consistent with the baseline (the programmer used -22 to capture less than a month, which in the questionnaire is captured by "A")

forvalues i=1/25 { 
replace s9q2b_`i'=0.5 if s9q2b_`i'==-22
replace s9q2b_`i'=. if s9q2b_`i'~=-22 & s9q2b_`i'<0
}

egen job_time =rowtotal(s9q2b_*) //total months of experience 


*** Hours worked in the past 7 days
forvalues i=1/25 { 
replace s9q17_`i'=. if s9q17_`i'<0
}

egen work_days =rowtotal(s9q17_*) //Days worked in the 7 days
replace work_days=7 if work_days>7

egen hours_per_day=rowtotal(s9q18_*)
replace hours_per_day=24 if hours_per_day>7

gen laborhours7d=hours_per_day*work_days
gen laborhours7dw=laborhours7d
sum laborhours7d, d
replace laborhours7dw=r(p99) if laborhours7dw>r(p99)


*** Hours worked in the past 6 months
forvalues i=1/25 { 
replace s9q13_`i'=. if s9q13_`i'<0
replace s9q15_`i'=. if s9q15_`i'<0
replace  s9q16_`i'=. if s9q16_`i'<0
}

egen work_months =rowtotal(s9q13_*) //months worked in the 6 months
replace  work_months=6 if work_months>6

gen empl_time6m=work_months //Respondent has done job in last 6 months
gen D_empl_time6m=(work_months>0) // has worked at least for a month

egen days_month=rowtotal(s9q15_*) // days worked in a month
replace  days_month=30 if days_month>30

egen hours_day_usual=rowtotal(s9q16_*) // hours usually worked in a day
replace  hours_day_usual=24 if hours_day_usual>24

gen laborhours6m=work_months*days_month*hours_day_usual
gen laborhours6mw=laborhours6m
sum laborhours6m, d
replace laborhours6mw=r(p99) if laborhours6mw>r(p99)


*/
*** Demographics 
gen married_BL=s4q4
recode married (1 3 4 = 0) (2 = 1)
label var married_BL "Dummy for married woman - FL"

gen any_children=s4q7_1
replace any_children=1 if any_children>0 & any_children!=.
label var any_children "Any children - FL"

gen partnered=s4q5
recode partnered (2=0)
label var partnered "If unmarried, having a partner - FL"
   
* Education
gen schooling = s3q1
label var schooling "Highest grade at primary or secondary school completed - FL"
gen college_recent = (s3q30==1)
label var college_recent "Dummy for attending college/university education in the past six months FL"
gen TVET_recent = (s3q6==1)
label var TVET_recent "Dummy for finishing TVET education  attendance in the past six months FL"
gen currently_schooling=s3q2
recode currently_schooling (2=0)
label var currently_schooling "Currently in formal school - FL"

gen any_training=0
replace any_training=1 if s3q2==1 | s3q36==1 | s3q28==1 | s3q22==1 | s3q12==1
label var any_training "Currently at TVET, in training, at University or other training - FL"
   
* Dummy for migration status
gen D_migrant= .
replace  D_migrant= 1 if s2q24a!=97
replace D_migrant= 0 if s2q24a==97
label var D_migrant "Dummy for migrating to Addis Ababa - FL"
   
*** Age
gen age = s2q15a
label var age "Age in years - FL"

  
  
*** Generating relevant variables ***

*** Health score (two scores)
local var "s2q41 s2q42 s2q43 s2q44 s2q45 s2q46 s2q47 s2q48 s2q49 s2q50"
egen health_score = rowtotal(`var'), missing
egen zhealth = std (health_score)
label var zhealth "Standardized health score -FL"

foreach x of varlist `var' {
		gen `x'D=`x'
		recode `x'D (2 3 4 = 1) (1 = 0)
	}

local var "s2q41 s2q42 s2q43 s2q44 s2q45 s2q46 s2q47 s2q48 s2q49 s2q50"
egen health_scoreD=rowtotal(`var'), missing
egen zhealthD = std(health_scoreD)
label var zhealthD "Standardized health score (based on dummies ('at least with slight difficulty')) - FL"
   
   
*** Cognitive development
forvalues i=1/8 {
recode s11q`i'(2=0) //assign zero to incorrect answer
}

local var "s11q1 s11q2 s11q3 s11q4 s11q5 s11q6 s11q7 s11q8"
egen cog_score = rowtotal(`var'), missing
egen zcog_score = std (cog_score)
label var zcog_score "Standardized cognitive score (based on digit recall) - FL"


  
*** Numeracy
forvalues i=16/26 {
tempname x 
local x=(`i'-15)
rename s11q`i' numeracy`x'
local j=1
while `j'<11 {
recode numeracy`x' (2=0)
label var numeracy`x' "Numeracy question `i' - FL"
local j=`j'+1 
}
}

egen num_score = rowtotal(numeracy*), missing
egen znum_score = std(num_score)
label var znum_score "Standardized numeracy score (based and 11 true/false questions - FL"



* Travel time
recode s2q30a s2q30b (-77 -99 -88=.)
gen traveltime = (60*s2q30a) + s2q30b 
label var traveltime "Travel time to Goro roundabout in minutes - FL"

* Travel cost
recode s2q31 (-99=.)
gen travelcost = s2q31
lab var travelcost "Travel cost Goro roundabout in birr - FL"
   
   
* Rent
gen pay_rent=s4q2==1 /*273 women do not pay rent */
cap gen rent = s4q3 
lab var rent "Amount paid for occupancy of home - FL"
gen D_rent = ( s4q3 > 0 & s4q2==1)
label var D_rent "Dummy for positive rent - FL"
gen rent_pvt = ( s4q1==5)
label var rent_pvt "Dummy for renting house from private person - FL: s4q1_`i'==5"

  
  
* Dummy for continuing to search for a job
gen D_jobsearch = s8q13
recode D_jobsearch (2=0)
label var D_jobsearch "Dummy for continuing to search for a job while working at a factory - FL"


**************************************************************************************************  
  *** Earnings, savings and transfers  
  ******************************************
  
replace s5q2 = 0 if s5q1==0 /*if earnings (s5q1) are zero then savings from earnings (s5q2) would also be zero */
replace s5q4 = 0 if s5q3==0
replace s5q6 = 0 if s5q5==0
replace s5q8 = 0 if s5q7==0
gen savings_7d = s5q6 + s5q7
replace savings_7d=0 if savings_7d==. //if missing then we can fairly assume that there was no savings
label var savings_7d "Savings past 7 days - `FL'"
	
gen savings_4w = s5q2 +  s5q4
label var savings_4w "Savings past 4 weeks -FL"

gen earnings_7d = s5q5
label var earnings_7d "Cash earnings in the past 7 days - FL"
replace earnings_7d=0 if earnings_7d==.
tab earnings_7d

gen earningsW_7d= earnings_7d
sum  earningsW_7d, d
replace earningsW_7d=r(p99) if earningsW_7d> r(p99) & earningsW_7d!=0 & earningsW_7d!=.
label var earningsW_7d "Cash earnings in the past 7 days (winsorized) - FL"

gen earnings_4w = s5q1
label var earnings_4w "Cash earnings in the past 4 weeks - FL"

gen earningsW_4w=earnings_4w
sum earningsW_4w, d
replace earningsW_4w=r(p99) if earningsW_4w>r(p99) & earningsW_4w!=0 & earningsW_4w!=.
label var earningsW_4w "Cash earnings in the past 4 weeks (winsorized) - FL"

gen anyearnings_4w = s5q1
replace anyearnings_4w=1 if s5q1>0 & s5q1!=.
label var anyearnings_4w "Dummy for any cash earnings in the past 4 weeks - `FL"
	
gen transfers_7d = s5q7
label var transfers_7d "Total transfers received in the past 7 days - FL"
gen transfers_4w= s5q3
label var transfers_4w "Total transfers received in the past 4 weeks - FL"   

gen remitt_6m = s5q11
replace remitt_6m = 0 if s5q9==2
label var remitt_6m "Amount sent in remittances over the last 6 months - FL"

   
 *** Non-food and Food expenditure
 local var "s10q11 s10q13 s10q14 s10q15 s10q16 s10q17"
 foreach var in `var' {
 replace `var'=. if `var'<0
  }
  
 local var "s10q11 s10q13 s10q14 s10q15 s10q16 s10q17"
 egen nfoodexp_1m = rowtotal(`var'), missing
 label var nfoodexp_1m "Non-Food expenditure past month - FL"
 
 replace s10q12=. if s10q12<0
 rename s10q12 foodexp_7d
 label var foodexp_7d "Food expenditure past 7 days - FL"
   

  *** Search
	local j=1
	while `j'<6 {
		rename s8q`j' searchact`j'
		replace searchact`j'=. if searchact`j'<0
		gen searchact`j'D=.
		replace searchact`j'D=0 if searchact`j'==0
		replace searchact`j'D=1 if searchact`j'>0 & searchact`j'!=.
		local j=`j'+1
	}
	
	
egen searchactn = rowtotal(searchact*D)
label var searchactn "Number of job search activities conducted - FL"
	
gen searchactD = searchactn
replace searchactD=1 if searchactn>0 & searchactn!=.
label var searchactD "Any job search activities conducted - `i'"
rename s14q4 expinc30d
recode expinc30d (2=0)
rename s14q10 expinc12m
recode expinc12m (2=0)

 

 *** Decision making
 foreach v in a b c d e {
 gen D_jointdecmaker_`v' = (s6q1`v'==1|s6q1`v'==5)
 gen D_soledecmaker_`v' = (s6q1`v'==1)
	}
	
egen DM_jointscore = rowtotal(D_jointdecmaker_a D_jointdecmaker_b D_jointdecmaker_c D_jointdecmaker_d D_jointdecmaker_e  )
label var DM_jointscore "Number of decision making items repondent solely/jointly decides on - `FL"
egen DM_solescore = rowtotal(D_soledecmaker_a D_soledecmaker_b D_soledecmaker_c D_soledecmaker_d D_soledecmaker_e  )
label var DM_solescore "Number of decision making items repondent solely decides on - FL'"
   
   
   gen ownbankacc=s6q2
   recode ownbankacc (1=1) (2 3=0)
   label var ownbankacc "Owns her own bank account - FL"
   
    gen contrause=s6q6
	recode contrause (2=0)
	label var contrause "Uses any method to avoid pregnancy - `i'"

   gen empowerscore=0
   replace empowerscore=empowerscore+1 if s6q12==2
   replace empowerscore=empowerscore+1 if s6q13==1
   replace empowerscore=empowerscore+1 if s6q14==1
   replace empowerscore=empowerscore+1 if s6q15==1
   replace empowerscore=empowerscore+1 if s6q16==2
   replace empowerscore=empowerscore+1 if s6q17==2
   label var empowerscore "Empowerment score based on opinions - FL"
   
 
  *** Mobility 
   
   foreach v in a b c d {
   gen D_mobility_`v' = (s6q18`v'==1)
   }
   egen mob_score = rowtotal(D_mobility_a D_mobility_b D_mobility_c D_mobility_d )
   label var mob_score "Number of places respondent goes to without needing permission - `i'"
   
   
*** Perceptions about factory jobs 
	gen D_healthy = (s8q10==1)
	gen D_temp = (s8q11==1)
	gen D_permanent = (s8q11==2)
	gen D_steadyin = (s8q15==1)
	label var D_healthy "Dummy for respondent thinking factory jobs are healthy -`i'"
	label var D_temp "Dummy for respondent thinking factory jobs are temporary -`i'"
	label var D_permanent "Dummy for respondent thinking factory jobs are permanent -`i'"
	label var D_steadyin "Dummy for respondent thinking factory jobs provide a steady income -`i'"

	rename s2q40 expfacinc
	gen expfacincw=expfacinc
	sum expfacincw if expfacincw>0, d
	replace expfacincw=r(p99) if expfacincw>r(p99) & expfacincw!=.
	replace expfacincw=. if expfacincw<=0
	rename s8q9 factjobqual 
	
	

*** Employment in Bole Lemi
gen BLinterview=s2q38
recode BLinterview (2=0)
label var BLinterview "Ever applied or interviewed at firm in Bole Lemi - FL"

gen BLinterviewIMP=s2q38
replace BLinterviewIMP=0 if BLinterviewIMP==.
label var BLinterviewIMP "Ever applied or interviewed at firm in Bole Lemi (zeros imputed) - FL"

gen BLjobaccepted=s2q40e3
recode BLjobaccepted (2=0)
label var BLjobaccepted "Conditional on treatment, accept job and started working - EL"
gen BLjobacceptedIMP=BLjobaccepted
replace BLjobacceptedIMP=0 if BLjobacceptedIMP==. & _treatment==1
label var BLjobacceptedIMP "Conditional on treatment, accept job and started working (zeros imputed) - FL"

gen BLjobstillwork=s2q40e10
recode BLjobstillwork(2=0)
label var BLjobstillwork "Conditional on treatment and if accepted job, still working at the firm - EL"
gen BLjobstillworkIMP=BLjobstillwork
replace BLjobstillworkIMP=0 if BLjobstillworkIMP==. & _treatment==1
label var BLjobstillworkIMP "Conditional on treatment, still working at the firm (zeros imputed) - EL"


*** Shock
gen any_shock=0
foreach var of varlist s4q24 s4q25 s4q26 s4q27 s4q28 s4q29 s4q30 s4q31 {
	replace any_shock=1 if `var'==1
}

tab any_shock
  

**** Generating outcome INDICES *****

*** EMPLOYMENT Index
*gen zscores of the variables

foreach var in ever_emplDV currently_emplDV laborhours7d {
		sum `var' if _treatment==0
		gen z_`var'=(`var'-r(mean))/r(sd)
		sum z_`var' if _treatment==1
		replace z_`var'=r(mean) if _treatment==1 & z_`var'==.
		sum z_`var' if _treatment==0
		replace z_`var'=r(mean) if _treatment==0 & z_`var'==.
	}

*averaging 
	egen employment=rowmean(z_ever_emplDV z_currently_emplDV z_laborhours7d)


*** FINANCE Index
*gen zscores of the variables
	foreach var in earningsW_4w foodexp_7d nfoodexp_1m savings_4w {
		sum `var' if _treatment==0
		gen z_`var'=(`var'-r(mean))/r(sd)
		sum z_`var' if _treatment==1
		replace z_`var'=r(mean) if _treatment==1 & z_`var'==.
		sum z_`var' if _treatment==0
		replace z_`var'=r(mean) if _treatment==0 & z_`var'==.
	}
*averaging 
	egen finance=rowmean(z_earningsW_4w z_foodexp_7d z_nfoodexp_1m z_savings_4w)

/*
*** JOB QUALITY Index
*gen zscores of the variables (I'm not sure whether this is right, needs checking)

gen skillsjobDVX=1
gen riskyjobDVX=1
gen notsafejobDVX=1
gen disagreejobDVX=1
gen harassedjobDVX=1


recode  riskyjobDVX notsafejobDVX disagreejobDVX harassedjobDVX (0=1) (1=0)

	foreach var in  skillsjobDVX riskyjobDVX notsafejobDVX disagreejobDVX harassedjobDVX {
		sum `var' if _treatment==0
		gen z_`var'=(`var'-r(mean))/r(sd)
		sum z_`var' if _treatment==1
		replace z_`var'=r(mean) if _treatment==1 & z_`var'==.
		sum z_`var' if _treatment==0
		replace z_`var'=r(mean) if _treatment==0 & z_`var'==.
	}
	*averaging 
	egen jobquality=rowmean(z_skillsjobDVX z_riskyjobDVX z_notsafejobDVX z_disagreejobDVX z_harassedjobDVX)

drop skillsjobDVX riskyjobDVX notsafejobDVX disagreejobDVX harassedjobDVX 
*/


*** EMPOWERMENT Index
*gen zscores of the variables
	foreach var in ownbankacc contrause DM_jointscore mob_score empowerscore {
		sum `var' if _treatment==0
		gen z_`var'=(`var'-r(mean))/r(sd)
		sum z_`var' if _treatment==1
		replace z_`var'=r(mean) if _treatment==1 & z_`var'==.
		sum z_`var' if _treatment==0
		replace z_`var'=r(mean) if _treatment==0 & z_`var'==.
	}
	*averaging 
egen empowerment=rowmean(z_ownbankacc z_contrause z_DM_jointscore z_mob_score z_empowerscore)



*** Generate zscores of all the indexes
	foreach var in  employment finance empowerment {
		sum `var' if _treatment==0
		gen z_`var'=(`var'-r(mean))/r(sd)
		sum z_`var' if _treatment==1
		replace z_`var'=r(mean) if _treatment==1 & z_`var'==.
		sum z_`var' if _treatment==0
		replace z_`var'=r(mean) if _treatment==0 & z_`var'==.
		drop `var'
	}


label var z_finance"Finance index - FL"
label var z_employment "Employment index - FL"
label var z_empowerment "Empowerment index - FL"

*label var z_jobquality_BL "Job quality index - BL"


* Saving the dataset
save "$directory\Analysis data\CJC_clean2020.dta", replace


exit



**********************************************************************
**********Merge with the two previous survey waves**************

*Attach suffix to prepare for reshape
clear
use "$directory\Analysis data\CJC_clean2020.dta"
		
foreach x of varlist _all {
	rename `x' `x'_FL
} 

rename cjc_id_FL cjc_id


*First merge with the restericted and then with all

preserve
   use "$directory\Analysis data\CJCpanel_wide_inclusive.dta" , clear
   *use "$directory\Analysis data\CJCpanel_wide_all.dta" , clear
   tempfile previous
   save `previous'
 restore
   
merge 1:1 cjc_id using `previous'
drop _merge

recode survey_EL (2=1)
gen panel_final=1 if (survey_BL==1 & survey_EL==1 & survey_FL==1 )
replace panel_final=0 if panel_final==.
tab panel_final  //687 balanced panel


count if survey_BL==1 & survey_EL==1 // 827 in the first follow-up
count if survey_BL==1 & survey_EL==. & survey_FL==1 // 54 firms that were not in the first follow-up are interviewed in the last round in 2020
count if survey_BL==1 & survey_EL==1 & survey_FL==. // 140 firms that were in the first follow-up are not interviewed in the last round in 2020

/*
*Used to check whether there are id problems
count if survey_BL==. & survey_EL==. & survey_FL==1 // 5 observations have wrong ids.

ed cjc_id s2q4_FL _name_FL  s2q1_FL  if survey_BL==. & survey_EL==. & survey_FL==1 // Collect the phone numbers and names to match with the baseline

ed cjc_id s2q4_BL s2q1_BL if s2q4_BL==921944422     // The correct cjc_id for G012G3 is G012G1   
         
ed cjc_id s2q4_BL s2q1_BL _name_FL s2q4_FL if cjc_id=="E210G2"   // the correct cjc_id for E210G1 is E210G2, matched by name 

ed cjc_id s2q4_BL s2q1_BL _name_FL if s2q4_BL==947096355     // The correct cjc_id for E166G1 is E166G2   

ed cjc_id s2q4_BL s2q1_BL _name_FL if s2q4_BL==961106220     // The correct cjc_id for E153G1 is E153G3   

sort s2q1_BL
ed cjc_id s2q4_BL s2q1_BL _name_FL    // The correct cjc_id for E111G1 is E155G1   

*Confirm whether these ids are not already used in the baseline
count if (cjc_id=="G012G1" | cjc_id=="E210G2" | cjc_id=="E166G2" | cjc_id=="E153G3" | cjc_id=="E155G1")
*/


*First define duration between the treatment and surveys 
drop interview_date_FL
rename today_FL interview_date_FL

des interview_date_*
destring interview_date_BL, replace

gen eval_duration_EL= interview_date_EL-interview_date_BL
gen eval_duration_FL= interview_date_FL-interview_date_BL

label var eval_duration_EL "Evaluation period (number of days between first endline and baseline)"
label var eval_duration_FL "Evaluation period (number of days between second endline and baseline)"


/*
*Alternative defintion of eval_duration, we work with months since the baseline survey (I do not understand the negative eval_duration in the earlier do file).
foreach i in BL EL FL {
gen interview_temp_`i'  = real(substr(string(interview_date_BL),1,4)) 
}
// this counts the digits, and appear to be different even within a survey wave, so a simple subtraction of dates between waves would not work
drop interview_month_FL

foreach i in BL EL FL {
gen interview_month_`i'=mod(interview_date_`i', 100000) // keep the last five digits
replace  interview_month_`i'=mod(interview_date_`i', 1000000) if interview_month_`i'==2016 // keep the last five digits
gen temp_1_`i'=floor(interview_month_`i'/10000) //keep months only
tostring temp_1_`i', format(%02.0f) replace // important to add zeros to balance the digits 
gen temp_2_`i'=mod(interview_month_`i', 10000)
egen survey_month_`i'= concat(temp_2 temp_1)
drop temp_1_`i' temp_2_`i'
}

replace survey_month_EL="201612" if survey_month_EL=="201602"
replace survey_month_EL="." if survey_month_EL==".."
replace survey_month_FL="." if survey_month_FL==".."
destring survey_month_*, replace

gen eval_duration_EL=survey_month_EL-survey_month_BL
gen eval_duration_FL=survey_month_FL-survey_month_BL

label var eval_duration_EL "Evaluation period (number of days between endline and baseline)"
*/


* Saving the dataset
save "$directory\Analysis data\CJC_wide_all2020.dta", replace


