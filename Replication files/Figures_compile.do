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
		 
		 

use "$directory\Analysis data\CJC_final_working.dta", 

keep if panel_final==1
*generate baseline values for the fixed variables in the follow-up (ti use ANOCA even if the data is in long format)
rename married_BL_FL married_FL

replace age_EL=age_BL + 1
replace age_FL =age_BL + 4

gen woreda_EL=woreda_BL
replace woreda_FL=woreda_BL

gen treatment_BL=treatment
gen treatment_EL=treatment
gen treatment_FL=treatment

gen interview_month_BL=interview_month_FL
gen interview_month_EL=interview_month_FL

gen eval_duration_BL=eval_duration_EL

gen any_children_EL=any_children_BL
gen D_migrant_EL=D_migrant_BL

*Generate baseline hours and earnings for ancova 

*basline hours 
gen hours_BL=laborhours7dw_BL 
gen hours_EL=laborhours7dw_BL
gen hours_FL=laborhours7dw_BL

*Basleine earnings
gen earn_BL=earnings_4w_BL
gen earn_EL=earnings_4w_BL
gen earn_FL=earnings_4w_BL



keep cjc_id laborhours7dw_BL laborhours7dw_EL laborhours7dw_FL  earnings_4w_BL earnings_4w_EL earnings_4w_FL  iwght_EL iwght_FL  age_BL age_EL age_FL  married_BL married_EL married_FL schooling_BL schooling_EL schooling_FL interview_month_FL woreda_BL woreda_EL woreda_FL eval_duration_BL eval_duration_EL eval_duration_FL any_children_BL any_children_EL  any_children_FL D_migrant_BL D_migrant_EL D_migrant_FL treatment_BL treatment_EL treatment_FL  hours_BL hours_EL hours_FL earn_BL earn_EL earn_FL

*Rename vairables with time
rename *_BL *2016
rename *_EL *2017
rename *_FL *2020


reshape long laborhours7dw hours earnings_4w earn iwght treatment age married schooling  interview_month eval_duration woreda  any_children  D_migrant, i(cjc_id) j(year)
*/



******************** FIGURES 1 AND 2 ********************

*Construct graphs using the same model as before.

global balance " age married schooling  eval_duration any_children  D_migrant"
tab interview_month
quietly tab interview_month, gen (month_dummy) // Covid-19 period dummies
xi, prefix(_W) i.woreda

rename hours bs_laborhours7dw
rename earn  bs_earnings_4w

gen time=year
recode time (2016=1) (2017=2) (2020=3)
gen timeplot =. 
gen depvar =""
gen y_cm =. 
gen y_b =. 
gen y_cit =. 
gen y_cib =. 


local j=1
local counter =1
foreach outcome in laborhours7dw earnings_4w      {
forvalues x =1/3  {
sum `outcome' if treatment==0 & time==`x' 
replace y_cm = r(mean) in `j' 

reg `outcome' treatment bs_`outcome'  $balance  if time==`x'  
replace y_b  = _b[treatment] + y_cm    in `j' 
replace y_cit = y_b +1.645*_se[treatment]  in `j' 
replace y_cib =  y_b -1.645*_se[treatment]  in `j' 

replace timeplot = `x' in `j'
replace depvar = "`outcome'" in `j'

local j = `j'+1
}
local j = `j'+1
}

gen no = _n 
keep if _n<`j' 


drop if timeplot==.
*\replace no = no -1


local bar_details color(black)  blw(*.4)  barwidth(0.5)
local cap_details fcolor(black) lcolor(black) 

**# Bookmark #1
preserve
keep if depvar=="earnings_4w"
replace no = no-4

local bar_details color(black)  blw(*.4)  barwidth(0.5)
local cap_details fcolor(black) lcolor(black) 

twoway (bar y_cm no,  bcolor(gs12) fintensity(inten80) `bar_details' ylabel(0(500)2000 ) ymtick(0 (0.2)0.3 ) ) ///
(scatter y_b  no if treatment==1  , yaxis(1)  mcolor(blue) msymbol(diamond) ) ///
(rcap y_cit y_cib no if treatment==1 , yaxis(1)  `cap_details' )    /// 
(scatter y_b  no if treatment==0  , yaxis(1)  mcolor(red) msymbol(circle) ) ///
(rcap y_cit y_cib no if treatment==0 , yaxis(1)  `cap_details' )  ///   
, xlabel(1 "2016" 2 "2017" 3 "2020", noticks labsize(small) ) ///
xtitle("Survey Year") ytitle("Monthly earnings",  axis(1))   plotregion(fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white)) ///
title("Effect on monthly earnings over time", position(12) ring(0) alignment(top) height(3) size(medium)) ///
legend(order(1 "Control Mean" 2 "Treatment" 3 "90% CI for treatment effect") ///
cols(4) symxsize(6) rowgap(2) size(small) pos(6)) 

graph export "$directory/output/Figures/figure_1.pdf", as(pdf) replace

restore

/*
preserve 
keep if depvar=="laborhours7dw"
twoway (bar y_cm no,  bcolor(gs12) fintensity(inten80) `bar_details' ylabel(10(20)100 ) ymtick(0 (0.2)0.3 ) ) ///
(scatter y_b  no if treatment==1  , yaxis(1)  mcolor(blue) msymbol(diamond) ) ///
(rcap y_cit y_cib no if treatment==1 , yaxis(1)  `cap_details' )    /// 
(scatter y_b  no if treatment==0  , yaxis(1)  mcolor(red) msymbol(circle) ) ///
(rcap y_cit y_cib no if treatment==0 , yaxis(1)  `cap_details' )  ///   
, xlabel(1 "2016" 2 "2017" 3 "2020", noticks labsize(small) ) ///
xtitle("Survey Year") ytitle("Weekly hours of work",  axis(1))   plotregion(fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white)) ///
title("Effect on hours of work over time", position(12) ring(0) alignment(top) height(3) size(medium)) ///
legend(order(1 "Control Mean" 2 "Treatment" 3 "90% CI for treatment effect") ///
cols(4) symxsize(6) rowgap(2) size(small) pos(6)) 

graph export "$directory/output/Figures/figure_2.pdf", as(pdf) replace

restore 
*/

exit


***********************************************Online Appendix Figures**********************************
*** Spider Graph ***

****************************
*Figure A2:Health Score Radar first follow-up (2017)
************************************

*First follow-up
clear
use "$directory\Analysis data\CJC_final_working.dta", 
keep if panel_final==1


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
	gen health`i'_FL=`var'_FL
	recode health`i'_BL health`i'_BL (2=0)
	local i=`i'+1
}

gen zero=.
gen graphtreatment=.
gen cimax=.
gen cimin=.

local n=1
if "`outcome'"=="Health_Score" local m=10

preserve
while `n'<=`m' {

	reg health`n'_EL health`n'_BL treatment age_BL married_BL schooling_BL _W* eval_duration any_children_BL  D_migrant_BL
	
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


radar Health_Score graphtreatment zero cimax cimin, r(`min' `min' 0 `max') lc(red black blue blue) lp(solid solid dash dash) lw(thick medthick medium medium) labsize(*.7) note("") graphregion(color(white)) legend(label(1 ITT) label(3 95% Confidence Interval)) legend(order(1 3)) aspect(1)

graph export "$directory/output/Figures/figure_3.pdf", as(pdf) replace

exit

*********************************
*Figure A3: Health Score Radar second follow-up (2020)
********************************

*Four year Follow-up
*** Spider Graph ***
clear
use "$directory\Analysis data\CJC_final_working.dta", 


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
	gen health`i'_FL=`var'_FL
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

	reg health`n'_FL health`n'_BL treatment age_BL married_BL schooling_BL _W* eval_duration month_dummy* any_children_BL  D_migrant_BL
	
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


radar Health_Score graphtreatment zero cimax cimin, r(`min' `min' 0 `max') lc(red black blue blue) lp(solid solid dash dash) lw(thick medthick medium medium) labsize(*.7) note("") graphregion(color(white)) legend(label(1 ITT) label(3 95% Confidence Interval)) legend(order(1 3)) aspect(1)

graph export "$directory/output/Figures/figure_4.pdf", as(pdf) replace

exit


*********************
*Figure A4: Earnings comparision between factor and other jobs 
***********************

use "$datain\CJC_final_working.dta", clear 
preserve

keep cjc_id pull_id_FL earnings_4w_BL earnings_4w_EL earnings_4w_FL currently_factory_BL currently_factory_EL currently_factory_FL treatment panel_final

rename *_BL *2016
rename *_EL *2017
rename *_FL *2020

reshape long earnings_4w currently_factory, i(cjc_id) j(time)

label define factory  0 "Other employment"  1 "Factory employment", modify 
label values currently_factory factory 
tab currently_factory
	 
*Graph
graph bar earnings_4w, over (currently_factory) over(time) asyvars showyvars leg(off) scheme(s1mono) ytitle("Four weeks" "earnings in Birr")  ylabel(,labsize(small)) bargap(10)  yvaroptions(relabel(1 "Other" 2 "Factory" ) label(angle(vertical))) 

graph save "Graph" "$output\WBER\\compare_wage_final.gph", replace // this needs editing of the x axis for visualization

restore


*****************************************************
*Figure A5: Reasons for rejecting job offers for factory work
************************************************************
*Generate reasons for deciding not to take the position at Bole Lemi after the interview  
gen compensation=1 if [s2q40e14_1_FL==1 | s2q40e14_2_FL==1 | s2q40e14_3_FL==1 | s2q40e14_4_FL==2 | s2q40e14_1_FL==2 | s2q40e14_2_FL==2 | s2q40e14_3_FL==2 | s2q40e14_4_FL==1]
replace compensation=0 if compensation!=1 & s2q40e14_1_FL!=.
gen house_rent=1 if [s2q40e14_1_FL==3 | s2q40e14_2_FL==3 | s2q40e14_3_FL==3]
replace house_rent=0 if house_rent!=1 & s2q40e14_1_FL!=.
gen working_conditions=1 if inrange(s2q40e14_1_FL, 4, 6) | inrange(s2q40e14_2_FL, 4, 6) | inrange(s2q40e14_3_FL, 4, 6) | inrange(s2q40e14_4_FL, 4, 6) | inrange(s2q40e14_1_FL, 10, 12) | inrange(s2q40e14_2_FL, 10, 12) | inrange(s2q40e14_3_FL, 10, 12) | inrange(s2q40e14_4_FL, 10, 12) | s2q40e14_5_FL==10
replace working_conditions=0 if working_conditions!=1 & s2q40e14_1_FL!=.
gen health_safety=1 if [s2q40e14_1_FL==7 | s2q40e14_2_FL==7 | s2q40e14_3_FL==7 | s2q40e14_4_FL==7 | s2q40e14_5_FL==7 | s2q40e14_6_FL==7] | [s2q40e14_1_FL==13 | s2q40e14_2_FL==13 | s2q40e14_3_FL==13 | s2q40e14_4_FL==13]
replace health_safety=0 if health_safety!=1 & s2q40e14_1_FL!=.
gen transport_options=1 if [s2q40e14_1_FL==9 | s2q40e14_2_FL==9 | s2q40e14_3_FL==9 | s2q40e14_4_FL==9 |s2q40e14_6_FL==9]
replace transport_options=0 if transport_options!=1 & s2q40e14_1_FL!=.

*We merge travel time and transport into one indicators 
gen travel_time=1 if [s2q40e14_1_FL==8 | s2q40e14_2_FL==8 | s2q40e14_3_FL==8 | s2q40e14_4_FL==8 | s2q40e14_5_FL==8]
replace travel_time=0 if travel_time!=1 & s2q40e14_1_FL!=.

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

*Figure A5
local var "compensation  working_conditions travel_time transport_options  health_safety house_rent family_reason interview found_job no_need_job"
graph bar `var', asyvars showyvars leg(off) scheme(s1mono) ylab(0 "0" 0.1 "10" 0.2 "20" 0.3 "30" 0.4 "40" 0.5 "50" 0.6 "60") ytitle("Percent")  ylabel(,labsize(small)) bargap(10)  yvaroptions(relabel(1 "Compensation" 2 "Working conditions" 3 "Travel Time" 4 "Transport" 5 "Health and Safety" 6 "House rent" 7 "Family objection" 8 "Did not like the interview" 9 "Found a new job" 10 "No need for a job") label(angle(vertical))) 



************************************************************
*Figure A6: Reasons for leaving factory employment
*************************************************************
*Second where do workers go to after leaving the factories 
* We already show this in the new table of turnovers, now we look at why they leave factory jobs
**# Bookmark #10
tab currently_factory_EL 
tab currently_factory_FL
tab ever_factory_FL

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

*Figure A6
graph bar quit_dummy*, asyvars showyvars leg(off) scheme(s1mono) ylab(0 "0" 0.1 "10" 0.2 "20" 0.3 "30" ) ytitle("Percent")  ylabel(,labsize(small)) bargap(10)  yvaroptions(relabel(1 "Compensation" 2 "Job ended" 3 "To search more" 4 "Dissatisfaction with job" 5 "Fired" 6 "Illness/disability" 7 "Marriage/Pregnancy" 8 "Childcare/family" 9 "Education/training" 10 "Distance" 11 "Strenouity"  12 "Migration") label(angle(vertical))) 


**************
*Figure A7: turnover across survey waves 
**************
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
graph bar retention_3m , over(treatment)
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

stop


*********************************END**********************************************************************************
*************************************************************************************************************************











