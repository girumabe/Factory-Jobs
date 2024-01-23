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


stop


******************
*Wage comparision figure
*******************











*********************************END**********************************************************************************
*************************************************************************************************************************











