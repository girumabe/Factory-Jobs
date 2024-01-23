
***** Examples of the Different Multiple Hypothesis Test Commands in Stata  ************

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
		 global Output			"$directory\Output\WBER\Q_values"
		 }
		 

use "$directory\Analysis data\CJC_final_working.dta", 

keep if panel_final==1

*************************************************************************************************
*Table 2 in the paper (with first endline-2017)
*******************************************************************************************************
*--------------------

*Outcome variables (seven variables in one family and only one treatment)
local  var	ever_factory_EL currently_emplDV_EL currently_self_emplDV_EL currently_wageDV_EL currently_factory_EL empl_time6m_EL  laborhours7dw_EL 


******************************************************
*** A regression with 7 outcomes and 1 treatments ****
******************************************************

*First without controls 

eststo clear
reg ever_factory_EL treatment [pweight=iwght_EL]
eststo table2_1
reg currently_emplDV_EL treatment [pweight=iwght_EL]
eststo table2_2
reg currently_self_emplDV_EL treatment [pweight=iwght_EL]
eststo table2_3
reg currently_wageDV_EL treatment [pweight=iwght_EL]
eststo table2_4
reg currently_factory_EL treatment [pweight=iwght_EL]
eststo table2_5
reg empl_time6m_EL treatment [pweight=iwght_EL]
eststo table2_6
reg laborhours7dw_EL treatment [pweight=iwght_EL]
eststo table2_7

#delimit ;
esttab table2_*  using "$Output/MHT_2.csv", replace depvar legend label nonumbers
	b(%9.3f) p star(* 0.10 ** 0.05 *** 0.01) nogaps drop(_cons)
	stats( N , fmt( %9.0g) labels( "Sample Size")) 
	title("Impacts") addnotes("""") ;
#delimit cr


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(7,3,.)  // create a matrix y with seven outcomes and 1 treatment 7 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)7 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)7 {
mat y[`j',2]=1
}


*prepare outcome variable for looping

gen Y1=ever_factory_EL
gen Y2=currently_emplDV_EL
gen Y3=currently_self_emplDV_EL
gen Y4=currently_wageDV_EL
gen Y5=currently_factory_EL
gen Y6=empl_time6m_EL
gen Y7=laborhours7dw_EL

local i=1
foreach var of varlist Y1 Y2 Y3 Y4 Y5 Y6 Y7 {
	reg `var' treatment [pweight=iwght_EL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_2.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_2.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_2.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_2.dta", replace

restore

exit



**********
*Table 2 with ITT ANCOVA (with a control)
*************

*Outcome variables (seven variables in one family and only one treatment)
local  var	ever_factory_EL currently_emplDV_EL currently_self_emplDV_EL currently_wageDV_EL currently_factory_EL empl_time6m_EL  laborhours7dw_EL 


******************************************************
*** A regression with 7 outcomes and 1 treatments ****
******************************************************

*With controls 

xi, prefix(_W) i.woreda_BL //woreda dummies 

global indep "age_BL married_BL schooling_BL eval_duration"
eststo clear
reg ever_factory_EL treatment  ever_factory_BL _W* $indep [pweight=iwght_EL]
eststo table2_1
reg currently_emplDV_EL  treatment currently_emplDV_BL  _W* $indep [pweight=iwght_EL]
eststo table2_2
reg currently_self_emplDV_EL treatment currently_self_emplDV_BL _W* $indep [pweight=iwght_EL]
eststo table2_3
reg currently_wageDV_EL  treatment currently_wageDV_BL _W* $indep [pweight=iwght_EL]
eststo table2_4
reg currently_factory_EL treatment currently_factory_BL _W* $indep [pweight=iwght_EL]
eststo table2_5
reg empl_time6m_EL treatment empl_time6m_BL _W* $indep [pweight=iwght_EL]
eststo table2_6
reg laborhours7dw_EL treatment laborhours7dw_BL _W* $indep [pweight=iwght_EL]
eststo table2_7

#delimit ;
esttab table2_*  using "$Output/MHT_2.csv", replace depvar legend label nonumbers
	b(%9.3f) p star(* 0.10 ** 0.05 *** 0.01) nogaps drop(_cons)
	stats( N , fmt( %9.0g) labels( "Sample Size")) 
	title("Impacts") addnotes("""") ;
#delimit cr


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(7,3,.)  // create a matrix y with seven outcomes and 1 treatment 7 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (seven outcomes)
forvalues j=1(1)7 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)7 {
mat y[`j',2]=1
}


*prepare baseline outcome variable for looping

gen b_Y1=ever_factory_BL
gen b_Y2=currently_emplDV_BL
gen b_Y3=currently_self_emplDV_BL
gen b_Y4=currently_wageDV_BL
gen b_Y5=currently_factory_BL
gen b_Y6=empl_time6m_BL
gen b_Y7=laborhours7dw_BL


local i=1
foreach var of varlist Y1 Y2 Y3 Y4 Y5 Y6 Y7 {
	reg `var' treatment b_`var' _W* $indep [pweight=iwght_EL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_2b.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_2b.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_2b.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_2b.dta", replace

restore

exit

drop Y1 Y2 Y3 Y4 Y5 Y6 Y7

*-----------------------------------------

*************************************************************************************************
*Table 2 in the paper (with second endline-2020)
*******************************************************************************************************
*--------------------


*Replace all _EL above by _FL

*Outcome variables (six variables in one family and only one treatment)
local  var	ever_factory_FL currently_emplDV_FL currently_self_emplDV_FL currently_wageDV_FL currently_factory_FL empl_time6m_FL  laborhours7dw_FL 


******************************************************
*** A regression with 7 outcomes and 1 treatments ****
******************************************************

*First without controls 

eststo clear
reg ever_factory_FL treatment [pweight=iwght_FL]
eststo table2_1
reg currently_emplDV_FL treatment [pweight=iwght_FL]
eststo table2_2
reg currently_self_emplDV_FL treatment [pweight=iwght_FL]
eststo table2_3
reg currently_wageDV_FL treatment [pweight=iwght_FL]
eststo table2_4
reg currently_factory_FL treatment [pweight=iwght_FL]
eststo table2_5
reg empl_time6m_FL treatment [pweight=iwght_FL]
eststo table2_6
reg laborhours7dw_FL treatment [pweight=iwght_FL]
eststo table2_7

#delimit ;
esttab table2_*  using "$Output/MHT_2.csv", replace depvar legend label nonumbers
	b(%9.3f) p star(* 0.10 ** 0.05 *** 0.01) nogaps drop(_cons)
	stats( N , fmt( %9.0g) labels( "Sample Size")) 
	title("Impacts") addnotes("""") ;
#delimit cr


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(7,3,.)  // create a matrix y with seven outcomes and 1 treatment 7 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)7 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)7 {
mat y[`j',2]=1
}


*prepare outcome variable for looping

gen Y1=ever_factory_FL
gen Y2=currently_emplDV_FL
gen Y3=currently_self_emplDV_FL
gen Y4=currently_wageDV_FL
gen Y5=currently_factory_FL
gen Y6=empl_time6m_FL
gen Y7=laborhours7dw_FL


local i=1
foreach var of varlist Y1 Y2 Y3 Y4 Y5 Y6 Y7 {
	reg `var' treatment [pweight=iwght_FL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_2c.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_2c.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_2c.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_2c.dta", replace

restore

exit 


**********
*Table 2 with ITT ANCOVA (with a control)
*************

*Outcome variables (six variables in one family and only one treatment)
local  var	ever_factory_FL currently_emplDV_FL currently_self_emplDV_FL currently_wageDV_FL currently_factory_FL empl_time6m_FL  laborhours7dw_FL 


******************************************************
*** A regression with 7 outcomes and 1 treatments ****
******************************************************

*With controls 

xi, prefix(_W) i.woreda_BL //woreda dummies 

global indep "age_BL married_BL schooling_BL eval_duration_FL"
eststo clear
reg ever_factory_FL treatment  ever_factory_BL _W* month_dummy*  $indep [pweight=iwght_FL]
eststo table2_1
reg currently_emplDV_FL  treatment currently_emplDV_BL  _W* month_dummy*  $indep [pweight=iwght_FL]
eststo table2_2
reg currently_self_emplDV_FL treatment currently_self_emplDV_BL _W* month_dummy*  $indep [pweight=iwght_FL]
eststo table2_3
reg currently_wageDV_FL  treatment currently_wageDV_BL _W* month_dummy*  $indep [pweight=iwght_FL]
eststo table2_4
reg currently_factory_FL treatment currently_factory_BL _W* month_dummy*  $indep [pweight=iwght_FL]
eststo table2_5
reg empl_time6m_FL treatment empl_time6m_BL _W* month_dummy*  $indep [pweight=iwght_FL]
eststo table2_6
reg laborhours7dw_FL treatment laborhours7dw_BL _W* month_dummy*  $indep [pweight=iwght_FL]
eststo table2_7

#delimit ;
esttab table2_*  using "$Output/MHT_2.csv", replace depvar legend label nonumbers
	b(%9.3f) p star(* 0.10 ** 0.05 *** 0.01) nogaps drop(_cons)
	stats( N , fmt( %9.0g) labels( "Sample Size")) 
	title("Impacts") addnotes("""") ;
#delimit cr


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(7,3,.)  // create a matrix y with seven outcomes and 1 treatment 7 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (seven outcomes)
forvalues j=1(1)7 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)7 {
mat y[`j',2]=1
}

/*

*prepare baseline variable for looping

gen b_Y1=ever_factory_BL
gen b_Y2=currently_emplDV_BL
gen b_Y3=currently_self_emplDV_BL
gen b_Y4=currently_wageDV_BL
gen b_Y5=currently_factory_BL
gen b_Y6=empl_time6m_BL
gen b_Y7=laborhours7dw_BL
*/

local i=1
foreach var of varlist Y1 Y2 Y3 Y4 Y5 Y6 Y7 {
	reg `var' treatment b_`var' _W* month_dummy* $indep [pweight=iwght_FL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_2d.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_2d.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_2d.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_2d.dta", replace

restore


drop Y1 Y2 Y3 Y4 Y5 Y6 Y7


exit 


*********************************************
*     Table 3: Impacts on income, expenditures and savings
***************************************

*******************************************************
*                    3a, First follow_up and without the baseline controls 
***********************************************


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

*** Save p-values and get them in a data file to use 
mat y = J(4,3,.)  // create a matrix y with four outcomes and 1 treatment 4 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (four outcomes)
forvalues j=1(1)4 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)4 {
mat y[`j',2]=1
}

*prepare outcome variable for looping

gen Y1=earnings_4w_EL
gen Y2=foodexp_7d_EL
gen Y3=nfoodexp_1m_EL
gen Y4=savings_4w_EL


local i=1
foreach var of varlist Y1 Y2 Y3 Y4  {
	reg `var' treatment [pweight=iwght_EL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_3a.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_3a.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_3a.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_3a.dta", replace

restore

exit


drop b_Y1 b_Y2 b_Y3 b_Y4 b_Y5 b_Y6 b_Y7



*****************************
*Table 3: First follow-up with controls 
**************************************
*First follow-up


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

*** Save p-values and get them in a data file to use 
mat y = J(4,3,.)  // create a matrix y with four outcomes and 1 treatment 4 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)4 {
mat y[`j',1]=`j'
}



gen b_Y1=earnings_4w_BL
gen b_Y2=foodexp_7d_BL
gen b_Y3=nfoodexp_1m_BL
gen b_Y4=savings_4w_BL



* Treatment (only one treatment)
forvalues j=1(1)4 {
mat y[`j',2]=1
}

/*

*prepare outcome variable for looping

gen Y1=earnings_4w_EL
gen Y2=foodexp_7d_EL
gen Y3=nfoodexp_1m_EL
gen Y4=savings_4w_EL
*/

local i=1
foreach var of varlist Y1 Y2 Y3 Y4  {
	reg `var' treatment b_`var' _W* month_dummy* $indep [pweight=iwght_EL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_3b.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_3b.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_3b.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_3b.dta", replace

restore

exit

drop b_Y1 b_Y2 b_Y3 b_Y4 
 


*************************************************************************************************
*Table 4 in the paper (with first endline-2017)
*In this table we have two panels representing two families of outcomes 
              *The first is health realted outcomes, which contains strenousity score and respondents assement of factory worker as unhealthy 
              *The second is perceptions and expectations, which includes perceived job quality, expected income from factory job, whether factory job is a source of steady income, and whether factory job is permanent postion 
*******************************************************************************************************
*--------------------


*First reverse the health dummy so that it is consistent with the standaredized strneousity scores

foreach i in BL EL FL {
gen bad_health_`i'=.
replace bad_health_`i'=1 if D_healthy_`i'==0
replace bad_health_`i'=0 if D_healthy_`i'==1
label var bad_health_`i' "Perceives factory work as unhealthy_`i'"
}

*First family : zhealth  bad_health

******************************************************
*** A regression with 2 outcomes and 1 treatments ****
******************************************************

eststo clear
reg zhealth_EL treatment [pweight=iwght_EL]
eststo table4_1
reg bad_health_EL treatment [pweight=iwght_EL]
eststo table4_2

#delimit ;
esttab table4_*  using "$Output/MHT_4.csv", replace depvar legend label nonumbers
	b(%9.3f) p star(* 0.10 ** 0.05 *** 0.01) nogaps drop(_cons)
	stats( N , fmt( %9.0g) labels( "Sample Size")) 
	title("Impacts") addnotes("""") ;
#delimit cr



*********************************************
*     Table 4: Impacts on health
***************************************

*******************************************************
*    4a, First follow_up and without the baseline controls 
***********************************************


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(2,3,.)  // create a matrix y with two outcomes and 1 treatment 2 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)2 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)2 {
mat y[`j',2]=1
}

*prepare outcome variable for looping

gen Y1=zhealth_EL
gen Y2=bad_health_EL

local i=1
foreach var of varlist Y1 Y2  {
	reg `var' treatment [pweight=iwght_EL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_4a.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_4a.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_4a.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_4a.dta", replace

restore

exit

drop Y1 Y2 

******************************
*First follow-up but Now with the control  (4b)
********************************

*First family : zhealth  bad_health


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(2,3,.)  // create a matrix y with two outcomes and 1 treatment 2 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)2 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)2 {
mat y[`j',2]=1
}

*prepare outcome variable for looping

gen b_Y1=zhealth_BL
gen b_Y2=bad_health_BL

  

global indep "age_BL married_BL schooling_BL eval_duration_EL any_children_BL D_migrant_BL"
xi, prefix(_W) i.woreda_BL //woreda dummies 


local i=1
foreach var of varlist Y1 Y2  {
	reg `var' treatment b_`var' _W* month_dummy* $indep [pweight=iwght_EL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_4b.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_4b.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_4b.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_4b.dta", replace

restore

exit



*******************************************************
*    4C, Second follow_up and without the baseline controls 
***********************************************

*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(2,3,.)  // create a matrix y with two outcomes and 1 treatment 2 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)2 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)2 {
mat y[`j',2]=1
}

*prepare outcome variable for looping
drop Y1 Y2

gen Y1=zhealth_FL
gen Y2=bad_health_FL

local i=1
foreach var of varlist Y1 Y2  {
	reg `var' treatment [pweight=iwght_FL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_4c.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_4c.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_4c.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_4c.dta", replace

restore

exit


******************************
*Second follow-up but Now with the control 
********************************

*First family : zhealth  bad_health


*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(2,3,.)  // create a matrix y with two outcomes and 1 treatment 2 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)2 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)2 {
mat y[`j',2]=1
}

*prepare outcome variable for looping

/*
gen b_Y1=zhealth_BL
gen b_Y2=bad_health_BL
*/

  

global indep "age_BL married_BL schooling_BL eval_duration_FL any_children_BL D_migrant_BL"
xi, prefix(_W) i.woreda_BL //woreda dummies 

local i=1
foreach var of varlist Y1 Y2  {
	reg `var' treatment b_`var' _W* month_dummy* $indep [pweight=iwght_FL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_4d.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_4d.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_4d.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_4d.dta", replace

restore

exit


*************************************************************************************************************************

*Perception and Expectation: Second Panel of Table 4 (Let us label this as Table 5 for simplicity throughout our coding)

*************************************************************************************************************************



*Second family of outcomes include: factjobqual expfacinc D_steadyin D_permanent

******************************************************
*** A regression with 4 outcomes and 1 treatments ****
******************************************************

eststo clear
reg factjobqual_EL treatment [pweight=iwght_EL]
eststo table5_1
reg expfacinc_EL treatment [pweight=iwght_EL]
eststo table5_2
reg D_steadyin_EL treatment [pweight=iwght_EL]
eststo table4_1
reg D_permanent_EL treatment [pweight=iwght_EL]
eststo table4_2


#delimit ;
esttab table5_*  using "$Output/MHT_5.csv", replace depvar legend label nonumbers
	b(%9.3f) p star(* 0.10 ** 0.05 *** 0.01) nogaps drop(_cons)
	stats( N , fmt( %9.0g) labels( "Sample Size")) 
	title("Impacts") addnotes("""") ;
#delimit cr



*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************

*** Save p-values and get them in a data file to use 
mat y = J(4,3,.)  // create a matrix y with two outcomes and 1 treatment 2 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)4 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)4 {
mat y[`j',2]=1
}

*prepare outcome variable for looping
drop Y1 Y2  // drop the health measures

gen Y1=factjobqual_EL
gen Y2=expfacinc_EL
gen Y3=D_steadyin_EL
gen Y4=D_permanent_EL

local i=1
foreach var of varlist Y1 Y2 Y3 Y4  {
	reg `var' treatment [pweight=iwght_EL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_5a.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_5a.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_5a.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_5a.dta", replace

restore

exit


******************************
*First follow-up but Now with the control 
********************************

*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(4,3,.)  // create a matrix y with two outcomes and 1 treatment 2 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)4 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)4 {
mat y[`j',2]=1
}

*prepare outcome variable for looping, first drop the health baseline
drop b_Y1 b_Y2

gen b_Y1=factjobqual_BL
gen b_Y2=expfacinc_BL
gen b_Y3=D_steadyin_BL
gen b_Y4=D_permanent_BL
  

global indep "age_BL married_BL schooling_BL eval_duration_EL any_children_BL D_migrant_BL"
xi, prefix(_W) i.woreda_BL //woreda dummies 


local i=1
foreach var of varlist Y1 Y2 Y3 Y4 {
	reg `var' treatment b_`var' _W* month_dummy* $indep [pweight=iwght_EL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_5b.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_5b.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_5b.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_5b.dta", replace

restore

exit



*******************************************************
*    5C, Second follow_up and without the baseline controls 
***********************************************
******************************************************
*** A regression with 4 outcomes and 1 treatments ****
******************************************************

*First without control 
eststo clear
reg factjobqual_FL treatment [pweight=iwght_FL]
eststo table5_1
reg expfacinc_FL treatment [pweight=iwght_FL]
eststo table5_2
reg D_steadyin_FL treatment [pweight=iwght_FL]
eststo table4_1
reg D_permanent_FL treatment [pweight=iwght_FL]
eststo table4_2


#delimit ;
esttab table5_*  using "$Output/MHT_5.csv", replace depvar legend label nonumbers
	b(%9.3f) p star(* 0.10 ** 0.05 *** 0.01) nogaps drop(_cons)
	stats( N , fmt( %9.0g) labels( "Sample Size")) 
	title("Impacts") addnotes("""") ;
#delimit cr



*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************

*** Save p-values and get them in a data file to use 
mat y = J(4,3,.)  // create a matrix y with two outcomes and 1 treatment 2 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)4 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)4 {
mat y[`j',2]=1
}

*prepare outcome variable for looping
drop Y1 Y2 Y3 Y4 // drop the first follow-up measures and generate the second follow-up measures 

gen Y1=factjobqual_FL
gen Y2=expfacinc_FL
gen Y3=D_steadyin_FL
gen Y4=D_permanent_FL

local i=1
foreach var of varlist Y1 Y2 Y3 Y4  {
	reg `var' treatment [pweight=iwght_FL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_5c.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_5c.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_5c.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_5c.dta", replace

restore

exit


******************************
*Second follow-up but Now with the control 
********************************

*************************************************************************************
***** Sharpened q-values ************************************************************
*************************************************************************************

***** Example of how to get the FDR sharpened q-values **************
*outcome 1 has no controls, so create constant variable to make code easier
*gen b_Y1=0

*** Save p-values and get them in a data file to use 
mat y = J(4,3,.)  // create a matrix y with two outcomes and 1 treatment 2 (rows) by 3 (columns) matrix)

* Populate Outcome and Treatment 

* Outcome (six outcomes)
forvalues j=1(1)4 {
mat y[`j',1]=`j'
}


* Treatment (only one treatment)
forvalues j=1(1)4 {
mat y[`j',2]=1
}

/*
*prepare outcome variable for looping
factjobqual expfacinc D_steadyin D_permanent
gen b_Y1=factjobqual_BL
gen b_Y2=expfacinc_BL
gen b_Y3=D_steadyin_BL
gen b_Y4=D_permanent_BL
 */

global indep "age_BL married_BL schooling_BL eval_duration_FL any_children_BL D_migrant_BL"
xi, prefix(_W) i.woreda_BL //woreda dummies 


local i=1
foreach var of varlist Y1 Y2 Y3 Y4 {
	reg `var' treatment b_`var' _W* month_dummy* $indep [pweight=iwght_FL]
	test treatment=0
	mat y[`i',3]=r(p)
	local i=`i'+1
}


mat colnames y = "Outcome" "Treatment" "p-value" 
mat2txt, matrix(y) saving("$Output/Tablepvals_5d.xls") replace
preserve
drop _all
svmat double y
rename y1 outcome
rename y2 treatment
rename y3 pval
save "$Output/Tablepvals_5d.dta", replace
restore


**** Now use Michael Anderson's code for sharpened q-values
preserve

use "$Output/Tablepvals_5d.dta", clear
version 10
set more off

* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BKY (2006) sharpened q-values

gen bky06_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.


while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank1 = reject_temp1*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected1 = max(reject_rank1)

	* Second Stage
	* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
	local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
	* Generate value q_2st*r/M
	gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q_2st*r/M
	gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
}
	

quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
display	"Sorting order is the same as the original vector of p-values"

keep outcome treatment pval bky06_qval
save "$Output/sharpenedqvals_5d.dta", replace

restore

exit


*----------------------------------End-----------------------------------------------------



