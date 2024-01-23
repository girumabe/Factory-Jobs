
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


