*First draft July 2023
*By: Girum Abebe
**************************************************************
*This do file considers the UEUS of CSA from 2017/18 to produce tables that can characterize our sample

******************************************************************
*Summary Table
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
		 global output			"$directory\Output\WBER"
		 }
		 
		 
		
use "$directory\Analysis data\UEUS 2018.dta", 


*Key variables we want are: age, marital status, motherhood, migraiton, education (years of shcooling and current enrollment), employment status (ever employed, employed in factory, cash in four weeks, pay rent)

*Divide the sample into Addis Ababa and national 

gen csa=1 // to indicate that this is not the survey data 
tab ID101
label list ID101
gen Addis=ID101==14  // Addis Ababa
tab Addis

tab UE204
label list UE204
gen female=UE204==2

tab UE205
gen age=UE205

tab UE211
label list UE211
gen married=UE211==2

tab UE208  //highest eduction completed, need to change to years of schooling
label list UE208
gen years_schooling=UE208

replace years_schooling=9  if UE208==21 
replace years_schooling=10 if UE208==22
replace years_schooling=11 if UE208==23 | UE208==25 | UE208==26 | UE208==28
replace years_schooling=12 if UE208==23 | UE208==27 | UE208==29
replace years_schooling=13 if UE208==23 | UE208==30 | UE208==31
replace years_schooling=14 if UE208==32 
replace years_schooling=15 if  UE208==33 
replace years_schooling=16 if  UE208==34
replace years_schooling=17 if  UE208==35
replace years_schooling=0 if  UE208>=93

sum years_schooling, d

tab UE301
gen work_seven=UE301==1  // worked in the last 7 days 
tab work_seven

gen hours_seven=UE304  //number of total hours worked in the 7 days 

*Convert payments into monthly amount 
gen paid_hourly=UE314==1
gen paid_daily=UE314==2
gen paid_weekly=UE314==3
gen paid_biweekly=UE314==4
gen paid_monthly=UE314==5
gen paid_yearly=UE314==6

*There are some errors in the number of times that individuals recieved their payments, we can cap it using the maxmimum that they could recieve 
gen frequency_payment=UE315
replace frequency_payment=360 if paid_hourly==1 & UE315>360  //assuming they work 12 hours per day for 30 days
replace frequency_payment=30 if paid_daily==1 & UE315>30   // max number of days is 30
replace frequency_payment=4 if paid_weekly==1 & UE315>4  // max number of weeks is 4
replace frequency_payment=2 if paid_biweekly==1 & UE315>2 // max number of weeks is 2


gen monthly_earning=.
replace monthly_earning=frequency_payment*UE316 if paid_monthly==0 & UE314!=. 
replace monthly_earning=UE317 if paid_monthly==1

*Monthly earnings and frquency of payment of some individuals is too large, suggesting errors in that the total amount recorded in UE316 is the total monthly earning not the payment recieved in the frequency_payment variable. But since we cannnot correct for this accurately, we assign missing to these values or only focuse on the monthly earning group. 
sum monthly_earning, d

sum monthly_earning if paid_monthly==1,   // this looks more accurate 

*Currently in school/trainng 
label list UE307
gen any_training=UE307==3


gen work_six_month=UE501==1

/*
*Define ever employed
gen ever_emplDV=1 if work_seven==1 //First currently employed
replace ever_emplDV=1 if work_six_month==1



// among the unemployed those who were ever employed UE410
*/


*Comparing industrial work in Bole Lemi with Alternative employment people typically take 
*UE312 UE313 UE314 UE316
tab UE310  

*Generate sector of activities 
gen sector=.
replace  sector=1 if UE310<500  //agriculture 
replace sector=2 if UE310>=500 & UE310<1000 //mining 
replace sector=3 if UE310>1000 & UE310<=3290 //manufacturing 
replace sector=4 if UE310>3290 & UE310<3700 // repair and instalation 
replace sector=5 if  UE310>3700 & UE310<=3900 //waste managment 
replace sector=6 if  UE310>4100 & UE310<=4390 //construction 
replace sector=3 if  UE310>4500 & UE310<=4520 // manufacturing 
replace sector=7 if  UE310>4520 & UE310<=4690 // Wholesale trade
replace sector=8 if  UE310>4710 & UE310<=4792 // retail trade
replace sector=9 if  UE310>4910 & UE310<=5120 // transport 
replace sector=10 if UE310>5120 & UE310<=8413 // services  
replace sector=11 if UE310>8413 & UE310<=8430 // Defense and security   
replace sector=12 if UE310>8430 & UE310<=8550 // Education  
replace sector=13 if UE310>8610 & UE310<=8890 // Health care  
replace sector=14 if UE310>9000 & UE310<=9410 // Arts and entertainment   
replace sector=15 if UE310>9410 & UE310<=9499 // unions and political organization 
replace sector=4 if  UE310>9499 & UE310<=9529 // repair
replace sector=10 if UE310>9529 //services 

tab sector
tab UE310  if sector==10
 

label define sector 1 "Agriculture" 2 "Mining" 3 "Manufacuring" 4 "Repair and maintainence" 5 "Waste managment" 6 "Construction" 7 "Wholesale" 8 "Retail" 9 "Transport" 10 "Services" 11 "Defense and security" 12 "Education" 13 "Health care" 14 "Arts and entertainment" 15 "Unions and political organizations"
label list sector  
label values sector sector 

*Adjust variable naming and measurment to match the survey data
rename age age_BL
rename married married_BL
rename years_schooling schooling_BL
rename any_training any_training_BL
rename work_six_month work_six_month_BL 
rename work_seven work_seven_EL

gen anyearnings_4w_BL=(monthly_earning>0 & monthly_earning!=.)
gen ever_factory_BL=sector==3

*key variables: female, age,  married, years_schooling, work_seven, hours_seven, monthly_earning, paid_monthly, sector, 

tabout  Addis using "$output\New_tab1.xls" if csa==1, cells( mean female mean age_BL mean married_BL mean schooling_BL mean any_training_BL mean ever_factory_BL mean work_seven mean work_six_month mean anyearnings_4w mean csa) format (2 2) clab(Female Age Married Schooling Training Factory Sevendays Sixmonth Anyearnings CSA) sum oneway replace 

*keep Addis female age_BL married_BL schooling_BL any_training_BL ever_factory_BL anyearnings_4w_BL work_six_month_BL work_seven_EL csa

preserve
use "$directory\Analysis data\CJC_final_working.dta", clear
keep age_BL married_BL schooling_BL any_training_BL ever_factory_BL anyearnings_4w_BL  laborhours12m_BL s8q1e1_EL
gen csa=0
gen female=1
gen Addis=1
gen  work_six_month_BL=(laborhours12m_BL>0 & laborhours12m_BL!=.)
gen  work_seven_EL=(s8q1e1_EL==1 & s8q1e1_EL!=.)
tempfile survey 
save `survey.dta'
restore

append using `survey.dta'

*keep if Addis==1 // keep only Addis 
count 
exit 

tabout  csa using "$output\New_tab1.xls", cells( mean female mean age_BL mean married_BL mean schooling_BL mean any_training_BL mean ever_factory_BL mean work_seven mean work_six_month_BL mean anyearnings_4w_BL mean csa) format (2 2) clab(Female Age Married Schooling Training Factory Sevendays Sixmonth Anyearnings CSA) sum oneway replace  

tabout  csa using "$output\New_tab1.xls" if Addis==1, cells( mean female mean age_BL mean married_BL mean schooling_BL mean any_training_BL mean ever_factory_BL mean work_seven mean work_six_month_BL mean anyearnings_4w_BL mean csa) format (2 2) clab(Female Age Married Schooling Training Factory Sevendays Sixmonth Anyearnings CSA) sum oneway append // Addis only   

tabout  csa using "$output\New_tab1.xls" if Addis==1 & age_BL>=18 & age_BL<35 , cells( mean female mean age_BL mean married_BL mean schooling_BL mean any_training_BL mean ever_factory_BL mean work_seven mean work_six_month_BL mean anyearnings_4w_BL count csa) format (2 2) clab(Female Age Married Schooling Training Factory Sevendays Sixmonth Anyearnings CSA) sum oneway append // Addis only with the relevant age category 


*APpendix Tables 
*Now produce table with t-test results
asdoc sum age_BL married_BL schooling_BL any_training_BL ever_factory_BL anyearnings_4w_BL work_six_month anyearnings_4w if csa==1, stat(mean count) replace save(csa.doc)  title (summary stat)

asdoc sum age_BL married_BL schooling_BL any_training_BL ever_factory_BL anyearnings_4w_BL work_six_month anyearnings_4w if csa==1 & Addis==1, stat(mean count) append 

asdoc sum age_BL married_BL schooling_BL any_training_BL ever_factory_BL anyearnings_4w_BL work_six_month anyearnings_4w if csa==0, stat(mean) append  

asdoc sum female age_BL married_BL schooling_BL any_training_BL ever_factory_BL anyearnings_4w_BL work_six_month anyearnings_4w if csa==1 & schooling_BL<=10 & (age>=18 & age<=35) , stat(mean) append    

*T-test
preserve
keep if Addis==1
asdoc ttest female, by(csa)  cnames(Survey, CSA)   replace  
asdoc ttest age_BL, by(csa)  rowappend  
asdoc ttest married_BL, by(csa) rowappend 
asdoc ttest schooling_BL, by(csa) rowappend 
asdoc ttest any_training_BL, by(csa) rowappend 
asdoc ttest ever_factory_BL, by(csa) rowappend 
asdoc ttest work_seven, by(csa) rowappend 
asdoc ttest work_six_month, by(csa) rowappend 
asdoc ttest anyearnings_4w, by(csa) rowappend 
restore 


****************************
******Relevant age, city and education
**********************************
**# Bookmark #1
preserve
keep if Addis==1 & (schooling_BL>=5 & schooling_BL<=10) & (age>=18 & age<=35)
asdoc ttest female, by(csa)  cnames(Survey, CSA)   replace  
asdoc ttest age_BL, by(csa)  rowappend  
asdoc ttest married_BL, by(csa) rowappend 
asdoc ttest schooling_BL, by(csa) rowappend 
asdoc ttest any_training_BL, by(csa) rowappend 
asdoc ttest ever_factory_BL, by(csa) rowappend 
asdoc ttest work_seven, by(csa) rowappend 
asdoc ttest work_six_month, by(csa) rowappend 
asdoc ttest anyearnings_4w, by(csa) rowappend 
restore

********************End ***********************************************************************************

