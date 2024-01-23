This repository contains 10 separate files that are used for the analysis of the research project. Please refer to the descriptions below for more information on each file:

1.	variable_define_and_construct.do:
•	Description: This file provides details on how variable names and measurements are constructed from the raw data.
•	Purpose: It serves as a reference guide for understanding the variables used in the analysis and their corresponding construction methods.

2.	generating_tables.do:
•	Description: This file contains the stata code for conducting analysis based on the variables defined in the model.
•	Purpose: It generates results and outputs based on the ITT-difference and ANCOVA models specified in the paper.

3.	Online_Appendix.do:
•	Description: This file includes the stata code for conducting various analysis that are presented in the online appendix part of the paper.
•	Purpose: It define the attrtion outcome at the two follow-up surveys, shows attrition results, presents the drivers of selection into interview, turnup and employment in factory jobs. It also illustrates the construction of indexs as are they    used as a robustness check for the outcome variables in the main text.

4. Figures_compile: do
•	Description: This file includes the stata code for drawing the figures in the paper as presented in the online Appendix.
•	Purpose: It reshapes the data into long fromat to generate stata figures depicting coefficients from the earnings regressions across baseline, first follow-up and four-years follow-up. The do file also generates the radar figures illustrating    drivers of adverse health impacts in the first follow-up, and a relatively benign distrution of such effects in the four-year follow-up. 

5. Attrition_weighting:do 
•	Description: This file includes the stata code for the constructing of two sets of weights: 1) attrition from the first follow-up & 2) attrition from the second follow-up.
•	Purpose: The file shows the inverse probability weights that attaches higher probability for observations that are likely to attrite (to compensate for their underreprsentaiton in the follow-up) and lower probablities for observations that are    likely to be tracked in the follow-up surveys. 

6. Turnover_describe:do 
•	Description: This do file is prepared to respond to the referees various comments on labor turnover.
•	Purpose:  

7. Qvalues_computed_stored:do
•	Description: This do file presents the q-values based on the FDR sharpened q-values.
•	Purpose:  It shows how the q-values are obtained from each of the regression equation by manually feeding the p-values from the main regression tables. 

8. Appendix_TableA2:do
•	Description: This do file presents Table A2 in the Appendix using the UEUS from the 2017 survey round and our sample.
•	Purpose:  It compares our sample with a nationally representative sample obtained from the annual survey of individuals in urban areas . 

9. Bounds:do
•	Description: This do file presents the bounding results through variou imputaitons strategies to account for attrition
•	Purpose:  It implements a series of bounding approaches to check the sensitivity of our results to various assumptions on the nature of missing data due to attrition. We focus on testing the sensitivity of our main result only―impacts on              employability and earnings at the first follow-up―to these assumptions. 

10. Using_only_first_follow_up:d0
•	Description: This do file presents estimations from all short-run impacts using the entire panel data for the baseline and first follow-up only as suggested by the referees.
•	Purpose: It shows that the balanced panel effects in the first follow-up are the same as the strongly balanced panel used for all the three periods; the impacts (overall) are the same qualitatively and that the magnitudes of estimated             coefficients are very close to the main results

Please note that the files provided in this repository are meant to be used for replication purposes and should be referenced accordingly. For any questions or clarifications, please contact the authors. Please also note that, The data underlying this article will be deposited at the World Bank's microdata library. Requests prior to storage at the library can be directed to the corresponding author, who will provide access to the anonymized data for all reasonable requests. 