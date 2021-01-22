
***********************************
***                             ***
***         POLI 204B           ***
***           Lab 3           	***
***   Simple Hypothesis Tests   ***
***                             ***
***********************************

*** Contents ***
* One-sample Propotions and Means Tests
* Differences-in-Proportions Tests
* Differences-in-Means Tests

sysuse nlsw88, clear


*** One-sample Tests ***

* One-Sample Proportion Test *

* Was the proportion of workers in the manufacturing sector who had a college
* education different from 0.1?
// H0: The proportion of manufacturing workers with a college education is 0.1
//     (p = 0.1)
// HA: The proportion is not 0.1
//     (p != 0.1)
prtest collgrad == .1  if industry == 4  


* One-Sample Mean Test *

* Was the average number of hours worked per week by workers in the manufacturing
* sector different from 40?
// H0: average hrs = 40
// HA: average hrs != 40
ttest hours == 40  if industry == 4  



*** Two-sample Tests ***

* Difference in Proportions Test *

* Was the proportion of workers who were educated different between the South
* and the rest of the country?
// H0: p_South - p_Rest = 0
// HA: p_South - p_Rest != 0
prtest collgrad, by(south)


* Difference in Means Test *

* Was the average number of hours worked by workers in the manufacturing sector
* different from the hours worked by workers in the finance/real estate sector?
// H0: hrs_manufacturing - hrs_finance = 0
// HA: hrs_manufacturing - hrs_finance != 0
ttest hours  if industry == 4 | industry == 7, by(industry)



*** Exercise ***
sysuse nlsw88, clear

* Use that same 1988 national labor survey to answer the following 2 questions:
* (For each question, state your null and alternative hypotheses before running the test)

* 1. Were African American workers more likely to belong to a union than White workers?




* 2. Did the average hourly wage for non-college educated union members differ from
* 		the average hourly wage for non-college educated workers who were not part 
*		of a union?

