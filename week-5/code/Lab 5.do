********************************
**** 		 Lab 5:			****
**** Displaying Regression 	****
****   Results in STATA!	****
********************************

sysuse nlsw88

regress wage union  // Runs our regression
est sto m1			// Stores the regression output as m1
regress wage union collgrad
est sto m2
regress wage union collgrad tenure south c_city
est sto m3

* Now we have all of these regression results stored in memory
* (And we can go back to them if we need to...)
estimates dir


* A regression table that you can copy and paste into word
esttab m1 m2 m3, label se nonumber title("Effect of Union Membership on Wage") r2

* A regression table in Latex format
esttab m1 m2 m3 using examplelab5.tex, label se nonumber title("Effect of Union Membership on Wage") r2 replace 


