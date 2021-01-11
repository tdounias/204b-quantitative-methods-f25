******************************
*** POLI 204b: Lab 1 (STATA) ***
******************************

*** CONTENTS ***

** Do-File
** Basic Commands
** Making new variables
** Adding labels to categorical variables
** Summary Statistics
** Graphs
** Subsetting
** Loading Data


** Do-File **

* This document is called a "do-file." 
* You can run individual lines of code by selecting them and 
* -Hitting the Do button in the top right corner
* -Pressing COMMAND + SHIFT + D (on Mac) or CTRL + D (on Windows)
* You can also run the entire document at once

** Basic Commands **

display in red "Hello, World!"
ls			   // Lists the files in the working directory
browse		   // Opens the data browser window
help		   // Opens a general help window
help regress   // Opens the help window for the "regress" command

* In STATA, you do not need to write out the full command name
he reg		   // Does the same thing as the previous line

* Let's begin by openning one of STATA's canned datasets
sysuse lifeexp, clear

* We can see the variables in the dataset listed in the right panel of the 
* main screen; we can also look at the raw data by clicking on the Data Browser
* button


** Making new variables **

* We can create new variables using the generate command
generate Ones = 1

* This generates a new variable called IncomeLevel that is equal to gnppc
generate IncomeLevel = gnppc 

* This recodes IncomeLevel to be a three-category variable
recode IncomeLevel (min/2000=1) (2001/10000=2) (10001/max=3)

* We can also recode variables like this:
generate IncomeLevel2 = . 
replace IncomeLevel2 = 1 if gnppc <= 2000
replace IncomeLevel2 = 2 if gnppc > 2000 & gnppc <= 10000
replace IncomeLevel2 = 3 if gnppc > 10000 & gnppc != .


** Adding labels to categorical variables **

* First, we have to create the label
label define income_label 1 "Low" 2 "Medium" 3 "High" 

* Then we assign that label to our variable
label values IncomeLevel income_label  


** Summary Statistics **

* The summarize command returns basic summary statistics for a variable
summarize lexp

* If we want a more detailed summary, we can specify the detail option
summarize lexp, detail

* The tabulate command creates a frequency table for a variable;
* in most cases, this is only useful if our variable is categorical
tabulate region

* If we run the tabulate command on two categorical variables at once,
* we get a "crosstab"
tabulate region IncomeLevel

* The version above reports the counts that fall into each cell; we could
* have specified percentages instead
tabulate region IncomeLevel, column nofreq
tabulate region IncomeLevel,  nofreq row


** Graphs ** 

* Boxplots *

* Boxplots are one of the simplest graphs that we will use in this course
graph box lexp

* In some cases, it is useful to create separate boxplots for different groups.
* We can do this using the "over" option.
* What do we learn about life expectancy from the following graph?
graph box lexp, over(region)

* For the graphs that you make on the homeworks, you should try to put legible and
* meaningful titles on the axes and on the graph itself
* You can make make the main title for the graph using the title option:
graph box lexp, over(region) title("Life Expectancy by Region")


* Histogram *

* Histograms give us a somewhat more-detailed idea of how the data are shaped
hist lexp, title("Histogram of Life Expectancy")

* By default, the hist command puts the "density" on the y-axis
* If we wanted frequency instead, we could specify the "frequency" option
hist lexp, frequency title("Histogram of Life Expectancy")

* We can also play around with the bin width
hist lexp, frequency w(1) title("Histogram of Life Expectancy")
hist lexp, frequency w(2) title("Histogram of Life Expectancy")
hist lexp, frequency w(10) title("Histogram of Life Expectancy")

* By default, STATA will use the variable label (if there is one) to make the
* axis label. You can change the axis label on the graph by specifying the
* xtitle or ytitle options:
hist lexp, frequency xtitle("Life Expectancy") title("Histogram of Life Expectancy")


* Stem-and-Leaf Plots *

* Stem and leaf plots are really just a special kind of histogram:
stem lexp


*** EXERCISE *** 
* Calculate the mean and standard deviation of gnppc (GNP per capita)
* Then make a boxplot and a histogram of gnppc; make sure that both
*     graphs have titles and labeled axes!



* Scatterplots *

* We can see how *two* variables covary by making a scatterplot
scatter lexp gnppc, title("Scatterplot of Income and Life Expectancy")

* We can also add a fitted line through the points
twoway (scatter lexp gnppc) (lfit lexp gnppc), title("Scatterplot of Income and Life Expectancy")



* Instead of memorizing all of the commands and functions and each of their options,
* it is a better use of your time to learn how to look stuff up when you need it.

* We can open the help page for a command by prefacing the command name with "help"
help lfit

* You can also look up functions on Stack Exchange.

*** EXERCISE *** 
* Although Scott doesn't like pie charts, make a pie chart of region anyway.
* Unless you already know which command to use, you will need to look it up.



** Subsetting **

* We can run these commands on only certain observations by using "if" plus a logical statement:
summarize lexp if IncomeLevel == 1
summarize lexp if IncomeLevel > 1 | IncomeLevel < 3, detail

* We can also run the command separately for each value of a categorical variable
* by prefacing the command with "by"
* In order for this to work, we need to sort the dataset on the grouping variable first!
sort IncomeLevel
by IncomeLevel: summarize lexp

* If there are observations in our dataset that we know we will never use, we can "drop" them
drop if IncomeLevel == . // This drops the observations that have a "missing" value for IncomeLevel

* We can "keep" just the observations that we need
keep if popgrowth > 1

* We can also drop or keep variables
drop IncomeLevel2
keep country lexp gnppc IncomeLevel 


** Loading Data **

* Let's begin by making sure that we have the correct working directory
cd "Wherever your lab 1 folder is stored"
* In my case...
cd "/Users/bertrandwilden/Documents/204b-quantitative-methods-w21/week-1"

* Let's see what we have in this folder
ls

* .dta file *
use lifeexp, clear
use "/Users/bertrandwilden/Documents/204b-quantitative-methods-w21/week-1/lifeexp.dta", clear

* We can load .dta files from the web the same way
use "http://www.stata-press.com/data/r10/auto.dta", clear

* .csv file *
insheet using lifeexp.csv, clear

* .xls file *
import excel lifeexp.xls, firstrow clear


* fixed width files *

* First, we need to download the Chile 1958 dataset from ICPSR

* The command that we will use to read in this is "infix,"
* but we will have to do some extra work in order to get it to work

* Let's start by taking a glance at the data file itself

* Now, let's open up the codebook for this dataset
* The codebook tells us the column and line where each variable is located

infix 6 lines 1: Commune 5-5 3: Votechoice 40-40 Ideology 76-76 4: Sex 49-49 using "07050-0001-Data-card_image.txt", clear

* 1958 was Chile's second general election in which women were allowed to vote
* We want to know if there was a "gender gap" in support for presidential candidates

* First, we should give more-meaningful labels to our Sex variable
label define sex_lab 1 Male 2 Female
label values Sex sex_lab

* Suffrage in Chile was still fairly restricted at the time (in theory, only the literate could vote),
* so a large share of the sample was not able to vote in this election.
* We can drop non-voters from the analysis by "keeping" only the observations that
* specified a vote choice in the survey:
keep if Votechoice <= 4

* Now we can give more meaningful labels to the vote-choice variable
label define candidate_lab  1 "Alessandri (Independent)" 2 "Allende (Socialist)" 3 "Bossay (Radical)" 4 "Frei (Christian Democrat)"
label values Votechoice candidate_lab

* Finally, we are ready to analyze the relationship between sex and vote-choice.
* We can do this by making a simple crosstab
tab  Votechoice Sex, column nofreq
* What does this table tell us about gender and political support in Chile in the late 1950s?

* Now, let's graph the average ideology by sex
graph bar (mean) Ideology if Ideology <= 3, over(Sex)


*** Other STATA Resources ***

* Check out the great tutorials posted at UCLA's IDRE:
* https://stats.idre.ucla.edu/stata/modules/
