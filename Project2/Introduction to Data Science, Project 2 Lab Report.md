

# Introduction to Data Science, Project 2 Report

**Deng Kaisheng**

[TOC]

## Code running guidance

1. Put "P02_Corporate tax.xlsx" in the code file path.
2. Put all the ACS data files in a folder called "ACS_DP02 data", and put all the csv files in the folder. Put the folder in the code file path.
3. The relative paths are like this:

![1564261944759](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564261944759.png)

Then run the code, the output files will be in the same path of the code files.

## task A

**Full code see file "task_A.R"**

* Using the variables given in the 3 equations in page 55, I trained 3 linear models, and their summaries **fit the conclusion in the paper perfectly**. 
* Based on equation 3, if a country has corporate tax rate = 20%, GDP per capita in 2000 = $10,000, and debt to GDP ratio = 35%, **the hypothetical GDP per capita growth will be 3.24%. ** 
* I used R code to plot a figure almost the same as figure 4. **See file "Figure4_using R.png"**

![1564259454162](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564259454162.png)

* Think in the Next: Why do I use corporate tax rates averaged from 2000 to 2008 instead
  of from 2000 to 2015? 
  * I think one reason is the complex interaction between the different factors.
  * **Another reason, I guess, is that there should be some time for the independent variables to take effect.**

* I used *olsrr* package to do model selection works. **The report of model selection is in file "Model_Selection_Report.csv**. In the column "predictors", the variables are the most capable to train the model are listed. We can reach the following conclusions:

  * When it comes to Adjusted-R Squared value(adjr), the most effective model uses 6 predictors: ctax, ypc2000, dty, trade, ihc, and y2000.
  * When it comes to AIC value(aic), the most "balanced"(complexity and accuracy) model uses 4 predictors: ctax, ypc2000, trade, and ihc.
  * I also visualize the comparations between these models. See file "Model_Selection_Analysis 1.png" and "Model_Selection_Analysis 2.png"

  ![1564260019849](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564260019849.png)

  ![1564260029954](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564260029954.png)

* And then **I compared the model with 4 predictors and that with 6 predictors, the results are like below**,(see file "Model_Summary_4_Predictors.png" and "Model_Summary_6_Predictors.png")

![1564260181339](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564260181339.png)

![1564260203637](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564260203637.png)

* It can be found that **in the 6-predictors' model, the p-value of Intercept, dty and y2000 are not within a acceptable range, which actually means they are not suitable in this model.**
* However, **in the 4-predictors' model, all the 4 predictors' p-value are within an acceptable range, which ensure the "mistaken probability" less than 5%**
* What's more, the increase of adjr of 6-predictors from 4-predictors are actually not very obvious. **And because of the lower AIC, I believe the 4-predictors' model will perform better than the 6-predictors' one in generalization.**
* According to the previous analysis, I personally prefer the model with 4-predictors, the equation is:

<center>ypcg = 1.499 - 0.085 * ctax - 0.0000378 * ypc2000 + 0.0066 * trade + 1.038 * ihc</center>



## task B

**Full code see file "task_B.R"**

* The output data frame is written in file "CHCI_POP.csv"
* The counties with top 10 highest chcigr is listed in file "CHCIGR_top10.csv"
* The counties with top 10 highest popgr is listed in file "POPGR_top10.csv"
* Using *ggplot()* function I plotted two figures, one with gradient color and the other with divergent color. See file "CHCI_GradientColor.png" and file "CHCI_DivergentColor.png"

![1564261016196](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564261016196.png)

![1564261024354](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564261024354.png)

* **There are three figures plotted by Tableau, and I exported all of them to PDF files. ** See file "CHCI2017 Of Mainland, United States.pdf", "CHCI2017 Of Alaska, United States.pdf" and "CHCI2017 Of Hawaii, United States.pdf", the screen cuts are like below:

![1564261120321](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564261120321.png)

![1564261242176](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564261242176.png)

![1564261291373](C:\Users\46949\AppData\Roaming\Typora\typora-user-images\1564261291373.png)

## References

[ColorBrewer: Color Advice for Maps](http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3)

[Tableau Help: Create a Simple Calculated Field](https://onlinehelp.tableau.com/current/pro/desktop/en-us/calculations_calculatedfields_formulas.htm)

[Maps in R: Plotting data points on a map](http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/)