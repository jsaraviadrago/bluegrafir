
<img src="bluegrafir.PNG" style="width:25%; margin-right: 20px" align="right">

# Bluegrafir: Package with psychometric tools

`install_github("jsaraviadrago/bluegrafir")`

`library(bluegrafir)`

## Nice tables with fit indices, reliability and item description

- grafit: This function has its name for one of my cats. It helps organize the lavaan results from CFA or SEM models. It gives a list of 3 data frames: model fit indexes, model estimators and modification indexes. 

- grafirel: This function is called for my cat. It helps organize information about reliability and organize the output of McDonald's Omega reliability index. 

- graficat: This is function is called for my cat. It helps organize information by setting a nice table of frequencies, proportions and cumulative frequencies from categories. 

## Calculation of Composite reliability and Average variance extracted (AVE)

- crel: This function calculates Composite reliability and it is based on the formulas in Raykov, T. (2004). Behavioral scale realiability and measurement invariance evaluation using latent variable modeling. Behavior therapy, 35, 299-331.

- avex: This function calculates Average variance extracted (AVE) and it is also based on Raykov's paper (Raykov, T. (2004). Behavioral scale realiability and measurement invariance evaluation using latent variable modeling. Behavior therapy, 35, 299-331).

## Calculation of linear and none linear transformations to compare multiple measuremente between groups and through time


- bluebase: This function is called for my other cat. It helps transform a vector into a more intrepetable value with a mean 500 and a standard deviation of 100. The function first standardizes with mean 0 and standard deviation 1, afterwards it makes a linear transformation the default are the values previously mentioned but it is possible to set any value.

- bluecomp: This function is called for my other cat. It helps transform a vector into a more intrepetable value with a mean 500 and a standard deviation of 100. But it uses the mean and the standard deviation of a baseline measurement you have to set. It helps compare a recent measurement against the baseline. It is commonly used to compare measurements through out the years. Both vectors need to be in the same scale in order for the transformation to have a correct interpretation.

- bluebare: it is a function to calculate percentile scaling to calculate thresholds of continous variables. It uses the cumulative frequency of the ordered raw scores and calculates standard values (Zscores). Thresholds seperate three groups, "low", which are values with 1 standard deviation below the mean, "medium" are values with a standard deviation between -1 and 1 standard deviations and "high" which are values with 1 standard deviation over the mean.  



