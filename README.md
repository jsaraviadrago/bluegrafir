### Package to calculate composite reliability and AVE

This package has 4 functions. One function calculates the Composite reliability and another Average variance extracted (AVE). The other 2 transform lavaan results in tidy tables. 

- crel: This function calculates Composite reliability and it is based on the formulas in Raykov, T. (2004). Behavioral scale realiability and measurement invariance evaluation using latent variable modeling. Behavior therapy, 35, 299-331.

- ave: This function calculates Average variance extracted (AVE) and it is also based on Raykov's paper (Raykov, T. (2004). Behavioral scale realiability and measurement invariance evaluation using latent variable modeling. Behavior therapy, 35, 299-331).

- grafi: This function has its name for one of my cats. It helps organize the lavaan results from CFA or SEM models. It gives a list of 3 data frames: model fit indexes, model estimators and modification indexes. 

- grafi2: This function is called for my cat. It helps organize information about reliability and organize the output of McDonald's Omega reliability index. 

- blue: This function is called for my other cat. It helps transform a vector into a more intrepetable value with a mean 500 and a standard deviation of 100. The function first standardizes with mean 0 and standard deviation 1, afterwards it makes a linear transformation the default are the values previously mentioned but it is possible to set any value.

- blue2: This function is called for my other cat. It helps transform a vector into a more intrepetable value with a mean 500 and a standard deviation of 100. But it uses the mean and the standard deviation of a baseline measurement you have to set. It helps compare a recent measurement against the baseline. It is commonly used to compare measurements through out the years. Both vectors need to be in the same scale in order for the transformation to have a correct interpretation. 



