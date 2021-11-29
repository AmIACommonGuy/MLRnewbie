
# MLRnewbie

<!-- badges: start -->

[![R-CMD-check](https://github.com/AmIACommonGuy/MLRnewbie/workflows/R-CMD-check/badge.svg)](https://github.com/AmIACommonGuy/MLRnewbie/actions)

[![codecov](https://codecov.io/gh/AmIACommonGuy/MLRnewbie/branch/main/graph/badge.svg?token=P9T8UX5W02)](https://codecov.io/gh/AmIACommonGuy/MLRnewbie)
<!-- badges: end -->

The goal of MLRnewbie is to provides functionality for conducting
multiple linear regression analysis. The packages contains functions
that can carry out model construction, residual calculation, and model
sum of squares summary.

## Functions

There are 3 functios:  
\* `linearRegression()` Generate linear model.  
\* `model_residuals()` Calculate linear model’s residuals, including
studentized residuals.  
\* `ssmodel()` Summary SSR, SSE, and SSY for the linear model.

## Example

``` r
## Import the library
library(MLRnewbie)

## Import data set
mc <- mtcars
```

``` r
## Multiple linear regression with intercept
## We choose "cyl", "disp","hp" in mtcars as X, and "mpg" as Y.
model_car <- linearRegression(data = mc, 
                              predictors = c("cyl", "disp","hp"),
                              response = "mpg")
#> [1] Regression coefficients: 
#>                Estimate    Std_Err t_statistics      p_value
#> (Intercept) 34.18491917 2.59077758    13.194849 1.536549e-13
#> cyl         -1.22741994 0.79727631    -1.539516 1.349044e-01
#> disp        -0.01883809 0.01040369    -1.810711 8.092901e-02
#> hp          -0.01467933 0.01465087    -1.001943 3.249519e-01
#> 
#> [1] Multiple R squared: 
#> [1] 0.7678877
```

``` r
## Calculating useful residuals. From the left to right, they are Residual, Std_Residual, and Studentized_Residual.
head(model_residuals(model_car))
#>                    Residual Std_Residual Studentized_Residual
#> Mazda RX4         -1.191579   -0.3900089           -0.4074351
#> Mazda RX4 Wag     -1.191579   -0.3900089           -0.4074351
#> Datsun 710        -3.075548   -1.0066400           -1.0533824
#> Hornet 4 Drive     1.054553    0.3451598            0.3593604
#> Hornet Sportabout  3.685035    1.2061276            1.2644356
#> Valiant           -2.940500   -0.9624383           -0.9940011
```

``` r
## All three sum of squares of the linear regression model
ssmodel(model_car)
#>     Sum_of_Square degree_freedom
#> SSR      864.6778              3
#> SSE      261.3694             28
#> SST     1126.0472             31
```
