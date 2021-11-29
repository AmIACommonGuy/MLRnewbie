testthat::test_that("model w interaction", {
    #Generate the independent variable and the error
    x1=rnorm(50000,100,16)
    x2=rnorm(50000,200,36)
    x3=rnorm(50000,300,25)
    error=rnorm(50000,0,25)
    #Generate the dependent variable (b0=150, b1=-4, b2=2.5)
    y1=200-(5*x1)+(10*x2)+x3+error

    testdata <- as.data.frame(cbind(y1,x1,x2,x3))

    ## The regression function in MLRnewbie
    model_nb <- linearRegression(data = testdata,
                                 predictors = c("x1","x2","x3"),
                                 response = "y1")

    ## The base regression function
    model_base <- lm(data=testdata, formula =  y1~x1+x2+x3)

    testthat::expect_equal(round(model_nb[[1]][,1], digits=10),
                           round(model_base$coefficients, digits=10))
})

testthat::test_that("model wo interaction", {
    #Generate the independent variable and the error
    x1=rnorm(50000,100,16)
    x2=rnorm(50000,200,36)
    x3=rnorm(50000,300,25)
    error=rnorm(50000,0,25)
    #Generate the dependent variable (b0=150, b1=-4, b2=2.5)
    y1=200-(5*x1)+(10*x2)+x3+error

    testdata <- as.data.frame(cbind(y1,x1,x2,x3))

    ## The regression function in MLRneebie
    model_nb <- linearRegression(data = testdata,
                                 predictors = c("x1","x2","x3"),
                                 response = "y1",
                                 FALSE)

    ## The base regression function
    model_base <- lm(data=testdata, formula =  y1~-1+x1+x2+x3)

    testthat::expect_equal(round(model_nb[[1]][,1], digits=10),
                           round(model_base$coefficients, digits=10))
})

testthat::test_that("resNss", {
    #Generate the independent variable and the error
    x1=rnorm(50000,100,16)
    x2=rnorm(50000,200,36)
    x3=rnorm(50000,300,25)
    error=rnorm(50000,0,25)
    #Generate the dependent variable (b0=150, b1=-4, b2=2.5)
    y1=200-(5*x1)+(10*x2)+x3+error

    testdata <- as.data.frame(cbind(y1,x1,x2,x3))

    ## The regression function in MLRneebie
    model_nb <- linearRegression(data = testdata,
                                 predictors = c("x1","x2","x3"),
                                 response = "y1",
                                 FALSE)
    ## The base regression function
    model_base <- lm(data=testdata, formula =  y1~-1+x1+x2+x3)

    anr <- c(sum(anova(model_base)$"Sum Sq"[-4]),
             anova(model_base)$"Sum Sq"[4],
             anova(model_base)$"Sum Sq"[4]+sum(anova(model_base)$"Sum Sq"[-4]))
    ssmr <- unname(ssmodel(model_nb)[,1])
    testthat::expect_equal(round(anr, digits = 10),
                           round(ssmr, digits = 10))
})

testthat::test_that("resNss", {
    #Generate the independent variable and the error
    x1=rnorm(50000,100,16)
    x2=rnorm(50000,200,36)
    x3=rnorm(50000,300,25)
    error=rnorm(50000,0,25)
    #Generate the dependent variable (b0=150, b1=-4, b2=2.5)
    y1=200-(5*x1)+(10*x2)+x3+error

    testdata <- as.data.frame(cbind(y1,x1,x2,x3))

    ## The regression function in MLRneebie
    model_nb <- linearRegression(data = testdata,
                                 predictors = c("x1","x2","x3"),
                                 response = "y1",
                                 )
    ## The base regression function
    model_base <- lm(data=testdata, formula =  y1~x1+x2+x3)

    anr <- c(sum(anova(model_base)$"Sum Sq"[-4]),
             anova(model_base)$"Sum Sq"[4],
             anova(model_base)$"Sum Sq"[4]+sum(anova(model_base)$"Sum Sq"[-4]))
    ssmr <- unname(ssmodel(model_nb)[,1])
    testthat::expect_equal(round(anr, digits = 10),
                           round(ssmr, digits = 10))
})




