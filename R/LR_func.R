#' Fitting Linear Models
#'
#' @description linearRegression is a mimic of lm() that is used to fit linear models.
#'
#' @param data The data set for multiple linear regression. It should be in the form of a 2d matrix
#' @param predictors A string array of all the predictors' names.
#' @param response A single string of the response.
#' @param intercept Boolean. TRUE by default for a model with intercept. Setting as FALSE will force the model to have no intercept.
#'
#' @return A list with 6 elements:
#' 1. beta_hat; 2.R^2; 3.X; 4.Fitted values; 5.Residuals; 6.intercept or not
#'
#' @importFrom stats qt
#'
#' @export
#'
#' @examples
#' mt <- mtcars
#' linearRegression(mt, "mpg", "cyl", TRUE)
#'
linearRegression <- function(data, predictors, response, intercept = TRUE) {
    ## Retrieve important matrixs
    Y <- as.matrix(data[,response])
    X <- as.matrix(data[,predictors])
    ## Dimensions
    n <- nrow(X)
    p <- ncol(X)
    ## If there is an intercept (default)
    ## X need to cbind with a vector of ones
    if (intercept) {
        X <- cbind(rep(1,n), X)
        p <- p+1
    }
    ## Estimated beta
    betahat <- solve(t(X)%*%X)%*%t(X)%*%Y
    ## Fitted value
    Yhat <- X%*%betahat
    ## Residuals
    epsilonhat <- Y-Yhat
    sigma_squared <- t(epsilonhat)%*%epsilonhat/(n-p)
    ## Varaince of estimated coefficients
    var_betahat <- diag(solve(t(X)%*%X))*c(sigma_squared)
    se_betahat <- sqrt(var_betahat)
    ## T stats
    t_statistics <- c(betahat/se_betahat)
    p_value <- c(2*(1-stats::pt(q=abs(t_statistics), df = n-p)))

    ## Output
    print(noquote("Regression coefficients: "))
    output_mat <- cbind(Estimate = c(betahat),
                              Std_Err = se_betahat,
                              t_statistics = t_statistics,
                              p_value = p_value)
    if(intercept) {
        rownames(output_mat) <- c("(Intercept)",predictors)
    } else {
        rownames(output_mat) <- c(predictors)
    }
    print(output_mat)
    cat("\n")

    ## R squared value
    print(noquote("Multiple R squared: "))
    print(R_value <- 1-sum(epsilonhat^2)/sum((Y-mean(Y))^2))

    ## A list of 6 components:
    ## 1. beta_hat; 2.R^2; 3.X; 4.Fitted values; 5.Residuals; 6.intercept or not
    return(list(output_mat, R_value, X, Yhat, epsilonhat, intercept))
}

#' Summarize Model Residuals
#'
#' @description model_residuals is a function which extracts model residuals, standardized
#' residuals, and internally studentized residuals from the lists returned by
#' modeling functions
#'
#' @param model A list object returned by linearRegression.
#'
#' @return A table contains the regular residual, the standartized residual,
#' and the internally studentized residual of the model.
#' @export
#'
#' @examples
#' mt <- mtcars
#' model_residuals(linearRegression(mt, "mpg", "cyl", TRUE))
model_residuals <- function(model) {
    ## get all of the needed info
    X <- model[[3]]
    betahat <- model[[1]][,1]
    resid <- model[[5]]
    ## dimensions
    n <- nrow(X)
    p <- length(betahat)
    ## MSE
    msigma <- sqrt( sum(resid^2)/(n-p) )
    standard_res <- resid/msigma
    denom<- sqrt(1-diag(X%*%solve(t(X)%*%X)%*%t(X)))*msigma
    ## Internally studentized residual
    studentized <- resid/denom
    output_mat <- cbind(Residual = resid,
                        Std_Residual = standard_res,
                        Studentized_Residual = studentized)
    colnames(output_mat) <- c("Residual","Std_Residual","Studentized_Residual")
    return(output_mat)
}


#' Simple Anova Table
#' @description Compute analysis of variance (or deviance) tables
#' for a single fitted model objects returned by linearRegression function.
#'
#' @param model A list object returned by linearRegression.
#'
#' @return A list of the three sum of squares: Residual sum of square,
#' Regression sum of square, and total sum of square.
#' @export
#'
#' @examples
#' mt <- mtcars
#' ssmodel(linearRegression(mt, "mpg", "cyl", TRUE))
ssmodel <- function(model) {
    ## Retrieve all important info
    X <- model[[3]]
    betahat <- model[[1]][,1]
    ## dimensions
    ## it is needed for degree of freedom
    n <- nrow(X)
    p <- length(betahat)
    epsilonhat <- model[[5]]
    SSE <- t(epsilonhat)%*%epsilonhat
    Y <- model[[4]] +model[[5]]
    ## For a cell mean coding regression
    ## SSY is defined as sum of Y^2 instead of the differences
    if(model[[6]]) {
        ## This is the common one
        Ym <- Y - mean(Y)
        SST <- t(Ym)%*%(Ym)
        df <- c(p-1, n-p, n-1)
    } else {
        ## Note the degree of freedon can be meaningless here
        ST <- t(Y)%*%(Y)
        df <- c(p, n-p, n)
    }
    ##This might cause rounding error
    SSR <- SST-SSE
    ## Output
    output_mat <- cbind(Sum_of_Square = c(SSR,SSE,SST),
                        degree_freedom = df)
    rownames(output_mat) <- c("SSR","SSE","SST")
    print(output_mat)
}
