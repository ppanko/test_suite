### Title:    Orthgonalization function unit tests via `testthat` 
### Author:   Pavel Panko
### Created:  2019-FEB-22
### Modified: 2019-FEB-22

##
initializeEnvironment <- function() {
    ## 
    data(iris)
    ##
    trueFormula <- "Petal.Width ~ Sepal.Width"
    trueData    <- iris
    trueGroup   <- iris$Species
    ##
    falseFormula <- 1
    falseData    <- as.matrix(iris)
    falseGroup1  <- matrix()
    falseGroup2   <- 1:4
    ## 
    modFrame <- stats::model.frame(trueFormula, trueData)
    ##
    y <- model.response(modFrame, "numeric")
    X <- model.matrix(attr(modFrame, "terms"), modFrame)
    ##
    lmGroupResid <- function(..., group, bare = FALSE, intercept = FALSE) {
        groupResidVec <- vector("numeric", length = length(group))
        unqGroups <- unique(group)
        for(g in unqGroups) {
            selectedRows <- group == g
            if(bare == FALSE) {
                groupResidVec[selectedRows] <- lmResid(formula = formula, data = data[selectedRows,], intercept = intercept)
            } else {
                groupResidVec[selectedRows] <- lmResid(x = X[selectedRows,], y = y[selectedRows], intercept = intercept, bare = TRUE)
            }
        }
        return(groupResidVec)
    }
    ##
    lmResid <- function(..., intercept = FALSE, bare = FALSE) {
        if(bare == TRUE) {
            model <- lm.fit(...)
        } else {
            model <- lm(...)
        }
        resid <- as.vector(model$resid) + (model$coef[[1]] * intercept)
        return(resid)
    }
}

##
checkCatchableErrors <- function() {
    ## 
    test_that(
        "Catchable errors",
        {
            ## Incorrect formula
            expect_error(
                orthogonalize(formula = falseFormula, data = trueData),
                "provide the `formula` as a `formula` or `character` class"
            )
            ## Incorrect data class
            expect_error(
                orthogonalize(formula = trueFormula, data = falseData),
                "provide the `data` as a `data.frame` class"
            )
            ## Incorrect grouping variable class
            expect_error(
                orthogonalize(formula = trueFormula, data = trueData, group = falseGroup1),
                "group must be a vector containing the grouping variable or the name of a single grouping variable in the data"
            )
            ## Object that is not an atomic vector provided to "group"
            expect_error(
                orthogonalize(formula = trueFormula, data = trueData, group = falseGroup2),
                "group must be a vector containing the grouping variable or the name of a single grouping variable in the data"
            )
        }
    )
}

##
checkFrontEnd <- function() {
    ##
    test_that(
        "Front-end functionality",
        {
            ## 
            expect_equal(
                lmResid(formula = trueFormula, data = trueData),
                orthogonalize(formula = trueFormula, data = trueData)
            )
            expect_equal(
                lmResid(formula = trueFormula, data = trueData, intercept = TRUE),
                orthogonalize(formula = trueFormula, data = trueData, intercept = TRUE)
            )
            expect_equal(
                lmGroupResid(formula = trueFormula, data = trueData, group = trueGroup),
                orthogonalize(formula = trueFormula, data = trueData, group = trueGroup)
            )
            expect_equal(
                lmGroupResid(formula = trueFormula, data = trueData, group = trueGroup, intercept = TRUE),
                orthogonalize(formula = trueFormula, data = trueData, group = trueGroup, intercept = TRUE)
            )
        }
    )   
}

##
checkBackEnd <- function() {
    ##
    test_that(
        "Back-end functionality",
        {
            expect_equal(
                as.matrix(lmResid(x = X, y = y, bare = TRUE)),
                get_residuals(X = X, y = y, intercept = 0)
            )
            expect_equal(
                as.matrix(lmResid(x = X, y = y, intercept = TRUE, bare = TRUE)),
                get_residuals(X = X, y = y, intercept = 1)
            )
            expect_equal(
                as.matrix(lmGroupResid(x = X, y = y, group = trueGroup, bare = TRUE)),
                get_group_residuals(X = X, y = y, GroupVec = as.integer(trueGroup), intercept = 0)
            )
            expect_equal(
                as.matrix(lmGroupResid(x = X, y = y, group = trueGroup, bare = TRUE, intercept = TRUE)),
                get_group_residuals(X = X, y = y, GroupVec = as.integer(trueGroup), intercept = 1)
            )
        }
    )
}
