### Title:    Orthgonalization function unit tests via `testthat` 
### Author:   Pavel Panko
### Created:  2019-FEB-22
### Modified: 2019-FEB-22

##
initializeEnvironment <- function() {
    ## 
    data(iris)
    ##
    trueFormula   <<- "Petal.Width ~ Sepal.Width"
    trueData      <<- iris
    trueGroupVec  <<- iris$Species
    trueGroupName <<- "Species"
    trueIntercept <<- TRUE
    ##
    falseFormula   <<- 1
    falseData      <<- as.matrix(iris)
    falseGroup1    <<- data.frame()
    falseGroup2    <<- 1:4
    falseIntercept <<- "true"
    ## 
    modFrame <<- stats::model.frame(trueFormula, trueData)
    ##
    y <<- model.response(modFrame, "numeric")
    X <<- model.matrix(attr(modFrame, "terms"), modFrame)
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
            ## The value of "intercept" is not of class "logical"
            expect_error(
                orthogonalize(formula = trueFormula, data = trueData, intercept = falseIntercept),
                "intercept argument must be TRUE or FALSE"
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
                lmResid(formula = trueFormula, data = trueData, intercept = trueIntercept),
                orthogonalize(formula = trueFormula, data = trueData, intercept = trueIntercept)
            )
            expect_equal(
                lmGroupResid(formula = trueFormula, data = trueData, group = trueGroupVec),
                orthogonalize(formula = trueFormula, data = trueData, group = trueGroupVec)
            )
            expect_equal(
                lmGroupResid(formula = trueFormula, data = trueData, group = trueGroupVec, intercept = trueIntercept),
                orthogonalize(formula = trueFormula, data = trueData, group = trueGroupVec, intercept = trueIntercept)
            )
            expect_equal(
                lmGroupResid(formula = trueFormula, data = trueData, group = trueGroupVec),
                orthogonalize(formula = trueFormula, data = trueData, group = trueGroupName)
            )
            expect_equal(
                lmGroupResid(formula = trueFormula, data = trueData, group = trueGroupVec, intercept = trueIntercept),
                orthogonalize(formula = trueFormula, data = trueData, group = trueGroupName, intercept = trueIntercept)
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
                as.matrix(lmResid(x = X, y = y, intercept = trueIntercept, bare = TRUE)),
                get_residuals(X = X, y = y, intercept = 1)
            )
            expect_equal(
                as.matrix(lmGroupResid(x = X, y = y, group = trueGroupVec, bare = TRUE)),
                get_group_residuals(X = X, y = y, GroupVec = as.integer(trueGroupVec), intercept = 0)
            )
            expect_equal(
                as.matrix(lmGroupResid(x = X, y = y, group = trueGroupVec, intercept = trueIntercept, bare = TRUE)),
                get_group_residuals(X = X, y = y, GroupVec = as.integer(trueGroupVec), intercept = 1)
            )
        }
    )
}


##
sanityCheck <- function() {
    ##
    test_that(
        "Things make sense",
        {
            expect_that(
                orthogonalize(formula = trueFormula, data = trueData),
                is_a("numeric")
            )
            expect_that(
                orthogonalize(formula = trueFormula, data = trueData, intercept = trueIntercept),
                is_a("numeric")
            )
            expect_that(
                orthogonalize(formula = trueFormula, data = trueData, group = trueGroupVec),
                is_a("numeric")
            )
            expect_that(
                orthogonalize(formula = trueFormula, data = trueData, group = trueGroupName),
                is_a("numeric")
            )
            expect_that(
                orthogonalize(formula = trueFormula, data = trueData, intercept = trueIntercept, group = trueGroupVec),
                is_a("numeric")
            )
            expect_that(
                orthogonalize(formula = trueFormula, data = trueData, intercept = trueIntercept, group = trueGroupName),
                is_a("numeric")
            )
            expect_equal(
                length(orthogonalize(formula = trueFormula, data = trueData)),
                nrow(trueData)
            )
            expect_equal(
                length(orthogonalize(formula = trueFormula, data = trueData, intercept = trueIntercept)),
                nrow(trueData)
            )
            expect_equal(
                length(orthogonalize(formula = trueFormula, data = trueData, group = trueGroupVec)),
                nrow(trueData)
            )
            expect_equal(
                length(orthogonalize(formula = trueFormula, data = trueData, group = trueGroupName)),
                nrow(trueData)
            )
            expect_equal(
                length(orthogonalize(formula = trueFormula, data = trueData, intercept = trueIntercept, group = trueGroupVec)),
                nrow(trueData)
            )
            expect_equal(
                length(orthogonalize(formula = trueFormula, data = trueData, intercept = trueIntercept, group = trueGroupName)),
                nrow(trueData)
            )
        }
    )
}


##
lmGroupResid <- function(..., group, intercept = FALSE, bare = FALSE) {
    ##
    list2env(list(...), environment())
    ##
    groupResidVec <- vector("numeric", length = length(group))
    unqGroups     <- unique(group)
    ##
    for(g in unqGroups) {
        selectedRows <- group == g
        if(bare == FALSE) {
            ##
            groupResidVec[selectedRows] <- lmResid(
                formula   = formula,
                data      = data[selectedRows,],
                intercept = intercept
            )
        } else {
            ##
            groupResidVec[selectedRows] <- lmResid(
                x         = X[selectedRows,],
                y         = y[selectedRows],
                intercept = intercept,
                bare      = TRUE
            )
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

## 
url <- "https://raw.githubusercontent.com/ppanko/orthogonalize/master/src/get_residuals.cpp"
tf <- tempfile(fileext = ".cpp")
download.file(url, tf, quiet = TRUE)
sourceCpp(tf)
