### Title:    orthogonalize: unit tests via `testthat` 
### Author:   Pavel Panko
### Created:  2019-FEB-22
### Modified: 2019-MAR-17

##
initializeEnvironment <- function() {
  ## 
  data(iris)
  iris$numId <<- 1:nrow(iris)
  iris$charId <<- as.character(iris$numId)
  ##
  trueFormulaUni   <<- Petal.Width ~ Sepal.Width
  trueData         <<- iris
  trueInterceptUni <<- TRUE
  trueGroupNameUni <<- "Species"
  ##
  trueFormulaMulti <<- list(Petal.Width ~ Sepal.Width, Petal.Width ~ Sepal.Length)
  trueInterceptMulti1 <<- c(TRUE, FALSE)
  trueGroupNameMulti1 <<- c("Species", "Species")
  trueGroupNameMulti2 <<- c("", "Species")
  ##
  trueInterceptInt0 <<- 0
  trueInterceptInt1 <<- 1
  trueGroupInt0 <<- vector("integer", 0)
  trueGroupIntV <<- as.integer(iris$Species)
  trueInterceptIntV1 <<- c(0, 0)
  trueInterceptIntV2 <<- c(1, 1)
  trueGroupList1 <<- list(as.integer(iris$Species), as.integer(iris$Species))
  trueGroupList2 <<- list(as.integer(iris$Species), vector("integer", 0))
  ##
  falseData <<- as.matrix(iris)
  falseFormulaUni1 <<- "Petal.Width ~ Sepal.Width"
  falseFormulaUni2 <<- Petal.Width ~ Sepal.Width ~ 1
  falseFormulaUni3 <<- Petal.Width + Sepal.Width ~ 1
  falseFormulaUni4 <<- Petal.Width ~ Garden
  falseGroupUni1 <<- TRUE
  falseGroupUni2 <<- vector("character", 0)
  falseGroupUni3 <<- "Garden"
  falseGroupUni4 <<- "numId"
  falseGroupUni5 <<- "charId"
  falseIntUni1 <<- "all"
  falseIntUni2 <<- vector("logical", 0)
  falseFormulaMulti1 <<- c("Petal.Width ~ Sepal.Width", "Petal.Width ~ Sepal.Width")
  falseFormulaMulti2 <<- list(Petal.Width ~ Sepal.Width ~ 1, Petal.Width ~ Sepal.Width)
  falseFormulaMulti3 <<- list(Petal.Width + Sepal.Width ~ 1, Petal.Width ~ Sepal.Width)
  falseFormulaMulti4 <<- list(Petal.Width ~ Garden, Petal.Width ~ Sepal.Width)
  falseFormulaMulti5 <<- list(Petal.Width ~ Sepal.Width, "Petal.Width ~ Sepal.Width")
  falseGroupMulti1 <<- c(TRUE, FALSE)
  falseGroupMulti2 <<- c("Species", "", "Species")
  falseGroupMulti3 <<- c("Species", "Garden")
  falseGroupMulti4 <<- c("Species", "numId")
  falseGroupMulti5 <<- c("Species", "charId")
  falseIntMulti1 <<- c(TRUE, TRUE, FALSE)
  falseIntMulti2 <<- c("all", "all")
  ## 
  modFrame <<- stats::model.frame(trueFormulaUni, trueData)
  y <<- model.response(modFrame, "numeric")
  X <<- model.matrix(attr(modFrame, "terms"), modFrame)
  ##
  yList <<- xList <<- vector("list", length(trueFormulaMulti))
  for(i in seq_along(yList)) {
    modFrame <<- stats::model.frame(trueFormulaMulti[[i]], trueData)
    yList[[i]] <<- model.response(modFrame, "numeric")
    xList[[i]] <<- model.matrix(attr(modFrame, "terms"), modFrame)    
  }
}

##
checkUnivariateCatchableErrors <- function() {
  ## 
  test_that(
    "Catchable errors - single formula",
    {
      ## Not a formula 
      expect_error(
        orthogonalize(formula = falseFormulaUni1, data = trueData),
        "Provide a formula or a list of formulas",
        )
      ## Multiple tildes
      expect_error(
        orthogonalize(formula = falseFormulaUni2, data = trueData),
        "Too many tilde symbols found in the formula"
      )
      ## Multivariate formula 
      expect_error(
        orthogonalize(formula = falseFormulaUni3, data = trueData),
        'Multivariate formulas are not supported'
      )
      ## Not all formula vars in data  
      expect_error(
        orthogonalize(formula = falseFormulaUni4, data = trueData),
        "Not all variables provided in the formula are in the data"
      )
      ## Incorrect data class
      expect_error(
        orthogonalize(formula = trueFormulaUni, data = falseData),
        'Provide the data as a "data.frame" class'
      )
      ## Group not character
      expect_error(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = falseGroupUni1),
        '"group" must be a character vector*'
      )
      ## Length of group > 1
      expect_error(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = falseGroupUni2),
        '"group" must be a character vector*'
      )
      ## Group is "character" but not in data 
      expect_error(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = falseGroupUni3),
        '"group" must be either a "factor", "logical", or "character" column in the data'
      )
      ## Group is in data but does not match required type
      expect_error(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = falseGroupUni4),
        '"group" must be either a "factor", "logical", or "character" column in the data'
      )
      ## All unq grouping variable 
      expect_error(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = falseGroupUni5),
        '"group" must not contain all unique values'
      )
      ## Intercept not logical
      expect_error(
        orthogonalize(formula = trueFormulaUni, data = trueData, intercept = falseIntUni1),
        '"intercept" must be a logical vector with length equal to 1 or the number of response variables'
      )
      ## Intercept length > 1 
      expect_error(
        orthogonalize(formula = trueFormulaUni, data = trueData, intercept = falseIntUni2),
        '"intercept" must be a logical vector with length equal to 1 or the number of response variables'
      )
    }
  )
}

##
checkMultivariateCatchableErrors <- function() {
  ## 
  test_that(
    "Catchable errors - multiple formula",
    {
      ## Not a formula/list 
      expect_error(
        orthogonalize(formula = falseFormulaMulti1, data = trueData),
        "Provide a formula or a list of formulas"
      )
      ## Multiple tildes
      expect_error(
        orthogonalize(formula = falseFormulaMulti2, data = trueData),
        "Too many tilde symbols found in the formula"
      )
      ## Multivariate formula 
      expect_error(
        orthogonalize(formula = falseFormulaMulti3, data = trueData),
        'Multivariate formulas are not supported'
      )
      ## Not all formula vars in data  
      expect_error(
        orthogonalize(formula = falseFormulaMulti4, data = trueData),
        "Not all variables provided in the formula are in the data"
      )
      ## List of formulas containing one wrong member
      expect_error(
        orthogonalize(formula = falseFormulaMulti5, data = trueData),
        "All elements of the formula list must be formulas"
      )
      ## Incorrect data class
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = falseData),
        'Provide the data as a "data.frame" class'
      )
      ## Group not character - univariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupUni1),
        '"group" must be a character vector*'
      )
      ## Length of group is not 1 and it does not match formula length - univariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupUni2),
        '"group" must be a character vector*'
      )
      ## Group is "character" but not in data - univariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupUni3),
        '"group" must be either a "factor", "logical", or "character" column in the data'
      )
      ## Group is in data but does not match required type
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupUni4),
        '"group" must be either a "factor", "logical", or "character" column in the data'
      )
      ## All unq grouping variable - univariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupUni5),
        '"group" must not contain all unique values'
      )
      ## Group not character - multivariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupMulti1),
        '"group" must be a character vector*'
      )
      ## Length of group is not 1 and it does not match formula length - multivariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupMulti2),
        '"group" must be a character vector*'
      )
      ## Group is "character" but not in data - multivariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupMulti3),
        '"group" must be either a "factor", "logical", or "character" column in the data'
      )
      ## Group is in data but does not match required type - multivariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupMulti4),
        '"group" must be either a "factor", "logical", or "character" column in the data'
      )
      ## All unq grouping variable - multivariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = falseGroupMulti5),
        '"group" must not contain all unique values'
      )
      ## Intercept not logical - univariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = falseIntUni1),
        '"intercept" must be a logical vector with length equal to 1 or the number of response variables'
      )
      ## Length of intercept is not 1 and it does not match formula length  - univariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = falseIntUni2),
        '"intercept" must be a logical vector with length equal to 1 or the number of response variables'
      )
      ## Intercept not logical - multivariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = falseIntMulti1),
        '"intercept" must be a logical vector with length equal to 1 or the number of response variables'
      )
      ## Length of intercept is not 1 and it does not match formula length  - multivariate
      expect_error(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = falseIntMulti2),
        '"intercept" must be a logical vector with length equal to 1 or the number of response variables'
      )
    }
  )
}

##
checkUnivariateFrontEnd <- function() {
  ##
  test_that(
    "Front-end - single formula",
    {
      ## 
      expect_equal(
        lm_(x = trueFormulaUni, data = trueData),
        orthogonalize(formula = trueFormulaUni, data = trueData)
      )
      expect_equal(
        lm_(x = trueFormulaUni, data = trueData, intercept = trueInterceptUni),
        orthogonalize(formula = trueFormulaUni, data = trueData, intercept = trueInterceptUni)
      )
      expect_equal(
        lmGroup_(x = trueFormulaUni, data = trueData, group = trueGroupNameUni),
        orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni)
      )
      expect_equal(
        lmGroup_(x = trueFormulaUni, data = trueData, group = trueGroupNameUni, intercept = trueInterceptUni),
        orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni, intercept = trueInterceptUni)
      )
    }
  )   
}

##
checkMultivariateFrontEnd <- function() {
  ##
  test_that(
    "Front-end - multiple formula",
    {
      ## 
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData),
        orthogonalize(formula = trueFormulaMulti, data = trueData)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni),
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, group = trueGroupNameUni),
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameUni)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameUni),
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameUni)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1),
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti1),
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti1)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti2),
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti2)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameUni),
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameUni)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti1),
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti1)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti2),
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti2)
      )     
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti1),
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti1)
      )
      expect_equal(
        lmMultiFront_(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti2),
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti2)
      )
    }
  )   
}

##
checkUnivariateBackEnd <- function() {
  ##
  test_that(
    "Back-end - single formula",
    {
      expect_equal(
        lm_(x = X, y = y, intercept = trueInterceptInt0),
        get_residuals_single(X = X, y = y, intercept = trueInterceptInt0, GroupVec = trueGroupInt0)
      )
      expect_equal(
        lm_(x = X, y = y, intercept = trueInterceptInt1),
        get_residuals_single(X = X, y = y, intercept = trueInterceptInt1, GroupVec = trueGroupInt0)
      )
      expect_equal(
        lmGroup_(x = X, y = y, intercept = trueInterceptInt0, group = trueGroupIntV),
        get_residuals_single(X = X, y = y, intercept = trueInterceptInt0, GroupVec = trueGroupIntV)
      )
      expect_equal(
        lmGroup_(x = X, y = y, intercept = trueInterceptInt1, group = trueGroupIntV),
        get_residuals_single(X = X, y = y, intercept = trueInterceptInt1, GroupVec = trueGroupIntV)
      )
    }
  )
}

##
checkMultivariateBackEnd <- function() {
  ##
  test_that(
    "Back-end - multiple formula",
    {
      expect_equal(
        lmMultiBack_(xList = xList, yList = yList, intercept = trueInterceptIntV1, group = trueGroupList1),
        get_residuals_multi(xList = xList, yList = yList, intercept = trueInterceptIntV1, GroupList = trueGroupList1)
      )
      expect_equal(
        lmMultiBack_(xList = xList, yList = yList, intercept = trueInterceptIntV1, group = trueGroupList2),
        get_residuals_multi(xList = xList, yList = yList, intercept = trueInterceptIntV1, GroupList = trueGroupList2)
      )     
      expect_equal(
        lmMultiBack_(xList = xList, yList = yList, intercept = trueInterceptIntV2, group = trueGroupList1),
        get_residuals_multi(xList = xList, yList = yList, intercept = trueInterceptIntV2, GroupList = trueGroupList1)
      )                                                                                          
      expect_equal(                                                                              
        lmMultiBack_(xList = xList, yList = yList, intercept = trueInterceptIntV2, group = trueGroupList2),
        get_residuals_multi(xList = xList, yList = yList, intercept = trueInterceptIntV2, GroupList = trueGroupList2)
      )     
    }
  )
}

##
checkUnivariateClasses <- function() {
  ##
  test_that(
    "Classes are consistent - single formula",
    {
      ## 
      expect_that(
        orthogonalize(formula = trueFormulaUni, data = trueData),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaUni, data = trueData, intercept = trueInterceptUni),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni, intercept = trueInterceptUni),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaUni, data = trueData, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaUni, data = trueData, intercept = trueInterceptUni, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni, intercept = trueInterceptUni, simplify = TRUE),
        is_a("numeric")
      )
      ## 
      expect_equal(
        c(nrow(trueData), 1),
        dim(orthogonalize(formula = trueFormulaUni, data = trueData))
      )
      expect_equal(
        c(nrow(trueData), 1),
        dim(orthogonalize(formula = trueFormulaUni, data = trueData, intercept = trueInterceptUni))
      )
      expect_equal(
        c(nrow(trueData), 1),
        dim(orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni))
      )
      expect_equal(
        c(nrow(trueData), 1),
        dim(orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni, intercept = trueInterceptUni))
      )
      ## 
      expect_equal(
        nrow(trueData),
        length(orthogonalize(formula = trueFormulaUni, data = trueData, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData),
        length(orthogonalize(formula = trueFormulaUni, data = trueData, intercept = trueInterceptUni, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData),
        length(orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData),
        length(orthogonalize(formula = trueFormulaUni, data = trueData, group = trueGroupNameUni, intercept = trueInterceptUni, simplify = TRUE))
      )
    }
  )
}

##
checkMultivariateClasses <- function() {
  ##
  test_that(
    "Classes are consistent - multiple formulas",
    {
      ## 
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameUni),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameUni),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti1),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti2),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameUni),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti1),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti2),
        is_a("matrix")
      )     
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti1),
        is_a("matrix")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti2),
        is_a("matrix")
      )
      ## 
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameUni, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameUni, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti1, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti2, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameUni, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti1, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti2, simplify = TRUE),
        is_a("numeric")
      )     
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti1, simplify = TRUE),
        is_a("numeric")
      )
      expect_that(
        orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti2, simplify = TRUE),
        is_a("numeric")
      )
      ## 
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameUni))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameUni))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti1))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti2))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameUni))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti1))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti2))
      )     
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti1))
      )
      expect_equal(
        c(nrow(trueData), length(trueFormulaMulti)),
        dim(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti2))
      )
      ## 
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameUni, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameUni, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti1, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, group = trueGroupNameMulti2, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameUni, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti1, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptMulti1, group = trueGroupNameMulti2, simplify = TRUE))
      )     
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti1, simplify = TRUE))
      )
      expect_equal(
        nrow(trueData)*length(trueFormulaMulti),
        length(orthogonalize(formula = trueFormulaMulti, data = trueData, intercept = trueInterceptUni, group = trueGroupNameMulti2, simplify = TRUE))
      )
    }
  )
}

##
checkUnload <- function() {
  test_that(
    expect_that(
      unloadPrintDll(),
      is_a("NULL")
    )
  )
}


lmMultiFront_ <- function(formula, data, intercept = 0, group = "", simplify = FALSE) {
  ##
  operations <- length(formula)
  outMat     <- matrix(NA, nrow = nrow(data), ncol = operations)
  ##
  if (length(group) == 1) {
    group <- rep(group, operations)
  }
  if (length(intercept) == 1) {
    intercept <- rep(intercept, operations)
  }
  ##
  for(i in 1:operations) {
    if (group[i] == "") {
      outMat[,i] <- lm_(
        x         = formula[[i]],
        data      = data,
        intercept = intercept[i]
      )
    } else {
      outMat[,i] <- lmGroup_(
        x         = formula[[i]],
        data      = data,
        intercept = intercept[i],
        group     = group[i]
      )
    }
  }
  if (simplify) {
    as.vector(outMat)
  } else {
    colnames(outMat) <- sapply(formula, function(f) as.character(f[[2]]))
    outMat
  }
}

lmMultiBack_ <- function(xList, yList, intercept = 0, group = vector("integer", 0), simplify = FALSE) {
  ##
  operations <- length(yList)
  outMat     <- matrix(NA, nrow = length(yList[[1]]), ncol = operations)
  ##
  if (length(group) == 1) {
    group <- rep(group, operations)
  }
  if (length(intercept) == 1) {
    intercept <- rep(intercept, operations)
  }
  ##
  for(i in 1:operations) {
    if (!length(group[[i]])) {
      outMat[,i] <- lm_(
        x     = xList[[i]],
        y     = yList[[i]],
        intercept = intercept[i]
      )
    } else {
      outMat[,i] <- lmGroup_(
        x     = xList[[i]],
        y     = yList[[i]],
        intercept = intercept[i],
        group     = group[[i]]
      )
    }
  }
  if (simplify)
    as.vector(outMat)
  outMat
}

lmGroup_ <- function(..., group, simplify = FALSE) {
  groupVar      <- .getGroup(group, ...)
  groupResidVec <- vector("numeric", length = length(groupVar))
  unqGroups     <- unique(groupVar)
  ##
  for(g in unqGroups) {
    selectObs <- groupVar == g
    groupResid <- lm_(..., subset = selectObs, simplify = FALSE)
    if(g == unqGroups[1]) {
      name <- colnames(groupResid)
    }
    groupResidVec[selectObs] <- groupResid
  }
  if(!simplify) {
    groupResidVec <- as.matrix(groupResidVec)
    colnames(groupResidVec) <- name
  }
  groupResidVec
}

.getGroup <- function(group, ...) {
  UseMethod(".getGroup", group)
}

.getGroup.character <- function(group, data, ...) {
  data[[group]]
}

.getGroup.integer <- function(group, ...) {
  group
}

lm_ <- function(..., intercept = FALSE, simplify = FALSE) {
  model <- .doLm(...)
  resid <- as.vector(model$resid) + (model$coef[[1]] * intercept)
  if (!simplify) {
    resid <- as.matrix(resid)
    colnames(resid) <- names(model$model)[1]
  } else {
    attr(resid, "name") <- names(model$model)[1]
  }
  resid
}

.doLm <- function(x, ...) {
  UseMethod(".doLm", x)
}

.doLm.formula <- function(x, data, subset = NULL) {
  if (!is.null(subset)) {
    data <- data[subset,]
  }
  lm(x, data)
}

.doLm.matrix <- function(x, y, subset = NULL) {
  if (!is.null(subset)) {
    x <- x[subset,]
    y <- y[subset]
  }
  lm.fit(x, y)
}

unloadPrintDll <- function() {
  detach("package:orthogonalize", unload = TRUE)
  getLoadedDLLs()$orthogonalize
}

## 
url <- "https://raw.githubusercontent.com/ppanko/orthogonalize/master/src/get_residuals.cpp"
tf <- tempfile(fileext = ".cpp")
download.file(url, tf, quiet = TRUE)
sourceCpp(tf)
