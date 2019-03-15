##' This functions fits one or several machine learning methods on a given dataset
##' @title Apply fitting of machine learning functions
##' @param dataframe dataframe containing variables of interest
##' @param aim_variable Character string with the name of the aim variable
##' @param co_variables Character string with the name of the co-variables
##' @param method default = c("DT","BDT","RF"); which method should be used: DecisionTree, BoostedDecisionTree and/or RandomForest?
##' @return list containing the models
##' @importFrom C50 C5.0
##' @importFrom randomForest randomForest
##' @export machine_fitter
##' @author Wolfgang Hamer
##' @examples
machine_fitter <- function(dataframe, aim_variable, co_variables, method=c("DT","BDT","RF")){
  if(any(is.element(method,"DT"))){
    DT <- C50::C5.0(dataframe[,co_variables],
                    as.factor(dataframe[,aim_variable]),
                    control = C50::C5.0Control(minCases =round((dim(dataframe)[1]/100)*.7,0)))
    
  }
  
  if(any(is.element(method,"BDT"))){
    BDT <- C50::C5.0(dataframe[,co_variables],
                     as.factor(dataframe[,aim_variable]),
                     control = C50::C5.0Control(minCases =round((dim(dataframe)[1]/100)*.7,0)),
                     trials = 5)
  }
  
  if(any(is.element(method,"RF"))){
    RF <- randomForest::randomForest(dataframe[,co_variables],
                                     dataframe[,aim_variable])
  }
  
  models <- Map(function(x){get(x)},x=method)
  
  return(models)
}




