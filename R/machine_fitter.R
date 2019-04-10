##' This functions fits one or several machine learning methods on a given dataset
##' @title Apply fitting of machine learning functions
##' @param dataframe dataframe containing variables of interest
##' @param aim_variable Character string with the name of the aim variable
##' @param co_variables Character string with the name of the co-variables
##' @param method default = c("DT","BDT","RF"); which method should be used: DecisionTree, BoostedDecisionTree and/or RandomForest?
##' @param splitper default = 70; percentage of dataset used for fitting the model
##' @return list containing the models
##' @importFrom C50 C5.0
##' @importFrom randomForest randomForest
##' @importFrom ROCR performance prediction
##' @importFrom dplyr progress_estimated
##' @export machine_fitter
##' @author Wolfgang Hamer
##' @examples
machine_fitter <- function(dataframe, aim_variable, co_variables, method=c("BDT","RF"),splitper = 70){
  method <- method[is.element(method,c("BDT","RF"))]
  
  if(any(!is.element(co_variables,names(dataframe)))){
    warning(paste(co_variables[!is.element(co_variables,names(dataframe))], "not a colname of dataframe!"))
  }
  
  sel_el <- sample(x = 1:dim(dataframe)[1],
                   size = (dim(dataframe)[1]/100)*splitper)
  traindat <- dataframe[sel_el,]
  testdat <- dataframe[-sel_el,]
  

  if(any(is.element(method,"BDT"))){
    pb <- dplyr::progress_estimated(length(c(1, 5, 10, 15, 20, 25, 30, 35)))
    
    BDT <- do.call("rbind", 
                       Map(function(tria){
      BDTmod <- C50::C5.0(traindat[,co_variables],
                       as.factor(traindat[,aim_variable]),
                       control = C50::C5.0Control(minCases =round((dim(traindat)[1]/100)*.7,0)),
                       trials = tria)
      tempres <- data.frame(Observed = as.factor(testdat[,aim_variable]),
                            Predicted = predict(BDTmod, testdat[,co_variables]))
      
      # Evaluating the Area under the ROC curve.
      dt_pre <- ROCR::prediction(predictions = as.numeric(tempres$Predicted),
                                 labels = tempres$Observed)
      dt_perfa <- ROCR::performance(dt_pre,
                                    measure = "auc")                                                
      dt_auc <- unlist(dt_perfa@y.values)
      pb$tick()$print()
      return(data.frame(Trials = tria,
                        ROCAUC = dt_auc))
    },
    tria = c(1, 5, 10, 15, 20, 25, 30, 35)))
  }
  
  
  if(any(is.element(method,"RF"))){
    pb <- dplyr::progress_estimated(length(seq(1,2E6,.25E6))*length(c(2,4,8,18)))
    
    RF <- do.call("rbind",Map(function(j){
      return(do.call("rbind",Map(function(o){
        tryrf <- try(
            randomForest::randomForest(traindat[,co_variables],
                                       traindat[,aim_variable],
                                       importance=TRUE, 
                                       ntree=1000,
                                       mtry=j,
                                       classwt = c(1,o)), 
            silent=TRUE)
        if (any(class(tryrf) == "try-error")){
          rf_auc=0
        }else{
          tempres <- data.frame(Observed = testdat[,aim_variable],
                                Predicted = predict(tryrf, testdat[,co_variables]))
          
          # Evaluating the Area under the ROC curve.
          rf_pre <- ROCR::prediction(predictions = as.numeric(tempres$Predicted),
                                     labels = tempres$Observed)
          rf_perfa <- ROCR::performance(rf_pre,
                                        measure = "auc")                                                
          rf_auc <- unlist(rf_perfa@y.values)
        }
        pb$tick()$print()
        return(data.frame(Mtry = j,
                          Classwt = o,
                          ROCAUC = rf_auc))
      },o = seq(1,2E6,.25E6))))},
      j = c(2,4,8,18)
    ))
  }
  
  
  models <- Map(function(x){get(x)},x=method)
  
  return(models)
}




