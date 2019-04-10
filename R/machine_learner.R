##' This functions applies one or several machine learning methods on a given dataset
##' @title Apply machine learning functions
##' @param dataframe dataframe containing variables of interest
##' @param aim_variable Character string with the name of the aim variable
##' @param co_variables Character string with the name of the co-variables
##' @param method default = c("DT","BDT","RF"); which method should be used: DecisionTree, BoostedDecisionTree and/or RandomForest?
##' @param additionalparameters default = FALSE; list containing additional parameters for the ML procedures / e.g.: list(RF=list(ntree=10000))
##' @return list containing the models
##' @importFrom C50 C5.0
##' @importFrom randomForest randomForest
##' @export machine_learner
##' @author Wolfgang Hamer
##' @examples
machine_learner <- function(dataframe, aim_variable, co_variables, method=c("DT","BDT","RF"), additionalparameters=FALSE){
  if(!is.logical(additionalparameters)){
    availableparameters <- names(additionalparameters)
  }else{
    availableparameters <- c()
  }
  if(any(is.element(method,"DT"))){
    avpa <- list(x = dataframe[,co_variables],
                 y = as.factor(dataframe[,aim_variable]),
                 control = C50::C5.0Control(minCases =round((dim(dataframe)[1]/100)*.7,0)))
    if(is.element("DT",availableparameters)){
      avpa <- modifyList(avpa,additionalparameters$DT)
    }
    
    DT <- do.call(C50::C5.0,avpa)
  }

  if(any(is.element(method,"BDT"))){
    avpa <- list(x = dataframe[,co_variables],
                 y = as.factor(dataframe[,aim_variable]),
                 control = C50::C5.0Control(minCases =round((dim(dataframe)[1]/100)*.7,0)),
                 trials = 5)
    if(is.element("BDT",availableparameters)){
      avpa <- modifyList(avpa,additionalparameters$BDT)
    }
    
    BDT <- do.call(C50::C5.0,avpa)
  }

  if(any(is.element(method,"RF"))){
    avpa <- list(x = dataframe[,co_variables],
                 y = dataframe[,aim_variable])
    if(is.element("RF",availableparameters)){
      avpa <- modifyList(avpa,additionalparameters$RF)
    }
    
    RF <- do.call(randomForest::randomForest,avpa)
  }

  models <- Map(function(x){get(x)},x=method)

  return(models)
}


##' This functions applies one or several machine learning methods on a given dataset
##' @title Apply machine learning functions
##' @param dataframe dataframe containing variables of interest
##' @param aim_variable Character string with the name of the aim variable
##' @param co_variables Character string with the name of the co-variables
##' @param method default = c("DT","BDT","RF"); which method should be used: DecisionTree, BoostedDecisionTree and/or RandomForest?
##' @param additionalparameters default = FALSE; list containing additional parameters for the ML procedures / e.g.: list(RF=list(ntree=10000))
##' @return list containing the models
##' @importFrom C50 C5.0
##' @importFrom randomForest randomForest
##' @export auto_machine_learner
##' @author Wolfgang Hamer
##' @examples
auto_machine_learner <- function(dataframe, aim_variable, co_variables, method=c("DT","BDT","RF")){
  
  ma_fi <- machine_fitter(dataframe = dataframe,
                          aim_variable = aim_variable,
                          co_variables = co_variables,
                          method = method)
  
  additionalparameters <- list()
  
  if(is.element("DT",method)){
    additionalparameters$DT <- list()
  }
  
  if(is.element("BDT",method)){
    additionalparameters$BDT <- list(trials = ma_fi$BDT$Trials[which.max(ma_fi$BDT$ROCAUC)])
  }
  
  if(is.element("RF",method)){
    additionalparameters$RF <- list(mtry = ma_fi$RF$Mtry[which.max(ma_fi$RF$ROCAUC)],
                                    classwt = c(1,ma_fi$RF$Classwt[which.max(ma_fi$RF$ROCAUC)]))
  }
  
  models <- machine_learner(dataframe = dataframe,
                            aim_variable = aim_variable,
                            co_variables = co_variables,
                            method = method,
                            additionalparameters = additionalparameters)
  return(models)
}



##' This functions applies one or several machine learning methods on a given dataset and checks for the validity of the prediction
##' @title Apply loocv machine learning functions
##' @param dataframe dataframe containing variables of interest
##' @param aim_variable Character string with the name of the aim variable
##' @param co_variables Character string with the name of the co-variables
##' @param location defalut = FALSE; Character string with the name of the location. If FALSE each observation is treated as unique location
##' @param method default = c("DT","BDT","RF"); which method should be used: DecisionTree, BoostedDecisionTree and/or RandomForest?
##' @param additionalparameters default = FALSE; list containing additional parameters for the ML procedures / e.g.: list(RF=list(ntree=10000))
##' @param auto default = FALSE; boolean operator defining if "machine_learner" or "auto_machine_learner" should be used
##' @return list containing the models
##' @import dplyr
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export loocv_machine_learner
##' @author Wolfgang Hamer
##' @examples
loocv_machine_learner <- function(dataframe, aim_variable, co_variables, location=FALSE, method=c("DT","BDT","RF"), additionalparameters=FALSE, auto=FALSE){
  if(is.logical(location)){
    dataframe$Loc_LOOCV <-  1:nrow(dataframe)
  }else{
    dataframe$Loc_LOOCV <-  dataframe[,location]
  }

  unl <- unique(dataframe$Loc_LOOCV)

  predi <- do.call("rbind",
                   Map(function(leftout){
                     dataframe_cali <- dataframe %>%
                       dplyr::filter(Loc_LOOCV != leftout)

                     dataframe_vali <- dataframe %>%
                       dplyr::filter(Loc_LOOCV == leftout)

                     if(auto){
                       models <- auto_machine_learner(dataframe = dataframe_cali,
                                                      aim_variable = aim_variable,
                                                      co_variables = co_variables,
                                                      method = method)
                     }else{
                       models <- machine_learner(dataframe = dataframe_cali,
                                                 aim_variable = aim_variable,
                                                 co_variables = co_variables,
                                                 method = method, 
                                                 additionalparameters=additionalparameters)
                     }
                     

                     unm <- names(models)

                     predictions <- do.call("rbind",
                                            Map(function(ds){
                                              data.frame(Observed = dataframe_vali[,aim_variable],
                                                         Predicted = predict(get(ds, models),dataframe_vali),
                                                         Location = leftout,
                                                         Method = ds)
                                            }, ds = unm))
                     return(predictions)
                   }, leftout = unl))

  # Generate output file

  if(class(predi$Observed)=="numeric"){
    results <- list(RMSE_all = sqrt(mean((predi$Observed-predi$Predicted)^2,na.rm=TRUE)))

    results$RMSE_ML <- Map(function(eachml){
      ob <- predi %>%
        dplyr::filter(Method == eachml) %>%
        dplyr::select(Observed)

      pr <- predi %>%
        dplyr::filter(Method == eachml) %>%
        dplyr::select(Predicted)

      return(sqrt(mean((unlist(ob-pr))^2,na.rm=TRUE)))
    },
    eachml = method)

    results$RMSE_ML_Loc <- Map(function(eachml,eachloc){
      ob <- predi %>%
        dplyr::filter(Method == eachml & Location == eachloc) %>%
        dplyr::select(Observed)

      pr <- predi %>%
        dplyr::filter(Method == eachml & Location == eachloc) %>%
        dplyr::select(Predicted)

      assign(paste0(eachml,"_",eachloc),sqrt(mean((unlist(ob-pr))^2,na.rm=TRUE)))

      return(get(paste0(eachml,"_",eachloc)))
    },
    eachml = method,
    eachloc = unl)
    names(results$RMSE_ML_Loc) <- Map(function(eachml,eachloc){
      paste0(eachml,"_",eachloc)
    },
    eachml = method,
    eachloc = unl)

    results$LOOCVtab <- predi

    return(results)

    }else{
    results <- list(Table_all = table(Observed = predi$Observed,Predicted = predi$Predicted))




    results$Table_ML <- Map(function(eachml){
      ob <- predi %>%
        dplyr::filter(Method == eachml) %>%
        dplyr::select(Observed)

      pr <- predi %>%
        dplyr::filter(Method == eachml) %>%
        dplyr::select(Predicted)

      return(table(Observed = unlist(ob), Predicted = unlist(pr)))
    },
    eachml = method)

    results$Table_ML_Loc <- Map(function(eachml,eachloc){
      ob <- predi %>%
        dplyr::filter(Method == eachml & Location == eachloc) %>%
        dplyr::select(Observed)

      pr <- predi %>%
        dplyr::filter(Method == eachml & Location == eachloc) %>%
        dplyr::select(Predicted)

      return(table(Observed = unlist(ob), Predicted = unlist(pr)))
    },
    eachml = method,
    eachloc = unl)
    names(results$Table_ML_Loc) <- Map(function(eachml,eachloc){
      paste0(eachml,"_",eachloc)
    },
    eachml = method,
    eachloc = unl)

    results$LOOCVtab <- predi

    return(results)
    }
}








