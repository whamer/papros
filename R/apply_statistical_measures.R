##' This functions calculates statistical measures for dataframes containing Observed and Predicted factors
##' @title Calculate statistical measures
##' @param dataframe dataframe containing Observed and Predicted variables as created by `loocv_machine_learner`
##' @param ObservedName Character string of the Observed column; default = "Observed"
##' @return dataframe containing Accuracy, Sensitivity, Specificity, Precision and ROC AUC
##' @importFrom ROCR performance prediction
##' @importFrom dplyr mutate select group_by summarize select pull
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export apply_statistical_measures
##' @author Wolfgang Hamer
apply_statistical_measures <- function(dataframe,ObservedName="Observed",PredictedName="Predicted"){
  
  names(dataframe)[which(names(dataframe)==ObservedName)] <- "Observed"
  names(dataframe)[which(names(dataframe)==PredictedName)] <- "Predicted"
  
  year_persp <- dataframe %>% 
    dplyr::mutate(Categories = paste0(Observed,Predicted)) %>% 
    dplyr::select(Categories) %>% 
    dplyr::mutate(Categories = ifelse(Categories== "11","True Positive",ifelse(Categories=="00","True Negative",ifelse(Categories == "10","False Negative","False Positive")))) %>% 
    dplyr::group_by(Categories) %>%
    dplyr::summarize(Count = n())
  
  false_negative <- year_persp %>% dplyr::filter(Categories =="False Negative") %>% dplyr::select(Count) %>% pull()
  false_positive <- year_persp %>% dplyr::filter(Categories =="False Positive") %>% dplyr::select(Count) %>% pull()
  true_negative <- year_persp %>% dplyr::filter(Categories =="True Negative") %>% dplyr::select(Count) %>% pull()
  true_positive <- year_persp %>% dplyr::filter(Categories =="True Positive") %>% dplyr::select(Count) %>% pull()
  
  accuracy <- ifelse(sum(true_negative,true_positive,false_negative,false_positive)!=0,
                     sum(true_negative,true_positive)/sum(true_negative,true_positive,false_negative,false_positive),
                     NA)
  sensitivity <- ifelse(sum(true_positive,false_negative)!=0,
                        true_positive/sum(true_positive,false_negative),
                        NA)
  specificity <- ifelse(sum(true_negative,false_positive)!=0,
                        true_negative/sum(true_negative,false_positive),
                        NA)
  precision <- ifelse(sum(true_positive,false_positive)!=0,
                      true_positive/sum(true_positive,false_positive),
                      NA)
  
  dataframe_pre <- ROCR::prediction(predictions = as.numeric(dataframe$Predicted),
                                    labels = dataframe$Observed)
  dataframe_perfa <- try(ROCR::performance(dataframe_pre,measure = "auc"),silent=TRUE)                                                   
  if(class(dataframe_perfa)!="try-error"){
    dataframe_auc <- unlist(dataframe_perfa@y.values)
  }else{
    dataframe_auc <- NA
  }
  return(data.frame(Measure = c("Accuracy","Sensitivity","Specificity","Precision","ROC AUC"),
                    Value = c(accuracy,sensitivity,specificity,precision,dataframe_auc)))
}






