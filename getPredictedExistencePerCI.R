getPredictedExistencePerCI = function(newData, glm_model){
  return_DF=data.frame()
  for(model in c("Logit", "Firth", "Lasso", "RSS")){
    NEW_Data  = data.frame(Distribution = newData$Distribution, 
                           Overlap=newData$overlap, 
                           Even=newData$evenness, Model=model, N=newData$n)
    options(warn=-1)
    
    # Predict in linear space       
    pred_levels  = predict(glm_model,  newdata=NEW_Data, type="response", 
                           allow.new.levels=TRUE, se.fit=TRUE)
    # Predict in probability space
    pred_levels_link  = predict(glm_model,  newdata=NEW_Data, type="link", 
                                allow.new.levels=TRUE, se.fit=TRUE)
    
    NEW_Data$linear_predict = pred_levels$fit
    NEW_Data$pred_probability = pred_levels_link$fit
    
    NEW_Data$Overlap_P = paste(100*overlap,'%',sep="")
    NEW_Data$Even_P    = paste(100*evenness,'%',sep="")
    
    return_DF = rbind(return_DF,NEW_Data)
  }
  
  # use ciTools to add a bootstrapped confidence interval
  library(ciTools)
  a9095.exist.CI <- add_ci(return_DF, glm_model, names = c("lwr", "upr"), alpha = 0.05, 
                              nsims = 5000, type="parametric", yhatName="boot_pred")
  return_DF$Boot.fit = a9095.exist.CI$boot_pred
  return_DF$Boot.lwr = a9095.exist.CI$lwr
  return_DF$Boot.upr = a9095.exist.CI$upr
  
  return(return_DF)
}