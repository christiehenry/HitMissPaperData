
predict_a90_percent_bias = function(folder, Distribution, overlap, evenness, n){
  
  LoadLocation = paste(folder,Distribution,"_a90_Percent_Bias_Models.RData", sep="")
  
  load(LoadLocation)
  linear_model = a90.Wald.boxcox.perc
  lambda       = a90.bc_lambda_Wald.perc
   min_value    = Wald.min.a90.perc
  
  Bias_perc_df=data.frame()
  for(model in c("Logit", "Firth", "Lasso", "RSS")){
    NEW_Data  = data.frame(Distribution = Distribution, Overlap=overlap, 
                           Even=evenness, Model=model, N=n)
    options(warn=-1)
    
    pred.LR   = predict(linear_model,   newdata=NEW_Data, type="response",
                        allow.new.levels=TRUE,se.fit=TRUE)
    pred.LR.pi  =predict(linear_model, NEW_Data, interval="predict") 
    pred.LR.ci  =predict(linear_model, NEW_Data, interval="confidence") 

    if(length(pred.LR.pi)==3){
      pred.LR.df=data.frame(
        ci.lwr = pred.LR.ci[[2]],
        ci.upr = pred.LR.ci[[3]],
        pi.lwr = pred.LR.pi[[2]],
        pi.upr = pred.LR.pi[[3]]
      )
    }else{
      pred.LR.df= data.frame(
        ci.lwr = pred.LR.ci[,2],
        ci.upr = pred.LR.ci[,3],
        pi.lwr = pred.LR.pi[,2],
        pi.upr = pred.LR.pi[,3]
      )
    }
    
    if(lambda!=0){
      NEW_Data$percent.bias      = (pred.LR$fit*lambda+1)^(1/lambda)-1e-7+min_value
      NEW_Data$low.confidence    = (pred.LR.df$ci.lwr*lambda+1)^(1/lambda)-1e-7+min_value
      NEW_Data$up.confidence     = (pred.LR.df$ci.upr*lambda+1)^(1/lambda)-1e-7+min_value
      NEW_Data$low.pred.interval = (pred.LR.df$pi.lwr*lambda+1)^(1/lambda)-1e-7+min_value
      NEW_Data$up.pred.interval  = (pred.LR.df$pi.upr*lambda+1)^(1/lambda)-1e-7+min_value
    }else{
      NEW_Data$percent.bias      = exp(pred.LR$fit)-1e-7+min_value
      NEW_Data$low.confidence    = exp(pred.LR.df$ci.lwr)-1e-7+min_value
      NEW_Data$up.confidence     = exp(pred.LR.df$ci.upr)-1e-7+min_value
      NEW_Data$low.pred.interval = exp(pred.LR.df$pi.lwr)-1e-7+min_value
      NEW_Data$up.pred.interval  = exp(pred.LR.df$pi.upr)-1e-7+min_value
    }
    
    NEW_Data$Overlap_P = paste(100*overlap,'%',sep="")
    NEW_Data$Even_P    = paste(100*evenness,'%',sep="")
    
    Bias_perc_df = rbind(Bias_perc_df,NEW_Data)
  }
  return(Bias_perc_df)
}



