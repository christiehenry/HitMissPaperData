
predict_a9095_percent_bias = function(folder, Distribution, overlap, evenness, n, CI){

  if(CI=="all"){
    load(paste(folder,Distribution,"_a9095_Percent_Bias_Models_ModWald.RData", sep=""))
    load(paste(folder,Distribution,"_a9095_Percent_Bias_Models_StdWald.RData", sep=""))
    load(paste(folder,Distribution,"_a9095_Percent_Bias_Models_LR.RData", sep=""))
    load(paste(folder,Distribution,"_a9095_Percent_Bias_Models_MLR.RData", sep=""))
  }else{
    LoadLocation = switch(
      CI,
      "Mod. Wald"  = paste(folder,Distribution,"_a9095_Percent_Bias_Models_ModWald.RData", sep=""),
      "Std. Wald"  = paste(folder,Distribution,"_a9095_Percent_Bias_Models_StdWald.RData", sep=""),
      "LR"         = paste(folder,Distribution,"_a9095_Percent_Bias_Models_LR.RData", sep=""),
      "MLR"        = paste(folder,Distribution,"_a9095_Percent_Bias_Models_MLR.RData", sep="")
    )
    load(LoadLocation)
  }

  
  newData  = data.frame(Distribution = Distribution, 
                        overlap      = overlap, 
                        evenness     = evenness, 
                        n            = n)
  
  if(CI=="LR"){  
    Bias_perc_df = getPredictedBiasPerCI(newData, 
                          linear_model=a9095.LR.boxcox.perc, 
                          lambda=bc_lambda_LR.perc, 
                          min_value=LR.min.a9095.perc)
    Bias_perc_df$CI = "LR"
  }
  if(CI=="MLR"){  
    Bias_perc_df = getPredictedBiasPerCI(newData, 
                                         linear_model=a9095.MLR.boxcox.perc, 
                                         lambda=bc_lambda_MLR.perc, 
                                         min_value=MLR.min.a9095.perc)
    Bias_perc_df$CI = "MLR"
  }  
  if(CI=="Std. Wald"){  
    Bias_perc_df = getPredictedBiasPerCI(newData, 
                                         linear_model=a9095.Wald.old.boxcox.perc, 
                                         lambda=bc_lambda_Wald.old.perc, 
                                         min_value=Wald.old.min.a9095.perc)
    Bias_perc_df$CI = "Std. Wald"
  }
  if(CI=="Mod. Wald"){  
    Bias_perc_df = getPredictedBiasPerCI(newData, 
                                         linear_model=a9095.Wald.boxcox.perc, 
                                         lambda=bc_lambda_Wald.perc, 
                                         min_value=Wald.min.a9095.perc)
    Bias_perc_df$CI = "Mod. Wald"
  }
  
  if(CI=="all"){  
    LR_Bias_perc_df = getPredictedBiasPerCI(newData, 
                                         linear_model=a9095.LR.boxcox.perc, 
                                         lambda=bc_lambda_LR.perc, 
                                         min_value=LR.min.a9095.perc)
    LR_Bias_perc_df$CI="LR"
    MLR_Bias_perc_df = getPredictedBiasPerCI(newData, 
                                         linear_model=a9095.MLR.boxcox.perc, 
                                         lambda=bc_lambda_MLR.perc, 
                                         min_value=MLR.min.a9095.perc)
    MLR_Bias_perc_df$CI="MLR"
    Std.Wald.Bias_perc_df = getPredictedBiasPerCI(newData, 
                                         linear_model=a9095.Wald.old.boxcox.perc, 
                                         lambda=bc_lambda_Wald.old.perc, 
                                         min_value=Wald.old.min.a9095.perc)
    Std.Wald.Bias_perc_df$CI="Std. Wald"
    Wald.Bias_perc_df = getPredictedBiasPerCI(newData, 
                                         linear_model=a9095.Wald.boxcox.perc, 
                                         lambda=bc_lambda_Wald.perc, 
                                         min_value=Wald.min.a9095.perc)
    Wald.Bias_perc_df$CI="Mod. Wald"
    Bias_perc_df = rbind(LR_Bias_perc_df, MLR_Bias_perc_df,Std.Wald.Bias_perc_df,Wald.Bias_perc_df)
  }
  
  return(Bias_perc_df)
}



