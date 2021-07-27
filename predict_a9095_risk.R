
predict_a9095_risk = function(folder, Distribution, overlap, evenness, n, CI){
  
  
  if(CI=="all"){
    load(paste(folder,Distribution,"_a9095_Probability_of_Not_Existing_ModWald.RData", sep=""))
    load(paste(folder,Distribution,"_a9095_Probability_of_Not_Existing_StdWald.RData", sep=""))
    load(paste(folder,Distribution,"_a9095_Probability_of_Not_Existing_LR.RData", sep=""))
    load(paste(folder,Distribution,"_a9095_Probability_of_Not_Existing_MLR.RData", sep=""))
  }else{
    LoadLocation = switch(
      CI,
      "Mod. Wald"  = paste(folder,Distribution,"_a9095_Probability_of_Not_Existing_ModWald.RData", sep=""),
      "Std. Wald"  = paste(folder,Distribution,"_a9095_Probability_of_Not_Existing_StdWald.RData", sep=""),
      "LR"         = paste(folder,Distribution,"_a9095_Probability_of_Not_Existing_LR.RData", sep=""),
      "MLR"        = paste(folder,Distribution,"_a9095_Probability_of_Not_Existing_MLR.RData", sep="")
    )
    load(LoadLocation)
  }


  newData  = data.frame(Distribution = Distribution, 
                        overlap      = overlap, 
                        evenness     = evenness, 
                        n            = n)
  
  if(CI=="LR"){  
    pone = getPredictedExistencePerCI(newData, glm_model = LR.a9095.exist)
    pone$CI = "LR"
  }
  if(CI=="MLR"){  
    pone = getPredictedExistencePerCI(newData, glm_model = MLR.a9095.exist)
    pone$CI = "MLR"
  }  
  if(CI=="Std. Wald"){  
    pone = getPredictedExistencePerCI(newData, glm_model = Wald.old.a9095.exist)
    pone$CI = "Std. Wald"
  }
  if(CI=="Mod. Wald"){  
    pone = getPredictedExistencePerCI(newData, glm_model = Wald.a9095.exist)
    pone$CI = "Mod. Wald"
  }
  if(CI=="all"){  
    LR.pone = getPredictedExistencePerCI(newData, glm_model = LR.a9095.exist)
    LR.pone$CI = "LR"
    MLR.pone = getPredictedExistencePerCI(newData, glm_model = MLR.a9095.exist)
    MLR.pone$CI = "MLR"
    Std.Wald.pone = getPredictedExistencePerCI(newData, glm_model = Wald.old.a9095.exist)
    Std.Wald.pone$CI = "Std. Wald"
    Wald.pone = getPredictedExistencePerCI(newData, glm_model = Wald.a9095.exist)
    Wald.pone$CI = "Mod. Wald"
    pone = rbind(LR.pone, MLR.pone, Std.Wald.pone, Wald.pone)
  }
  return(pone)
}



