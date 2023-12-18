Ajustes <- function(data,
                    name_respuesta,
                    p = 0.8,
                    formulas = formula,
                    method_cv = "boot",
                    number = 100,
                    Model= c('glm','rf'),
                    plotroc = c("both","rf","glm"),
                    n = 50) { 
  
  MetricasF <- data.frame()
  PredichosF <- data.frame()
  Metricas <- data.frame()
  Predichos <- data.frame()
  plots<- c()
  
  if (!(name_respuesta %in% colnames(data))) {
    stop("name_respuesta (", 
         name_respuesta,
         ") must be a colname in data")
  }
  
  vec_respuesta <- as.factor(data[, name_respuesta])
  data[, name_respuesta] <- as.factor(data[, name_respuesta])
  
  
  for (i in seq_len(n)) {
    entrenar <- caret::createDataPartition(
      vec_respuesta,
      p = p,
      list = F,
      times = 1
    )
    Entrenamiento <- data[entrenar,]
    Validacion <- data[-entrenar,] 
    
    trControl <- caret::trainControl(
      method = method_cv,
      number = number,
      allowParallel = T,
      savePredictions = T)
    
    if('glm' %in% Model) {
      for (i in seq(length(formulas))){
        modelo <- caret::train(
          formulas[[i]],
          data = Entrenamiento,
          trControl =  trControl,
          family = "binomial",
          method = "glm",
          metric = "Accuracy"
        )
        
        pred <-  predict(modelo, Validacion)
        
        pred_en_prob <-  predict(modelo, Validacion, type = "prob")
        Predichos <- rbind(Predichos, data.frame(Predichos = pred_en_prob,
                                                 Observados = Validacion[[name_respuesta]],
                                                 Modelo = "RL",
                                                 Seleccion = names(formulas)[[i]]))
        
        
        
        
        resultados <- data.frame(
          n = n,
          Acc_train = modelo$results$Accuracy,
          Acc_test = caret::confusionMatrix(Validacion[[name_respuesta]], pred,
                                            positive = '1', mode = 'everything')$overall['Accuracy'],
          Spe_test = caret::confusionMatrix(Validacion[[name_respuesta]], pred,
                                            positive = '1', mode = 'everything')$byClass['Specificity'],
          Sen_test = caret::confusionMatrix(Validacion[[name_respuesta]], pred,
                                            positive = '1', mode = 'everything')$byClass['Sensitivity'],
          AUC_test = rfUtilities::accuracy(pred, Validacion[[name_respuesta]])$auc,
          row.names = NULL
        )
        
        resultados$Modelo <- "Log"
        resultados$Seleccion <- names(formulas)[[i]]
        Metricas <- rbind(Metricas,resultados)
      }
      
    }
    
    if('rf' %in% Model){
      for (i in seq(length(formulas))){
        modelo <- caret::train(
          formulas[[i]],
          data = Entrenamiento,
          trControl =  trControl,
          method = "rf",
          metric = "Accuracy"
        )
        
        pred <- predict(modelo, Validacion)
        
        
        pred_en_prob <-  predict(modelo, Validacion, type = "prob")
        Predichos <- rbind(Predichos, data.frame(Predichos = pred_en_prob,
                                                 Observados = Validacion[[name_respuesta]],
                                                 Modelo = "RF",
                                                 Seleccion = names(formulas)[[i]]))
        
        
        
        
        resultados <- data.frame(
          n = n,
          Acc_train = max(modelo$results$Accuracy),
          Acc_test = caret::confusionMatrix(Validacion[[name_respuesta]], pred,
                                            positive = '1', 
                                            mode = 'everything')$overall['Accuracy'],
          Spe_test = caret::confusionMatrix(Validacion[[name_respuesta]], pred,
                                            positive = '1', 
                                            mode = 'everything')$byClass['Specificity'],
          Sen_test = caret::confusionMatrix(Validacion[[name_respuesta]], pred,
                                            positive = '1', 
                                            mode = 'everything')$byClass['Sensitivity'],
          AUC_test = rfUtilities::accuracy(pred, Validacion[[name_respuesta]])$auc,
          row.names = NULL
        )
        
        resultados$Modelo <- "RF"
        resultados$Seleccion <- names(formulas)[[i]]
        Metricas <- rbind(Metricas,resultados)
      }
    }
    
  } 
  MetricasF <- rbind(MetricasF,Metricas)
  PredichosF <- rbind(PredichosF,Predichos)
  
  library(paletteer)
  if ('rf' %in% plotroc) {
    grupos1 <-
      split(
        x = PredichosF,
        f = list(PredichosF[PredichosF$Modelo == "RF", ]$Modelo , PredichosF$Seleccion),
        drop = TRUE
      ) |>
      lapply(FUN = \(x) {
        pROC::roc(Observados ~ Predichos.1, data = x)
      })
    plot_roc1 <- pROC::ggroc(grupos1,size = 1.2) + 
      theme_minimal()+
      geom_abline(slope = 1, intercept = 1, linetype = "dashed")+
      xlab("Especificidad") + ylab("Sensibilidad") +
      scale_color_manual(name = "Combinacion RF y metodo seleccion",
                         values=c(paletteer_d("ggsci::planetexpress_futurama")))
    
    plots$roc_RF <- plot_roc1
    
  }
  
  if ('glm' %in% plotroc){
    grupos2 <-
      split(
        x = PredichosF,
        f = list(PredichosF[PredichosF$Modelo == "RL", ]$Modelo , PredichosF$Seleccion),
        drop = TRUE
      ) |>
      lapply(FUN = \(x) {
        pROC::roc(Observados ~ Predichos.1, data = x)
      })
    
    

    plot_roc2 <- pROC::ggroc(grupos2,size = 1.2) + 
      theme_minimal()+
      geom_abline(slope = 1, intercept = 1, linetype = "dashed")+
      xlab("Especificidad") + ylab("Sensibilidad") +
      scale_color_manual(name = "Combinacion RL y metodo seleccion",
                         values=c(paletteer_d("ggsci::planetexpress_futurama")))
    
    plots$roc_glm <- plot_roc2
  }

  if ('both' %in% plotroc){
    grupos1 <-
      split(
        x = PredichosF,
        f = list(PredichosF[PredichosF$Modelo == "RF", ]$Modelo , PredichosF$Seleccion),
        drop = TRUE
      ) |>
      lapply(FUN = \(x) {
        pROC::roc(Observados ~ Predichos.1, data = x)
      })
    plot_roc1 <- pROC::ggroc(grupos1,size = 1.2) + 
      theme_minimal()+
      geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
      xlab("Especificidad") + ylab("Sensibilidad") +
      scale_color_manual(name = "Combinacion RF y metodo seleccion",
                         values=c(paletteer_d("ggsci::planetexpress_futurama")))
    
    grupos2 <-
      split(
        x = PredichosF,
        f = list(PredichosF[PredichosF$Modelo == "RL", ]$Modelo , PredichosF$Seleccion),
        drop = TRUE
      ) |>
      lapply(FUN = \(x) {
        pROC::roc(Observados ~ Predichos.1, data = x)
      })
    
    
    
    plot_roc2 <- pROC::ggroc(grupos2,size = 1.2) + 
      theme_minimal()+
      geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
      xlab("Especificidad") + ylab("Sensibilidad") +
      scale_color_manual(name = "Combinacion RL y metodo seleccion",
                         values=c(paletteer_d("ggsci::planetexpress_futurama")))
    library(patchwork)
    
    both <- plot_roc1 / plot_roc2

    plots$both <- both
  }
  
  
  return(list(Metricas = MetricasF,Predichos = PredichosF, Plot = plots))
}


