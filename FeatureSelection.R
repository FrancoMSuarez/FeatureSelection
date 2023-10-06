library(Boruta)
library(caret)
library(parallel)
library(stringr)

# El codigo esta pensado para una variable respuesta binaria.  
# Los metodos que usa son boruta, filtrado, stepwise forward, Step_VIF_Pv y/o 
# algoritmo genetico.
# Puede dar el grafico de la importancia de la variable 


FeatureSelection <- function(data, 
                              name_respuesta, 
                              step = 100, 
                              method = c('boruta','ag',
                                         'Filtrado','Stepwise',
                                         'stepVIF'), 
                              plot = TRUE,
                              importance = TRUE,
                              family = "binomial", 
                              VIF = 5) { 
  
  
  if (!(name_respuesta %in% colnames(data))) {
    stop("name_respuesta (", 
         name_respuesta,
         ") must be a colname in data")
  }
  
  vec_respuesta <- as.factor(data[, name_respuesta])
  data[, name_respuesta] <- as.factor(data[, name_respuesta])
  
  if (nlevels(vec_respuesta) != 2) {
    stop(name_respuesta,
         "must have 2 levels and has", 
         nlevels(vec_respuesta))
  }
  
  cl = max(parallel::detectCores() - 1, 1)
  doParallel::registerDoParallel(cl)
  
  

  
  
  base_final <- data.frame()
  formulas <- list()
  graficos <- list()
  Var_Seleccionadas <- list()
  
  if ('boruta' %in% method) {
    set.seed(123)
    b <- Boruta::Boruta(vec_respuesta ~ .,
                        data = data|> dplyr::select(-dplyr::all_of(name_respuesta)),
                        doTrace = 3,
                        maxRuns = step)
    
    if (plot) {plot(b, las = 2, cex.axis = 0.5,
                    ylab = "Importance by Boruta")}
    
    selectedAttrBor <- Boruta::getSelectedAttributes(b, withTentative = F)
    selectedAttrBor <- selectedAttrBor[selectedAttrBor != name_respuesta]
    
    res_boruta <- data.frame(
      "Metodo" = "Boruta",
      # "Var Seleccionadas" = paste(selectedAttrBor, collapse = ", "),
      "nro var" = length(selectedAttrBor),
      row.names = NULL)
    
    base_final <- rbind(base_final, res_boruta)
    
    var_b <- as.character(selectedAttrBor)
    
    Var_Seleccionadas$Boruta <- var_b
    
    fml_boruta <- as.formula(paste(name_respuesta, 
                                   paste(selectedAttrBor, collapse = '+'),
                                   sep = "~"))
    
    formulas$Boruta <- fml_boruta
    
  } 
  
  if ('ag' %in% method) {
    set.seed(123)
    agr <- caret::gafs(x = data |> dplyr::select(-dplyr::all_of(name_respuesta)),
                      y = vec_respuesta,
                      iters = 10,
                      popSize = 10,
                      ntree = 100,
                      gafsControl = caret::gafsControl(
                        functions = caret::rfGA,
                        method = "cv",
                        number = 5,
                        allowParallel = TRUE,
                        genParallel = TRUE, 
                        verbose = FALSE)
    )
    res_ga <- data.frame(
      "Metodo" = "AG",
      # "Var Seleccionadas" = paste(agr$optVariables,
      #                             collapse = ", "),
      "nro var" = length(agr$optVariables),
      row.names = NULL)
    
    base_final <- rbind(base_final, res_ga)
    
    var_ag <- as.character(agr$optVariables)
    
    Var_Seleccionadas$AG <- var_ag
    
    fml_AG<- as.formula(paste(name_respuesta, 
                                   paste(agr$optVariables, collapse = '+'),
                                   sep = "~"))
    
    formulas$AG  <- fml_AG
    
    if('ag' %in% method & importance) {
      importancia_ag <- caret::varImp(
        agr,
        metric = agr$control$metric["external"],
        maximize = agr$control$maximize["external"]
      )
      
      imp_ag <- importancia_ag |> 
        dplyr::filter(rownames(importancia_ag) %in% agr$optVariables & 
                        !is.na(importancia_ag))
      
      plot_importancia_ag <- ggplot2::ggplot(imp_ag,aes(x=stats::reorder(rownames(imp_ag),Accuracy),y=Accuracy)) +
        geom_bar(fill='skyblue', size=0.5, alpha=0.6, stat = 'identity') +
        xlab('Variable')+
        ylab('Overall Importance by AG')+
        theme_minimal() +
        theme(axis.text.y = element_text(size = 5))+
        coord_flip()
      
      graficos$Importance_AG <- plot_importancia_ag
      
    }
  } 
  
  if ('Filtrado' %in% method) {
    set.seed(123)
    fil <- caret::sbf(
      as.formula(paste(name_respuesta, paste('.'), sep = "~")),
      data = data,
      sbfControl = 
        caret::sbfControl(functions = caret::rfSBF, 
                          method = "repeatedcv",
                          number = 5, repeats = 10,
                          verbose = FALSE, seeds = NULL,
                          saveDetails = TRUE, allowParallel = TRUE),
      ntree = 500)
    
    res_fil <- data.frame(
      "Metodo" = "Filtrado",
      # "Var Seleccionadas" = paste(fil$optVariables,
      #                             collapse = ", "),
      "nro var" = length(fil$optVariables),
      row.names = NULL)
    
    base_final <- rbind(base_final, res_fil)
    
    Var_Seleccionadas$Filtrado <- fil$optVariables
    
    
    fml_FILT<- as.formula(paste(name_respuesta, 
                              paste(fil$optVariables, collapse = '+'),
                              sep = "~"))
    
    formulas$Filtrado <- fml_FILT
    
    if('Filtrado' %in% method & importance) {
      
      Importancia_Fil <- data.frame(fil[["fit"]][["importance"]])
      
      plot_importancia_Fil <- ggplot2::ggplot(Importancia_Fil,aes(x=stats::reorder(rownames(Importancia_Fil),MeanDecreaseGini),y=MeanDecreaseGini)) +
        geom_bar(fill='skyblue', size=0.5, alpha=0.6, stat = 'identity') +
        xlab('Variable')+
        ylab('Overall Importance by Filter (MeanDecreaseGini)')+
        theme_minimal() +
        coord_flip()
      
      graficos$Importance_Fil <- plot_importancia_Fil
      
    }
    
    
  } 
  
  if ('Stepwise' %in% method) {
    step_for <-
      caret::train(
        as.formula(paste(name_respuesta, paste('.'), sep = "~")),
        data = data,
        method = "glmStepAIC",
        direction = 'forward',
        trControl = caret::trainControl(method = "repeatedcv",
                                        repeats = 10,
                                        number = 5),
        metric = "Accuracy",
        allowParallel = T,
        steps = step
      )
    
    res_Step_F <- data.frame(
      "Metodo" = "Step Forward",
      # "Var Seleccionadas" = paste(names(step_for$finalModel$coefficients)[-1], 
      #                             collapse = ", "),
      "nro var" = paste(dplyr::count(caret::varImp(step_for$finalModel))),
      row.names = NULL)
    
    base_final <- rbind(base_final, res_Step_F)
    
    var_Step <- as.character(names(step_for$finalModel$coefficients)[-1])
    
    Var_Seleccionadas$StepwiseForward <- var_Step
    
    fml_StepForward <- as.formula(paste(name_respuesta, 
                                paste(names(step_for$finalModel$coefficients)[-1], collapse = '+'),
                                sep = "~"))
    
    formulas$StepwiseForward <- fml_StepForward
    
    if('Stepwise' %in% method & importance) {
      
      Importancia_Step <- data.frame(caret::varImp(step_for, scale = T)$importance)
      
      imp_step <- Importancia_Step |> dplyr::filter(rownames(Importancia_Step) %in% names(step_for$finalModel$coefficients)[-1] )
      
      plot_importancia_Step <- ggplot2::ggplot(imp_step,aes(x=stats::reorder(rownames(imp_step),X0),y=X0)) +
        geom_bar(fill='skyblue', size=0.5, alpha=0.6, stat = 'identity') +
        xlab('Variable')+
        ylab('Overall Importance by Stepwise Forward')+
        theme_minimal() +
        coord_flip()
      
      graficos$Importance_StepwiseFOrward <- plot_importancia_Step
      
    }
    
  }
  
  if ('stepVIF' %in% method) {
    if('Stepwise' %in% method){
    fml_step <- as.formula(
      paste(name_respuesta, 
            paste(stringr::str_replace_all(
              as.character(paste(names(step_for$finalModel$coefficients)[-1], 
                                collapse = ", ")), ',' , "+")), 
            sep = "~"))
    
    modelo_step <- stats::glm(fml_step, data = data, family = family)
    
    vif_step <- data.frame(variable = names(car::vif(modelo_step)),
                           valor = car::vif(modelo_step),
                           row.names = NULL)
    vif_step <-
      vif_step[with(vif_step, order(-vif_step$valor)), ]
    
    while (vif_step[1, 2] > VIF) {
      var_selec_con_vif <-
        vif_step[-c(1), 1]
      
      fml_new_step <-
        as.formula(paste(name_respuesta, 
                         paste(var_selec_con_vif, collapse = "+"),
                         sep = "~"))
      
      modelo_step <- stats::glm(fml_new_step,
                             data = data,
                             family = family)
      
      vif_step <- data.frame(
        variable = names(car::vif(modelo_step)),
        valor = (car::vif(modelo_step)),
        row.names = NULL
      )
      
      vif_step  <-
        vif_step[with(vif_step, order(-vif_step$valor)), ]
      vif_step
    }
    
    var_vif <- summary(modelo_step)
    
    Coef <- data.frame(var_vif$coefficients)
    
    
    
    var_significativas <- row.names(Coef[Coef$Pr...z.. < 0.05, ])[-1]
    
    res_Step_VIF_Pv <- data.frame(
      "Metodo" = "Step Forward + vif + pv",
      # "Var Seleccionadas" = paste(var_significativas, 
      #                             collapse = " , "),
      "nro var" = length(var_significativas),
      row.names = NULL)
    
    base_final <- rbind(base_final, res_Step_VIF_Pv)
    
    
    
    Var_Seleccionadas$StepVIF <- as.character(var_significativas)
    
    fml_StepVIF <- as.formula(paste(name_respuesta, 
                                        paste(var_significativas, collapse = '+'),
                                        sep = "~"))
    
    formulas$Step_VIF_Pv <-  fml_StepVIF
    
    
    if('stepVIF' %in% method & importance) {
      
      modelo_step2 <- stats::glm(fml_StepVIF,
                                data = data,
                                family = family)
      
      imporSVP <- caret::varImp(modelo_step2, scale = T)
      
      plot_importancia_StepVIFPv <- ggplot2::ggplot(imporSVP,aes(x=stats::reorder(rownames(imporSVP), Overall),y= Overall)) +
        geom_bar(fill='skyblue', size=0.5, alpha=0.6, stat = 'identity') +
        xlab('Variable')+
        ylab('Overall Importance by Step+VIF+PV')+
        theme_minimal() +
        coord_flip()
      
      graficos$Importance_Step_VIF_Pv <- plot_importancia_StepVIFPv
    }
    }
    else (cat('WARNING: StepVIF WAS NOT IMPLEMENTED. To implement StepVIF you must implement Stepwise'))
    
    
  } 
  
  if(length(graficos)>0) {
    plts <- gridExtra::arrangeGrob(grobs = graficos)
    gridExtra::grid.arrange(plts)
  }
  doParallel::stopImplicitCluster()
  return(list('Nro Variables'= base_final, 
              'Formulas'=formulas,
              'Var Seleccionadas' = Var_Seleccionadas))
} 


Ajustes <- function(data,
                    name_respuesta,
                    p = 0.8,
                    formulas = formula,
                    method_cv = "boot",
                    number = 100,
                    Model= c('glm','rf'),
                    plotroc = T,
                    n = 50) { 
  
  MetricasF <- data.frame()
  PredichosF <- data.frame()
  Metricas <- data.frame()
  Predichos <- data.frame()
  
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
    allowParallel = F,
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
                                               Modelo = "Logistic Regresion",
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
                                               Modelo = "Random Forest",
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

  
  if (plotroc) {
    grupos <-  split(x = PredichosF, f = list(PredichosF$Modelo, PredichosF$Seleccion), drop = TRUE) |> 
    lapply(FUN = \(x) {
      pROC::roc(Observados ~ Predichos.1, data = x)
     })
  

  plot_roc <- pROC::ggroc(grupos) + 
    theme_minimal()+
    geom_abline(slope = 1, intercept = 1, linetype = "dashed")+
    scale_color_discrete(name = "Combinacion Seleccion y Ajuste")
  }
 
  
  return(list(Metricas = MetricasF,Predichos = PredichosF, Plot = plot_roc))
  }


poroto <-
  read.table(
    "poroto_begomo_renombrado.txt",
    sep = "\t",
    dec = ".",
    header = T,
    na.strings = "."
  )

names(poroto)

colnames(poroto) <- noquote(c("Y",(paste0("X",seq(ncol(poroto)-1)))))
poroto

set.seed(1234)
seleccion <- FeatureSelection(data  = poroto, 
                               name_respuesta = "Y", 
                               step = 100, 
                               method = c('boruta','ag',
                                          'Filtrado','Stepwise'),
                               plot = TRUE,
                               importance = TRUE,
                               family = "binomial", 
                               VIF = 5)


seleccion$`Nro Variables`
seleccion$Formulas
seleccion$`Var Seleccionadas`

library(ggVennDiagram)

ggVennDiagram(seleccion$`Var Seleccionadas`[1:4],
              color = 1, lwd = 0.7,
              label = "count",
              label_alpha = 0,
              category.names = c("Boruta",
                                 "AG",
                                 "Filtrado",
                                 "S.Forward"))+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  theme(legend.position = "none")





validacion <- Ajustes(data = poroto,
                      name_respuesta = 'Y',
                      p = 0.8,
                      formulas = seleccion$Formulas,
                      method_cv = "boot",
                      number = 10,
                      Model= c('glm','rf'),
                      plotroc = T,
                      n = 5)


resumen <- validacion$Metricas |> dplyr::group_by(Modelo, Seleccion) |> 
  dplyr::summarise_all(list(mean, sd))


summary(validacion$Metricas)
validacion$Plot

