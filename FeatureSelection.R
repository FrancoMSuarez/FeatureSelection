# El codigo esta pensado para una variable respuesta binaria.
# Los metodos que usa son boruta, filtrado, stepwise forward, Step_VIF_Pv y/o
# algoritmo genetico.
# Puede dar el grafico de la importancia de la variable


FeatureSelection <- function(data,
                             name_respuesta,
                             step = 100,
                             method = c('boruta', 'ag',
                                        'Filtrado', 'Stepwise',
                                        'stepVIF','Lasso'),
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

  ## BORUTA ----  
  if ('boruta' %in% method) {
    set.seed(123)
    b <- Boruta::Boruta(
      vec_respuesta ~ .,
      data = data |> dplyr::select(-dplyr::all_of(name_respuesta)),
      doTrace = 3,
      maxRuns = step
    )
    
    if (plot) {
      plot(b,
           las = 2,
           cex.axis = 0.5,
           ylab = "Importance by Boruta")
    }
    
    selectedAttrBor <-
      Boruta::getSelectedAttributes(b, withTentative = F)
    selectedAttrBor <-
      selectedAttrBor[selectedAttrBor != name_respuesta]
    
    res_boruta <- data.frame(
      "Metodo" = "Boruta",
      # "Var Seleccionadas" = paste(selectedAttrBor, collapse = ", "),
      "nro var" = length(selectedAttrBor),
      "lambda" = NA,
      row.names = NULL
    )
    
    base_final <- rbind(base_final, res_boruta)
    
    var_b <- as.character(selectedAttrBor)
    
    Var_Seleccionadas$Boruta <- var_b
    
    fml_boruta <- as.formula(paste(
      name_respuesta,
      paste(selectedAttrBor, collapse = '+'),
      sep = "~"
    ))
    
    formulas$Boruta <- fml_boruta
    
  }
  
  ## AG ----  
  if ('ag' %in% method) {
    set.seed(123)
    agr <-
      caret::gafs(
        x = data |> dplyr::select(-dplyr::all_of(name_respuesta)),
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
          verbose = FALSE
        )
      )
    res_ga <- data.frame(
      "Metodo" = "AG",
      # "Var Seleccionadas" = paste(agr$optVariables,
      #                             collapse = ", "),
      "lambda" = NA,
      "nro var" = length(agr$optVariables),
      row.names = NULL
    )
    
    base_final <- rbind(base_final, res_ga)
    
    var_ag <- as.character(agr$optVariables)
    
    Var_Seleccionadas$AG <- var_ag
    
    fml_AG <- as.formula(paste(
      name_respuesta,
      paste(agr$optVariables, collapse = '+'),
      sep = "~"
    ))
    
    formulas$AG  <- fml_AG
    
    if ('ag' %in% method & importance) {
      importancia_ag <- caret::varImp(
        agr,
        metric = agr$control$metric["external"],
        maximize = agr$control$maximize["external"]
      )
      
      imp_ag <- importancia_ag |>
        dplyr::filter(rownames(importancia_ag) %in% agr$optVariables &
                        !is.na(importancia_ag))
      
      plot_importancia_ag <-
        ggplot2::ggplot(imp_ag, aes(
          x = stats::reorder(rownames(imp_ag), Accuracy),
          y = Accuracy
        )) +
        geom_bar(
          fill = 'skyblue',
          size = 0.5,
          alpha = 0.6,
          stat = 'identity'
        ) +
        xlab('Variable') +
        ylab('Overall Importance by AG') +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 5)) +
        coord_flip()
      
      graficos$Importance_AG <- plot_importancia_ag
      
    }
  }
  
  ## FILTRADO ----  
  if ('Filtrado' %in% method) {
    set.seed(123)
    fil <- caret::sbf(
      as.formula(paste(name_respuesta, paste('.'), sep = "~")),
      data = data,
      sbfControl =
        caret::sbfControl(
          functions = caret::rfSBF,
          method = "repeatedcv",
          number = 5,
          repeats = 10,
          verbose = FALSE,
          seeds = NULL,
          saveDetails = TRUE,
          allowParallel = TRUE
        ),
      ntree = 500
    )
    
    res_fil <- data.frame(
      "Metodo" = "Filtrado",
      "lambda" = NA,
      # "Var Seleccionadas" = paste(fil$optVariables,
      #                             collapse = ", "),
      "nro var" = length(fil$optVariables),
      row.names = NULL
    )
    
    base_final <- rbind(base_final, res_fil)
    
    Var_Seleccionadas$Filtrado <- fil$optVariables
    
    
    fml_FILT <- as.formula(paste(
      name_respuesta,
      paste(fil$optVariables, collapse = '+'),
      sep = "~"
    ))
    
    formulas$Filtrado <- fml_FILT
    
    if ('Filtrado' %in% method & importance) {
      Importancia_Fil <- data.frame(fil[["fit"]][["importance"]])
      
      plot_importancia_Fil <-
        ggplot2::ggplot(Importancia_Fil,
                        aes(
                          x = stats::reorder(rownames(Importancia_Fil), MeanDecreaseGini),
                          y = MeanDecreaseGini
                        )) +
        geom_bar(
          fill = 'skyblue',
          size = 0.5,
          alpha = 0.6,
          stat = 'identity'
        ) +
        xlab('Variable') +
        ylab('Overall Importance by Filter (MeanDecreaseGini)') +
        theme_minimal() +
        coord_flip()
      
      graficos$Importance_Fil <- plot_importancia_Fil
      
    }
    
    
  }
  ## LASSO ----  
  if ("Lasso" %in% method){
    
    lambda <- seq(0, 1, by= 0.001)
    set.seed(1234)
    lasso <- caret::train(
      as.formula(paste(name_respuesta,paste('.'),sep = "~")),
      data = data,
      method = "glmnet",
      trControl = caret::trainControl(
        method = "repeatedcv",
        repeats = 10,
        number = 5
      ),
      preProcess = c("center", "scale"),
      tuneGrid = expand.grid(alpha = 1, lambda = lambda),
      metric = "Accuracy",
      family = "binomial"
    )
    
    var_lasso <- data.frame(as.matrix(coef(lasso$finalModel, 
                                           lasso$bestTune$lambda)))
    var_select <- row.names(var_lasso)[var_lasso$s1!=0]
    var_select <- var_select[!var_select %in% ("(Intercept)")]
    
    Var_Seleccionadas$LASSO <- as.character(var_select)
    
    fml_LASSO <- as.formula(paste(
      name_respuesta,
      paste(var_select, collapse = '+'),
      sep = "~"
    ))
    
    formulas$LASSO <- fml_LASSO
    
    res_LASSO <- data.frame(
      "Metodo" = "LASSO",
      "lambda" = lasso$bestTune$lambda,
      # "Var Seleccionadas" = paste(fil$optVariables,
      #                             collapse = ", "),
      "nro var" = length(var_select),
      row.names = NULL
    )
    
    base_final <- rbind(base_final, res_LASSO)
    
    if ('Lasso' %in% method & importance) {
      imporLASSO <- caret::varImp(lasso, scale = T)
      
      plot_importancia_lasso <-
        ggplot2::ggplot(imporLASSO, aes(
          x = stats::reorder(rownames(imporLASSO), Overall),
          y = Overall
        )) +
        geom_bar(
          fill = 'skyblue',
          size = 0.5,
          alpha = 0.6,
          stat = 'identity'
        ) +
        xlab('Variable') +
        ylab('Overall Importance by LASSO') +
        theme_minimal() +
        coord_flip()
      
      graficos$Importance_LASSO <-
        plot_importancia_lasso
      
    }
    
  }
  
  ## Stepwise F ----  
  if ('Stepwise' %in% method) {
    step_for <-
      caret::train(
        as.formula(paste(name_respuesta, paste('.'), sep = "~")),
        data = data,
        method = "glmStepAIC",
        direction = 'forward',
        trControl = caret::trainControl(
          method = "repeatedcv",
          repeats = 10,
          number = 5
        ),
        preProcess = c("center", "scale"),
        metric = "Accuracy",
        allowParallel = T,
        steps = step
      )
    
    res_Step_F <- data.frame(
      "Metodo" = "Step Forward",
      "lambda" = NA,
      # "Var Seleccionadas" = paste(names(step_for$finalModel$coefficients)[-1],
      #                             collapse = ", "),
      "nro var" = paste(dplyr::count(caret::varImp(
        step_for$finalModel
      ))),
      row.names = NULL
    )
    
    base_final <- rbind(base_final, res_Step_F)
    
    var_Step <-
      as.character(names(step_for$finalModel$coefficients)[-1])
    
    Var_Seleccionadas$StepwiseForward <- var_Step
    
    fml_StepForward <- as.formula(paste(name_respuesta,
                                        paste(
                                          names(step_for$finalModel$coefficients)[-1], collapse = '+'
                                        ),
                                        sep = "~"))
    
    formulas$StepwiseForward <- fml_StepForward
    
    if ('Stepwise' %in% method & importance) {
      Importancia_Step <-
        data.frame(caret::varImp(step_for, scale = T)$importance)
      
      imp_step <-
        Importancia_Step |> dplyr::filter(rownames(Importancia_Step) %in% names(step_for$finalModel$coefficients)[-1])
      
      plot_importancia_Step <-
        ggplot2::ggplot(imp_step, aes(x = stats::reorder(rownames(imp_step), X0), y =
                                        X0)) +
        geom_bar(
          fill = 'skyblue',
          size = 0.5,
          alpha = 0.6,
          stat = 'identity'
        ) +
        xlab('Variable') +
        ylab('Overall Importance by Stepwise Forward') +
        theme_minimal() +
        coord_flip()
      
      graficos$Importance_StepwiseFOrward <- plot_importancia_Step
      
    }
    
  }
  ## Stepwise + VIF + pvalor ----  
  if ('stepVIF' %in% method) {
    if ('Stepwise' %in% method) {
      fml_step <- as.formula(paste(name_respuesta,
                                   paste(
                                     stringr::str_replace_all(as.character(paste(
                                       names(step_for$finalModel$coefficients)[-1],
                                       collapse = ", "
                                     )), ',' , "+")
                                   ),
                                   sep = "~"))
      
      modelo_step <-
        stats::glm(fml_step, data = data, family = family)
      
      vif_step <-
        data.frame(
          variable = names(car::vif(modelo_step)),
          valor = car::vif(modelo_step),
          row.names = NULL
        )
      vif_step <-
        vif_step[with(vif_step, order(-vif_step$valor)),]
      
      while (vif_step[1, 2] > VIF) {
        var_selec_con_vif <-
          vif_step[-c(1), 1]
        
        fml_new_step <-
          as.formula(paste(
            name_respuesta,
            paste(var_selec_con_vif, collapse = "+"),
            sep = "~"
          ))
        
        modelo_step <- stats::glm(fml_new_step,
                                  data = data,
                                  family = family)
        
        vif_step <- data.frame(
          variable = names(car::vif(modelo_step)),
          valor = (car::vif(modelo_step)),
          row.names = NULL
        )
        
        vif_step  <-
          vif_step[with(vif_step, order(-vif_step$valor)),]
        vif_step
      }
      
      var_vif <- summary(modelo_step)
      
      Coef <- data.frame(var_vif$coefficients)
      
      
      
      var_significativas <-
        row.names(Coef[Coef$Pr...z.. < 0.05,])[-1]
      
      res_Step_VIF_Pv <- data.frame(
        "Metodo" = "Step Forward + vif + pv",
        "lambda" = NA,
        # "Var Seleccionadas" = paste(var_significativas,
        #                             collapse = " , "),
        "nro var" = length(var_significativas),
        row.names = NULL
      )
      
      base_final <- rbind(base_final, res_Step_VIF_Pv)
      
      
      
      Var_Seleccionadas$StepVIF <- as.character(var_significativas)
      
      fml_StepVIF <- as.formula(paste(
        name_respuesta,
        paste(var_significativas, collapse = '+'),
        sep = "~"
      ))
      
      formulas$Step_VIF_Pv <-  fml_StepVIF
      
      
      if ('stepVIF' %in% method & importance) {
        modelo_step2 <- stats::glm(fml_StepVIF,
                                   data = data,
                                   family = family)
        
        imporSVP <- caret::varImp(modelo_step2, scale = T)
        
        plot_importancia_StepVIFPv <-
          ggplot2::ggplot(imporSVP, aes(
            x = stats::reorder(rownames(imporSVP), Overall),
            y = Overall
          )) +
          geom_bar(
            fill = 'skyblue',
            size = 0.5,
            alpha = 0.6,
            stat = 'identity'
          ) +
          xlab('Variable') +
          ylab('Overall Importance by Step+VIF+PV') +
          theme_minimal() +
          coord_flip()
        
        graficos$Importance_Step_VIF_Pv <-
          plot_importancia_StepVIFPv
      }
    }
    else
      (cat(
        'WARNING: StepVIF WAS NOT IMPLEMENTED. To implement StepVIF you must implement Stepwise'
      ))
    
    
  }
  
  if (length(graficos) > 0) {
    plts <- gridExtra::arrangeGrob(grobs = graficos)
    gridExtra::grid.arrange(plts)
  }
  doParallel::stopImplicitCluster()
  return(
    list(
      'Nro Variables' = base_final,
      'Formulas' = formulas,
      'Var Seleccionadas' = Var_Seleccionadas,
      'plots' = graficos
    )
  )
}








