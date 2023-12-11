library(Boruta)
library(caret)
library(parallel)
library(stringr)

poroto <-
  read.table(
    "poroto_begomo_renombrado.txt",
    sep = "\t",
    dec = ".",
    header = T,
    na.strings = "."
  )

set.seed(1234)
seleccion <- FeatureSelection(data  = poroto, 
                              name_respuesta = "Y", 
                              step = 100,
                              method = c('boruta', 'ag',
                                         'Filtrado', 'Stepwise',
                                         'stepVIF','Lasso'),
                              plot = TRUE,
                              importance = TRUE,
                              family = "binomial",
                              VIF = 5)


seleccion$`Nro Variables`
seleccion$Formulas
seleccion$`Var Seleccionadas`


library(ggVennDiagram)

ggVennDiagram(seleccion$`Var Seleccionadas`,
              color = 1, lwd = 0.7,
              label = "count",
              label_alpha = 0,
              category.names = c("Boruta",
                                 "AG",
                                 "Filtrado",
                                 "LASSO",
                                 "S.Forward",
                                 "StepVIF"))+
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
  dplyr::summarise(dplyr::across(!n,
                                 list("media"=mean, "desvio"=sd)))


summary(validacion$Metricas)
validacion$Plot
