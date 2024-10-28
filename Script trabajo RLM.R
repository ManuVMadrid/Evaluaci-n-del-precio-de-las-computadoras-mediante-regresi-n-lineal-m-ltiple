datos
datos$Brand<- as.factor(datos$Brand)
datos$Processor<- as.factor(datos$Processor)
datos$graphiccard<- as.factor(datos$`Graphics Card`)
datos$operativesistem<- as.factor(datos$`Operating System`)
datos$screen<- as.factor(datos$`Screen Size`)
#1 analisis exploratorios, 

####Graficos con las variables cualitativas####
boxplot(formula=Price ~ Brand, data=datos)
     #Con model da muy raro
boxplot(formula=Price ~ Processor, data=datos)
boxplot(formula=Price ~ `Graphics Card`, data=datos)
boxplot(formula=Price ~ `Operating System`, data=datos)
boxplot(formula=Price ~ `Screen Size`, data=datos)

####Graficas con las variables cuantitativas####
# Seleccionar solo las columnas cuantitativas
var_cuanti <- datos[, c("Weight", "Battery Life", "Price")]
# Crear la matriz de dispersión
pairs(var_cuanti)
library(ggplot2)

ggplot(var_cuanti, aes(x = Price, y = `Battery Life`)) + 
  geom_point() + 
  geom_smooth(method = lm, color = 'purple', fill = 'black', se = TRUE)

ggplot(var_cuanti, aes(x = Price, y = Weight)) + 
  geom_point() + 
  geom_smooth(method = lm, color = 'purple', fill = 'black', se = TRUE)

#analizar si son cualti o cuanti, 
#cual es la variable respuesta y regresoras
#hacer los graficos, boxplot, graficos de dispersion, matris de dispercion 
#graficas solas con la variable respuesta

##2 analisis de multicolinealidad
library(car)

m1 <- lm(Price ~ ., data = var_cuanti)
summary(m1)
vif(m1)

m2 <- lm(Price ~ ., data = datos)
summary(m2)
vif(m2)

anova <- aov(Price ~ ., data = datos)
summary(anova)

#se observa en la matriz de graficos de dispersion
#con el vif analizo multicolinealidad si son mayores a 5 y la correlacion entre las x
#analizar si hay multicolinealidad si las quito, si las agrupo

###3 hacer el modelo
#hacer normalidad, varianza, independencia
#Normalidad
residuales <- rstandard(m1)
shapiro.test(residuales)
#no son normales 

#Homocedasticidad
library(lmtest)
bptest(m1)
#son normales

#independencia
bgtest(m1)
#normales

#si no funciona lo ajusto, cuales son los puntos de influencias, outliars

####4 refinar el modelo con la seleccion de las variable
## con los steps, sin embargo, mirar que se puede eliminar y que no
influence.measures(m1)

quantile(datos$Weight..kg., probs =c(0.10, 0.95))
quantile(datos$Battery.Life..hours., probs =c(0.10, 0.95))

modf <- step(m1, trace = T, direction = "backward")# va de adelante hacia atras buscando el mejor modelo
modf
#####5 evaluacion del modelo
#train
sample <- sample.int(n = nrow(var_cuanti), size = floor(0.80*nrow(var_cuanti)), replace = F)
sample
train <- var_cuanti[sample, ] #entrenamiento
test <- var_cuanti[-sample, ]

modelotrainin <- lm(formula = Price~., data = train)
summary(modelotrainin)
#test

modelotesting <- lm(formula = Price ~ ., data = test)
prediccion <- predict.lm(modelotesting, data = test[, "Battery.Life..hours.", "Weight..kg."])
summary(prediccion)

plot(test$Price, prediccion)

#matrics

library(Metrics)
metricas <- c(mae(test$Price, prediccion),
              mape(test$Price, prediccion),
              mse(test$Price, prediccion),
              rmse(test$Price, prediccion),
              AIC(modelotesting),
              BIC(modelotesting),
              summary(modelotesting)$r.squared)
names(metricas) <- c("MAE", "MAPE", "MSE", "RMSE", "AIC", "BIC", "R2")
metricas

# ver si responde a las preguntas del inicio del problema
