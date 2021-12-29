if(!require(ggplot2)){
  install.packages('ggplot2')
  library(ggplot2)
}
if(!require(dplyr)){
  install.packages('dplyr')
  library(dplyr)
}
if(!require(tidyverse)){
  install.packages('tidyverse')
  library(tidyverse)
}
if(!require(ggcorrplot)){
  install.packages("ggcorrplot")
  library(ggcorrplot)
}
if(!require(Hmisc)){
  install.packages("Hmisc")
  library(Hmisc)
}
if(!require(grid)){
  install.packages("grid")
  library(grid)
}
if(!require(gridExtra)){
  install.packages("gridExtra")
  library(gridExtra)
}
if(!require(nortest)){
  install.packages("nortest")
  library(nortest)
}
if(!require(scales)){
  install.packages("scales")
  library(scales)
}
# Usar comma_format




# Lectura de datos
red.wines.DT <- read.csv("D:/Master Ciencia de Datos/Tipología y ciclo de vida de datos/Practica 2/winequality-red.csv")
head(red.wines.DT)

summary(red.wines.DT)

# Tipo de dato asignado a cada campo
sapply(red.wines.DT, function(x) class(x))
str(red.wines.DT)

rows <- dim(red.wines.DT)[1]
columns <- dim(red.wines.DT)[2]
rows
columns


# Comprobamos si existen datos NA's o "".
colSums(is.na(red.wines.DT))
colSums(red.wines.DT == "")
# Otra forma de conocer los valores NA's
sapply(red.wines.DT, function(x) sum(is.na(x)))


# Análisis de outliers
summary(red.wines.DT)
# IQR para fixed.acidity
Q1.fa <- 22
Q3.fa <- 62

IQR.fa <- Q3.fa - Q1.fa 
IQR.fa
slight.outlier.value.fa <- Q1.fa - 1.5 * IQR.fa
extreme.outlier.value.fa <- Q1.fa - 3 * IQR.fa
slight.outlier.value.fa
extreme.outlier.value.fa
# Calculamos el umbral por encima para valores atípicos leves
Q1.fa + 1.5 * IQR.fa
# Calculamos el umbral por debajo para valores atípicos leves
Q1.fa - 1.5 * IQR.fa
# Calculamos el umbral por encima para valores atípicos extremos
Q1.fa + 3 * IQR.fa
# Calculamos el umbral por debajo para valores atípicos extremos
Q1.fa - 3 * IQR.fa
# Todos los valores del muestreo que superen el valor de 142 son outliers
# No hay valores atípicos inferiores al umbral pues los importes todos son positivos. El mínimo es 6.

# Comprobamos cuantas observaciones se encuentran por encima de 142.
nrow(red.wines.DT[red.wines.DT$total.sulfur.dioxide > 142,])
# Procedemos a su eliminación
red.wines.DT <- red.wines.DT[red.wines.DT$total.sulfur.dioxide <= 142,]
nrow(red.wines.DT)


# Usar boxplot.stats
boxplot.stats(red.wines.DT$fixed.acidity)
boxplot.stats(red.wines.DT$volatile.acidity)
boxplot.stats(red.wines.DT$citric.acid)
boxplot.stats(red.wines.DT$residual.sugar)
boxplot.stats(red.wines.DT$chlorides)
boxplot.stats(red.wines.DT$free.sulfur.dioxide)
boxplot.stats(red.wines.DT$total.sulfur.dioxide)
boxplot.stats(red.wines.DT$density)
boxplot.stats(red.wines.DT$pH)
boxplot.stats(red.wines.DT$sulphates)
boxplot.stats(red.wines.DT$alcohol)


# Usando boxplots
grid.newpage()
ggg01 <- ggplot(red.wines.DT, aes(y=fixed.acidity)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg02 <- ggplot(red.wines.DT, aes(y=volatile.acidity)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg03 <- ggplot(red.wines.DT, aes(y=citric.acid)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg04 <- ggplot(red.wines.DT, aes(y=residual.sugar)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg05 <- ggplot(red.wines.DT, aes(y=chlorides)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg06 <- ggplot(red.wines.DT, aes(y=free.sulfur.dioxide)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg07 <- ggplot(red.wines.DT, aes(y=total.sulfur.dioxide)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg08 <- ggplot(red.wines.DT, aes(y=density)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg09 <- ggplot(red.wines.DT, aes(y=pH)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg10 <- ggplot(red.wines.DT, aes(y=sulphates)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
ggg11 <- ggplot(red.wines.DT, aes(y=alcohol)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=2)
grid.arrange(ggg01, ggg02, ggg03, ggg04,
             ggg05, ggg06, ggg07, ggg08,
             ggg09, ggg10, ggg11, ncol=3)


# Exportación de los datos preprocesados
write.csv(red.wines.DT, "D:/Master Ciencia de Datos/Tipología y ciclo de vida de datos/Practica 2/RedWines_data_clean.csv")

head(red.wines.DT)
# Comprobación de la normalidad y homogeneidad de la varianza

# Test de Anderson-Darling
alpha = 0.05
col.names = colnames(red.wines.DT) 

for (i in 1:ncol(red.wines.DT)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(red.wines.DT[,i]) | is.numeric(red.wines.DT[,i])) {
    p_val = ad.test(red.wines.DT[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(red.wines.DT)) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}

# Análisis de correlación
corr_matrix
corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlación para cada variable cuantitativa
# con respecto a la variables dependiente quality.
for (i in 1:(ncol(red.wines.DT) - 1)) {
  if (is.integer(red.wines.DT[,i]) | is.numeric(red.wines.DT[,i])) {
    spearman_test = cor.test(red.wines.DT[,i],
                             red.wines.DT[,length(red.wines.DT)],
                             method = "spearman", exact = FALSE)
    corr_coef = spearman_test$estimate
    p_val = spearman_test$p.value
    # Add row to matrix
    pair = matrix(ncol = 2, nrow = 1)
    pair[1][1] = corr_coef
    pair[2][1] = p_val
    corr_matrix <- rbind(corr_matrix, pair)
    rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(red.wines.DT)[i]
  }
}
print(corr_matrix)


matriz.Correlacion <- cor(red.wines.DT)
round(matriz.Correlacion, 1)

matriz.PValues <- rcorr(as.matrix(red.wines.DT))
matriz.PValues

# Chart correlaciones entre observaciones
ggcorrplot(matriz.Correlacion, hc.order = TRUE,
           outline.color = "white", lab = TRUE, ggtheme = ggplot2::theme_classic())


# Regresion Lineal Multiple con todos los predictors

lm.fit.mul.RW <- lm(quality~., data = red.wines.DT)
lm.fit.mul.RW
summary(lm.fit.mul.RW)

# Regresores cuantitativos con mayor coeficiente
# de correlación con respecto a la variable quality
# volatile.acidity, chlrides, free.sulfur.dioxide, total.sulfur.dioxide, pH, sulphates y alcohol



# Generación de varios modelos
lm.fit.mul.RW.01 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
                         total.sulfur.dioxide + pH + sulphates + alcohol, data = red.wines.DT)
lm.fit.mul.RW.02 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
                         total.sulfur.dioxide + pH + sulphates, data = red.wines.DT)
lm.fit.mul.RW.03 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
                         total.sulfur.dioxide + pH, data = red.wines.DT)
lm.fit.mul.RW.04 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
                         total.sulfur.dioxide, data = red.wines.DT)
lm.fit.mul.RW.05 <- lm(quality ~ volatile.acidity + alcohol, data = red.wines.DT)

# Tabla con los coeficientes de determinación de cada modelo
tabla.coeficientes <- matrix(c(1, summary(lm.fit.mul.RW.01)$r.squared,
                               2, summary(lm.fit.mul.RW.02)$r.squared,
                               3, summary(lm.fit.mul.RW.03)$r.squared,
                               4, summary(lm.fit.mul.RW.04)$r.squared,
                               5, summary(lm.fit.mul.RW.05)$r.squared),
                             ncol = 2, byrow = TRUE)
colnames(tabla.coeficientes) <- c("Modelo", "R^2")
tabla.coeficientes



# Otra forma de selección del mejor modelo
# Selección de los mejores predictores
step(object = lm.fit.mul.RW, direction = "both", trace = 1)

# Mejor modelo de regresión del proceso de selección
lm.fit.mul.RW.best <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
                           total.sulfur.dioxide + pH + sulphates +
                           alcohol, data = red.wines.DT)
summary(lm.fit.mul.RW.best)
head(red.wines.DT)
# Predicción de la calidad de los vinos tintos
newdata <- data.frame(
  volatile.acidity = 0.5,
  chlorides = 0.06,
  free.sulfur.dioxide = 10,
  total.sulfur.dioxide = 45,
  pH = 3,
  sulphates = 0.5,
  alcohol = 10)


predict(lm.fit.mul.RW.01, newdata)


# Contraste de hipótesis
min(red.wines.DT$pH)
max(red.wines.DT$pH)
mean.pH <- mean(red.wines.DT$pH)
sum(red.wines.DT$pH < mean.pH)


red.wines.DT["segment_pH"] <- cut(red.wines.DT$pH, breaks = c(0, mean.pH, 4), 
                                   labels = c("< mean.pH", ">= mean.pH"))

head(red.wines.DT)

plot(red.wines.DT$segment_pH, main="Number by pH", 
     xlab="pH", ylab="Numbers",col = "ivory")
red.wines.LM.quality <- red.wines.DT[red.wines.DT$segment_pH == "< mean.pH",]$quality
red.wines.BEM.quality <- red.wines.DT[red.wines.DT$segment_pH == ">= mean.pH",]$quality
t.test(red.wines.LM.quality, red.wines.BEM.quality, alternative = "less")
