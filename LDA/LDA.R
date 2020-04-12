library(psych)
library(MASS)

iris <- read.csv("iris.csv")

data(iris)
head(iris)

# Correlación entre las variables independientes
pairs.panels(iris[1:4], gap = 0, bg = c("blue","green","red")[iris$Species], pch = 21)

# Dividimos los datos de entrenamiento y de prueba
set.seed(345)
ir <- sample(2, nrow(iris), replace = TRUE, prob = c(0.75, 0.25))
train <- iris[ir == 1,]
test <- iris[ir == 2,]

# LDA
lnr <- lda(Species~., train)

# Atributos, probabilidas a priori y escalamiento de los valores 
attributes(lnr)
lnr$prior
lnr$scaling

# Valores de la función discriminante
prd <- predict(lnr, train)
ldahist(data = prd$x[,1], g = train$Species)

# Biplot
library(devtools)
library(ggord)
ggord(lnr, train$Species, ylim = c(-5, 5))

# Matriz de confusión y precisión con datos de entrenamiento
p.train <- predict(lnr, train)$class
table1 <- table(predicted = p.train, Actual = train$Species)
table1

sum(diag(table1)/sum(table1))
# Precisión del 97.34%

# Matriz de confusión y precisión con datos de prueba
p.test <- predict(lnr, test)$class
table2 <- table(predicted = p.test, Actual = test$Species)
table2

sum(diag(table2)/sum(table2))
# Precisión del 100%

# Se puede inferir que todas las flores pertenecen a su respectiva especie


