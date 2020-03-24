# Usando o pacote caret para criar modelo de machine learning em R

# Definindo o diretório de trabalho
setwd("C:/MLR/caret")
getwd()

# Instalando os pacotes
install.packages("caret")
install.packages("randomForest")

# carregando as bibliotecas
library(caret)
library(randomForest)
library(datasets)

# Usando o dataset mtcars
View(mtcars)

# Função do caret para divisao do dados
split <- createDataPartition(y = mtcars$mpg, p = 0.7, list = FALSE)

# Dividindo os dados em treino e teste
dados_treino <- mtcars[split, ]
dados_teste <- mtcars[-split, ]

# Treinando o modelo
names(getModelInfo())

# Regressão Linear
modelo_lm_v0 <- train(mpg ~ ., data =  dados_treino, method = "lm")

modelo_lm_v1 <- train(mpg ~ wt + hp + qsec + drat, data = dados_treino, method = "lm")

# Mostrando a importancia das variaveis para criação do modelo
?varImp
varImp(modelo_lm_v0)

# Random Forest
modelo_rf_v1 <- train(mpg ~ wt + hp + qsec + drat, data = dados_treino, method = "rf")

# Resumo do modelo
summary(modelo_lm_v1) # 88%
summary(modelo_rf_v1)

# Ajustando o modelo
?expand.grid
?trainControl
controle1 <- trainControl(method = "cv", number = 10)

modelo_lm_v2 <- train(mpg ~ wt + hp + qsec + drat,
                      data = dados_treino, 
                      method = "lm",
                      trControl = controle1,
                      numeric = "Rsquared")

# Resumo do modelo
summary(modelo_lm_v2)

# Coletando os residuos
residuals <- resid(modelo_lm_v2)
residuals

# Previsões
predictvalues <- predict(modelo_lm_v1, dados_teste)
predictvalues
plot(dados_teste$mpg, predictvalues)

# plot das variaveis mais relevantes no modelo
plot(varImp(modelo_lm_v1))










