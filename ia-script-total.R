#####################################################################
# Plots
library(ggplot2)

# Filtros
library(dplyr)

#####################################################################

# Confusion Matrix

library(lattice)
library(caret)
library(class)

#####################################################################


# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

treeImplementation <- function(train, test, classesTest) {
  modeloTree <- rpart(
    formula = classe~.,
    data = train,
    method = "class",
    control = rpart.control(minsplit = 1))
  
  predictTree <- predict(modeloTree, test, type = "class")
  
  plot <- rpart.plot(modeloTree, type = 3)
  
  confusionMatrix(predictTree, classesTest) 
}

#####################################################################

library(e1071)

svmImplementation = function(train, test, classesTest) {
  classificadorSVM = svm(
    formula = classe~ .,
    data = train,
    type = 'C-classification',
    kernel = 'linear')
  
  predictSVM = predict(classificadorSVM, newdata = test)
  
  confusionMatrix(predictSVM, classesTest)
}

#####################################################################

# Local file

getwd()
location <- getwd()
setwd(location)

#####################################################################

enemDataset2014 <- read.csv("enem-2014.csv", nrows=500000, header=TRUE, sep=",", na.strings=c(""))

str(enemDataset2014)

# Removendo alunos que não declararam o tipo de escola
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$TP_ESCOLA),]

# Removendo alunos que não tem notas nas provas
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_CH),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_CN),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_LC),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_MT),]

# Transformando a coluna de Tipo de Escola em Factor
enemDataset2014$TP_ESCOLA <- as.factor(enemDataset2014$TP_ESCOLA)
enemDataset2014$ST_CONCLUSAO <- as.factor(enemDataset2014$ST_CONCLUSAO)
enemDataset2014$IN_TP_ENSINO <- as.factor(enemDataset2014$IN_TP_ENSINO)
enemDataset2014$UF_NASCIMENTO <- as.factor(enemDataset2014$UF_NASCIMENTO)
enemDataset2014$ID_LOCALIZACAO_ESC <- as.factor(enemDataset2014$ID_LOCALIZACAO_ESC)
enemDataset2014$ID_DEPENDENCIA_ADM_ESC <- as.factor(enemDataset2014$ID_DEPENDENCIA_ADM_ESC)
enemDataset2014$UF_ESC <- as.factor(enemDataset2014$UF_ESC)
enemDataset2014$Q003 <- as.factor(enemDataset2014$Q003)

enemDatasetComplete <- enemDataset2014
enemDatasetBackup <- enemDataset2014

# Excluindo variaveis
rm(enemDataset2014)

#####################################################################

# Sumarizando valores da redação:
enemDatasetComplete$NU_NOTA_REDACAO <- cut(enemDatasetComplete$NU_NOTA_REDACAO,
                                           c(-1, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                           labels=c("0-50", "50-100", "100-150", "150-200", "200-250",
                                                    "250-300", "300-350", "350-400", "400-450", "450-500",
                                                    "500-550", "550-600", "600-650",
                                                    "650-700", "700-750", "750-800", "800-850",
                                                    "850-900", "900-950", "950-1000"))



enemDatasetComplete$NOTA_CH <- cut(enemDatasetComplete$NOTA_CH,
                                   c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                   labels=c("0-500", "500-550", "550-600", "600-650",
                                            "650-700", "700-750", "750-800", "800-850",
                                            "850-900", "900-950", "950-1000"))

enemDatasetComplete$NOTA_CN <- cut(enemDatasetComplete$NOTA_CN,
                                   c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                   labels=c("0-500", "500-550", "550-600", "600-650",
                                            "650-700", "700-750", "750-800", "800-850",
                                            "850-900", "900-950", "950-1000"))

enemDatasetComplete$NOTA_MT <- cut(enemDatasetComplete$NOTA_MT,
                                   c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                   labels=c("0-500", "500-550", "550-600", "600-650",
                                            "650-700", "700-750", "750-800", "800-850",
                                            "850-900", "900-950", "950-1000"))

enemDatasetComplete$NOTA_LC <- cut(enemDatasetComplete$NOTA_LC,
                                   c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                   labels=c("0-500", "500-550", "550-600", "600-650",
                                            "650-700", "700-750", "750-800", "800-850",
                                            "850-900", "900-950", "950-1000"))

#####################################################################

# Removendo colunas que só tem NA
enemDatasetComplete <- enemDatasetComplete[, colSums(is.na(enemDatasetComplete)) != nrow(enemDatasetComplete)]

# Removendo colunas que tenham NA
enemDatasetComplete <- enemDatasetComplete[, colSums(is.na(enemDatasetComplete)) == 0]

#####################################################################

# Removendo colunas inuteis
enemDatasetComplete$TX_RESPOSTAS_CH <- NULL
enemDatasetComplete$TX_RESPOSTAS_CN <- NULL
enemDatasetComplete$TX_RESPOSTAS_LC <- NULL
enemDatasetComplete$TX_RESPOSTAS_MT <- NULL

enemDatasetComplete$GABARITO_CH <- NULL
enemDatasetComplete$GABARITO_CN <- NULL
enemDatasetComplete$GABARITO_LC <- NULL
enemDatasetComplete$GABARITO_MT <- NULL

enemDatasetComplete$NU_INSCRICAO <- NULL
enemDatasetComplete$NU_ANO <- NULL

enemDatasetComplete$COD_MUNICIPIO_PROVA <- NULL
enemDatasetComplete$NO_MUNICIPIO_PROVA <- NULL
enemDatasetComplete$UF_PROVA <- NULL
enemDatasetComplete$COD_UF_PROVA <- NULL

# enemDatasetComplete$NU_NOTA_COMP1 <- NULL
# enemDatasetComplete$NU_NOTA_COMP2 <- NULL
# enemDatasetComplete$NU_NOTA_COMP3 <- NULL
# enemDatasetComplete$NU_NOTA_COMP4 <- NULL
# enemDatasetComplete$NU_NOTA_COMP5 <- NULL

#####################################################################


# Preparando o dataset

# Adicionando questões sociais:
notaRedacaoAspectosSociais <- enemDatasetComplete[tail(names(enemDatasetComplete), 76)]


# Adicionando a classe
notaRedacaoAspectosSociais$classe <- notaRedacaoAspectosSociais$NU_NOTA_REDACAO
notaRedacaoAspectosSociais$NU_NOTA_REDACAO <- NULL


#####################################################################

# Qauntas vezes as classes apate
percentage <- prop.table(table(notaRedacaoAspectosSociais$classe)) * 100
cbind(freq=table(notaRedacaoAspectosSociais$classe), percentage=percentage)

summary(notaRedacaoAspectosSociais)

#####################################################################

# Dividindo o dataset entre treino e test

indexes <- sample(1:nrow(notaRedacaoAspectosSociais), as.integer(0.8 * nrow(notaRedacaoAspectosSociais)))

train <- notaRedacaoAspectosSociais[indexes, ]
trainBackup <- train
trainClasses <- train$classe

test <- notaRedacaoAspectosSociais[-indexes, ]
testClasses <- factor(test$classe)
test$classe <- NULL

#####################################################################

treeImplementation(train, test, testClasses)

#####################################################################

svmImplementation(train, test, testClasses)

#####################################################################
#####################################################################
#####################################################################

# Removendo dados de competência da redação

newTrain <- train
newTest <- test

newTrain$NU_NOTA_COMP1 <- NULL
newTrain$NU_NOTA_COMP2 <- NULL
newTrain$NU_NOTA_COMP3 <- NULL
newTrain$NU_NOTA_COMP4 <- NULL
newTrain$NU_NOTA_COMP5 <- NULL

newTest$NU_NOTA_COMP1 <- NULL
newTest$NU_NOTA_COMP2 <- NULL
newTest$NU_NOTA_COMP3 <- NULL
newTest$NU_NOTA_COMP4 <- NULL
newTest$NU_NOTA_COMP5 <- NULL

#####################################################################

treeImplementation(newTrain, newTest, testClasses)

#####################################################################

svmImplementation(newTrain, newTest, testClasses)

#####################################################################
#####################################################################
#####################################################################
