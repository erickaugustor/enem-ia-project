#####################################################################
# Plots
library(ggplot2)

# Filtros
library(dplyr)

#####################################################################
# Local file

getwd()
location <- getwd()
setwd(location)

#####################################################################

enemDataset2014 <- read.csv("enem-2014.csv", nrows=10000, header=TRUE, sep=",")

str(enemDataset2014)

# Removendo alunos que não declararam o tipo de escola
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$TP_ESCOLA),]

# Removendo alunos que não tem notas nas provas
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_CH),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_CN),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_LC),]
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$NOTA_MT),]

enemDatasetBackup <- enemDataset2014

# Transformando a coluna de Tipo de Escola em Factor
enemDataset2014$TP_ESCOLA <- as.factor(enemDataset2014$TP_ESCOLA)
enemDataset2014$ST_CONCLUSAO <- as.factor(enemDataset2014$ST_CONCLUSAO)
enemDataset2014$IN_TP_ENSINO <- as.factor(enemDataset2014$IN_TP_ENSINO)
enemDataset2014$UF_NASCIMENTO <- as.factor(enemDataset2014$UF_NASCIMENTO)
enemDataset2014$ID_LOCALIZACAO_ESC <- as.factor(enemDataset2014$ID_LOCALIZACAO_ESC)
enemDataset2014$ID_DEPENDENCIA_ADM_ESC <- as.factor(enemDataset2014$ID_DEPENDENCIA_ADM_ESC)
enemDataset2014$UF_ESC <- as.factor(enemDataset2014$UF_ESC)
enemDataset2014$RENDA_FAMILIAR <- as.factor(enemDataset2014$Q003)

enemDatasetComplete <- enemDataset2014

# Excluindo variaveis
rm(enemDataset2014)

#####################################################################

enemDataset <- subset(enemDatasetComplete, select = c(
  "TP_ESCOLA",
  "NOTA_CN",
  "NOTA_CH",
  "NOTA_LC",
  "NOTA_MT",
  "NU_NOTA_REDACAO",
  "ST_CONCLUSAO",
  "IN_TP_ENSINO",
  "UF_NASCIMENTO",
  "ID_LOCALIZACAO_ESC",
  "ID_DEPENDENCIA_ADM_ESC",
  "UF_ESC",
  "RENDA_FAMILIAR"
))

str(enemDataset)

#####################################################################

# Sumarizando valores da redação:
newEnemDataset <- data.frame()

enemDataset$R_NOTA_REDACAO <- cut(enemDataset$NU_NOTA_REDACAO,
                                  c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                  labels=c("0-500", "500-550", "550-600", "600-650",
                                           "650-700", "700-750", "750-800", "800-850",
                                           "850-900", "900-950", "950-1000"))

enemDataset$R_NOTA_HUMANAS <- cut(enemDataset$NOTA_CH,
                                  c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                  labels=c("0-500", "500-550", "550-600", "600-650",
                                           "650-700", "700-750", "750-800", "800-850",
                                           "850-900", "900-950", "950-1000"))

enemDataset$R_NOTA_NATUREZA <- cut(enemDataset$NOTA_CN,
                                  c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                  labels=c("0-500", "500-550", "550-600", "600-650",
                                           "650-700", "700-750", "750-800", "800-850",
                                           "850-900", "900-950", "950-1000"))

enemDataset$R_NOTA_MATEMATICA <- cut(enemDataset$NOTA_MT,
                                  c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                  labels=c("0-500", "500-550", "550-600", "600-650",
                                           "650-700", "700-750", "750-800", "800-850",
                                           "850-900", "900-950", "950-1000"))

enemDataset$R_NOTA_PORTUGUES <- cut(enemDataset$NOTA_LC,
                                     c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                     labels=c("0-500", "500-550", "550-600", "600-650",
                                              "650-700", "700-750", "750-800", "800-850",
                                              "850-900", "900-950", "950-1000"))

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

  # plot <- rpart.plot(modeloTree, type = 3)
  
  confusionMatrix(predictTree, classesTest) 
}

#####################################################################

# Preparando o dataset

# Adicionando questões sociais:
notaRedacaoAspectosSociais <- enemDatasetBackup[tail(names(enemDatasetBackup), 76)]

# Removendo algumas questões sociais não utilizadas
notaRedacaoAspectosSociais <- notaRedacaoAspectosSociais[1:53]
notaRedacaoAspectosSociais$Q025 <- NULL

# Adicionando o rage de notas da redação
notaRedacaoAspectosSociais$classe <- enemDataset$R_NOTA_REDACAO

notaRedacaoAspectosSociaisBackup <- notaRedacaoAspectosSociais

notaRedacaoAspectosSociais <- notaRedacaoAspectosSociais[complete.cases(notaRedacaoAspectosSociais), ]


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


svmImplementation(train, test, testClasses)


elbowImplementation <- function(dataFrame) {
  wss <- (nrow(dataFrame) - 1) * sum(apply(dataFrame, 2, var))
  
  for (i in 2:20) {
    wss[i] <- sum(kmeans(dataFrame, centers=i)$withinss)
  }
  
  plot(1:20, wss, type="b", xlab="Número de Clusters")  
}

elbowImplementation(notaRedacaoAspectosSociais)


#############################################################################

# KNN

install.packages("caret")

library(lattice)
library(caret)
library(class)

knnImplementation <- function(train, test, classesTrain) {
  k.optm = 1
  
  for (i in 1:15){
    knn.mod <- knn(train = train, test=test, cl=classesTrain, k=i)
    
    print(confusionMatrix(knn.mod, classesTest))
    
    k.optm[i] <- 100 * sum(classesTest == knn.mod)/NROW(classesTest)
    k=i
    cat(k, '=', k.optm[i], '')
  }
  
  
  plot(k.optm, type="b", xlab="Valor de K", ylab="Nível de Acurácia")
  
  return(k.optm)
}

#############################################################################

# Execute the function 
knnImplementation(train, test, trainClasses)

train$classe <- NULL
knn.mod <- knn(train = train, test=test, cl=trainClasses, k=1)

