#####################################################################
# For big tables

library(bigmemory)
library(biganalytics)
library(bigtabulate)

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

enemDataset2014 <- read.csv("enem-2014.csv", nrows=100000, header=TRUE, sep=",")

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

tipoEscola <- data.frame(table(enemDataset$TP_ESCOLA))

histTipoEscola <- ggplot(tipoEscola, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Tipo de escola",
    y="Quantidade de aluno",
    title="Histograma de Quantidade de aluno por tipo de escola"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip();

#####################################################################

enemDatasetTest <- filter(enemDataset, as.character(RENDA_FAMILIAR) %in% c("B", "Q"))

histNota_CH_LC_ESCOLA <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = TP_ESCOLA)) +
  ggtitle("Notas de Ciências Humanas e Linguagem e Comunicação por Tipo de Escola")

histNota_CH_LC_RENDA_FAMILIAR <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ciências Humanas e Linguagem e Comunicação por Renda Familiar")

histNota_CH_LC_RENDA_FAMILIAR <- ggplot(data = enemDatasetTest) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ciências Humanas e Linguagem e Comunicação por Renda Familiar")



histNota_MT_CN_ESCOLA <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = TP_ESCOLA)) +
  ggtitle("Notas de Ciências da Natureza e Matemática por Tipo de Escola") 

histNota_MT_CN_RENDA_FAMILIAR <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ciências da Natureza e Matemática por Renda Familiar")

histNota_MT_CN_RENDA_FAMILIAR <- ggplot(data = enemDatasetTest) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ciências da Natureza e Matemática por Renda Familiar")


#####################################################################

histREDACAO <- ggplot(enemDataset, aes(NU_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    position="identity",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

lineREDACAO <- ggplot(enemDataset, aes(NU_NOTA_REDACAO)) +
  geom_freqpoly(
    aes(color = TP_ESCOLA, linetype = TP_ESCOLA),
    bins = 30,
    size = 1.5
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#####################################################################

maioresNotasRedacao <- subset(enemDataset, NU_NOTA_REDACAO > 700)

histMaioresNotasREDACAO <- ggplot(maioresNotasRedacao, aes(NU_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    bins = 30,
    position="identity",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#####################################################################

menoresNotasREDACAO <- subset(enemDataset, NU_NOTA_REDACAO < 600)

histMenoresNotasREDACAO <- ggplot(menoresNotasREDACAO, aes(NU_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    bins = 30,
    position="identity",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#####################################################################




