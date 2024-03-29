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

enemDataset2014 <- read.csv("enem-2014.csv", nrows=500000, header=TRUE, sep=",")

str(enemDataset2014)

# Removendo alunos que n�o declararam o tipo de escola
enemDataset2014 <- enemDataset2014[!is.na(enemDataset2014$TP_ESCOLA),]

# Removendo alunos que n�o tem notas nas provas
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

# Criando um Range para as notas de reda��o

enemDataset$R_NOTA_REDACAO <- cut(enemDataset$NU_NOTA_REDACAO,
                                  c(-1, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000),
                                  labels=c("0-500", "500-550", "550-600", "600-650",
                                           "650-700", "700-750", "750-800", "800-850",
                                           "850-900", "900-950", "950-1000"))

#####################################################################

# Removendo notas zeradas nas provas

enemDataset <- enemDataset[!(enemDataset$NOTA_LC==0),]
enemDataset <- enemDataset[!(enemDataset$NOTA_CH==0),]
enemDataset <- enemDataset[!(enemDataset$NOTA_MT==0),]
enemDataset <- enemDataset[!(enemDataset$NOTA_CN==0),]

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

enemExtremoFaixaFamiliar <- filter(enemDataset, as.character(RENDA_FAMILIAR) %in% c("B", "Q"))

enemExtremoFaixaQ <- filter(enemDataset, as.character(RENDA_FAMILIAR) %in% c("Q"))
enemExtremoFaixaB <- filter(enemDataset, as.character(RENDA_FAMILIAR) %in% c("B"))

enemTipoEscola1 <- filter(enemDataset, TP_ESCOLA == 1)
enemTipoEscola2 <- filter(enemDataset, TP_ESCOLA == 2)

#####################################################################

histNota_CH_LC_ESCOLA <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = TP_ESCOLA)) +
  ggtitle("Notas de Ci�ncias Humanas e Linguagem e Comunica��o por Tipo de Escola")


# Separando por tipo de escola

histNota_CH_LC_ESCOLA1 <- ggplot(data = enemTipoEscola1) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = TP_ESCOLA)) +
  ggtitle("Notas de Ci�ncias Humanas e Linguagem e Comunica��o em Escolas P�blicas")

histNota_CH_LC_ESCOLA2 <- ggplot(data = enemTipoEscola2) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = TP_ESCOLA)) +
  ggtitle("Notas de Ci�ncias Humanas e Linguagem e Comunica��o em Escolas Privadas")

#####

# Separando port tipo de Renda Familiar:

histNota_CH_LC_RENDA_FAMILIAR <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ci�ncias Humanas e Linguagem e Comunica��o por Renda Familiar")

histNota_CH_LC_RENDA_FAMILIAR_EXTREMO <- ggplot(data = enemExtremoFaixaFamiliar) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ci�ncias Humanas e Linguagem e Comunica��o por Renda Familiar - Extremos")

#####

histNota_CH_LC_RENDA_FAMILIAR_EXTREMO_Q <- ggplot(data = enemExtremoFaixaQ) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ci�ncias Humanas e Linguagem e Comunica��o por Renda Familiar - Q")

histNota_CH_LC_RENDA_FAMILIAR_EXTREMO_B <- ggplot(data = enemExtremoFaixaB) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_LC, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ci�ncias Humanas e Linguagem e Comunica��o por Renda Familiar - B")

#####################################################################
#####################################################################
#####################################################################
#####################################################################

histNota_MT_CN_ESCOLA <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = TP_ESCOLA)) +
  ggtitle("Notas de Ci�ncias da Natureza e Matem�tica por Tipo de Escola") 


# Separando por tipo de escola

histNota_MT_CN_ESCOLA1 <- ggplot(data = enemTipoEscola1) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = TP_ESCOLA)) +
  ggtitle("Notas de Ci�ncias da Natureza e Matem�tica em Escolas P�blicas")

histNota_MT_CN_ESCOLA2 <- ggplot(data = enemTipoEscola2) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = TP_ESCOLA)) +
  ggtitle("Notas de Ci�ncias da Natureza e Matem�tica em Escolas Privadas")



####

# Separando port tipo de Renda Familiar:

histNota_MT_CN_RENDA_FAMILIAR <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ci�ncias da Natureza e Matem�tica por Renda Familiar")

histNota_MT_CN_RENDA_FAMILIAR_EXTREMO <- ggplot(data = enemExtremoFaixaFamiliar) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ci�ncias da Natureza e Matem�tica por Renda Familiar - Extremo")


#####

histNota_MT_CN_RENDA_FAMILIAR_EXTREMO_Q <- ggplot(data = enemExtremoFaixaQ) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ci�ncias da Natureza e Matem�tica por Renda Familiar - Q")

histNota_MT_CN_RENDA_FAMILIAR_EXTREMO_B <- ggplot(data = enemExtremoFaixaB) + 
  geom_point(aes(x = NOTA_MT, y = NOTA_CN, col = RENDA_FAMILIAR)) +
  ggtitle("Notas de Ci�ncias da Natureza e Matem�tica por Renda Familiar - B")


#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################

# Comparando Ci�ncias da Natureza e Ci�ncias Humanas

histNota_CH_CN_ESCOLA <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_CH, y = NOTA_CN, col = TP_ESCOLA)) +
  ggtitle("Notas de Ci�ncias Humanas e Ci�ncias da Natureza por Tipo de Escola")


# Comparando Matem�tica e Portugu�s

histNota_CH_CN_ESCOLA <- ggplot(data = enemDataset) + 
  geom_point(aes(x = NOTA_LC, y = NOTA_MT, col = TP_ESCOLA)) +
  ggtitle("Notas de Linguagem e Comunica��o e Matem�tica por Tipo de Escola")

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

histREDACAO_R <- ggplot(enemDataset, aes(R_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    position="identity",
    stat="count",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#####################################################################

enemDatasetBackup <- enemDataset

enemDataset <- enemDataset[!(enemDataset$R_NOTA_REDACAO=="0-500"),]

histREDACAO_R__ALTO <- ggplot(enemDataset, aes(R_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    position="identity",
    stat="count",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

enemDataset <- enemDatasetBackup

#####################################################################

enemDatasetBackup <- enemDataset

enemDataset <- enemDataset[(enemDataset$R_NOTA_REDACAO=="950-1000" |
                              enemDataset$R_NOTA_REDACAO=="900-950" |
                              enemDataset$R_NOTA_REDACAO=="850-900"),]

histREDACAO_R_ALTO <- ggplot(enemDataset, aes(R_NOTA_REDACAO)) +
  geom_histogram(
    aes(color = TP_ESCOLA, fill = TP_ESCOLA),
    alpha=0.4,
    position="identity",
    stat="count",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

enemDataset <- enemDatasetBackup

#####################################################################
