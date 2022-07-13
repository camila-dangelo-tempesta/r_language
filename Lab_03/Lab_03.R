#### LABORATÓRIO 03: Markket Basket Analysis (MBA) ####

#### 1. Working Directory ####
# Configurando o diretório de trabalho
setwd("C:/Users/Utilizador/repos/Formacao_cientista_de_dados/big_data_analytics_R_microsoft_azure_machine_learning/Modulo_05")
getwd()

#### 2. Imports ####
library(dplyr)
library(arules)
library(arulesViz)
library(htmlwidgets)
library(writexl)
options(warn=-1)

#### 3. Data Loading ####

dados <- read.csv("dataset_bd3.csv")

# Dimensions of an Object
dim(dados)

#  Data Viewer
View(dados)

# Summaries
summary(dados)

# Compactly Display the Structure of an Arbitrary R Object
str(dados)

#### 4. Feature engineering  ####
# Separamos as linhas pares das linhas ímpares: 
linhas_pares <- seq(2, nrow(dados), 2)
linhas_impares <- seq(1, nrow(dados), 2)

# Separamos os dados e então usaremos o dataset com as linhas pares (linhas de dados válidos)
df1 <- dados[linhas_pares, ]
View(df1)

df2 <- dados[linhas_impares, ] 
View(df2)

#### 5. Data Cleaning ####

# Verifica se temos valores ausentes no primeiro item de compra
sum(is.na(df1$Item01))

# Verifica se temos valores ausentes no segundo item de compra
# ATENÇÃO: aos espaços
sum(is.na(df1$Item02))
View(df1)

# Verifica se temos valores ausentes representados por espaço em branco
which(nchar(trimws(df1$Item01))==0)
which(nchar(trimws(df1$Item02))==0)

# Verifica se temos valores ausentes representados por espaço em branco (usando expressão regular)
grepl("^\\s*$", df1$Item02)

# Número de itens distintos
n_distinct(df1)

#### 6. Data Filtering ####

##### 6.1 Rows #####
# Vamos trabalhar somente com os registros onde o item 2 não fos nulo
df1_two <- df1[!grepl("^\\s*$", df1$Item02), ]

# Número de itens distintos
n_distinct(df1_two)

# Prepara o pacote convertendo as variáveis para o tipo fator 
# (variáveis que usaremos daqui em diante)
View(df1_two)

##### 6.2 Columns #####

# Selecionando as colunas que serão usadas
pacote <- df1_two
pacote$Item01 <- as.factor(pacote$Item01)
pacote$Item02 <- as.factor(pacote$Item02)
pacote$Item03 <- as.factor(pacote$Item03)
pacote$Item04 <- as.factor(pacote$Item04)
pacote$Item05 <- as.factor(pacote$Item05)
pacote$Item06 <- as.factor(pacote$Item06)

summary(pacote)

View(pacote)

str(pacote)

?split

pacote_split <- split(pacote$Item01, 
                      pacote$Item02,
                      pacote$Item03, 
                      pacote$Item04,
                      pacote$Item05, 
                      pacote$Item06)

View(pacote_split)

#### 7. Markket Basket Analysis ####
# Transações
?as
transacoes <- as(pacote_split, "transactions")

# Inspeção das regras
?inspect
inspect(head(transacoes, 5))

# Vamos verificar as regras de um produto: Off CDust-ompressed Gas 2 pack
?apriori
regras_produto1 <- apriori(transacoes, 
                           parameter = list(conf = 0.5, minlen = 3),
                           appearance = list(rhs = "Dust-Off Compressed Gas 2 pack", default = "lhs")) 

# Inspeção das regras
inspect(head(sort(regras_produto1, by = "confidence"), 5))

# Vamos verificar as regras de um produto: HP 61 ink
regras_produto2 <- apriori(transacoes,
                           parameter = list(minlen = 3, conf = 0.5),
                           appearance = list(rhs = "HP 61 ink",default = "lhs"))

# Inspeção das regras
inspect(head(sort(regras_produto2, by = "confidence"), 5))

# Vamos verificar as regras de um produto: VIVO Dual LCD Monitor Desk mount
regras_produto3 <- apriori(transacoes,
                           parameter = list(minlen = 3, conf = 0.5),
                           appearance = list(rhs = "VIVO Dual LCD Monitor Desk mount", default = "lhs"))

# Inspeção das regras
inspect(head(sort(regras_produto3, by = "confidence"), 5))

##### 7.1 Support, Confidence e Lift #####
# Vamos verificar novamente as regras do produto: Dust-Off Compressed Gas 2 pack, 
# alterando uma das métricas
regras_produto1 <- apriori(transacoes, 
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "Dust-Off Compressed Gas 2 pack", default = "lhs")) 

# Inspeção das regras
inspect(head(sort(regras_produto1, by = "confidence"), 5))

# Filtra as regras redundantes
regras_produto1_clean <- regras_produto1[!is.redundant(regras_produto1)]

# Inspeção das regras
inspect(head(sort(regras_produto1_clean, by = "confidence"), 5))

# Sumário
summary(regras_produto1_clean)

# Plot
plot(regras_produto1_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Vamos verificar novamente as regras do produto: HP 61 ink,
# alterando uma das métricas
regras_produto2 <- apriori(transacoes,
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "HP 61 ink", default = "lhs"))

# Inspeção das regras
inspect(head(sort(regras_produto2, by = "confidence"), 5))

# Filtra as regras redundantes
regras_produto2_clean <- regras_produto2[!is.redundant(regras_produto2)]

# Inspeção das regras
inspect(head(sort(regras_produto2_clean, by = "confidence"), 5))

# Sumário
summary(regras_produto2_clean)

# Plot
plot(regras_produto2_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Vamos verificar novamente as regras do produto: VIVO Dual LCD Monitor Desk mount,
# alterando uma das métricas
regras_produto3 <- apriori(transacoes,
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "VIVO Dual LCD Monitor Desk mount", default = "lhs"))

# Inspeção das regras
inspect(head(sort(regras_produto3, by = "confidence"), 5))

# Filtra as regras redundantes
regras_produto3_clean <- regras_produto3[!is.redundant(regras_produto3)]

# Inspeção das regras
inspect(head(sort(regras_produto3_clean, by = "confidence"), 5))

# Sumário
summary(regras_produto3_clean)

# Plot
plot(regras_produto3_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

#### 8. Top 3 rules ####
inspect(head(sort(regras_produto1_clean, by = "support", decreasing = TRUE), 1))
inspect(head(sort(regras_produto2_clean, by = "confidence", decreasing = TRUE), 1))
inspect(head(sort(regras_produto3_clean, by = "confidence", decreasing = TRUE), 1))

#### 9. Salving ####
# Salvamos o conjunto de regras dos 3 produtos como dataframe  e então salvamos em disco
View(regras_produto1_clean)

df_produto1 <- as(regras_produto1_clean, "data.frame")

View

write_xlsx(df_produto1, "df_produto1.xlsx")

df_produto2 <- as(regras_produto2_clean, "data.frame")
View(df_produto2)
write_xlsx(df_produto2, "df_produto2.xlsx")

df_produto3 <- as(regras_produto3_clean, "data.frame")
View(df_produto3)
write_xlsx(df_produto3, "df_produto3.xlsx")