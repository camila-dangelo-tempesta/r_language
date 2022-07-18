#### WEB SCRAPING: NY Times ####

#### 1. Working Directory ####
# Configurando o diretório de trabalho
setwd("C:/Users/Utilizador/repos/Formacao_cientista_de_dados/big_data_analytics_R_microsoft_azure_machine_learning/Modulo_07")
getwd()

#### 2. Imports ####

# Desativar warnings
options(warn = -1)

# Pacote xml2 para processar os dados
install.packages('xml2')
library(xml2)

# Pacote rvest - útil para quem não conhece HTML e CSS
install.packages('rvest')
library(rvest)

# Demais pacotes para manipulação de dados
library(stringr)
library(dplyr)
library(lubridate)
library(readr)

#### 3. Data Loading ####

# Leitura da web page - Retorna um documento xml
webpage <- read_html("https://www.nytimes.com/interactive/2017/06/23/opinion/trumps-lies.html")
webpage

# Extraindo os registros
# Cada elemento na web page acima tem o seguinte formato em html:
# <span class="short-desc"><strong> DATE </strong> LIE <span class="short-truth"><a href="URL"> EXPLANATION </a></span></span>
?html_nodes
results <- webpage %>% html_nodes(".short-desc")
results


# Construindo o dataset
records <- vector("list", length = length(results))
records

for (i in seq_along(results)) {
  # Data
  date <- str_c(results[i] %>% 
                  html_nodes("strong") %>% 
                  html_text(trim = TRUE), ', 2017')
  # Texto (mentira)
  lie <- str_sub(xml_contents(results[i])[2] %>% html_text(trim = TRUE), 2, -2)
  
  # Exlicação
  explanation <- str_sub(results[i] %>% 
                           html_nodes(".short-truth") %>% 
                           html_text(trim = TRUE), 2, -2)
  # Link: url do site original
  url <- results[i] %>% html_nodes("a") %>% html_attr("href")
  
  # Gravando o dataframe
  records[[i]] <- data_frame(date = date, lie = lie, explanation = explanation, url = url)
}


# Dataset final: juntando todas as linhas
df <- bind_rows(records)

# Transformando o campo data para o formato Date em R
df$date <- mdy(df$date)

# Exportando para CSV
write_csv(df, "mentiras_trump.csv")


# Lendo os dados
df <- read_csv("mentiras_trump.csv")
View(df)