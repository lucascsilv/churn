## CHURN ##
## Churn: avalia se uma pessoa cancela ou nao um serviço #

library(tidyverse)
library(readr)

churn <- read_csv("~/churn/churn.csv")
View(churn)

# visualizar um cruzamento de dados: quantos H/M tem churn

table(churn$Churn , churn$gender)

## purrr()  -> aplicar uma função a multiplos objetos 

help("purrr-package")

churn %>%
  map(table)

saveRDS(churn, "churn.rds")
save.image("base.rda")
load("base.rda")

#str() resumido e melhor:

glimpse(churn)

# trasformando variavel:

help(factor)

churn$SeniorCitizen <- churn$SeniorCitizen %>% 
  factor(labels=c("No", "Yes"))

glimpse(churn)

install.packages("dataMaid")

library(dataMaid)
library(magrittr)

#clean(churn) - > NAO fazer#

help(clean)

# Para manipular e transformar variaveis com purrr:

b <- as.factor(c(2,4,6))
b <- b %>% 
  as.character() %>% 
  as.numeric()
class(b)

help("map_dfc")

avg <- churn[19:20] %>% 
  na.omit() %>% 
  map(mean)

a <- avg %>% 
  map_dbl(~multiply_by(.x , 2))a

a <- a %>% 
  map_dfc(~divide_by(.x , 2))

double <- churn %>% 
  modify_if(is.double, ~multiply_by(.x , 2))

double <- double %>% 
  modify_at(19:20 , ~divide_by(.x , 2))
  
double[19:20]
churn[19:20]

# map_dfc vai trasformar todas as minhas colunas por uma unica funcao; #
# map_dfr faz a mesma coisa mas para linhas                            #

# webscraping com R  -  ou, como furar paywall de jornais !!!!! #

install.packages("boilerpipeR")
library(boilerpipeR)
install.packages("rJava")
install.packages("httr")
library(httr)
install.packages("rvest")
library(rvest)


url <- "https://www.wsj.com/articles/the-blockchain-is-the-internet-of-money-1506119424"
wsj <- httr::GET(url) %>% 
  httr::content("text")

mansur <- boiler

url2 <- "http://blogs.oglobo.globo.com/mansur/post/os-caminhos-da-genialidade.html"
mansur <- httr::GET(url2) %>% 
  httr::content("text")

url3 <- "https://en.wikipedia.org/wiki/Game_of_Thrones"

lista <- url3 %>% 
  read_html() %>% 
  html_table(fill=T)

df1 <- lista[[1]]
df2 <- lista[[2]]
df3 <- lista[[3]]
df4 <- lista[[4]]
View(df1)
View(df2)
View(df3)
View(df4)

df1 <- lista %>% 
  extract2(1)
View(df1)

GoT <- url3 %>% 
  read_html() %>% 
  html_node("#firstHeading i") %>% 
  html_text()

folha <- "http://www1.folha.uol.com.br/colunas/demetriomagnoli/2017/09/1921054-ausencia-de-punicao-a-pregacao-golpista-de-general-indica-que-algo-se-move-abaixo-dos-radares.shtml" %>% 
  read_html() %>% 
  html_nodes("p") %>% 
  html_text()
cat(folha)

wsj2 <- "https://www.wsj.com/articles/puerto-ricos-power-woes-are-decades-in-the-making-1506176140" %>% 
  read_html() %>% 
  html_nodes("p") %>% 
  html_text()

folha1 <- "http://www1.folha.uol.com.br/colunas/demetriomagnoli/2017/09/1921054-ausencia-de-punicao-a-pregacao-golpista-de-general-indica-que-algo-se-move-abaixo-dos-radares.shtml" 
xurl <- c(folha1, "http://painel.blogfolha.uol.com.br/2017/09/23/cunha-diz-que-funaro-usou-informacoes-de-sua-proposta-de-delacao-em-acordo-e-prepara-ofensiva/")

fsp <- map(xurl, ~{
  .x %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text()
})

cat(fsp[[1]])

# criando uma funcao que baixe os artigos da folha #

folha_download <- function(x){
  y <- x %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text()
  return(y)
}

readings <- map(folha1, folha_download)
cat(readings[[1]])

library(stringr)

# introducao a expressoes regulares #

L <- "ola, sou Lucas e tenho 25 anos"

L <- str_replace(L, "25", "26")

str_replace(L, "\\d", "1")
str_replace(L, "(?<=\\d)\\d", "6")
str_replace(L, "(?<=\\d{2})\\d", "6")

L %>% 
  str_extract_all("\\d*") %>% 
  unlist()

str_detect(L, "anos")

str_extract(str_extract(L, "\\d*.......$"), "\\d{2}")

L %>% 
  str_extract("\\d*.......$") %>% 
  str_extract("\\d{2}")

# quero selecionar determinado numero de digitos, e uso os codigos:        #
# "*": 0 ou mais; "+" 1 ou mais; "{2,}" 2 ou mais; "{2,5}" de 2 a 5        #
# para trabalhar extracts usamos "\\d" para digitos numericos,             #
# "\\D" para nao-numericos; "\\w" para palavra, e "\\W para nao palavra    #

str_replace(L, "\\d", "1")
str_replace(L, "\\+d", "26")

CPF <- "48.725.344-9"

str_replace_all(CPF, "\\D", "")

fone <- "+55 (11) 9-8696-4009"

fone <- str_replace_all(fone, "\\D", "")
str_replace(fone, "\\d", "+5")
