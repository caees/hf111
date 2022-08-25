# Autora: Rebeca Carmo de Souza Cruz
## 5 ago 2022

# ==== Introdução ====

library(foreign) 
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(readr)
library(readxl)
library(rio)
library(openxlsx)

system("defaults write org.R-project.R force.LANG en_US.UTF-8")

setwd("/Users/rebecacarmodesouzacruz/Library/CloudStorage/OneDrive-Pessoal/Ministério da Saúde/Produtos/Produtos 2021 2022/Produto 5/Dados/TG/MS")

#Lendo bases de 2018-2021 da Função 10 no R por meio de lista - é apenas uma pq se trata de biênio

tg_uniao <- import_list(dir("/Users/rebecacarmodesouzacruz/Library/CloudStorage/OneDrive-Pessoal/Ministério da Saúde/Produtos/Produtos 2021 2022/Produto 5/Dados/TG/MS", pattern = ".xlsx"))

tg_uniao <- do.call("rbind", tg_uniao) #empilhando listas numa única datatable
 
# fazendo a soma das despesas correntes em ASPS para Função 10

colnames(tg_uniao)

colSums(is.na(SHA_Uniao_MS))

SHA_Uniao_MS <-tg_uniao %>% 
  dplyr:: select("Ano Lançamento", Iduso, "Categoria Econômica Despesa", "Grupo Despesa", "DESPESAS PAGAS", "RESTOS A PAGAR PROCESSADOS PAGOS", "RESTOS A PAGAR NAO PROCESSADOS PAGOS") %>% 
  filter(Iduso == "6" & `Categoria Econômica Despesa` == "3") %>% 
  group_by(`Ano Lançamento`) %>% 
  mutate_at(c("DESPESAS PAGAS", "RESTOS A PAGAR PROCESSADOS PAGOS", "RESTOS A PAGAR NAO PROCESSADOS PAGOS"), ~replace_na(.,0)) %>%
  mutate(resultado = `DESPESAS PAGAS` + `RESTOS A PAGAR PROCESSADOS PAGOS` + `RESTOS A PAGAR NAO PROCESSADOS PAGOS`) %>%
  select(resultado) %>% 
  summarise(resultado_final_soma = sum(resultado))
  

setwd("/Users/rebecacarmodesouzacruz/Library/CloudStorage/OneDrive-Pessoal/Ministério da Saúde/Produtos/Produtos 2021 2022/Produto 5/Dados/TG/MEC")

#Lendo bases de 2018-2021 da Função 12 no R por meio de lista

tg_uniao_mec <- import_list(dir("/Users/rebecacarmodesouzacruz/Library/CloudStorage/OneDrive-Pessoal/Ministério da Saúde/Produtos/Produtos 2021 2022/Produto 5/Dados/TG/MEC", pattern = ".xlsx"))

tg_uniao_mec <- do.call("rbind", tg_uniao_mec) #empilhando listas numa única datatable

# fazendo a soma das despesas correntes em ASPS para Função 12

colnames(tg_uniao_mec)

SHA_Uniao_MEC <-tg_uniao_mec %>% 
  dplyr:: select("Ano Lançamento", "Subfunção Governo", "Categoria Econômica Despesa", "DESPESAS PAGAS", "RESTOS A PAGAR PROCESSADOS PAGOS", "RESTOS A PAGAR NAO PROCESSADOS PAGOS") %>% 
  filter(`Subfunção Governo`=="302" & `Categoria Econômica Despesa` == "3") %>% 
  group_by(`Ano Lançamento`) %>% 
  mutate_at(c("DESPESAS PAGAS", "RESTOS A PAGAR PROCESSADOS PAGOS", "RESTOS A PAGAR NAO PROCESSADOS PAGOS"), ~replace_na(.,0)) %>%
  mutate(resultado = `DESPESAS PAGAS` + `RESTOS A PAGAR PROCESSADOS PAGOS` + `RESTOS A PAGAR NAO PROCESSADOS PAGOS`) %>%
  select(resultado) %>% 
  summarise(resultado_final_mec = sum(resultado))

#lendo dados do script ler RREO estado e municípios -> ver script de leitura

#municipios

RREO_MUNICIPAL <- setnames(RREO_MUNICIPAL,tolower(names(RREO_MUNICIPAL))) #tudo em minusculo

variaveis <- c("Despesas_Liquidadasbim")

SHA_municipios <- RREO_MUNICIPAL %>% 
  select( ano, cod_uf, valor, itemdeinformacao2, rubrica) %>% 
  dplyr:: filter(`itemdeinformacao2`%in% variaveis & `rubrica`=="Despesas Correntes") %>% 
  group_by(ano, cod_uf) %>%
  mutate(valor = replace_na(valor, 0)) %>%
  ungroup(cod_uf) %>% 
  summarise(despesas_asps_municipios = sum(valor)) 

#estados

variaveis <- c("Despesas_Liquidadas_bim")

RREO_ESTADUAL <- setnames(RREO_ESTADUAL,tolower(names(RREO_ESTADUAL))) #tudo em minusculo

SHA_estados <-RREO_ESTADUAL %>% 
  select( ano, cod_uf, valor, itemdeinformacao2, rubrica) %>% 
  filter(`itemdeinformacao2`%in% variaveis & `rubrica`=="Despesas Correntes" ) %>% 
  group_by(ano, cod_uf) %>%
  mutate(valor = replace_na(valor, 0)) %>%
  ungroup(cod_uf) %>% 
  summarise(despesas_asps_estados = sum(valor)) 

#juntando todos os resultados 

SHA_SUS <- cbind(SHA_Uniao_MS$`Ano Lançamento`, SHA_Uniao_MS$resultado_final_soma, SHA_Uniao_MEC$resultado_final_mec, SHA_estados$despesas_asps_estados, SHA_municipios$despesas_asps_municipios)
colnames(SHA_SUS)
SHA_SUS <- as.data.table(SHA_SUS)
SHA_SUS <-SHA_SUS %>%
  rename(Ano=V1, 
         "HF 1.1.1 – SUS – Órgãos de Saúde - União"= V2,
         "HF 1.1.1 – MEC – Hospitais universitários e similares"= V3,
         "HF 1.1.1 – Órgãos de Saúde - Estados "= V4,
         "HF 1.1.1 – Órgãos de Saúde - Municípios"= V5)

setwd("/Users/rebecacarmodesouzacruz/Library/CloudStorage/OneDrive-Pessoal/Ministério da Saúde/Produtos/Produtos 2021 2022/Produto 5/Dados/Resultados")

write.xlsx(SHA_SUS, file="SHA_HF111.xlsx", sheetName="HF111", rowNames=FALSE)

write_csv2(RREO_ESTADUAL, "RREO_ESTADUAL")
write_csv2(RREO_MUNICIPAL, "RREO_MUNICIPAL")


