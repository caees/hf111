library(data.table)
library(tidyverse)
library(readr)
library(readxl)

PATH_RREO <- "path" # Copie o endereço que o arquivo está (lembre de trocar a direção das barras)

RREO_ESTADUAL <- data.table()
RREO_MUNICIPAL <- data.table()

for (ARQUIVO in dir(PATH_RREO, pattern = "ASPS")){
  
    for (SHEET in 1:4){
      
      PATH_RREO1 <-  paste0(PATH_RREO,"/",ARQUIVO)
      
      RREO <- read_excel(PATH_RREO1, skip = 2, sheet = SHEET)
      setDT(RREO)
      
      setnames(RREO, c("...1","...2","...3"), c("COD_UF","SG_UF","NM_UF"))
      RREO <- melt(RREO, c("COD_UF","SG_UF","NM_UF"), variable.name = "ItemdeInformacao", value.name = "VALOR")
      
      RREO <- RREO[,RUBRICA:=str_split(ItemdeInformacao, pattern = "[.][.][.]", n = 2, simplify = TRUE)[,1]]
      RREO <- RREO[,ItemdeInformacao1:=str_split(ItemdeInformacao, pattern = "[.][.][.]", n = 2, simplify = TRUE)[,2]]
      RREO <- RREO[,ItemdeInformacao1:=as.numeric(ItemdeInformacao1)]
      
      RREO <- RREO[ItemdeInformacao1 %between% c(4,25), ItemdeInformacao2 := "Dotacao_Inicial"]
      RREO <- RREO[ItemdeInformacao1 %between% c(26,47), ItemdeInformacao2 := "Dotacao_Atualizada"]
      RREO <- RREO[ItemdeInformacao1 %between% c(48,69), ItemdeInformacao2 := "Despesas_Empenhadasbim"]
      RREO <- RREO[ItemdeInformacao1 %between% c(70,91), ItemdeInformacao2 := "Despesas_Empenhadas%"]
      RREO <- RREO[ItemdeInformacao1 %between% c(92,113), ItemdeInformacao2 := "Despesas_Liquidadasbim"]
      RREO <- RREO[ItemdeInformacao1 %between% c(114,135), ItemdeInformacao2 := "Despesas_Liquidadas%"]
      RREO <- RREO[ItemdeInformacao1 %between% c(136,157), ItemdeInformacao2 := "Despesas_Pagasbim"]
      RREO <- RREO[ItemdeInformacao1 %between% c(158,179), ItemdeInformacao2 := "Despesas_Pagas%"]
      RREO <- RREO[ItemdeInformacao1 %between% c(180,201), ItemdeInformacao2 := "Inscritas em Restos a Pagar Não Processados"]
      
      RREO <- RREO[,ANO:=SHEET]
      RREO$VALOR[is.na(RREO$VALOR)] <- 0
      
      if (SHEET < 3) {
        
        RREO_ESTADUAL <- rbind(RREO_ESTADUAL,RREO,fill = T)
        RREO_ESTADUAL <- RREO_ESTADUAL[ANO == 1, ANO:=2020][ANO == 2, ANO:=2021]
        RREO_ESTADUAL[ItemdeInformacao1 %in% c(seq(4,201,22),seq(5,201,22),seq(6,201,22)), subfun:="AB"]
        RREO_ESTADUAL[ItemdeInformacao1 %in% c(seq(7,201,22),seq(8,201,22),seq(9,201,22)), subfun:="AHA"]
        RREO_ESTADUAL[ItemdeInformacao1 %in% c(seq(10,201,22),seq(11,201,22),seq(12,201,22)), subfun:="SPT"]
        RREO_ESTADUAL[ItemdeInformacao1 %in% c(seq(13,201,22),seq(14,201,22),seq(15,201,22)), subfun:="VS"]
        RREO_ESTADUAL[ItemdeInformacao1 %in% c(seq(16,201,22),seq(17,201,22),seq(18,201,22)), subfun:="VE"]
        RREO_ESTADUAL[ItemdeInformacao1 %in% c(seq(19,201,22),seq(20,201,22),seq(21,201,22)), subfun:="AN"]
        RREO_ESTADUAL[ItemdeInformacao1 %in% c(seq(22,201,22),seq(23,201,22),seq(24,201,22)), subfun:="OUTRAS"]
        RREO_ESTADUAL[ItemdeInformacao1 %in% c(seq(25,201,22)), subfun:="TODAS"]
        
        
      } else{
        
        RREO_MUNICIPAL <- rbind(RREO_MUNICIPAL,RREO, fill = T)
        RREO_MUNICIPAL <- RREO_MUNICIPAL[ANO == 3, ANO:=2020][ANO == 4, ANO:=2021]
        RREO_MUNICIPAL[ItemdeInformacao1 %in% c(seq(4,201,22),seq(5,201,22),seq(6,201,22)), subfun:="AB"]
        RREO_MUNICIPAL[ItemdeInformacao1 %in% c(seq(7,201,22),seq(8,201,22),seq(9,201,22)), subfun:="AHA"]
        RREO_MUNICIPAL[ItemdeInformacao1 %in% c(seq(10,201,22),seq(11,201,22),seq(12,201,22)), subfun:="SPT"]
        RREO_MUNICIPAL[ItemdeInformacao1 %in% c(seq(13,201,22),seq(14,201,22),seq(15,201,22)), subfun:="VS"]
        RREO_MUNICIPAL[ItemdeInformacao1 %in% c(seq(16,201,22),seq(17,201,22),seq(18,201,22)), subfun:="VE"]
        RREO_MUNICIPAL[ItemdeInformacao1 %in% c(seq(19,201,22),seq(20,201,22),seq(21,201,22)), subfun:="AN"]
        RREO_MUNICIPAL[ItemdeInformacao1 %in% c(seq(22,201,22),seq(23,201,22),seq(24,201,22)), subfun:="OUTRAS"]
        RREO_MUNICIPAL[ItemdeInformacao1 %in% c(seq(25,201,22)), subfun:="TODAS"]
        
      }
      
      
    }  
  
}

RREO_ESTADUAL <- RREO_ESTADUAL[,c(-4,-7)]
RREO_MUNICIPAL <- RREO_MUNICIPAL[,c(-4,-7)]


fwrite(RREO_ESTADUAL, paste0(PATH_EST,"/RREO_ESTADUAL.csv"), sep = ";", dec = ",")
fwrite(RREO_MUNICIPAL, paste0(PATH_EST,"/RREO_MUNICIPAL.csv"), sep = ";", dec = ",")

#########################################################################################################
