# rm(list = ls())
library(tidyverse)

# CARREGANDO Produção QUANTIDADE_E_VALOR_DA_PRODUÇÃO_COMERCIALIZADA ----
VPM <-
  read.table(
    "./data/DBAMB_QuantidadeEhValordaProducaoMineralComercializada.csv",
    header = TRUE, sep = "\t",dec = ".",
    fill = TRUE, quote = "", 
    # colClasses = c("integer","character","character","character","character","character","numeric","numeric"),
    fileEncoding = 'UTF-8', encoding = 'UTF-8')

 VPM <- 
   VPM[VPM$Ano.Base.Ral > 2009,]

# Ajustes das variáveis ----

colnames(VPM) <-
  colnames(VPM) |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
colnames(VPM)[c(4,7)] <- c("UF","Valor.Producao.Comercializada.Produto")

# VPM$Substancia.RAL <- 
#   VPM$Substancia.RAL |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

VPM$Substancia.AMB <-
  VPM$Substancia.AMB |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

VPM$Produto.Comercializado <-
  VPM$Produto.Comercializado |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

VPM$Municipio <-
  VPM$Municipio |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

# VPM$Nome <-
#   VPM$Nome |> iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |> tolower() |> stringr::str_squish()


# Formatando colunas de grandezas como numeric

VPM$Valor.Producao.Comercializada.Produto <- 
  gsub(VPM$Valor.Producao.Comercializada.Produto, 
       pattern = ",", replacement = "") |> as.numeric()

VPM$Valor.Producao.Comercializada.Substancia.AMB <- 
  gsub(VPM$Valor.Producao.Comercializada.Substancia.AMB, 
       pattern = ",", replacement = "") |> as.numeric()

  
# Impondo IDs de agregação ----

VPM$id_ANO_SUBSTANCIA <- 
  paste(VPM$Ano.Base.Ral, VPM$Substancia.AMB) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_SUBSTANCIA_UF <- 
  paste0(VPM$Ano.Base.Ral, VPM$Substancia.AMB, VPM$UF) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_PRODUTO <- 
  paste(VPM$Ano.Base.Ral, VPM$Produto.Comercializado) |> gsub(pattern = " ", replacement = "")

VPM$id_ANO_PRODUTO_UF <- 
  paste0(VPM$Ano.Base.Ral, VPM$Produto.Comercializado, VPM$UF) |> gsub(pattern = " ", replacement = "")


# Colunas de Market Share ----

# _____ VPM_Produto ----  
VPM <- 
  left_join(
    filter(VPM, Valor.Producao.Comercializada.Produto > 0),
    summarise(
      group_by(
        VPM,
        id_ANO_PRODUTO),  
      "VPM_Produto" = sum(Valor.Producao.Comercializada.Produto, na.rm = T)),
    by = c('id_ANO_PRODUTO'))

# _____ VPM_Substancia ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA),  
      "VPM_Substancia" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T)),
    by = c('id_ANO_SUBSTANCIA'))



# _____ VPM_Produto_UF ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_PRODUTO_UF),  
      "VPM_Produto_UF" = sum(Valor.Producao.Comercializada.Produto, na.rm = T)),
    by = c('id_ANO_PRODUTO_UF'))

# _____ VPM_Substancia_UF ----
VPM <- 
  left_join(
    VPM,
    summarise(
      group_by(
        VPM,
        id_ANO_SUBSTANCIA_UF),  
      "VPM_Substancia_UF" = sum(Valor.Producao.Comercializada.Substancia.AMB, na.rm = T)),
    by = c('id_ANO_SUBSTANCIA_UF'))


VPM <- 
  VPM[,c("Ano.Base.Ral", "CPF.CNPJ.Titular", "Municipio", "UF", "Substancia.AMB", 
         "Produto.Comercializado", "Valor.Producao.Comercializada.Produto", 
         "Valor.Producao.Comercializada.Substancia.AMB", 
         "VPM_Produto", "VPM_Substancia", "VPM_Produto_UF", "VPM_Substancia_UF")]



