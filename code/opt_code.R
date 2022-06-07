#Código para pegar dados do IDE

library(magrittr)
library(readxl)
library(dplyr)
library(plotly)
library(readr)
library(scales)
library(ggpmisc)
library(tidyverse)
library(tidyr)



pais <- c("Alemanha","China", "Chile","Panamá", "Mônaco", "Maurício", "Panamá")

# Baixa a Planilha
httr::GET("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/TabelasCompletasPosicaoIDP.xlsx",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "TabelasCompletasPosicaoIDP.xlsx"), overwrite = T))

# Carrega a página 5 da Planilha IDP
PG_IDP_Invest_Imediato <- ler_IDP_aba_5_7("data-raw/TabelasCompletasPosicaoIDP.xlsx", "5", 4)

# Carrega a página 6 da Planilha IDP
PG_IDP_Control_Final <- ler_IDP_aba_5_7("data-raw/TabelasCompletasPosicaoIDP.xlsx", "6", 4)

# Carrega a página 7 da Planilha IDP
PG_IDP_Oper_Intercomp <- ler_IDP_aba_5_7("data-raw/TabelasCompletasPosicaoIDP.xlsx", "7", 4)

# Carrega a página IDP ingresso por país da planilha Estrp
PG_InvEstrp_fluxo_Invest <- leitura_Estrp("data-raw/InvEstrp.xls","IDP ingresso por país", 4)

# Carrega a página Igressos por país da Planilha InterciaPassivo
PG_InterciaPassivo_Igressos <- leitura_InterciaPassivo("data-raw/InterciaPassivop.xls", "Ingressos por país", 4) 

# Carrega a página Amortizações por país da Planilha InterciaPassivo
PG_InterciaPassivo_Amortizacoes <- leitura_InterciaPassivo("data-raw/InterciaPassivop.xls", "Amortizações por país", 4)


# Lista com os anos
anos <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020")

# Carrega as linhas 1 a 6 da tabela 1
Linha_I <- ler_linha(PG_IDP_Invest_Imediato)
Linha_II <- ler_linha(PG_IDP_Control_Final)
Linha_III <- ler_linha(PG_IDP_Oper_Intercomp)
Linha_IV <- ler_linha(PG_InvEstrp_fluxo_Invest)
Linha_VI <- ler_linha(PG_InterciaPassivo_Igressos)
Linha_VII <- ler_linha(PG_InterciaPassivo_Amortizacoes)

# realiza soma das linhas dos países e o pivot
defI <- soma_linhas(Linha_I)
defII <- soma_linhas(Linha_II)
defIII <- soma_linhas(Linha_III)
defIV <- soma_linhas(Linha_IV)
defVI <- soma_linhas(Linha_VI)
defVII <- soma_linhas(Linha_VII)
defV <- defVI - defVII

# junta as linhas em uma só tabela 
Tabela_1 <- bind_rows(defI,defII,defIII,defIV,defV,defVI,defII)

setor <- c("IDP-Participação no Capital(Invest.Imed)", 
           "IDP-Participação no Capital(Control. Final)",
           "IDP-Operações Intercompanhia", 
           "Fluxo-Participação no Capital(Invest.Imed)",
           "Fluxo Líquido-Operações Intercompanhia", 
           "Empréstimos Intercompanhias-Ingressos",
           "Empréstimos Intercompanhias-Amortizações")

Tabela_1$setor <- setor

Tabela_1 <- Tabela_1 %>%
  select(setor, anos)





#------------------------------------------------Tabela 2 ----------------------------------------------

anos_ate19 <- c("2010", "2011", "2012", "2013", "2014", "2015",
                "2016", "2017", "2018", "2019")

httr::GET("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/TabelasCompletasPosicaoIDE.xlsx",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "TabelasCompletasPosicaoIDE.xlsx"), overwrite = T))

PG_IDE_Invest_Imediato <- leitura_IDE("data-raw/TabelasCompletasPosicaoIDE.xlsx", "3", 4)

PG_IDE_Oper_Intercomp <- leitura_IDE("data-raw/TabelasCompletasPosicaoIDE.xlsx", "9", 4)

PG_IDE_Acoes <- leitura_IDE("data-raw/TabelasCompletasPosicaoIDE.xlsx", "11", 4)

PG_IDE_RF_Longo_Prazo <- leitura_IDE("data-raw/TabelasCompletasPosicaoIDE.xlsx", "13", 4)

PG_IDE_RF_Curto_Prazo <- leitura_IDE("data-raw/TabelasCompletasPosicaoIDE.xlsx", "12", 4)

PG_IDE_Moedas_19 <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", 
                               sheet = "14", skip = 4, col_types = c("text", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric"))

PG_IDE_Moedas_20 <- leitura_IDE_2020("data-raw/TabelasCompletasPosicaoIDE.xlsx", "15", 4)

PG_IDE_Imoveis_19 <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                                sheet = "16", skip = 4, col_types = c("text", "numeric",
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric"))

PG_IDE_Imoveis_20 <- leitura_IDE_2020("data-raw/TabelasCompletasPosicaoIDE.xlsx", "17", 4)

PG_IDE_Fluxo_Invest_Imediato <- read_xls("data-raw/InvBrap.xls",
                                         sheet = "IDE saídas por país", skip = 4,
                                         col_types = c("text", 
                                                       "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric")
)



moedas <- full_join(PG_IDE_Moedas_19, PG_IDE_Moedas_20, by = c("Discriminação"))
moedas <- select(moedas,Discriminação, anos)

imoveis <- full_join(PG_IDE_Imoveis_19, PG_IDE_Imoveis_20, by = c("Discriminação"))
imoveis <- select(imoveis,Discriminação, anos)


# Carrega as linhas 1 a 6 da tabela 1
Linha_IDE_1 <- ler_linha(PG_IDE_Invest_Imediato)
Linha_IDE_2 <- ler_linha(PG_IDE_Oper_Intercomp)
Linha_IDE_4 <- ler_linha(PG_IDE_Acoes)
Linha_IDE_5 <- ler_linha(PG_IDE_RF_Longo_Prazo)
Linha_IDE_6 <- ler_linha(PG_IDE_RF_Curto_Prazo)
Linha_IDE_7 <- ler_linha(moedas)
Linha_IDE_8 <- ler_linha(imoveis)
Linha_IDE_9 <- ler_linha(PG_IDE_Fluxo_Invest_Imediato)

# realiza soma das linhas dos países e o pivot
def_IDE_1 <- soma_linhas(Linha_IDE_1)
def_IDE_2 <- soma_linhas(Linha_IDE_2)
def_IDE_4 <- soma_linhas(Linha_IDE_4)
def_IDE_5 <- soma_linhas(Linha_IDE_5)
def_IDE_6 <- soma_linhas(Linha_IDE_6)
def_IDE_7 <- soma_linhas(Linha_IDE_7)
def_IDE_8 <- soma_linhas(Linha_IDE_8)
def_IDE_9 <- soma_linhas(Linha_IDE_9)
def_IDE_3 <- def_IDE_4 + def_IDE_5 + def_IDE_6

Tabela_2 <- bind_rows(def_IDE_1,def_IDE_2,def_IDE_3,def_IDE_4,def_IDE_5,def_IDE_6,def_IDE_7,def_IDE_8, def_IDE_9)


setor <- c("IBD - Participação no Capital", 
           "IBD - Operações Intercompanhia",
           "Invest. em Carteira ", 
           "Ações",
           "Renda Fixa de Longo Prazo", 
           "Renda Fixa de Curto Prazo",
           "Moedas/Depósitos",
           "Imóveis",
           "Fluxo - Participação no Capital ")

Tabela_2$setor <- setor

Tabela_2 <- Tabela_2 %>%
  select(setor, anos)




