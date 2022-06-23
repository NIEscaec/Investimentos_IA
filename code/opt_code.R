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
Invest_Imediato_IDP <- ler_excel("data-raw/TabelasCompletasPosicaoIDP.xlsx", "5", 4)

# Carrega a página 6 da Planilha IDP
Control_Final_IDP <- ler_excel("data-raw/TabelasCompletasPosicaoIDP.xlsx", "6", 4)

# Carrega a página 7 da Planilha IDP
Oper_Intercomp_IDP <- ler_excel("data-raw/TabelasCompletasPosicaoIDP.xlsx", "7", 4)

# Carrega a página IDP ingresso por país da planilha Estrp
fluxo_Invest_InvEstrp <- ler_excel("data-raw/InvEstrp.xls","IDP ingresso por país", 4)

# Carrega a página Igressos por país da Planilha InterciaPassivo
Igressos_InterciaPassivo <- ler_excel("data-raw/InterciaPassivop.xls", "Ingressos por país", 4)

# Carrega a página Amortizações por país da Planilha InterciaPassivo
Amortizacoes_InterciaPassivo <- ler_excel("data-raw/InterciaPassivop.xls", "Amortizações por país", 4)


# Lista com os anos
anos <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020")

# Carrega as linhas 1 a 6 da tabela 1
Invest_Imediato_IDP <- ler_linha(Invest_Imediato_IDP,12)
Control_Final_IDP <- ler_linha(Control_Final_IDP,12)
Oper_Intercomp_IDP <- ler_linha(Oper_Intercomp_IDP,12)
fluxo_Invest_InvEstrp <- ler_linha(fluxo_Invest_InvEstrp,12)
Igressos_InterciaPassivo <- ler_linha(Igressos_InterciaPassivo,12)
Amortizacoes_InterciaPassivo <- ler_linha(Amortizacoes_InterciaPassivo,12)

# realiza soma das linhas dos países e o pivot
Invest_Imediato_IDP <- soma_linhas(Invest_Imediato_IDP)
Control_Final_IDP <- soma_linhas(Control_Final_IDP)
Oper_Intercomp_IDP <- soma_linhas(Oper_Intercomp_IDP)
fluxo_Invest_InvEstrp <- soma_linhas(fluxo_Invest_InvEstrp)
Igressos_InterciaPassivo <- soma_linhas(Igressos_InterciaPassivo)
Amortizacoes_InterciaPassivo <- soma_linhas(Amortizacoes_InterciaPassivo)
fluxo_liq_IDP <- Igressos_InterciaPassivo - Amortizacoes_InterciaPassivo

# junta as linhas em uma só tabela 
Tabela_1 <- bind_rows(Invest_Imediato_IDP,Control_Final_IDP,Oper_Intercomp_IDP,fluxo_Invest_InvEstrp,
                      fluxo_liq_IDP,Amortizacoes_InterciaPassivo,Amortizacoes_InterciaPassivo)

setor <- c("IDP-Participação no Capital(Invest.Imed)", 
           "IDP-Participação no Capital(Control. Final)",
           "IDP-Operações Intercompanhia", 
           "Fluxo-Participação no Capital(Invest.Imed)",
           "Fluxo Líquido-Operações Intercompanhia", 
           "Empréstimos Intercompanhias-Ingressos",
           "Empréstimos Intercompanhias-Amortizações")

Tabela_1 <- criar_tabela(Tabela_1, setores_tab1)

#------------------------------------------------Tabela 2 ----------------------------------------------

httr::GET("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/TabelasCompletasPosicaoIDE.xlsx",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "TabelasCompletasPosicaoIDE.xlsx"), overwrite = T))

# Carrega a página 3 da Planilha IDE
Invest_Imediato_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "3", 4)

# Carrega a página 9 da Planilha IDP
Oper_Intercomp_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "9", 4)

# Carrega a página 11 da Planilha IDP
Acoes_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "11", 4)

# Carrega a página 13 da Planilha IDP
RF_Longo_Prazo_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "13", 4)

# Carrega a página 12 da Planilha IDP
RF_Curto_Prazo_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "12", 4)

# Carrega a página 14 da Planilha IDP
Moedas_19_IDE <-ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "14", 4)

# Carrega a página 15 da Planilha IDP
Moedas_20_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "15", 4)

# Carrega a página 16 da Planilha IDP
Imoveis_19_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx","16",4)

# Carrega a página 17 da Planilha IDP
Imoveis_20_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "17", 4)

# Carrega a página IDE saídas por país da Planilha InvBrap
Fluxo_Invest_Imediatop_IDE <- ler_excel("data-raw/InvBrap.xls", "IDE saídas por país", 4)

moedas <- full_join(PG_IDE_Moedas_19, PG_IDE_Moedas_20, by = c("Discriminação"))
moedas <- select(moedas,Discriminação, anos)

imoveis <- full_join(PG_IDE_Imoveis_19, PG_IDE_Imoveis_20, by = c("Discriminação"))
imoveis <- select(imoveis,Discriminação, anos)


# Carrega as linhas 1 a 6 da tabela 1
Invest_Imediato_IDE <- ler_linha(Invest_Imediato_IDE,12)
Oper_Intercomp_IDE <- ler_linha(Oper_Intercomp_IDE,12)
Acoes_IDE <- ler_linha(Acoes_IDE,12)
RF_Longo_Prazo_IDE <- ler_linha(RF_Longo_Prazo_IDE,12)
RF_Curto_Prazo_IDE <- ler_linha(RF_Curto_Prazo_IDE,12)
moedas <- ler_linha(moedas,12)
imoveis <- ler_linha(imoveis,12)
Fluxo_Invest_Imediatop_IDE <- ler_linha(Fluxo_Invest_Imediatop_IDE,12)


# realiza soma das linhas dos países e o pivot
Invest_Imediato_IDE <- soma_linhas(Invest_Imediato_IDE)
Oper_Intercomp_IDE <- soma_linhas(Oper_Intercomp_IDE)
Acoes_IDE <- soma_linhas(Acoes_IDE)
RF_Longo_Prazo_IDE <- soma_linhas(RF_Longo_Prazo_IDE)
RF_Curto_Prazo_IDE <- soma_linhas(RF_Curto_Prazo_IDE)
moedas <- soma_linhas(moedas)
imoveis <- soma_linhas(imoveis)
Fluxo_Invest_Imediatop_IDE <- soma_linhas(Fluxo_Invest_Imediatop_IDE)
PG_IDE_invest_carteira <- Acoes_IDE + RF_Longo_Prazo_IDE + RF_Curto_Prazo_IDE

Tabela_2 <- bind_rows(Invest_Imediato_IDE,Oper_Intercomp_IDE,PG_IDE_invest_carteira,Acoes_IDE,RF_Longo_Prazo_IDE,
                      RF_Curto_Prazo_IDE,moedas,imoveis, Fluxo_Invest_Imediatop_IDE)

setor <- c("IBD - Participação no Capital", 
           "IBD - Operações Intercompanhia",
           "Invest. em Carteira ", 
           "Ações",
           "Renda Fixa de Longo Prazo", 
           "Renda Fixa de Curto Prazo",
           "Moedas/Depósitos",
           "Imóveis",
           "Fluxo - Participação no Capital ")

Tabela_2 <- criar_tabela(Tabela_2, setor_tab2)

#------------------------------------------------Cria a primeira tabela referente a Plan 2-----------------------------------
Tabela_4<- bind_rows(Invest_Imediato_IDE,Oper_Intercomp_IDE,Fluxo_Invest_Imediatop_IDE)
setor_tab4 <- c("IBD - Participação no Capital",
                "IBD - Operações Intercompanhia",
                "Fluxo - Participação no Capital ")

Tabela_4 <- criar_tabela(Tabela_4, setor_tab4)


# IDP Quantidade de Investidores
anos15e20 <- c("2015", "2020")

Quantidade_Control_Final  <- ler_excel("data-raw/TabelasCompletasPosicaoIDP.xlsx", "9", 4)

Quantidade_Control_Final <- IDP_Qtd_Invest(Quantidade_Control_Final, anos15e20)

Quantidade_Control_Final <- soma_Qtd_Invest(Quantidade_Control_Final,3 ,anos15e20)

Quantidade_Invest_Imediato <- ler_excel("data-raw/TabelasCompletasPosicaoIDP.xlsx", "8", 4) 

Quantidade_Invest_Imediato <- IDP_Qtd_Invest(Quantidade_Invest_Imediato, anos15e20)

Quantidade_Invest_Imediato <- soma_Qtd_Invest(Quantidade_Invest_Imediato,3 ,anos15e20)

Quantidade_Control_Final <- add_column(Quantidade_Control_Final, Setor = "Controlador Final", .before = 1)
Quantidade_Invest_Imediato <- add_column(Quantidade_Invest_Imediato, Setor = "Investimento Imediato", .before = 1)

Qtd_Invest <- full_join(Quantidade_Invest_Imediato, Quantidade_Control_Final)

#######################################################################

#-------------------------------------Código que cria as planilhas referentes ao setor de 2020 das plan 1 e 2 ---------------------------------------


IDP_Por_Setor_Control_Final <- read_xlsx("data-raw/TabelasCompletasPosicaoIDP.xlsx",sheet = "14", range = "A5:AJ20")
IDP_Por_Setor_Control_Final <- setores(IDP_Por_Setor_Control_Final)
IDP_Por_Setor_Control_Final <- setores_final(IDP_Por_Setor_Control_Final, TESTE)

IDP_Por_Setor_Inv_Imed <- read_xlsx("data-raw/TabelasCompletasPosicaoIDP.xlsx",sheet = "13", range = "A5:AJ20")
IDP_Por_Setor_Inv_Imed <- setores(IDP_Por_Setor_Inv_Imed)
IDP_Por_Setor_Inv_Imed <- setores_final(IDP_Por_Setor_Inv_Imed, TESTE2)

# Outros

outros_Control_Final <- outros_func(IDP_Por_Setor_Control_Final, Control_Final_IDP)
IDP_Por_Setor_Control_Final[nrow(IDP_Por_Setor_Control_Final) + 1,] <- outros_Control_Final

outros_Inv_Imed <- outros_func(IDP_Por_Setor_Inv_Imed, Invest_Imediato_IDP)
IDP_Por_Setor_Inv_Imed[nrow(IDP_Por_Setor_Inv_Imed) + 1,] <- outros_Inv_Imed

tabela_por_setor <- left_join(IDP_Por_Setor_Inv_Imed, IDP_Por_Setor_Control_Final, by = "Setores", suffix = c(".Invest Imediato", ".Control Final"))

################################################################################


IBD_Qtd_Invest <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "4", 4)
IBD_Qtd_Invest <- rename(IBD_Qtd_Invest, "2020" = "20202/")
IBD_Qtd_Invest <- IDP_Qtd_Invest(IBD_Qtd_Invest,anos15e20)
IBD_Qtd_Invest <- soma_Qtd_Invest(IBD_Qtd_Invest,3,anos15e20)

IBD_Por_Setor <- read_xlsx("data-raw/TabelasCompletasPosicaoIDE.xlsx",sheet = "18", range = "A5:BZ24")
IBD_Por_Setor <- setores_idb(IBD_Por_Setor)
IBD_Por_Setor <- setores_final(IBD_Por_Setor)

################################################################################

IBD_Por_Setor_Outros <- outros_func(IBD_Por_Setor, Invest_Imediato_IDE)
IBD_Por_Setor[nrow(IBD_Por_Setor) + 1,] <- IBD_Por_Setor_Outros

