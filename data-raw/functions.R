library(dplyr)

selecionarPais <- function(dataFrame){
  
  dataFrame %>%
    filter(Pais %in% pais) %>%
    select("Pais", anos) %>%
    arrange_all()
  
}


ler_linha <- function(nomePlan){
   nomePlan %>%
    renomear_disc() %>%
    selecionarPais()
}

soma_linhas <- function(nlinha){
    nlinha[is.na(nlinha)] <- 0
    soma <- apply(nlinha[,2:12], 2, FUN=sum)
    def <- data_frame(anos, soma)
    def <- def %>%
      pivot_wider(names_from = anos, values_from = soma)
}

leitura_planilha <- function(planName, sht, skp){
  if(length(col_types) == 23){
    read_xlsx(planName,
              sheet = sht, skip = skp, col_types = c("text", "numeric","numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric"))
    
  } else if(length(col_types) == 36){
    read_xlsx(planName, 
              sheet = sht, skip = skp, col_types = c("text", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric"))
  }
  
}




ler_IDP_aba_5_7 <- function(planName, sht, skp){
  
  return (read_xlsx(planName,
                    sheet = sht, skip = skp, col_types = c("text", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric") ) 
  )
  
}



ler_IPD_aba_13 <- function (planName, sht, rng) 
{
  return (read_xlsx(planName, sheet = sht, range = rng, col_types = c("text", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", "numeric", "numeric"))
  )
}  

renomear_disc <- function(dataframe){
  rename(dataframe, "Pais" = "Discriminação")
}



leitura_Estrp <- function(planName, sht, skp){
  read_xls(planName,
           sheet = sht , skip = skp, col_types = c("text", 
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric"))
  
}

leitura_InterciaPassivo <- function(planName, sht, skp){
  read_xls(planName, 
           sheet = sht, skip = skp, col_types = c(c("text", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric")))
}

leitura_IDE <- function(planName, sht, skp){
  read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
             sheet = sht, skip = skp, col_types = c(c("text", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric",
                                                   "numeric", "numeric")))
}

leitura_IDE_2020 <- function(planName, sht, skp){
  read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
             sheet = sht, skip = skp, col_types = c(c("text", "numeric", "numeric")))
}

  


