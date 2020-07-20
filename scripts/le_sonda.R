# Função geral de leitura de arquivos múltiplos de sonda
le_sonda <- function(pasta_SONDA) {
  library(abind)
  library(purrr)
  library(data.table)
  library(tidyr)
  
  # Função de leitura de arquivo indivudual *.xls da sonda
  le_sonda_arquivo <- function(arquivo_sonda) {
    library(readxl)
    library(stringr)
    library(data.table)
    library(lubridate)
    
    sonda_bruto <- read_excel(arquivo_sonda,
                              sheet = " Log data - 1",
                              col_types = c( "date", "date", "text", "text", "text",
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text",
                                             "text", "text", "text", "text", "text",
                                             "text")
                              )
    
    sonda_sel <- sonda_bruto[, c(1, 2, 3, 11, 15, 16, 4)]
    
    sonda_sel$Date <- ymd(sonda_sel$Date)
    sonda_sel$Time <- str_sub(as.character(sonda_sel$Time), 12, 19)
    sonda_sel$datahora_SONDA <- as.character.Date(paste(sonda_sel$Date, sonda_sel$Time))
    
    # Avaliar se faz sentido ter outras
    sonda_sel <- sonda_sel[, c(8, 3, 4, 5, 6, 7)]
    
    names(sonda_sel)[2] <- "Temp"
    names(sonda_sel)[3] <- "Sal"
    names(sonda_sel)[4] <- "OD"
    names(sonda_sel)[5] <- "Turb"
    
    sonda_sel$Temp <- round(as.double(sonda_sel$Temp), 2)
    sonda_sel$Sal <- round(as.double(sonda_sel$Sal), 2)
    sonda_sel$OD <- round(as.double(sonda_sel$OD), 2)
    sonda_sel$Turb <- round(as.double(sonda_sel$Turb), 2)
    sonda_sel$pH <- round(as.double(sonda_sel$pH), 2)
    
    # Muito outlier por entrar e sair da agua
    out_temp <- boxplot(sonda_sel$Temp, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$Temp %in% out_temp), ]
    
    out_sal <- boxplot(sonda_sel$Sal, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$Sal %in% out_sal), ]
    sonda_sel$Sal <- round(as.numeric(sonda_sel$Sal), 2)
    
    out_do <- boxplot(sonda_sel$OD, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$OD %in% out_do), ]
    
    out_turb <- boxplot(sonda_sel$Turb, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$Turb %in% out_turb), ]
    
    out_ph <- boxplot(sonda_sel$pH, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$pH %in% out_ph), ]
    
    sonda_sel_dt <- data.table(sonda_sel)
    
    return(sonda_sel_dt)
  }
  
  lista_arquivos_sonda <- list.files(pasta_SONDA,
                                     full.names = TRUE, 
                                     recursive = TRUE,
                                     pattern = "xls$")
  
  dados_sonda <- map_dfr(lista_arquivos_sonda, le_sonda_arquivo, .id = "registro_SONDA") %>%
    group_by(registro_SONDA) %>%
    nest()
  
  dados_sonda$data_sonda <-  ymd_hms(map_chr(dados_sonda[[2]], ~median(.$"datahora_SONDA")))
  dados_sonda$data_sonda <- ymd(str_sub(as.character.Date(dados_sonda$data_sonda), 1, 10))
  
  dados_sonda$datahora_I_sonda <-  ymd_hms(map_chr(dados_sonda[[2]], ~first(.$"datahora_SONDA")))
  dados_sonda$datahora_F_sonda <-  ymd_hms(map_chr(dados_sonda[[2]], ~last(.$"datahora_SONDA")))
  dados_sonda$med_Temp <- map_dbl(dados_sonda[[2]], ~median(.$Temp))
  dados_sonda$med_Sal <- map_dbl(dados_sonda[[2]], ~median(.$Sal))
  dados_sonda$med_OD <- map_dbl(dados_sonda[[2]], ~median(.$OD))
  dados_sonda$med_Turb <- map_dbl(dados_sonda[[2]], ~median(.$Turb))
  dados_sonda$med_pH <- map_dbl(dados_sonda[[2]], ~median(.$pH))
  
  dados_sonda <- dados_sonda %>%
    select(1, 3, 4, 5, 6, 7, 8, 9, 10, 2)
  
  return(dados_sonda)
}