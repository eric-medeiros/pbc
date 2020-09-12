# Funcao geral de leitura de arquivos múltiplos de GPS 

le_rota_L <- function(pasta_L) {
  library(purrr)
  library(abind)
  library(data.table)
  library(lubridate)
  library(tidyr)
  
  # Funcao de leitura de arquivo indivudual de *.gpx do tipo track
  le_rota_arquivo <- function(arquivo_rota) {
    library(rgdal)
    library(dplyr)
    library(lubridate)
    library(sp)
    
    rota_pontos <- readOGR(arquivo_rota, layer = "track_points", verbose = FALSE)
    rota_pontos@data$time <- ymd_hms(rota_pontos@data$time)
    
    rota_pontos_dt <- data.table(rota_pontos@data$time, rota_pontos@coords)
    names(rota_pontos_dt) <- c("datahora_rota", "lng", "lat")
    
    rota_pontos_dt$dist_p_prox <- c(spDists(rota_pontos_dt[, c(2, 3)], longlat = TRUE, segments = TRUE), 0)
    
    rota_pontos_dt$datahora_prox <- lead(rota_pontos_dt$datahora_rota)
    
    rota_pontos_dt$tempo_p_prox <- rota_pontos_dt$datahora_prox - rota_pontos_dt$datahora_rota
    
    rota_pontos_dt$datahora_prox <- NULL
    
    rota_pontos_dt$datahora_rota <- as.character.Date(rota_pontos_dt$datahora_rota)
    
    return(rota_pontos_dt)
  }
  
  # Listagem dos arquivos dentro da pasta_GPS
  lista_arquivos_rota <- list.files(paste(pasta_L, "/01_CAMPO/03_GPS", sep = ""),
                                   full.names = TRUE,
                                   pattern = "Trajecto",
                                   recursive = TRUE)
  
  # Aplicando a funcao le_rota_pontos_arquivo para cada arquivo na lista de arquivos da pasta_GPS
  
  dados_rota <- map_dfr(lista_arquivos_rota, le_rota_arquivo, .id = "registro_ROTA") %>%
    group_by(registro_ROTA) %>%
    nest()
  
  dados_rota$saida <- as.character(as.numeric(str_sub(lista_arquivos_rota, 85, 87)))
  dados_rota$data_rota <- ymd(str_sub(map_chr(dados_rota[[2]], ~nth(.$"datahora_rota", 100)) , 1, 10))
  dados_rota$datahora_I <-  ymd_hms(map_chr(dados_rota[[2]], ~first(.$"datahora_rota")))
  dados_rota$datahora_F <-  ymd_hms(map_chr(dados_rota[[2]], ~last(.$"datahora_rota")))
  dados_rota$KM <- round(map_dbl(dados_rota[[2]], ~sum(.$"dist_p_prox")), 1)
  dados_rota$T_BARCO <- seconds_to_period(map_dbl(dados_rota[[2]], ~sum(.$"tempo_p_prox", na.rm = TRUE)))
  dados_rota$arquivos_rota <- lista_arquivos_rota
  
  dados_rota <- dados_rota %>%
    select(1, 3:8, 2, 9)
  
  return(dados_rota)
}
