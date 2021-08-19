# Funcao geral de leitura de arquivos múltiplos de GPS 

le_rota_L <- function(pasta_L) {
  library(purrr)
  library(abind)
  library(data.table)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(stringr)
  library(dplyr)

  # Funcao de leitura de arquivo indivudual de *.gpx do tipo track
  le_rota_arquivo <- function(arquivo_rota) {
    library(rgdal)
    library(dplyr)
    library(lubridate)
    library(sp)
    
    rota_pontos <- readOGR(arquivo_rota, layer = "track_points", verbose = FALSE)
    rota_pontos@data$time <- ymd_hms(rota_pontos@data$time)
    
    rota_pontos_dt <- data.table(rota_pontos@data$time, rota_pontos@coords)
    names(rota_pontos_dt) <- c("datahora_ROTA", "lng", "lat")
    
    rota_pontos_dt$dist_p_prox <- c(spDists(rota_pontos_dt[, c(2, 3)], longlat = TRUE, segments = TRUE), 0)
    
    rota_pontos_dt$datahora_prox <- lead(rota_pontos_dt$datahora_ROTA)
    
    rota_pontos_dt$tempo_p_prox <- rota_pontos_dt$datahora_prox - rota_pontos_dt$datahora_ROTA
    
    rota_pontos_dt$datahora_prox <- NULL
    
    rota_pontos_dt$datahora_ROTA <- as.character.Date(rota_pontos_dt$datahora_ROTA)
    
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
  
  dados_rota$saida <- as.character(as.numeric(str_sub(lista_arquivos_rota, -45, -43)))
  dados_rota$data_rota <- ymd(str_sub(map_chr(dados_rota[[2]], ~nth(.$"datahora_ROTA", 100)) , 1, 10))
  dados_rota$arquivos_rota <- lista_arquivos_rota
  
  dados_rota <- dados_rota %>%
    unnest(cols = c(data)) %>%
    dplyr::select(1,7,2:6,8,9)
  
  return(dados_rota)
}
