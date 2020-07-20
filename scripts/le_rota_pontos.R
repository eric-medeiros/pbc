# Função geral de leitura de arquivos múltiplos de GPS 

le_rota_pontos <- function(pasta_GPS) {
  library(purrr)
  library(abind)
  library(data.table)
  library(lubridate)
  library(tidyr)
  
  # Função de leitura de arquivo indivudual de *.gpx do tipo track
  le_rota_pontos_arquivo <- function(arquivo_track) {
    library(rgdal)
    library(lubridate)
    library(sp)
    
    rota_pontos <- readOGR(arquivo_track, layer = "track_points", verbose = FALSE)
    rota_pontos@data$time <- ymd_hms(rota_pontos@data$time, tz = "America/Sao_Paulo")
    
    rota_pontos_dt <- data.table(rota_pontos@data$time, rota_pontos@coords)
    names(rota_pontos_dt)[1] <- "datahora_GPS"
    names(rota_pontos_dt)[2] <- "lng"
    names(rota_pontos_dt)[3] <- "lat"
    
    rota_pontos_dt$datahora_GPS <- as.character.Date(rota_pontos_dt$datahora_GPS)
    
    rota_pontos_dt$dist_p_prox <- c(spDists(rota_pontos_dt[, c(2, 3)], longlat = TRUE, segments = TRUE), 0)
    
    rota_pontos_dt$datahora_prox <- lead(rota_pontos_dt$datahora_GPS, default = 0)
    
    rota_pontos_dt$tempo_p_prox <- difftime(ymd_hms(rota_pontos_dt$datahora_prox),
                                            ymd_hms(rota_pontos_dt$datahora_GPS),
                                            units = "hours")
    rota_pontos_dt$datahora_prox <- NULL
    
    return(rota_pontos_dt)
  }
  
  # Listagem dos arquivos dentro da pasta_GPS
  lista_arquivos_GPS <- list.files(pasta_GPS,
                                   full.names = TRUE,
                                   pattern = "\\d{6}",
                                   recursive = TRUE)
  
  # Aplicando a função le_rota_pontos_arquivo para cada arquivo na lista de arquivos da pasta_GPS
  
  dados_track <- map_dfr(lista_arquivos_GPS, le_rota_pontos_arquivo, .id="registro_TRAJETO") %>%
    group_by(registro_TRAJETO) %>%
    nest()
  
  lista_datas <- map(dados_track[[2]], ~as.character.Date(median(as.Date(.$"datahora_GPS"))))
  dados_track$data_GPS <- ymd(abind(lista_datas))
  
  dados_track$datahora_I_track <-  ymd_hms(map_chr(dados_track[[2]], ~first(.$"datahora_GPS")))
  dados_track$datahora_F_track <-  ymd_hms(map_chr(dados_track[[2]], ~last(.$"datahora_GPS")))
  dados_track$dist_total <- map_dbl(dados_track[[2]], ~sum(.$"dist_p_prox"))
  dados_track$tempo_total <- map_dbl(dados_track[[2]], ~sum(.$"tempo_p_prox", na.rm = TRUE))
  
  dados_track <- dados_track %>%
    select(1,3,4,5,6,7,2)
  
  return(dados_track)
}
