# Função de leitura de arquivos múltiplos de WPs
le_WPs <- function(pasta_GPS) {
  library(data.table)
  library(purrr)
  library(abind)
  
  # Função de leitura de arquivo indivudual de *.gpx do tipo track
  le_WP <- function(arquivo_WP) {
    library(rgdal)
    library(dplyr)
    library(data.table)
    library(lubridate)
    
    WP <- readOGR(arquivo_WP, layer = "waypoints", verbose = FALSE)
    
    WP <- bind_cols(c(WP@data[, c(2, 5)], data.table(WP@coords)))
    
    names(WP) = c("datahora_WP", "WP_WP", "lng_WP", "lat_WP")
    
    WP$datahora_WP <- ymd_hms(WP$datahora_WP, tz = "America/Sao_Paulo")
    
    WP <- data.table(WP)
    
    return(WP)
  }
  
  # Listagem dos arquivos *.gpx da pasta_GPS
  lista_arquivos_WP <- list.files (pasta_GPS,
                                   recursive = TRUE,
                                   full.names = TRUE,
                                   pattern = "Pontos")
  
  # Aplicando a função le_WP para todos arquivos da lista lista_arquivos_WP
  dados_WP <- map(lista_arquivos_WP, le_WP) %>%
    abind(along = 1)
  
  dados_WP <- data.table(dados_WP)
  
  return(dados_WP)
}
