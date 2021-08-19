# Função de leitura de arquivos múltiplos de WPs
le_WPs_L <- function(pasta_L) {
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
    
    WP$saida <- as.character(as.numeric(str_sub(arquivo_WP, -47L, -45L)))
    
    names(WP) = c("datahora", "WP", "lng", "lat", "saida")
    
    WP <- WP %>%
      dplyr::select(c(5,1:4))
    
    return(WP)
  }
  
  # Listagem dos arquivos *.gpx da pasta_GPS
  lista_arquivos_WP <- list.files (paste(pasta_L, "/01_CAMPO/03_GPS", sep = ""),
                                   recursive = TRUE,
                                   full.names = TRUE,
                                   pattern = "Pontos")
  
  # Aplicando a função le_WP para todos arquivos da lista lista_arquivos_WP
  dados_WP <- map_dfr(lista_arquivos_WP, le_WP, .id="registro_PONTOS") %>%
    mutate(datahora = ymd_hms(datahora))
    
  invisible(dados_WP)
  
  return(dados_WP)
}
