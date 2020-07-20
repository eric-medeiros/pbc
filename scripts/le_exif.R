le_exif <- function(pasta_CAT) {
  library(exifr)
  library(lubridate)
  
  dados_fotos <- read_exif(pasta_CAT, 
                          recursive = TRUE, 
                          tags = c("GPS Latitude",
                                   "GPS Longitude",
                                   "FileName",
                                   "SourceFile",
                                   "DateTimeOriginal")
  )
  names(dados_fotos) <-  c("caminho_foto", "lat_foto", "lng_foto", "arquivo_foto", "datahora_foto")
  dados_fotos$datahora_foto <- ymd_hms(dados_fotos$datahora_foto)
  
  dados_fotos <- dados_fotos[c(5, 3, 2, 4, 1)]
  
  return(dados_fotos)
}
