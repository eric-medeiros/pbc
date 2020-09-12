le_exif <- function(pasta_L1) {
  library(exifr)
  library(lubridate)
  
  dados_fotos <- read_exif(paste(pasta_L1, "/03_ANALISES", sep = ""), 
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
  
  invisible(dados_fotos)
  
  return(dados_fotos)
}
