# função para leitura dos EXIF nas fotos

le_fotos_L1 <- function (pasta_L1) {
library(stringr)
library(exifr)
library(dplyr)
library(lubridate)

pasta_fotos <- paste0(pasta_L1,"/03_ANALISES/01_HIST_ID")

pastas_IDs <- list.dirs(pasta_fotos, recursive = FALSE)

IDs <- tibble("ID" = str_sub(pastas_IDs, -5, -1), "caminho" = "")

for (i in 1:length(pastas_IDs)) {
  
  arq_novos <- tibble("ID" = str_sub(pastas_IDs[[i]], -5, -1),
                      "caminho" = list.files(pastas_IDs[[i]],".JPG", full.names = TRUE))
  
  IDs <- IDs %>%
    full_join (arq_novos, by = "ID")
  
  vec <- vector(mode = "character", length = nrow(IDs))
  
  vec[!is.na(IDs$caminho.y)] <- IDs$caminho.y[!is.na(IDs$caminho.y)]
  vec[IDs$caminho.x != ""] <- IDs$caminho.x[IDs$caminho.x != ""]
  
  IDs$caminho <- vec
  IDs$caminho.x <- NULL
  IDs$caminho.y <- NULL
  IDs$arquivo_renome <- str_sub(IDs$caminho, 62, -5)
  IDs$arquivo <- str_replace(IDs$arquivo_renome, " [a-z]$", "")

}

info <- read_exif(IDs$caminho)

esa <- str_subset(unlist(str_split(IDs$arquivo, " ")), "E")

IDs$exp <- str_replace(str_subset(unlist(str_split(esa, "S")), "E"), "E", "")
IDs$grupo <- str_subset(unlist(str_split(esa, "A")), "E", negate = TRUE)


IDs$lng <- info$GPSLongitude
IDs$lat <- info$GPSLatitude
IDs$datahora <- ymd_hms(info$CreateDate)


IDs <- IDs %>%
  dplyr::select(5,6,1,9,7,8,3,2,4)


invisible(IDs)

return(IDs)
}