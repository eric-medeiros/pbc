# Função para inserir exif no banco de dados
junta_exif_pop <- function (bd_L1, dados_fotos) {
  library(dbplyr)
  library(stringr)

  bd_L1$fotos$data_arquivo_foto <- str_c(as.character(bd_L1$fotos$datahora_foto),"-", bd_L1$fotos$arquivo_foto)
  
  dados_fotos$data_arquivo_exif <- str_c((as.character(as.Date(dados_fotos$datahora_foto))),"-", dados_fotos$arquivo_foto)
  
  # Pegando somente as infos novas
  fotos_novas <- !dados_fotos$arquivo_foto %in% bd_L1$fotos$arquivo_foto
  
  anexo <- dados_fotos[fotos_novas,] %>%
    left_join(bd_L1$fotos, by = c("data_arquivo_exif" = "data_arquivo_foto"), suffix = c("",".y"))
  
  anexo <- anexo[c(7, 1, 9, 10, 11, 12, 13, 2, 3, 16, 4, 5)]
  
  bd_L1$fotos <- bd_L1$fotos[,-13]
  
  bd_L1$fotos <- bind_rows(bd_L1$fotos, anexo)
  
  invisible (bd_L1)
  
  return(bd_L1)
}
