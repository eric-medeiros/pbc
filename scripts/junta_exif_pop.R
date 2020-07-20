# Função para inserir exif no banco de dados
junta_exif_pop <- function (list_pop, dados_fotos) {
  library(dbplyr)
  library(stringr)

  list_pop$fotos$data_arquivo_foto <- str_c(as.character(list_pop$fotos$datahora_foto),"-", list_pop$fotos$arquivo_foto)
  
  dados_fotos$data_arquivo_exif <- str_c((as.character(as.Date(dados_fotos$datahora_foto))),"-", dados_fotos$arquivo_foto)
  
  # Pegando somente as infos novas
  fotos_novas <- !dados_fotos$arquivo_foto %in% list_pop$fotos$arquivo_foto
  
  anexo <- dados_fotos[fotos_novas,] %>%
    left_join(list_pop$fotos, by = c("data_arquivo_exif" = "data_arquivo_foto"), suffix = c("",".y"))
  
  anexo <- anexo[c(7, 1, 9, 10, 11, 12, 13, 2, 3, 16, 4, 5)]
  
  list_pop$fotos <- list_pop$fotos[,-13]
  
  list_pop$fotos <- bind_rows(list_pop$fotos, anexo)
  
  invisible (list_pop)
  
  return(list_pop)
}
