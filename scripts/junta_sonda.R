# Função para inserir sonda no banco de dados
junta_sonda <- function (list_xxx, dados_sonda) {
  library(dplyr)
  
  list_xxx$sonda <- list_xxx$saidas[, c(as.factor(1), 2)] %>%
    left_join(dados_sonda, by = c("data" = "data_sonda"))
  
  invisible(list_xxx)
  
  return(list_xxx)
}