# Função para inserir sonda no banco de dados
junta_sonda_pop <- function (list_pop, dados_sonda) {
  library(dplyr)
  
list_pop$sonda <- list_pop$saidas[, c(as.factor(1), 2)] %>%
  left_join(dados_sonda, by = c("data" = "data_sonda"))
  
  invisible(list_pop)
  
  return(list_pop)
}