# Função para inserir sonda no banco de dados
junta_sonda_L <- function (lista_L, dados_sonda) {
  library(dplyr)
  
  lista_L$sonda <- dados_sonda
  
  invisible(lista_L)
  
  return(lista_L)
}