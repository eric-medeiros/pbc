# Função para inserir rota no banco de dados
junta_rota_L <- function (lista_L, dados_rotas) {
  library(dplyr)
 
  # Juntando a rota toda  
  lista_L$rotas <- dados_rotas 
  
  invisible(lista_L)
  
  return(lista_L)
}
