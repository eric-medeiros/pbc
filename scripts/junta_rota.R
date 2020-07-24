# Função para inserir rota no banco de dados
junta_rota <- function (list_xxx, dados_rotas) {
  library(dplyr)
  
  list_xxx$rota <- list_xxx$saidas[, c(as.factor(1), 2)] %>%
    left_join(dados_rotas, by = c("data" = "data_GPS"))
  
  invisible(list_xxx)
  
  return(list_xxx)
}
