# Função para inserir rota no banco de dados
junta_rota_pop <- function (list_pop, dados_rotas) {
  library(dplyr)
  
  list_pop$rota <- list_pop$saidas[, c(as.factor(1), 2)] %>%
    left_join(dados_rotas, by = c("data" = "data_GPS"))
  
  invisible(list_pop)
  
  return(list_pop)
}

