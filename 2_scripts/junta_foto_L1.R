# Função para inserir dados do raven no banco de dados
junta_foto_L1 <- function(lista_L1, dados_fotos){
  library(dplyr)
  library(tidyr)
  

  lista_L1$identificacoes <- lista_L1$amostragens %>%
    dplyr::select(exp, saida) %>%
    right_join(lista_L1$identificacoes, by = "saida") %>%
    right_join(dados_fotos, by = c("exp", "grupo", "ID", "arquivo")) %>%
    dplyr::select(1,2,4,13:15,5,7:11,6,16,17,12)
  
  invisible(lista_L1)
  
  return(lista_L1)
}

