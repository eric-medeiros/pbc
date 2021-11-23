# Função para juntar dados dos audios no banco de dados
junta_audios_L3 <- function(lista_L3, dados_audios)
{
  library(dplyr)
  library(tidyr)
  
  lista_L3$gravacoes <- dados_audios %>%
    left_join(lista_L3$gravacoes, by = c("saida", "data", "arquivo_wav")) %>%
    ungroup() %>%
    dplyr::select(2,3,6,7,4,5,8,9)

  invisible(lista_L3)
  
  return(lista_L3)
}
