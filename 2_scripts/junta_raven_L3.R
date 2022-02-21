# Função para inserir dados do raven no banco de dados
junta_raven_L3 <- function(lista_L3, dados_raven)
{
  library(dplyr)
  library(tidyr)
  
  lista_L3$assobios <-  tibble(dados_raven) %>%
    group_by(saida, arquivo_wav) %>%
    nest () %>%
    left_join(lista_L3$gravacoes[,c(1,3,4,5)], by = c("saida", "arquivo_wav")) %>%
    unnest (cols = c(data)) %>%
    dplyr::select(saida, data, estacao, area, arquivo_wav, assobios, canal,
           BT, ET, LF, HF, MF, CF, APD, DT, DF, PF, PT, FI, FF, PF_check,
           modulacao,	PI,	sobreposicao, analisado,	OBS)

  invisible(lista_L3)
  
  return(lista_L3)
}
