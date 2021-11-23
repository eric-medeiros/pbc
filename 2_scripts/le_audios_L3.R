# função para leitura dos dados nos arquivos de aúdio

le_audios_L3 <- function (pasta_L3) {
  library(purrr)

  
  le_arquivo_audio <- function (arquivo_audio) {
    library(tuneR)
    library(lubridate)
    library(stringr)
    library(dplyr)
    
    saida <- as.character(as.integer(str_sub(arquivo_audio,-30, -28)))
    
    data <- ymd(str_sub(arquivo_audio, -26, -17))
    
    arquivo_wav <- str_sub(arquivo_audio,-15, -5)
    
    dados_arquivo_wav <- readWave(arquivo_audio, header = TRUE)
    
    duracao_s <- round(dados_arquivo_wav$samples/dados_arquivo_wav$sample.rate, 1)
    
    dados_audio <- tibble(saida, data, arquivo_wav, duracao_s)
    
    return(dados_audio)
    
  }
  
  lista_audios <- list.files(paste0(pasta_L3,"/01_CAMPO/00_AUDIOS"),
                             recursive = TRUE,
                             pattern = "wav",
                             full.names = TRUE)
 
  dados_audios <- lista_audios %>%
    map_dfr(le_arquivo_audio, .id = "registro_AUDIO") %>%
    group_by(registro_AUDIO) %>% 
    na.omit()
  
  invisible(dados_audios)
  
  return(dados_audios)

}
