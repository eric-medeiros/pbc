controle_L3 <- function(pasta_L3) {
  library(lubridate)
  library(readxl)
  library(stringr)
  library(dplyr)
  library(fontawesome)
  library(R.utils)
  
  atalho_controle <- paste(pasta_L3, "/01_CAMPO/02_EXCEL/controle_de_campo_atalho.lnk", sep = "")
  arquivo_controle <- readWindowsShortcut(atalho_controle)$pathname
  
  controle <- read_excel(arquivo_controle,
                         sheet = 1)
  
  controle$LINHA <-  str_c("L_",controle$Linha)
  controle$DATA <- ymd(controle$Data)
  controle$data <- ymd(controle$Data)
  controle$SAIDA <- str_pad(controle$Saida, 3, "left", "0")
  
  controle_L3 <- controle %>%
    filter(LINHA == "L_3") %>%
    select("SAIDA", "DATA", "data")
  
  # Para pegar todas variáveis nas pastas:
  
  pastas_audios_t <- list.files(paste(pasta_L3, "/01_CAMPO/00_AUDIOS", sep = ""), full.names = TRUE)
  pastas_audios_n <- list.files(paste(pasta_L3, "/01_CAMPO/00_AUDIOS", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_audios_s <- setdiff(pastas_audios_t, pastas_audios_n)
  pdf_scan_t <- list.files(paste(pasta_L3, "/01_CAMPO/01_SCAN", sep = ""), full.names = TRUE)
  pdf_scan_n <- list.files(paste(pasta_L3, "/01_CAMPO/01_SCAN", sep = ""), full.names = TRUE, pattern = "ini")
  pdf_scan_s <- setdiff(pdf_scan_t, pdf_scan_n)
  excel_t <- read_excel(paste(pasta_L3, "/01_CAMPO/02_EXCEL/acustica_PBC.xlsx", sep = ""), sheet = 1)
  excel_s <- excel_t[c(1,2)]
  pastas_gps_t <- list.files(paste(pasta_L3, "/01_CAMPO/03_GPS", sep = ""), full.names = TRUE)
  pastas_gps_n <- list.files(paste(pasta_L3, "/01_CAMPO/03_GPS", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_gps_s <- setdiff(pastas_gps_t, pastas_gps_n)
  pastas_sonda_t <- list.files(paste(pasta_L3, "/01_CAMPO/04_SONDA", sep = ""), full.names = TRUE)
  pastas_sonda_n <- list.files(paste(pasta_L3, "/01_CAMPO/04_SONDA", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_sonda_s <- setdiff(pastas_sonda_t, pastas_sonda_n)
  pastas_evid_t <- list.files(paste(pasta_L3, "/01_CAMPO/05_EVIDENCIAS", sep = ""), full.names = TRUE)
  pastas_evid_n <- list.files(paste(pasta_L3, "/01_CAMPO/05_EVIDENCIAS", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_evid_s <- setdiff(pastas_evid_t, pastas_evid_n)
  
  
  audios <- NULL
  audios$data <- ymd(str_sub(pastas_audios_s, -10, -1))
  audios$saida <- str_sub(pastas_audios_s, -14, -12)
  audios$dir <- pastas_audios_s
  audios <- tibble("data" = audios[[1]], "saida" = audios[[2]], "DIR_AUDIOS" = audios[[3]])
  
  scan <- NULL
  scan$data <- ymd(str_sub(pdf_scan_s, -14, -5))
  scan$saida <- str_sub(pdf_scan_s, -18, -16)
  scan$dir <- pdf_scan_s
  scan <- tibble("data" = scan[[1]], "saida" = scan[[2]], "DIR_SCAN" = scan[[3]])
  
  excel <- NULL
  excel$data <- ymd(excel_s$data)
  excel$saida <- str_pad(excel_s$saida, side = "left", width = 3, "0")
  excel <- tibble("data" = excel[[1]], "saida" = excel[[2]])
  
  gps <- NULL
  gps$data <- ymd(str_sub(pastas_gps_s, -10, -1))
  gps$saida <- str_sub(pastas_gps_s, -14, -12)
  gps$dir <- pastas_gps_s
  gps <- tibble("data" = gps[[1]], "saida" = gps[[2]], "DIR_GPS" = gps[[3]])
  
  sonda <- NULL
  sonda$data <- ymd(str_sub(pastas_sonda_s, -10, -1))
  sonda$saida <- str_sub(pastas_sonda_s, -14, -12)
  sonda$dir <- pastas_sonda_s
  sonda <- tibble("data" = sonda[[1]], "saida" = sonda[[2]], "DIR_SONDA" = sonda[[3]])
  
  evid <- NULL
  evid$data <- ymd(str_sub(pastas_evid_s, -10, -1))  
  evid$saida <- str_sub(pastas_evid_s, -14, -12)
  evid$dir <- pastas_evid_s
  evid <- tibble("data" = evid[[1]], "saida" = evid[[2]], "DIR_EVID" = evid[[3]])
  
  
  controle_L3$AUDIOS <- controle_L3$DATA %in% audios$data
  
  controle_L3 <- controle_L3 %>%
    left_join(audios, by = c("data")) %>%
    select(!saida)
  
  
  controle_L3$SCAN <- controle_L3$DATA %in% scan$data
  
  controle_L3 <- controle_L3 %>%
    left_join(scan, by = "data") %>%
    select(!saida)
  
  controle_L3$EXCEL <- controle_L3$DATA %in% excel$data
  
  controle_L3$GPS <- controle_L3$DATA %in% gps$data
  
  controle_L3 <- controle_L3 %>%
    left_join(gps, by = "data") %>%
    select(!saida)
  
  controle_L3$SONDA <- controle_L3$DATA %in% sonda$data
  
  controle_L3 <- controle_L3 %>%
    left_join(sonda, by = "data") %>%
    select(!saida)
  
  
  controle_L3$EVID <- controle_L3$DATA %in% evid$data
  
  controle_L3 <- controle_L3 %>%
    left_join(evid, by = "data") %>%
    select(!saida)
  
  controle_L3 <- controle_L3[c(1, 2, 4, 6, 8, 9, 11, 13, 5, 7, 10, 12, 14)]
  
  
  link_check <- function() {
    fa("fas fa-check", fill = "steelblue")
  }
  
  link_alerta <- function() {
    fa("fas fa-exclamation-triangle", fill = "orange")
  }
  
  troca_VF <- function(VF) {
    
    ifelse(VF == TRUE,
           link_check(),
           link_alerta()
    )
  }
  
  controle_L3$AUDIOS <- sapply(controle_L3$AUDIOS, troca_VF)
  controle_L3$SCAN <- sapply(controle_L3$SCAN, troca_VF)
  controle_L3$EXCEL <- sapply(controle_L3$EXCEL, troca_VF)
  controle_L3$GPS <- sapply(controle_L3$GPS, troca_VF)
  controle_L3$SONDA <-  sapply(controle_L3$SONDA, troca_VF)
  controle_L3$EVID <- sapply(controle_L3$EVID, troca_VF)
  
  return(controle_L3)
  
  invisible(controle_L3)
}