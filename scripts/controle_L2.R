controle_L2 <- function(pasta_L2) {
  library(lubridate)
  library(readxl)
  library(stringr)
  library(dplyr)
  library(fontawesome)
  library(R.utils)
  
  atalho_controle <- paste(pasta_L2, "/01_CAMPO/02_EXCEL/controle_de_campo_atalho.lnk", sep = "")
  arquivo_controle <- readWindowsShortcut(atalho_controle)$pathname
  
  controle <- read_excel(arquivo_controle,
                         sheet = 1)
  
  controle$LINHA <-  str_c("L_",controle$Linha)
  controle$DATA <- ymd(controle$Data)
  controle$data <- ymd(controle$Data)
  controle$SAIDA <- str_pad(controle$Saida, 3, "left", "0")
  
  controle_L2 <- controle %>%
    filter(LINHA == "L_2") %>%
    select("SAIDA", "DATA", "data")
  
  # Para pegar todas variáveis nas pastas:
  
  pdf_scan_t <- list.files(paste(pasta_L2, "/01_CAMPO/01_SCAN", sep = ""), full.names = TRUE)
  pdf_scan_n <- list.files(paste(pasta_L2, "/01_CAMPO/01_SCAN", sep = ""), full.names = TRUE, pattern = "ini")
  pdf_scan_s <- setdiff(pdf_scan_t, pdf_scan_n)
  excel_t <- read_excel(paste(pasta_L2, "/01_CAMPO/02_EXCEL/comportamento_PBC.xlsx", sep = ""), sheet = 1)
  excel_s <- excel_t[c(1,2)]
  pastas_gps_t <- list.files(paste(pasta_L2, "/01_CAMPO/03_GPS", sep = ""), full.names = TRUE)
  pastas_gps_n <- list.files(paste(pasta_L2, "/01_CAMPO/03_GPS", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_gps_s <- setdiff(pastas_gps_t, pastas_gps_n)
  pastas_sonda_t <- list.files(paste(pasta_L2, "/01_CAMPO/04_SONDA", sep = ""), full.names = TRUE)
  pastas_sonda_n <- list.files(paste(pasta_L2, "/01_CAMPO/04_SONDA", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_sonda_s <- setdiff(pastas_sonda_t, pastas_sonda_n)
  pastas_evid_t <- list.files(paste(pasta_L2, "/01_CAMPO/05_EVIDENCIAS", sep = ""), full.names = TRUE)
  pastas_evid_n <- list.files(paste(pasta_L2, "/01_CAMPO/05_EVIDENCIAS", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_evid_s <- setdiff(pastas_evid_t, pastas_evid_n)
  
  # Para classificar as variáveis
  
  scan <- NULL
  scan$data <- ymd(str_sub(pdf_scan_s, -14, -5))
  scan$saida <- str_sub(pdf_scan_s, -23, -21)
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
  
  
  controle_L2$SCAN <- controle_L2$DATA %in% scan$data
  
  controle_L2 <- controle_L2 %>%
    left_join(scan, by = "data") %>%
    select(DIR_SCAN) %>%
    bind_cols(controle_L2)
  
  controle_L2$EXCEL <- controle_L2$DATA %in% excel$data
  
  controle_L2$GPS <- controle_L2$DATA %in% gps$data
  
  controle_L2 <- controle_L2 %>%
    left_join(gps, by = "data") %>%
    select(DIR_GPS) %>%
    bind_cols(controle_L2)
  
  
  controle_L2$SONDA <- controle_L2$DATA %in% sonda$data
  
  controle_L2 <- controle_L2 %>%
    left_join(sonda, by = "data") %>%
    select(DIR_SONDA) %>%
    bind_cols(controle_L2)
  
  
  controle_L2$EVID <- controle_L2$DATA %in% evid$data
  
  controle_L2 <- controle_L2 %>%
    left_join(evid, by = "data") %>%
    select(DIR_EVID) %>%
    bind_cols(controle_L2)
  
  controle_L2 <- controle_L2[c(5, 6, 8:12, 4:1)]
  
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
  
  controle_L2$SCAN <- sapply(controle_L2$SCAN, troca_VF)
  controle_L2$EXCEL <- sapply(controle_L2$EXCEL, troca_VF)
  controle_L2$GPS <- sapply(controle_L2$GPS, troca_VF)
  controle_L2$SONDA <-  sapply(controle_L2$SONDA, troca_VF)
  controle_L2$EVID <- sapply(controle_L2$EVID, troca_VF)
  
  return(controle_L2)
  
  invisible(controle_L2)
}