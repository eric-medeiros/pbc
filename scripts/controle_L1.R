controle_L1 <- function(pasta_L1) {
  library(lubridate)
  library(readxl)
  library(stringr)
  library(dplyr)
  library(fontawesome)
  library(R.utils)
  
  atalho_controle <- paste(pasta_L1, "/01_CAMPO/02_EXCEL/controle_de_campo_atalho.lnk", sep = "")
  arquivo_controle <- readWindowsShortcut(atalho_controle)$pathname
  
  controle <- read_excel(arquivo_controle,
                         sheet = 1)
  
  controle$LINHA <-  str_c("L_",controle$Linha)
  controle$DATA <- ymd(controle$Data)
  controle$data <- ymd(controle$Data)
  controle$SAIDA <- str_pad(controle$Saida, 3, "left", "0")
  
  controle_L1 <- controle %>%
    filter(LINHA == "L_1") %>%
    select("SAIDA", "DATA", "data")
  
  # Para pegar todas variáveis nas pastas:
  
  pastas_fotos_t <- list.files(paste(pasta_L1, "/01_CAMPO/00_FOTOS", sep = ""), full.names = TRUE)
  pastas_fotos_n <- list.files(paste(pasta_L1, "/01_CAMPO/00_FOTOS", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_fotos_s <- setdiff(pastas_fotos_t, pastas_fotos_n)
  pdf_scan_t <- list.files(paste(pasta_L1, "/01_CAMPO/01_SCAN", sep = ""), full.names = TRUE)
  pdf_scan_n <- list.files(paste(pasta_L1, "/01_CAMPO/01_SCAN", sep = ""), full.names = TRUE, pattern = "ini")
  pdf_scan_s <- setdiff(pdf_scan_t, pdf_scan_n)
  excel_t <- read_excel(paste(pasta_L1, "/01_CAMPO/02_EXCEL/populacional_PBC.xlsx", sep = ""), sheet = 1)
  excel_s <- excel_t[c(1,2)]
  pastas_gps_t <- list.files(paste(pasta_L1, "/01_CAMPO/03_GPS", sep = ""), full.names = TRUE)
  pastas_gps_n <- list.files(paste(pasta_L1, "/01_CAMPO/03_GPS", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_gps_s <- setdiff(pastas_gps_t, pastas_gps_n)
  pastas_sonda_t <- list.files(paste(pasta_L1, "/01_CAMPO/04_SONDA", sep = ""), full.names = TRUE)
  pastas_sonda_n <- list.files(paste(pasta_L1, "/01_CAMPO/04_SONDA", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_sonda_s <- setdiff(pastas_sonda_t, pastas_sonda_n)
  pastas_evid_t <- list.files(paste(pasta_L1, "/01_CAMPO/05_EVIDENCIAS", sep = ""), full.names = TRUE)
  pastas_evid_n <- list.files(paste(pasta_L1, "/01_CAMPO/05_EVIDENCIAS", sep = ""), full.names = TRUE, pattern = "ini")
  pastas_evid_s <- setdiff(pastas_evid_t, pastas_evid_n)
  
  # Para classificar as variáveis
  
  fotos <- NULL
  fotos$data <- ymd(str_sub(pastas_fotos_s, -10, -1))
  fotos$saida <- str_pad(row.names(data.frame(fotos[[1]])), pad = "0", width = 3)
  fotos$dir <- pastas_fotos_s
  fotos <- tibble("data" = fotos[[1]], "SAIDA" = fotos[[2]], "DIR_FOTOS" = fotos[[3]])
  
  scan <- NULL
  scan$data <- ymd(str_sub(pdf_scan_s, 90, 99))
  scan$saida <- str_sub(pdf_scan_s, 86, 88)
  scan$dir <- pdf_scan_s
  scan <- tibble("data" = scan[[1]], "SAIDA" = scan[[2]], "DIR_SCAN" = scan[[3]])
  
  excel <- NULL
  excel$data <- ymd(excel_s$data)
  excel$saida <- str_pad(excel_s$saida, side = "left", width = 3, "0")
  excel <- tibble("data" = excel[[1]], "SAIDA" = excel[[2]])
  
  gps <- NULL
  gps$data <- ymd(str_sub(pastas_gps_s, -10, -1))
  gps$saida <- str_sub(pastas_gps_s, -14, -12)
  gps$dir <- pastas_gps_s
  gps <- tibble("data" = gps[[1]], "SAIDA" = gps[[2]], "DIR_GPS" = gps[[3]])
  
  sonda <- NULL
  sonda$data <- ymd(str_sub(pastas_sonda_s, -10, -1))
  sonda$saida <- str_sub(pastas_sonda_s, -14, -12)
  sonda$dir <- pastas_sonda_s
  sonda <- tibble("data" = sonda[[1]], "SAIDA" = sonda[[2]], "DIR_SONDA" = sonda[[3]])
  
  evid <- NULL
  evid$data <- ymd(str_sub(pastas_evid_s, -10, -1))  
  evid$saida <- str_sub(pastas_evid_s, -14, -12)
  evid$dir <- pastas_evid_s
  evid <- tibble("data" = evid[[1]], "SAIDA" = evid[[2]], "DIR_EVID" = evid[[3]])
  
  
  controle_L1$FOTOS <- controle_L1$SAIDA %in% fotos$SAIDA
  
  controle_L1 <- controle_L1 %>%
    left_join(fotos, by = "SAIDA") %>%
    select(DIR_FOTOS) %>%
    bind_cols(controle_L1)
  
  
  controle_L1$SCAN <- controle_L1$SAIDA %in% scan$SAIDA
  
  controle_L1 <- controle_L1 %>%
    left_join(scan, by = "SAIDA") %>%
    select(DIR_SCAN) %>%
    bind_cols(controle_L1)
  
  
  controle_L1$EXCEL <- controle_L1$SAIDA %in% excel$SAIDA
  
  
  controle_L1$GPS <- controle_L1$SAIDA %in% gps$SAIDA
  
  controle_L1 <- controle_L1 %>%
    left_join(gps, by = "SAIDA") %>%
    select(DIR_GPS) %>%
    bind_cols(controle_L1)
  
  
  controle_L1$SONDA <- controle_L1$SAIDA %in% sonda$SAIDA 
  
  controle_L1 <- controle_L1 %>%
    left_join(sonda, by = "SAIDA") %>%
    select(DIR_SONDA) %>%
    bind_cols(controle_L1)
  
  
  controle_L1$EVID <- controle_L1$SAIDA %in% evid$SAIDA
  
  controle_L1 <- controle_L1 %>%
    left_join(evid, by = "SAIDA") %>%
    select(DIR_EVID) %>%
    bind_cols(controle_L1)
  
  controle_L1 <- controle_L1[c(6, 7, 9:14, 5:1)]
  
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
  
  controle_L1$FOTOS <- sapply(controle_L1$FOTOS, troca_VF)
  controle_L1$SCAN <- sapply(controle_L1$SCAN, troca_VF)
  controle_L1$EXCEL <- sapply(controle_L1$EXCEL, troca_VF)
  controle_L1$GPS <- sapply(controle_L1$GPS, troca_VF)
  controle_L1$SONDA <-  sapply(controle_L1$SONDA, troca_VF)
  controle_L1$EVID <- sapply(controle_L1$EVID, troca_VF)
  
  return(controle_L1)
  
  invisible(controle_L1)
}