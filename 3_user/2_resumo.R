# Resumão de relatório

# Limpando o Global Environment 
rm(list = ls())

library(magrittr)
library(dplyr)
library(purrr)
library(lubridate)
library(data.table)

# Linha_1 ----

pasta_proj <- rprojroot::find_rstudio_root_file()

bd_L1 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L1.rds"))

rel_1 <- bd_L1$avistagens %>%
  mutate(tempo_grupo = bd_L1$avistagens$datahora_F - bd_L1$avistagens$datahora_I,
         tam_grupo = nafill(tam_grupo, fill = 0L),
         tam_min = nafill(tam_min, fill = 0L),
         tam_max = nafill(tam_max, fill = 0L)) %>%
  rowwise() %>%
  mutate(tam_est = sum(tam_grupo, mean(c(tam_min, tam_max)))) %>%
  ungroup() %>%
  dplyr::select(1:4, 25, 26) %>%
  group_by(saida) %>%
  summarise(.groups = "keep",
            SAIDA = first(saida),
            DATA = first(data),
            BOTOS = round(sum(tam_est), 0),
            T_BOTO = seconds_to_period(sum(tempo_grupo, na.rm = TRUE)),
            GRUPOS = n(),
            FOTOS = sum(num_fotos, na.rm = TRUE)) %>%
  right_join(bd_L1$saidas, by = "saida") %>%
  dplyr::select(1:7)

rel_1 <- bd_L1$rota %>%
  group_by(saida) %>%
  summarise(.groups = "keep",
            KM = round(sum(dist_p_prox, na.rm = TRUE),1),
            T_BARCO = seconds_to_period(sum(tempo_p_prox, na.rm = TRUE))) %>%
  right_join(rel_1, by = "saida") %>%
  dplyr::select(4:9, 2, 3)

rel_1$DATA <- as.character.Date(rel_1$DATA)

rel_1$T_BOTO <-  paste0(lubridate::hour(rel_1$T_BOTO), ":",
                        lubridate::minute(rel_1$T_BOTO),":",
                        lubridate::second(rel_1$T_BOTO))

rel_1$T_BARCO <-  paste0(lubridate::hour(rel_1$T_BARCO), ":",
                         lubridate::minute(rel_1$T_BARCO),":",
                         lubridate::second(rel_1$T_BARCO))

# Definindo caminho para o arquivo TXT
caminho_rel_1 <-  paste0(pasta_proj, "/4_export/2_resumo/rel_1.txt")

# Salvando TXT - Confira na pasta /4_export/2_resumo
write.table(x = rel_1,
            file = caminho_rel_1,
            sep = "\t",
            row.names = FALSE)
          


# Linha 2 ----

pasta_proj <- rprojroot::find_rstudio_root_file()

bd_L2 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L2.rds"))

rel_2 <- bd_L2$avistagens %>%
  mutate(tempo_grupo = bd_L2$avistagens$datahora_F - bd_L2$avistagens$datahora_I,
         tam_grupo = nafill(tam_grupo, fill = 0L),
         tam_min = nafill(tam_min, fill = 0L),
         tam_max = nafill(tam_max, fill = 0L)) %>%
  rowwise() %>%
  mutate(tam_est = round(sum(tam_grupo, mean(c(tam_min, tam_max))),0)) %>%
  ungroup() %>%
  dplyr::select(1:3, 25, 26) %>%
  group_by(saida) %>%
  summarise(.groups = "keep",
            SAIDA = first(saida),
            DATA = first(data),
            BOTOS = round(sum(tam_est), 0),
            T_BOTO = round(as.period(sum(tempo_grupo, na.rm = TRUE)),0),
            GRUPOS = n()) %>%
  right_join(bd_L2$saidas, by = "saida") %>%
  dplyr::select(1:6)

rel_2 <- bd_L2$comportamento %>%
  rowwise() %>%
  mutate(comp_sem_barco = sum(na, cre, esc, cond, rvz, int, bar, na.rm = TRUE)) %>%
  group_by(saida, grupo) %>%
  left_join(bd_L2$embarcacoes %>%
              group_by(saida, grupo) %>%
              transmute(comp_com_barco = sum (mud_dir, mud_coe_af, mud_tam_gru, mud_comp_gru, na.rm = TRUE)),
            by = c("saida", "grupo")) %>%
  group_by(saida) %>%
  summarise(.groups = "keep", COMPS = sum(comp_sem_barco, comp_com_barco, na.rm = TRUE)) %>%
  right_join(rel_2, by = "saida") %>%
  dplyr::select(1, 3:7, 2)

rel_2 <- bd_L2$rota[c(2, 6, 7)] %>%
  group_by(saida) %>%
  summarise(.groups = "keep",
            T_BARCO = as.period(sum(tempo_p_prox, na.rm = TRUE)),
            KM = round(sum(dist_p_prox, na.rm = TRUE),1)) %>%
  left_join(rel_2, by = "saida") %>%
  dplyr::select(4:9, 2, 3)

rel_2$DATA <- as.character.Date(rel_2$DATA)

rel_2$T_BOTO <-  paste0(lubridate::hour(rel_2$T_BOTO), ":",
                        lubridate::minute(rel_2$T_BOTO), ":",
                        lubridate::second(rel_2$T_BOTO))

rel_2$T_BARCO <-  paste0(lubridate::hour(rel_2$T_BARCO), ":",
                         lubridate::minute(rel_2$T_BARCO),":",
                         lubridate::second(rel_2$T_BARCO))

# Definindo caminho para o arquivo TXT
caminho_rel_2 <-  paste0(pasta_proj, "/4_export/2_resumo/rel_2.txt")

# Salvando TXT - Confira na pasta results/RDS
write.table(x = rel_2,
            file = caminho_rel_2,
            sep = "\t",
            row.names = FALSE)

# Linha 3 ----

pasta_proj <- rprojroot::find_rstudio_root_file()

bd_L3 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L3.rds"))

rel_3 <- bd_L3$gravacoes %>%
  group_by(saida) %>%
  summarise(SAIDA = first(saida),
            DATA = first(data),
            RECS = n(),
            T_REC = round(seconds_to_period(sum(duracao_s)), 0)) 

rel_3 <- bd_L3$assobios %>%
  group_by(saida) %>%
  summarise(ASSOBIOS = n()) %>%
  right_join(rel_3, by = "saida") %>%
  mutate(T_ANA = period("NA")) %>%
  dplyr::select(1,3:7, 2)

for (i in 1:nrow(rel_3)) {
  
  if(!is.na(rel_3$ASSOBIOS[[i]])) {
    rel_3$T_ANA[[i]] <- rel_3$T_REC[[i]] 
  }
  else{
    rel_3$T_ANA[[i]] <- as.period(rel_3$ASSOBIOS[[i]]) 
  }
  
}

rel_3 <- bd_L3$rota[c(2, 6, 7)] %>%
  group_by(saida) %>%
  summarise(T_BARCO = as.period(sum(tempo_p_prox, na.rm = TRUE)),
            KM = round(sum(dist_p_prox, na.rm = TRUE),1)) %>%
  right_join(rel_3, by = "saida") %>%
  dplyr::select(1, 4:9, 2, 3)

rel_3$DATA <- as.character.Date(rel_3$DATA)

rel_3$T_REC <-  paste0(lubridate::hour(rel_3$T_REC), ":",
                       lubridate::minute(rel_3$T_REC), ":",
                       lubridate::second(rel_3$T_REC))

rel_3$T_BARCO <-  paste0(lubridate::hour(rel_3$T_BARCO), ":",
                         lubridate::minute(rel_3$T_BARCO), ":",
                         lubridate::second(rel_3$T_BARCO))

rel_3$T_ANA <-  paste0(lubridate::hour(rel_3$T_ANA), ":",
                       lubridate::minute(rel_3$T_ANA), ":",
                       lubridate::second(rel_3$T_ANA))

# Definindo caminho para o arquivo TXT
caminho_rel_3 <-  paste0(pasta_proj, "/4_export/2_resumo/rel_3.txt")

# Salvando TXT - Confira na pasta results/RDS
write.table(x = rel_3,
            file = caminho_rel_3,
            sep = "\t",
            row.names = FALSE)




