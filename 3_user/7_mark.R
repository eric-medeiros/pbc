
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(RMark)

# Histórico de capturas

pasta_proj <- rprojroot::find_rstudio_root_file()

nova_pasta <- paste0(pasta_proj, "/4_export/7_mark")
dir.create(nova_pasta)
setwd(nova_pasta)


bd_L1 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L1.rds"))

# Tirar um theta para cada grupo

theta <- bd_L1$avistagens$n_marcados/bd_L1$avistagens$tam_grupo

# Fazendo a média do theta

theta_m <- mean(theta, na.rm=TRUE)

# Fazendo um desvio para theta

# Corrigindo lcl e ucl


historico_dia <- bd_L1$identificacoes %>%
  group_by(ID) %>%
  mutate(data = date(datahora),
         avis = 1L) %>%
  dplyr::select(ID, data, avis) %>%
  na.omit()


mark_dia <- historico_dia %>%
  pivot_wider(names_from = data,
              values_from = avis,
              values_fn = list(avis = mean),
              values_fill = 0) %>%
  unite("ch", 2:tail(names(.),1), sep = "") %>%
  dplyr::select(2,1)

dp_dia <- process.data(mark_dia, model = "Closed")

result <- mark(dp_dia)

# Corrigindo estimativa
result$results$derived$`N Population Size`$estimate / theta_m





# Limpando arquivo velhos e voltando o working directory
cleanup(ask = FALSE)

setwd(pasta_proj)
