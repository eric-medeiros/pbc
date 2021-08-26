# Scripts de como gerar os RDS

## Linha 1 ----

# Pegando a pasta do projeto - vai ser diferente em cada PC, mas ele acha sozinho
pasta_proj <- rprojroot::find_rstudio_root_file()

# Inserindo a continuação do caminho para a função bd_L1, que será sempre igual pois está dentro da estrutura do Projeto
caminho_func_1 <- paste0(pasta_proj, "/2_scripts/bd_L1.R")

# Importando as funções!!
source(caminho_func_1)

# Especificando os caminhos - Aqui são os dados de exemplo que estão no github
# pasta_L1 <- paste(pasta_proj, "/1_data/LINHA_1", sep = "")

# fazendo nas pastas na base
pasta_L1 <- "C:/Users/PBC-PESQUISA/Documents/PROJETO BOTO-CINZA/PESQUISA/LINHA_1"

# Executando - Pode mudar a pasta, mas só com estrutura interna da linha 1 
bd_L1 <- bd_L1(pasta_L1)

# Definindo caminho para o arquivo RDS
caminho_RDS_1 <-  paste0(pasta_proj, "/4_export/1_banco/bd_L1.rds")

# Salvando RDS - Confira na pasta results/RDS
saveRDS(object = bd_L1, file = caminho_RDS_1)

install.packages(, dependencies - TRUE)

## Linha 2 ----

# Pegando a pasta do projeto - vai ser diferente em cada PC, mas ele acha sozinho
pasta_proj <- rprojroot::find_rstudio_root_file()

# Inserindo a continuação do caminho para a função bd_L2, que será sempre igual pois está dentro da estrutura do Projeto
caminho_func_2 <- paste0(pasta_proj, "/2_scripts/bd_L2.R")

# Importando as funções!!
source(caminho_func_2)

# Especificando os caminhos - Aqui são os dados de exemplo que estão no github
# pasta_L2 <- paste(pasta_proj, "/1_data/LINHA_2", sep = "")

# fazendo nas pastas na base
pasta_L2 <- "C:/Users/PBC-PESQUISA/Documents/PROJETO BOTO-CINZA/PESQUISA/LINHA_2"

# Executando - Pode mudar a pasta, mas só com estrutura interna da linha 2 
bd_L2 <- bd_L2(pasta_L2)

# Definindo caminho para o arquivo RDS
caminho_RDS_2 <-  paste0(pasta_proj, "/4_export/1_banco/bd_L2.rds")

# Salvando RDS - Confira na pasta results/RDS
saveRDS(object = bd_L2, file = caminho_RDS_2)



## Linha 3 ----

# Pegando a pasta do projeto - vai ser diferente em cada PC, mas ele acha sozinho
pasta_proj <- rprojroot::find_rstudio_root_file()

# Inserindo a continuação do caminho para a função bd_L3, que será sempre igual pois está dentro da estrutura do Projeto
caminho_func_3 <- paste0(pasta_proj, "/2_scripts/bd_L3.R")

# Importando as funções!!
source(caminho_func_3)

# Especificando os caminhos - Aqui são os dados de exemplo que estão no github
# pasta_L3 <- paste(pasta_proj, "/1_data/LINHA_3", sep = "")

# fazendo nas pastas na base
pasta_L3 <- "C:/Users/PBC-PESQUISA/Documents/PROJETO BOTO-CINZA/PESQUISA/LINHA_3"

# Executando - Pode mudar a pasta, mas só com estrutura interna da linha 3 
bd_L3 <- bd_L3(pasta_L3)

# Definindo caminho para o arquivo RDS
caminho_RDS_3 <-  paste0(pasta_proj, "/4_export/1_banco/bd_L3.rds")

# Salvando RDS - Confira na pasta results/RDS
saveRDS(object = bd_L3, file = caminho_RDS_3)


