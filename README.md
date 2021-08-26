# Repositório de funções utilizadas no Projeto Boto-Cinza do Instituto de Pesquisas Cananéia - IPeC

Aqui estão disponibilizadas as versões mais atuais das funções utilizadas pelo PBC 2020-2022.

## /1_data

- 4 meses de dados bruto para exemplo em cada uma das 3 linhas de pesquisa 
- 1 shapefiles da geometria das águas amostradas para cada uma das linhas de pesquisa, utilizados nas análises como janela

## /2_scripts

#### Arquivos \*.R das funções de leitura dos arquivos e organização dos dados para a criação de um "banco de dados" para cada linha de pesquisa

## /3_user

#### Cada arquivos \*.R exporta um tipo de aquivo diferente.

- /1_banco.R - Roda todas as funções contidas em "\2_scripts"
- /2_resumo.R - Gera um arquivo \*txt simples de resumo de campo
- /3_shape.R - Gera shapefiles de linhas e pontos de todas as saidas
- /4_inter.R - Diferentes interpolações para cada necessidade
- /5_sonda.R - Gera rasters dos parâmetros da sonda
- /6_clima.R - Gera rasters dos parâmetros climáticos

## /4_export

#### Cada pasta receberá uma exportação diferente. Existe um "README_\*.txt" dentro de cada pasta.

- /1_banco/ - 3 arquivos \*.rds que estão servindo de banco de dados
- /2_resumo/ - 3 arquivos \*.txt que contém resumo dos campos
- /3_shape/ - ROTAS e PONTOS organizados em pastas separada
- /4_inter/ - 3 pastas para receber shapefiles e rasters das interpolações
- /5_sonda/- DIARIO e MEDIA organizados em pastas separadas
- /6_clima.R - DIARIO e MEDIA organizados em pastas separadas
