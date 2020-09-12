# Repositório de funções

Aqui estarão disponibilizadas as versões mais atuais das funções utilizadas no PBC.

## /scripts

#### Arquivos \*.R das funções

(arquivos com um "." na frente, estão sem uso no momento)

bd_L* --- PRINCIPAIS - Funções para criação do "banco de dados" de cada linha:

- bd_L1(<caminho da pasta da Linha 1>)
- bd_L2(<caminho da pasta da Linha 2>)
- bd_L3(<caminho da pasta da Linha 3>)

controle_L* --- Funções para o acompanhamento da insersão de dados de campo de cada linha:

- controle_L1(<caminho da pasta da Linha 1>)
- controle_L2(<caminho da pasta da Linha 2>)
- controle_L3(<caminho da pasta da Linha 3>)  

le_planilha_L* --- Funções para leitura da planilha excel de cada linha:

- le_planilha_L1(<caminho da pasta da Linha 1>)
- le_planilha_L2(<caminho da pasta da Linha 2>)
- le_planilha_L3(<caminho da pasta da Linha 3>)  

sub_WP_L* --- Funções de substituição dos WP das planilhas pelos dados de datahora, longitude, e latitude de cada linha:

- sub_WP_L1(<caminho da pasta da Linha 1>) - chama as funções le_planilha_L1() e le_WPs_L()
- sub_WP_L2(<caminho da pasta da Linha 2>) - chama as funções le_planilha_L2() e le_WPs_L()
- sub_WP_L3(<caminho da pasta da Linha 3>) - chama as funções le_planilha_L3() e le_WPs_L()

le_*_L --- Funções de leitura gerais, servem para qualquer linha:

- le_rota_L(<caminho da pasta da Linha 1,2 ou 3>) --- Função de para leitura dos arquivos de rotas do GPS de qualquer linha
- le_sonda_L(<caminho da pasta da Linha 1,2 ou 3>) --- Função de para leitura dos arquivos da sonda de qualquer linha
- le_WPs_L(<caminho da pasta da Linha 1,2 ou 3>) --- Função de para leitura dos arquivos de WP do GPS de qualquer linha

junta_*_L --- Funções intermediárias, são chamadas pelas bd_L1(), bd_L2() e bd_L3().

- junta_rota_L() --- Função intermediária de união dos dados de rotas do GPS na lista de qualquer linha
- junta_sonda_L() --- Função intermediária de união dos dados da sonda na lista de qualquer linha



## /rmarkdown

Arquivos markdown e html:

- resumo_de_campo: Gera o html para acompanhamento das linhas de pesquisa

- uso_das_funções: deprecated! to refazendo para subir com exemplos.

- dash_diario_foto: deprecated! vou usar de consulta no futuro.


## /data

Um mês de dados bruto para exemplo (juntar o do mês 08):

(deprecated)
- 3 \*.xls ou \*.xlsx de dados de campo (um mês de dados completo de cada linha, quando tiver)
- 7 \*.xls de log da sonda (referente à um mes de coleta, quando tiver)
- 7 \*.gpx de waypoint (quando tiver)
- 7 \*.gpx de trajeto (quando tiver)
- 10 \*.jpg como exemplo de foto identificação (pensando em alternativas melhores...)
