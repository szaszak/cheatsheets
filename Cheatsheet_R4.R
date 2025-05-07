# Estilo de código
# https://style.tidyverse.org/

# Carregar bibliotecas
# install.packages('sf')

# Para instalar o tidyverse, o vroom pode dar problema: -Werror=format-security
# 1. Tentar atualizar a lib Rcpp:
# install.packages("Rcpp")
# 2. Criar arquivo para sobrescrever o Makevars e, com isso, desconsiderar o problema como erro:
# mkdir -p ~/.R
# nano ~/.R/Makevars
# Inserir essa linha no arquivo e salvar:
# CXXFLAGS = -O2 -Wall
# install.packages("vroom", type = "source")
# Daí em diante, a instalação do tidyverse vai funcionar

library('tidyverse')
library('tidytable')
library('tidylog')
library('sf')
library('mapview')
library('readxl')

# Carregar bibliotecas - sem avisos
library('sf', quietly = TRUE)
suppressPackageStartupMessages(library('tidyverse'))
suppressPackageStartupMessages(library('tidylog'))

# Remover bibliotecas carregadas
detach("package:tidylog")


# Opções - Mostra valores sem notação científica
options(scipen = 999)
# Opções - Número máximo de linhas exibidas
options(repr.matrix.max.cols = 50)
# Definir outras opções
# getOption("timeout")
# getOption("max.print")
options(timeout = 1000)
options(max.print = 3000)


# Atualizar bibliotecas no RStudio
# https://community.rstudio.com/t/reinstalling-packages-on-new-version-of-r/7670/8
# update.packages(checkBuilt = TRUE, ask = FALSE)

# Referência de REGEX
# https://unicode-org.github.io/icu/userguide/strings/regexp.html

# Arquivos .csv
df <- read_delim(csv_file, delim = ';', # delim = "\t"
                 locale = locale(decimal_mark = ',', grouping_mark = '.', encoding = 'latin1'), # iso_8859-1, cp850 (SAT-CET), utf-8, ascii
                 n_max = 2, col_names = FALSE, # col_names = FALSE para arquivos sem header
                 col_types = "cicid") # 'c'='character', 'd'='double', 'l'='logical', 'i'='integer', D = date, T = datetime
                 # col_types = cols(.default = "c")

# Salvar arquivo .csv
write_delim(df, 'arquivo.csv', delim = ';')

# Excel
df <- read_excel('excel_file.xlsx')
# Ler abas existentes no arquivo Excel
excel_sheets('excel_file.xlsx')
df <- read_excel('excel_file.xlsx', sheet = 'sheet_name', col_names = FALSE)

# Gravar Excel
library('openxlsx')
write.xlsx(df, 'excel_file.xlsx')


# Arrow
# Instalar
Sys.setenv("NOT_CRAN" = "true")
install.packages("arrow")
library('arrow')

# Abrir arquivos
lala <- list.files(pasta_uni_out, pattern = '.parquet', full.names = TRUE, recursive = FALSE)
lala <- open_dataset(lala)

as.data.frame((Scanner$create(lala)$ToTable()))
schema(lala)

lala1 <- open_dataset(lala[1])
lala2 <- open_dataset(lala[2])

# Pré-processamentos para abrir arquivos
lala1 %>%
  left_join(lala2, by = 'cd_setor') %>%
  select(cd_setor, V0001, V01021) %>%
  collect()

# supported: select(), rename(), filter(), arrange(), group_by()
# not supported yet: summarize() or mutate() -> must collect() first
this %>%
  filter(str_starts(CD_GEOCODI, '35503085200000') & COD_ESPECIE %in% c(1, 2)) %>%
  compute() %>%
  select(CD_GEOCODI, lat, lon) %>%
  collect() %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% mapview(cex = 1)

# Gravar parquet
write_parquet(arq_uf, 'out_file.parquet')


# Ler arquivos .dbf e .dbc (Pesquisa OD, SIM-SUS)
# .dbf
library('foreign')
dados <- read.dbf('arquivo.dbf')
# .dbc
library('read.dbc')
dados <- read.dbc('arquivo.dbc')

# Ler arquivos shapefile
read_sf('arquivo.shp')
# Shapefile com encoding windows (Pesquisa OD, Censo)
st_read('arquivo.shp', options = "ENCODING=WINDOWS-1252") # manter sem espaçamento entre as aspas

# Puxar colunas de lat lon a partir do geom de um sf
hex_sp %>%
  st_centroid() %>%
  mutate(lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) %>%
  st_drop_geometry()

# Gravar arquivos shapefile
st_write(sf_object, 'shapefile.shp', driver = 'ESRI Shapefile', append = FALSE)
st_write(sf_object, 'shapefile.gpkg', driver = 'GPKG', append = FALSE, delete_layer = TRUE)

# Checar CRS
st_crs(sf_object)$input

# Padronizar os CRS para a mesma projeção
if (st_crs(sf_object_1) != st_crs(sf_object_2)){
    sf_object_1 <- sf_object_1 %>% st_transform(crs = st_crs(sf_object_2))
}

# Transformar dataframe em shape
# CRS 4326 = WGS84 (degrees)
# CRS 31983 = SIRGAS2000 UTM 23S
# CRS 32723 = WGS84 UTM 23S
df %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)
df %>% st_transform(4326)

# Checar se a geometria está ok
shape <- shape %>% st_make_valid()

# Juntar dois arquivos de shape
sf_out <- rbind(sf_object_1, sf_object_2)

# Filtrar primeiro item de id em dataframe com repetições do id nas linhas
df %>% group_by(id) %>% filter(row_number() == 1)

# Atualizar factors - atualizar o NA em ID_DOM para o valor da linha seguinte (Pesquisa OD)
# ZONA MUNI_DOM CO_DOM_X CO_DOM_Y ID_DOM
# <int>    <int>    <int>    <int> <fct>
# 1     1       36   333725  7394554 NA
# 2     1       36   333725  7394554 00100001
# 3     1       36   333725  7394554 00100002
# 4     1       36   333725  7394554 00100002
# 5     1       36   333725  7394554 00100002
# 6     1       36   333725  7394554 00100002
# 7     1       36   333067  7394620 00100003
# 8     1       36   333067  7394620 00100003
dados %>%
  # select(1:5) %>%
  group_by(ZONA, MUNI_DOM) %>%
  # Fill NAs with the first non-NA value in the group
  mutate(ID_DOM = replace(ID_DOM, is.na(ID_DOM), first(ID_DOM[!is.na(ID_DOM)]))) %>%
  # After filling the first NA, propagate values downward
  # mutate(ID_DOM = zoo::na.locf(ID_DOM, fromLast = FALSE, na.rm = FALSE)) %>%
  ungroup()

# Substituir NAs por 0 em todas as colunas
df %>% mutate_all(., ~replace(., is.na(.), 0))

# Criar coluna de total a partir de colunas que correspondem a um padrão
df %>% mutate(total = rowSums(across(starts_with("classe_"))))


# Funções - reconhecer variáveis como input

# Filtra dataframe e agrupa resultados por uma variável (por padrão, ANOOBITO)
agrupar <- function(df, filter_expr, group_var = expr(ANOOBITO), n_col = 'n') {
  df %>%
    filter(!!filter_expr) %>%
    group_by(!!group_var) %>%
    tally() %>%
    ungroup() %>%
    rename(!!n_col := n)
}
f_expr <- expr(str_starts(CAUSABAS, 'Y32'))
f_expr <- expr(str_starts(CAUSABAS, 'Y32') | str_starts(CAUSABAS, 'Y1'))
agrupar(sim_sus, f_expr, n_col = 'y32')

# Função com group_by reconhecendo coluna como input
estatisticas_descritivas_vg <- function(var) {
  # Descrição das variáveis categóricas - uma por vez, agrupado por viagem
  # https://uc-r.github.io/descriptives_categorical
  # https://dplyr.tidyverse.org/articles/programming.html
  out <-
    base_modelo %>%
    select(trip_id, {{var}}) %>%
    distinct() %>%
    group_by(across(all_of({{var}}))) %>%
    tally() %>%
    mutate(prop = n / sum(n) * 100)

  print(out)
}

# Nome da coluna passada como argumento da função
isolar_trecho_df <- function(sel_id, trip_id_col, df){
    # Nome da coluna passada como argumento da função precisa ser
    # lido de forma específica antes que a coluna possa ser usada
    # https://stackoverflow.com/questions/42100892/how-to-pass-a-string-to-dplyr-filter-in-a-function
    # Ver também o help de ?QU
    col_name = rlang::sym(as.character(trip_id_col))
    # col_value neste caso já é uma string
    # col_value = as.character(sel_id)
    df_out <- df %>% filter(!!col_name == sel_id)

    return(df_out)
}
isolar_trecho_df(sel_id = '28045039467d18dc5195c3553aea0fc231e890e1c5df470a75841b3c',
                 trip_id_col = 'tripid', df = df)

# Renomear coluna
sel_vars <- c(sprintf('classe_%d', seq(1, 6)))
for (var in sel_vars) {
  (...)
  # Selecionar colunas var (ex. classe_1) e renomear as calculadas (ex. classe_1_prop_vals)
  prop_ests <- prop_ests %>%
        select(!!var, prop_vals, dec_vals, int_vals) %>%
        rename_with(~ paste0(var, '_', .), c('prop_vals', 'dec_vals', 'int_vals'))
  }

# Group_by e tally // Count
df %>% group_by(x) %>% tally() %>% filter(n > 1)
df %>% count(x) %>% filter(n > 1)

