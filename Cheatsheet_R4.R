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

# Pacotes pré-compilados Posit Package Manager - pegar URL atualizada em:
# https://packagemanager.posit.co/client/#/repos/cran/setup
options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest'))

library('tidyverse')
library('tidytable')
library('tidylog')
library('sf')
library('mapview')
library('leaflet')
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
# getOption("max.print")
options(max.print = 3000)


# Atualizar bibliotecas no RStudio
# https://community.rstudio.com/t/reinstalling-packages-on-new-version-of-r/7670/8
# update.packages(checkBuilt = TRUE, ask = FALSE)

# Referência de REGEX
# https://unicode-org.github.io/icu/userguide/strings/regexp.html
# Any digit -	"\\d"
# Any word char	- "\\w"
# Any letter (Unicode) - "\\p{L}"
# Any number (Unicode) - "\\p{N}"
# Any letters only - "[A-Za-z]"
# Any character - "."
# Any whitespace (space, tab, newline, etc.) - "\\s"

# REGEX quantifiers
# *	- 0 or more
# +	- 1 or more
# ?	- 0 or 1
# {n}	- Exactly n
# {n,}	- n or more
# {n,m}	- Between n and m

# Arquivos .csv
df <- read_delim(csv_file, delim = ';', # delim = "\t"
                 locale = locale(decimal_mark = ',', grouping_mark = '.', encoding = 'latin1'), # iso_8859-1, cp850 (SAT-CET), utf-8, ascii
                 n_max = 2, col_names = FALSE, # col_names = FALSE para arquivos sem header
                 col_types = "cicid") # 'c'='character', 'd'='double', 'l'='logical', 'i'='integer', D = date, T = datetime, l = logical
                 # col_types = cols(.default = "c")

# Transformar em data
df %>% mutate(limite = as.POSIXct(limite, origin = "1970-01-01")) # Excel
df %>% mutate(tempo_em_segundos = tempo_fracao_horas * 3600))
df %>% mutate(tempo_em_segundos = tempo_fracao_dias * 86400))

# Comparar colunas dos dois dataframes
setdiff(names(df1), names(df2))
setdiff(names(df2), names(df1))
# Colunas que existem em ambos os dataframes
intersect(names(df1), names(df2))

# Criar colunas como NA
df %>% mutate(this = as.numeric(NA),
              that = as.date(NA),
              thou = as.character(NA))

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

# Ver mapa leaflet
df %>%  
  leaflet() %>%
  addProviderTiles("CartoDB.Positron", group = "Positron") %>%
  addProviderTiles("OpenStreetMap", group = "OSM") %>%
  addCircleMarkers(radius = 3,
                   color = "#2b8cbe",
                   fillOpacity = 0.8,
                   stroke = FALSE) %>% 
  addLayersControl(
    baseGroups = c("Positron", "OSM"),
    options = layersControlOptions(collapsed = FALSE)
  )

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

# Separar colunas
df %>% separate(tipo_logradouro, into = c('sigla', 'por_extenso'), sep = '( )+-[ ]?', remove = FALSE)

# Separar itens com delimitador em novas linhas (valores n repetem nas linhas):
# saidas_dh_segunda     n
# 1 Manhã                28
# 2 Manhã, Noite          7

# saidas_dh_segunda     n
# 1 Manhã                28
# 2 Manhã                 7
# 3 Noite                 7

df %>%
  # Para manter valores NA, substituir por string
  mutate(saidas_dh_segunda = replace_na(saidas_dh_segunda, "NA")) %>%
  # Aceita REGEX, mas superseded  
  # separate_rows(saidas_dh_segunda, sep = ",\\s*")
  # Não aceita REGEX
  separate_longer_delim(saidas_dh_segunda, delim = ", ")

# Remover trailing spaces e espaços internos às strings
df %>% mutate(across(where(is.character), str_trim),
              across(where(is.character), str_squish))

# Exemplos de str_extract()
# Linha 08 - Jardim Silveira, Linha 09 - Grajaú
# (?<= - ).+ é um lookbehind, ou seja, tudo o que estiver após ' - ' é pego pelo .+
df %>% mutate(estacao = str_extract(estacao, '(?<= - ).+'))

# Mudar caixa (case) de texto:
# Title case
str_to_title(top_voted[[1]])
# Sentence case
str_to_sentence(var)
# Lower case
df %>% mutate(origin2 = tolower(origin))
# Upper case
mutate(across(everything(), ~ toupper(.x)))

# Case_when
df %>% mutate(dep_on_time = case_when(dep_time < sched_dep_time ~ 1,
                                      TRUE ~ 0))

df %>% mutate(cat_partido = case_when(grepl('TC', SIGLA_PARTIDO) ~ 'Em dúvida',
                                      grepl('C', SIGLA_PARTIDO) ~ 'Esquerda (Comunistas)',
                                      grepl('T', SIGLA_PARTIDO) ~ 'Outros (Trabalhadores)',
                                      TRUE ~ 'Direita'))

# Filtrar primeiro item de id em dataframe com repetições do id nas linhas
df %>% group_by(id) %>% filter(row_number() == 1)
df %>% group_by(id) %>% slice_head(n = 1) %>% ungroup()
df %>% group_by(id) %>% slice_tail(n = 1) %>% ungroup()

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

# Unir data.tables por timestamp mais próximo
checkin  <- viam_t %>% filter(atividade == 'CHECKIN') %>% setDT()
checkout <- viam_t %>% filter(atividade == 'CHECKOUT') %>% setDT()
# df2 é o que vai manter a quantidade final de linhas
# Valor de timestamp mais próximo (antes ou depois) -> roll = "nearest"
# Somente valor de timestamp futuro mais próximo  -> roll = Inf
# Somente valor de timestamp passado mais próximo -> roll = -Inf
paired <- checkout[checkin, on = .(lacre, estacao, data), roll = "nearest",
                   # df1[df2, roll = "nearest", on = .(df1_col = df2_col),
                   .(lacre,
                     estacao,
                     # i. refers to right table (df2); x. to left table (df1)
                     checkin = i.atividade,
                     checkin_data = i.data,
                     checkout = x.atividade,
                     checkout_data = x.data)]


# Substituir NAs por 0 em todas as colunas
df %>% mutate_all(., ~replace(., is.na(.), 0))

# Remover todas as colunas que só possuem valor NA
df <- df %>% select(!where(~ all(is.na(.))))

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

# Função com group_by reconhecendo coluna como input (variável entra como nome da variável)
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
estatisticas_descritivas_vg(lala)

# Nome da coluna passada como argumento da função (variável entra como string)
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

# Retorna resumo da variável, com subtotais e percentuais (variável entra como string)
# resumir(df, 'como_soube')
resumir <- function(df, var) {
  col_name = rlang::sym(as.character(var))
  df %>% group_by(!!col_name) %>% tally() %>% mutate(perc = n / sum(n))
}

# Separa variável que se repete em diferentes linhas e retorna resumo, com 
# subtotais e percentuais (variável entra como nome da variável)
resumir_multivalores(df, saidas_dh_segunda, delim = ', ')
resumir_multivalores <- function(df, var, delim) {
  df %>%
    group_by({{ var }}) %>%
    tally() %>%
    mutate({{ var }} := replace_na({{ var }}, "_NA_")) %>%
    separate_longer_delim({{ var }}, delim = delim) %>%
    group_by({{ var }}) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    mutate(perc = n / sum(n))
}

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

# Obter contagem dos valores de todas as colunas
result <- map(df, ~ count(tibble(value = .x), value))

# Download de arquivos
# getOption("timeout")
# Aumentar tempo (segundos) para downloads grandes
options(timeout = 1000)

# Puxar arquivo PBF do OSM para o Sudeste do Brasil
url <- 'https://download.geofabrik.de/south-america/brazil/sudeste-latest.osm.pbf'
pbf_file <- sprintf('%s/%s', data_folder, basename(url))

# Puxar tamanho do arquivo de download
response <- httr::HEAD(url)
download_size <- httr::headers(response)[["Content-Length"]]
# Fazer download e checar sucesso por tamanho do arquivo
result <- try({ download.file(url, pbf_file)} , silent = FALSE)
if (file.size(pbf_file) == download_size) {
  print('PBF baixado corretamente')
} else {
  warning('PBF não baixado por inteiro. Considere aumentar o valor do timeout para downloads grandes')
}


# Descompactar arquivos - IBGE
# Tentar descompactar com unzip() vai dar erro devido ao uso de encodings
# inválidos dentro dos arquivos do IBGE. O 7z consegue fazer isso no linux.
# Em outros sistemas, deve ser possível fazer isso com o pacote 'archive':
# https://github.com/r-lib/archive
if (version$os == 'linux-gnu') {
  unzip_string <- sprintf('/usr/bin/7z x -o%s -y %s', pasta_set_shps, zip_file)
} else if (version$os == 'darwin17.0') {
  unzip_string <- sprintf('/Volumes/Macintosh\\ HD/Users/renataeflavio/Downloads/programas/7z2301-mac/7zz x -o%s -y %s', str_replace(pasta_set_shps, ' ', '\\\\ '), str_replace(zip_file, ' ', '\\\\ '))
}

system(unzip_string)
