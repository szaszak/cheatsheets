# Estilo de código
# https://style.tidyverse.org/

# Carregar bibliotecas
# install.packages('sf')
library('tidyverse')
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

# Gravar arquivos shapefile
st_write(sf_object, 'shapefile.shp', driver = 'ESRI Shapefile', append = FALSE)
st_write(sf_object, 'shapefile.gpkg', driver = 'GPKG', append = FALSE)

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

# Checar se a geometria está ok
shape <- shape %>% st_make_valid()

# Juntar dois arquivos de shape
sf_out <- rbind(sf_object_1, sf_object_2)
