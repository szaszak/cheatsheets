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
