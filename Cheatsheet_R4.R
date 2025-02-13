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
