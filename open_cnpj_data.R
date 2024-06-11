# begin -------------------------------------------------------------------

rm(list = ls())

library(jtools)
require(mfx)
# require(tidyverse)
require(haven)
require(sandwich)
require(lmtest)
require(fastDummies)
require(knitr)
require(kableExtra)
require(ggplot2)
require(readxl)
require(readr)
require(dplyr)
require(writexl)
library(stringr)
library(data.table)
library(fixest)
library(modelsummary)
library(janitor)
library(readxl)
library(lubridate)
library(stats)
library(rmarkdown)
library(knitr)
library(DescTools)
library(patchwork)
library(ClustOfVar)
library(MASS)
library(cobalt)
library(broom)
library(did)
library(geobr)
library(sf)



#setting working directory cloud
setwd("D:/work_related/")

#setting results folder
save_path = "/pix/data/cnpj_open/merged"


# read data ---------------------------------------------------------------
#CNAES----
data_file_name = "dados_abertos_cnpj_14_05_2024/Cnaes.zip"

zip_contents = unzip(data_file_name, list = TRUE)

file_to_unzip = zip_contents$Name[1]

raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")

converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")

cnaes_data = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>%
  clean_names()

write_csv(cnaes_data, "pix/data/cnpj_open/merged/cnaes_data.csv")
rm(cnaes_data)

#EMPRESAS----
empresas_all = data.frame()

for (i in 1:9) {

  print(i)

  data_file_name = paste0("dados_abertos_cnpj_14_05_2024/Empresas", i, ".zip")

  zip_contents = unzip(data_file_name, list = TRUE)

  file_to_unzip = zip_contents$Name[1]

  raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")

  converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")

  data_iter = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>%
    clean_names()

  empresas_all = bind_rows(empresas_all, data_iter)

}

write_csv(empresas_all, "pix/data/cnpj_open/merged/empresas_all.csv")
rm(empresas_all)


#SOCIOS----
socios_all = data.frame()

for (i in 1:9) {
  print(i)

  data_file_name = paste0("dados_abertos_cnpj_14_05_2024/Socios", i, ".zip")

  zip_contents = unzip(data_file_name, list = TRUE)

  file_to_unzip = zip_contents$Name[1]

  raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")

  converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")

  data_iter = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>%
    clean_names()

  socios_all = bind_rows(socios_all, data_iter)

}

write_csv(socios_all, "pix/data/cnpj_open/merged/socios_all.csv")
rm(socios_all)

#ESTABELECIMENTOS----
estabelecimentos_all = data.frame()

for (i in 1:9) {
  print(i)

  data_file_name = paste0("dados_abertos_cnpj_14_05_2024/Estabelecimentos", i, ".zip")

  zip_contents = unzip(data_file_name, list = TRUE)

  file_to_unzip = zip_contents$Name[1]

  raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")

  converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")

  data_iter = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>%
    clean_names() %>%
    mutate_all(as.character)

  estabelecimentos_all = bind_rows(estabelecimentos_all, data_iter)

}

write_csv(estabelecimentos_all, "pix/data/cnpj_open/merged/estabelecimentos_all.csv")
rm(estabelecimentos_all)

#ESTABELECIMENTOS READR----
estabelecimentos_all = data.frame()

for (i in 1:9) {
  print(i)

  data_file_name <- paste0("dados_abertos_cnpj_14_05_2024/Estabelecimentos", i, ".zip")

  zip_contents <- unzip(data_file_name, list = TRUE)

  file_to_unzip <- zip_contents$Name[1]

  data_iter <- read_delim(unz(data_file_name, file_to_unzip), delim = ";", col_names = FALSE, locale = locale(encoding = "ISO-8859-1")) %>%
    clean_names() %>%
    mutate(across(everything(), as.character))

  estabelecimentos_all <- bind_rows(estabelecimentos_all, data_iter)
}

write_csv(estabelecimentos_all, "pix/data/cnpj_open/merged/estabelecimentos_all.csv")
rm(estabelecimentos_all)


#MUNICIPIOS----
data_file_name = "dados_abertos_cnpj_14_05_2024/Municipios.zip"

zip_contents = unzip(data_file_name, list = TRUE)

file_to_unzip = zip_contents$Name[1]

raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")

converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")

munic_data = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>%
  clean_names()

write_csv(munic_data, "pix/data/cnpj_open/merged/munic_data.csv")


#DADOS DO SIMPLES----
data_file_name = "dados_abertos_cnpj_14_05_2024/Simples.zip"

zip_contents = unzip(data_file_name, list = TRUE)

file_to_unzip = zip_contents$Name[1]

raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")

converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")

simples_data = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>%
  clean_names()

write_csv(simples_data, "pix/data/cnpj_open/merged/simples_data.csv")









