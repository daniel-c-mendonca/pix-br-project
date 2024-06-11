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
# #CNAES----
# data_file_name = "dados_abertos_cnpj_14_05_2024/Cnaes.zip"
# 
# zip_contents = unzip(data_file_name, list = TRUE)
# 
# file_to_unzip = zip_contents$Name[1]
# 
# raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")
# 
# converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")
# 
# cnaes_data = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>% 
#   clean_names()
# 
# write_csv(cnaes_data, "pix/data/cnpj_open/merged/cnaes_data.csv")
# rm(cnaes_data)
# 
# #EMPRESAS----
# empresas_all = data.frame()
# 
# for (i in 1:9) {
#   
#   print(i)
#   
#   data_file_name = paste0("dados_abertos_cnpj_14_05_2024/Empresas", i, ".zip")
#   
#   zip_contents = unzip(data_file_name, list = TRUE)
#   
#   file_to_unzip = zip_contents$Name[1]
#   
#   raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")
#   
#   converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")
#   
#   data_iter = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>% 
#     clean_names()
#   
#   empresas_all = bind_rows(empresas_all, data_iter)
#   
# }
# 
# write_csv(empresas_all, "pix/data/cnpj_open/merged/empresas_all.csv")
# rm(empresas_all)
# 
# 
# #SOCIOS----
# socios_all = data.frame()
# 
# for (i in 1:9) {
#   print(i)
#   
#   data_file_name = paste0("dados_abertos_cnpj_14_05_2024/Socios", i, ".zip")
#   
#   zip_contents = unzip(data_file_name, list = TRUE)
#   
#   file_to_unzip = zip_contents$Name[1]
#   
#   raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")
#   
#   converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")
#   
#   data_iter = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>% 
#     clean_names()
#   
#   socios_all = bind_rows(socios_all, data_iter)
#   
# }
# 
# write_csv(socios_all, "pix/data/cnpj_open/merged/socios_all.csv")
# rm(socios_all)
# 
# #ESTABELECIMENTOS----
# estabelecimentos_all = data.frame()
# 
# for (i in 1:9) {
#   print(i)
#   
#   data_file_name = paste0("dados_abertos_cnpj_14_05_2024/Estabelecimentos", i, ".zip")
#   
#   zip_contents = unzip(data_file_name, list = TRUE)
#   
#   file_to_unzip = zip_contents$Name[1]
#   
#   raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")
#   
#   converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")
#   
#   data_iter = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>% 
#     clean_names() %>% 
#     mutate_all(as.character)
#   
#   estabelecimentos_all = bind_rows(estabelecimentos_all, data_iter)
#   
# }
# 
# write_csv(estabelecimentos_all, "pix/data/cnpj_open/merged/estabelecimentos_all.csv")
# rm(estabelecimentos_all)
# 
# #ESTABELECIMENTOS READR----
# estabelecimentos_all = data.frame()
# 
# for (i in 1:9) {
#   print(i)
#   
#   data_file_name <- paste0("dados_abertos_cnpj_14_05_2024/Estabelecimentos", i, ".zip")
#   
#   zip_contents <- unzip(data_file_name, list = TRUE)
#   
#   file_to_unzip <- zip_contents$Name[1]
#   
#   data_iter <- read_delim(unz(data_file_name, file_to_unzip), delim = ";", col_names = FALSE, locale = locale(encoding = "ISO-8859-1")) %>%
#     clean_names() %>%
#     mutate(across(everything(), as.character))
#   
#   estabelecimentos_all <- bind_rows(estabelecimentos_all, data_iter)
# }
# 
# write_csv(estabelecimentos_all, "pix/data/cnpj_open/merged/estabelecimentos_all.csv")
# rm(estabelecimentos_all)


# #MUNICIPIOS----
# data_file_name = "dados_abertos_cnpj_14_05_2024/Municipios.zip"
# 
# zip_contents = unzip(data_file_name, list = TRUE)
# 
# file_to_unzip = zip_contents$Name[1]
# 
# raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")
# 
# converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")
# 
# munic_data = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>%
#   clean_names()
# 
# write_csv(munic_data, "pix/data/cnpj_open/merged/munic_data.csv")
# 

#DADOS DO SIMPLES----
data_file_name = "dados_abertos_cnpj_14_05_2024/Simples.zip"

zip_contents = unzip(data_file_name, list = TRUE)

file_to_unzip = zip_contents$Name[1]

raw_content = readLines(unz(data_file_name, file_to_unzip), encoding = "ISO-8859-1")

converted_content = iconv(raw_content, from = "ISO-8859-1", to = "UTF-8")

simples_data = read.csv(textConnection(converted_content), sep = ";", header = FALSE) %>%
  clean_names()

write_csv(simples_data, "pix/data/cnpj_open/merged/simples_data.csv")


# treat data --------------------------------------------------------------
# CNAES FILTER ------------------------------------------------------------
cnaes_data = read_csv("pix/data/cnpj_open/merged/cnaes_data.csv")

cnaes_filter = cnaes_data %>% 
  rename(cnae = v1, desc = v2) %>% 
  mutate(desc = tolower(desc)) %>% 
  filter(
    !str_detect(
      desc,
      "atacad|laboratório|fabricação|cultivo|produção|criação|extração|pesca|abate|coleta|metalurgia|fundição|horticultura|apicultura|beneficiamento"
    )
  )

cnaes_filter_codes = cnaes_filter$cnae

# LOW PIX USAGE MUNICS ----------------------------------------------------
pay_methods_usage_bcb <- read_dta("pix/data/cnpj_open/raw/RCT_muni_cnae.dta") %>% 
  clean_names()

pix_usage_data = pay_methods_usage_bcb %>% 
  select(muni, cnae, perc_user_pix_firm, perc_user_pix_owner)


cnaes_from_bcb_pix_data = pix_usage_data %>% 
  distinct(cnae)

pix_usage_data_below_50pp_firms_cnae_filter = pix_usage_data %>% 
  filter(perc_user_pix_firm < 0.5) %>% 
  filter(cnae %in% cnaes_filter_codes)

munic_codes_pix_usage_cnae_filter = unique(pix_usage_data_below_50pp_firms_cnae_filter$muni)


# MATCH MUNIC CODES BCB-RECEITA FEDERAL -----------------------------------
code_equiv_receita_ibge_bcb = read_xlsx("pix/data/cnpj_open/raw/munic_code_equiv.xlsx") %>% 
  clean_names() %>% 
  select(mun_cd, mun_cd_rfb, mun_cd_ibge) %>% 
  mutate(mun_cd_ibge_6 = floor(mun_cd_ibge / 10))

br_munic = read_municipality(
  code_muni = "all",
  year = 2020,
  simplified = TRUE,
  showProgress = TRUE
)

munic_ibge = br_munic %>% 
  st_drop_geometry() %>% 
  select(code_muni, name_muni, abbrev_state) %>% 
  mutate(code_muni_6_ibge = floor(code_muni / 10)) %>% 
  mutate(name_muni_ibge = toupper(name_muni)) %>%
  mutate(
    name_muni_ibge = chartr(
      "ÁÂÃÄÅÀÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜ",
      "AAAAAACEEEEIIIINOOOOOUUUU",
      name_muni_ibge
    )
  ) %>%
  mutate(name_muni_ibge = gsub("-", " ", name_muni_ibge))
  

munic_receita = read_csv("pix/data/cnpj_open/merged/munic_data.csv")

munic_receita = munic_receita %>% 
  rename(code_muni_receita = v1, name_muni_receita = v2)

munic_ibge_receita = merge(munic_ibge, munic_receita, by.x = "name_muni_ibge", by.y = "name_muni_receita")
  
munic_ibge_receita = munic_ibge_receita %>% 
  












munic_low_pix = read_xlsx("dados_abertos_cnpj_14_05_2024/Municipios/munic_low_pix.xlsx") %>%
  mutate(munic_name = toupper(municipality)) %>%
  mutate(
    munic_name = chartr(
      "ÁÂÃÄÅÀÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜ",
      "AAAAAACEEEEIIIINOOOOOUUUU",
      munic_name
    )
  ) %>%
  mutate(munic_name = gsub("-", " ", munic_name)) %>%
  mutate(municipality = NULL)

munic_name_code = merge(munic_low_pix, munic_data, by = "munic_name")

munic_name_code_unique = munic_name_code %>%
  distinct(munic_name, .keep_all = T)

munic_code_filter = munic_name_code_unique$code

# OPEN AND FILTER ESTABELECIMENTOS LOW PIX CNAE ---------------------------
estabelecimentos_filter = data.frame()

for (i in 1:9) {
    print(i)

    data_file_name <- paste0("dados_abertos_cnpj_14_05_2024/Estabelecimentos", i, ".zip")

    zip_contents <- unzip(data_file_name, list = TRUE)

    file_to_unzip <- zip_contents$Name[1]

    data_iter <- read_delim(unz(data_file_name, file_to_unzip), delim = ";", col_names = FALSE, locale = locale(encoding = "ISO-8859-1")) %>%
      clean_names() %>%
      filter(x21 %in% munic_code_filter) %>% 
      mutate(across(everything(), as.character))

    estabelecimentos_filter <- bind_rows(estabelecimentos_filter, data_iter)

}

write_csv(estabelecimentos_filter, "pix/data/cnpj_open/merged/estabelecimentos_filter_low_pix.csv")

#FILTER CNAE ESTABELECIMENTOS
estabelecimentos_filter_cnae = estabelecimentos_filter %>% 
  filter(x12 %in% cnaes_filter_codes)

#EMPRESAS FILTER
empresas_all = read_csv("pix/data/cnpj_open/merged/estabelecimentos_filter_low_pix.csv")

empresas_micro = empresas_all %>% 
  filter(v6 == 1)


empresas_filter = empresas_all %>% 
  filter(!is.na(x23)) %>% 
  filter(!is.na(x22)) %>% 
  mutate(first_dig_tel = substr(x23, 1, 1)) %>% 
  filter(first_dig_tel %in% c(9,8,7,6))

#-exclude firms that have more than 1 establishments (check if the first 8 digits repeat)













