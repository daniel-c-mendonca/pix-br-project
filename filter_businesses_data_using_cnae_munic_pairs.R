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
save_path = "pix/data/cnpj_open/workdata/"


# BCB DATA GENERAL ----------------------------------------------------------------
pay_methods_usage_bcb = read_dta("pix/data/cnpj_open/raw/RCT_muni_cnae.dta") %>%
  clean_names()

#MATCH MUNIC CODES BCB-RECEITA FEDERAL
code_equiv_receita_ibge_bcb = read_xlsx("pix/data/cnpj_open/raw/munic_code_equiv.xlsx") %>%
  clean_names() %>%
  select(mun_cd, mun_cd_rfb, mun_cd_ibge) %>%
  mutate(mun_cd_ibge_6 = floor(mun_cd_ibge / 10))

pay_methods_usage_bcb = merge(pay_methods_usage_bcb, code_equiv_receita_ibge_bcb, by.x = "muni", by.y = "mun_cd")

#select pix data
pix_usage_data = pay_methods_usage_bcb %>%
  select(muni, cnae, perc_user_pix_firm, perc_user_pix_owner, mun_cd_rfb, mun_cd_ibge)

#select only cnaes present in bcb's rct data
cnaes_from_bcb_pix_data = pix_usage_data %>%
  distinct(cnae) %>%
  as.vector() %>%
  unlist()

# CNAES FILTER GENERAL ------------------------------------------------------------
cnaes_data = read_csv("pix/data/cnpj_open/merged/cnaes_data.csv")

#filter RFB cnae data
cnaes_filter = cnaes_data %>%
  rename(cnae = v1, desc = v2) %>%
  mutate(desc = tolower(desc)) %>%
  filter(cnae %in% cnaes_from_bcb_pix_data)

cnaes_filter_codes = cnaes_filter$cnae

pix_usage_data_cnaes_filtered = pix_usage_data %>%
  filter(cnae %in% cnaes_filter_codes)

#save cnae + munic data withouth pix usage threshold filter
write_csv(pix_usage_data_cnaes_filtered, paste0(save_path, "pix_usage_data_cnaes_filtered_general_usage", ".csv"))

# # use filtered cnaes to generate a smaller businesses RFB database - R U N  O N C E ----
# estabelecimentos_filtered_cnae_general = data.frame()
# 
# for (j in 1:9) {
#   print(j)
# 
#   data_file_name = paste0("dados_abertos_cnpj_14_05_2024/Estabelecimentos", j, ".zip")
# 
#   zip_contents = unzip(data_file_name, list = TRUE)
# 
#   file_to_unzip = zip_contents$Name[1]
# 
#   data_iter = read_delim(unz(data_file_name, file_to_unzip), delim = ";", col_names = FALSE, locale = locale(encoding = "ISO-8859-1")) %>%
#     clean_names() %>%
#     filter(x12 %in% cnaes_filter_codes) %>%
#     mutate(across(everything(), as.character))
# 
#   estabelecimentos_filtered_cnae_general = bind_rows(estabelecimentos_filtered_cnae_general, data_iter)
# 
# }
# 
# write_csv(estabelecimentos_filtered_cnae_general, "pix/data/cnpj_open/workdata/estabelecimentos_filtered_cnae_general_bcb_rct.csv")



# # SELECT MEIs ------------------------------------------------------------- - R U N  O N C E 
# simples_data = fread("pix/data/cnpj_open/merged/simples_data.csv")
# 
# empresas_mei_cnpj = simples_data %>%
#   filter(v5 == "S") %>%
#   select(v1) %>%
#   rename(cnpj_8dig_mei = v1) %>%
#   mutate(cnpj_8dig_mei = str_pad(cnpj_8dig_mei, 8, side = "left", pad = "0"))
# 
# saveRDS(empresas_mei_cnpj, paste0(save_path, "empresas_mei_cnpj_vector.rds"))

# # SELECT MEs -------------------------------------------------------------- - R U N  O N C E 
# empresas_all = fread("pix/data/cnpj_open/merged/empresas_all.csv")
# 
# empresas_micro_cnpj = empresas_all %>%
#   filter(v6 == 1) %>%
#   select(v1) %>%
#   rename(cnpj_8dig_me = v1) %>%
#   mutate(cnpj_8dig_me = str_pad(cnpj_8dig_me, 8, side = "left", pad = "0"))
# 
# saveRDS(empresas_micro_cnpj, paste0(save_path, "empresas_micro_cnpj_vector.rds"))

# # FILTER ONLY ME AND MEI -------------------------------------------------- - R U N  O N C E 
# estabelecimentos_filtered_cnae_general = fread("pix/data/cnpj_open/workdata/estabelecimentos_filtered_cnae_general_bcb_rct.csv")
# 
# empresas_mei_cnpj = read_rds(paste0(save_path, "empresas_mei_cnpj_vector.rds"))
# 
# empresas_micro_cnpj = read_rds(paste0(save_path, "empresas_micro_cnpj_vector.rds"))
# 
# mei_micro_cnpj_vector = c(empresas_mei_cnpj$cnpj_8dig_mei, empresas_micro_cnpj$cnpj_8dig_me) %>% unique()
# 
# estabelecimentos_filtered_cnae_general_me_mei = estabelecimentos_filtered_cnae_general %>%
#   mutate(cnpj_8dig = as.character(str_pad(x1, 8, side = "left", pad = "0"))) %>%
#   filter(cnpj_8dig %in% mei_micro_cnpj_vector)
# 
# write_csv(estabelecimentos_filtered_cnae_general_me_mei, "pix/data/cnpj_open/workdata/estabelecimentos_filtered_cnae_general_me_mei.csv")
# 

# FILTER BUSINESSES FROM RFB ----------------------------------------------
estabelecimentos_filtered_cnae_general_me_mei = fread("pix/data/cnpj_open/workdata/estabelecimentos_filtered_cnae_general_me_mei.csv")

estabelecimentos_filtered_cnae_general_me_mei = estabelecimentos_filtered_cnae_general_me_mei %>% 
  mutate(cnpj_8dig = str_pad(cnpj_8dig, 8, "left", pad = "0"))

usage_threshold = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)

estabelecimentos_filtered_usage_threshold = list()

for (i in 1:length(usage_threshold)) {
  
  usage_lim = usage_threshold[i]
  
  print(usage_lim)
  
  #select only pix data and filter through iteration's pix usage limit from BCB data
  pix_usage_data = pay_methods_usage_bcb %>% 
    select(muni, cnae, perc_user_pix_firm, perc_user_pix_owner, mun_cd_rfb, mun_cd_ibge) %>% 
    filter(pmax(perc_user_pix_firm, perc_user_pix_owner) <= usage_lim)
  
  #build a vector from filtered cnaes
  cnaes_from_bcb_pix_data = pix_usage_data %>% 
    distinct(cnae) %>% 
    as.vector() %>% 
    unlist()
  
  #filter RFB cnae data
  cnaes_filter = cnaes_data %>% 
    rename(cnae = v1, desc = v2) %>% 
    mutate(desc = tolower(desc)) %>% 
    filter(cnae %in% cnaes_from_bcb_pix_data) #%>% #filter general cnae RFB database to keep only cnaes present in BCB rct data
    # filter(
    #   !str_detect(
    #     desc,
    #     "atacad|laboratório|fabricação|cultivo|produção|criação|extração|pesca|abate|coleta|metalurgia|fundição|horticultura|apicultura|beneficiamento"
    #   ) #remove cnaes unrelated to our study based on description
    # )
  
  #generate a vector with filtered cnaes
  cnaes_filter_codes = cnaes_filter$cnae
  
  #generate final pix usage data for this specific usage threshold and cnaes related to our study
  pix_usage_data_cnaes_filtered = pix_usage_data %>% 
    filter(cnae %in% cnaes_filter_codes)
  
  #save filtered pix usage data
  write_csv(pix_usage_data_cnaes_filtered, paste0(save_path, "pix_usage_data_cnaes_filtered_pix_usage_limit_", usage_lim,".csv"))
  
  estabelecimentos_filtered_cnae_usage_threshold_iter = estabelecimentos_filtered_cnae_general_me_mei %>% 
    filter(x12 %in% cnaes_filter_codes)
  
  estabelecimentos_filtered = data.frame()
  
  length(cnaes_filter_codes)
  
  for (j in 1:length(cnaes_filter_codes)) {
    
    print(round(100*j/length(cnaes_filter_codes), 2))
    
    cnae_iter = cnaes_filter_codes[j]
    
    cnae_iter_munic_codes = pix_usage_data_cnaes_filtered %>% 
      filter(cnae == cnae_iter) %>% 
      select(mun_cd_rfb) %>% 
      as.vector() %>% 
      unlist()
    
    estabelecimentos_filtered_munic = estabelecimentos_filtered_cnae_usage_threshold_iter %>% 
      filter(x12 == cnae_iter) %>% 
      filter(x21 %in% cnae_iter_munic_codes)

    estabelecimentos_filtered = bind_rows(estabelecimentos_filtered, estabelecimentos_filtered_munic)
    
  }
  
  estabelecimentos_filtered_usage_threshold = append(estabelecimentos_filtered_usage_threshold, list(estabelecimentos_filtered))

  write_csv(estabelecimentos_filtered, paste0("pix/data/cnpj_open/workdata/thresholds/estabelecimentos_unfiltered_cnaes_NOTAGS_low_usage_munic_lim_", usage_lim,".csv"))
  
}



