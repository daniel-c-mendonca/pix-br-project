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

# BCB data ----------------------------------------------------------------
pay_methods_usage_bcb = read_dta("pix/data/cnpj_open/raw/RCT_muni_cnae.dta") %>%
  clean_names()

pay_methods_usage_bcb_pix = pay_methods_usage_bcb %>% 
  select(perc_user_pix_firm, perc_user_pix_owner) %>% 
  mutate(max_usage_pix = pmax(perc_user_pix_firm, perc_user_pix_owner))

# open and treat businesses with CNAE filters -----------------------------------------------------

usage_threshold = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)

estabelecimentos_list_filter = list()

stats_df = data.frame()

for (i in 1:length(usage_threshold)) {
  
  usage_lim = usage_threshold[i]
  
  print(usage_lim)
  
  estabelecimentos_iter = fread(
    paste0(
      "pix/data/cnpj_open/workdata/thresholds/estabelecimentos_filtered_cnaes_TAGS_low_usage_munic_lim_",
      usage_lim,
      ".csv"
    )
  ) %>%
    mutate(cnpj_8dig = as.character(str_pad(x1, 8, side = "left", pad = "0")))
  
  repeated_cnpj = estabelecimentos_iter %>%
    group_by(cnpj_8dig) %>%
    filter(n() > 1) %>%
    ungroup() %>% 
    select(cnpj_8dig) %>% 
    as.vector() %>% 
    unlist()
  
  estabelecimentos_iter_filter = estabelecimentos_iter %>% 
    filter(!cnpj_8dig %in% repeated_cnpj) %>% 
    filter(!is.na(x23)) %>% 
    filter(!is.na(x22)) %>% 
    filter(str_length(x23) >= 8) %>% 
    filter(str_length(x22) == 2) %>% 
    filter(substr(x23, 1, 1) %in% c(9,8,7,6)) %>% 
    mutate(cnpj_full = paste0(
      str_pad(x1, 8, side = "left", pad = "0"),
      str_pad(x2, 4, side = "left", pad = "0"),
      str_pad(x3, 2, side = "left", pad = "0")
    )) %>% 
    mutate(number_full = paste0("whatsapp:+55", x22, "9", x23))%>% 
    select(number_full, x5, cnpj_full, x12, x21) %>% 
    rename(number = number_full, name = x5, caseid = cnpj_full, sector = x12, munic = x21)

  repeated_contact = estabelecimentos_iter_filter %>%
    group_by(number) %>%
    filter(n() > 1) %>%
    ungroup() %>% 
    select(number) %>% 
    as.vector() %>% 
    unlist()
  
  estabelecimentos_iter_filter_unique_contacts = estabelecimentos_iter_filter %>% 
    filter(!number %in% repeated_contact)

  estabelecimentos_list = c(estabelecimentos_list, list(estabelecimentos_iter_filter_unique_contacts))
  
  pix_usage_col = pay_methods_usage_bcb_pix %>% 
    select(max_usage_pix) %>% 
    filter(max_usage_pix <= usage_lim) %>% 
    as.vector() %>% 
    unlist()
  
  stats_row = data.frame(
    threshold = usage_lim,
    obs = nrow(estabelecimentos_iter_filter_unique_contacts),
    avg_pix_usage = round(mean(pix_usage_col, na.rm = T),2),
    unique_cnaes = length(unique(estabelecimentos_iter_filter_unique_contacts$sector)),
    unique_munics = length(unique(estabelecimentos_iter_filter_unique_contacts$munic))
  )
  
  stats_df = bind_rows(stats_df, stats_row)
  
}

save_path_stats = "pix/results/tables/"

colnames(stats_df) = c("Pix Usage Threshold", "Total Obs.", "Avg. Pix Usage", "# Unique Sectors", "# Unique Munic.")

stats_df %>%
  kable() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = FALSE) %>%
  # add_header_above(c("Contacts Data Statistics - MEIs and MEs - No CNAE Filters" = 5), bold = TRUE) %>%
  add_header_above(c("Contacts Data Statistics - MEIs and MEs - With CNAE Filters" = 5), bold = TRUE) %>%
  row_spec(0, bold = TRUE) %>% 
  footnote(general = "Terms used to eliminate sectors: atacad, laboratório, fabricação, cultivo, produção, criação, \nextração, pesca, abate, coleta, metalurgia, fundição, horticultura, apicultura, beneficiamento.") %>%
  # footnote(general = "Includes all sector codes (CNAEs) provided in the Central Bank's data.") %>%
  save_kable(file = paste0(save_path_stats,"Contacts Data Statistics - MEIs and MEs - With CNAE Filters", ".png"), self_contained = T)

stats_df %>%
  kable() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = FALSE) %>%
  # add_header_above(c("Contacts Data Statistics - MEIs and MEs - No CNAE Filters" = 5), bold = TRUE) %>%
  add_header_above(c("Contacts Data Statistics - MEIs and MEs - With CNAE Filters" = 5), bold = TRUE) %>%
  footnote(general = "Terms used to eliminate sectors: atacad, laboratório, fabricação, cultivo, produção, criação, \nextração, pesca, abate, coleta, metalurgia, fundição, horticultura, apicultura, beneficiamento.") %>%
  # footnote(general = "Includes all sector codes (CNAEs) provided in the Central Bank's data.") %>%
  row_spec(0, bold = TRUE) 


# open and treat businesses without CNAE filters -----------------------------------------------------

usage_threshold = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)

estabelecimentos_list = list()

stats_df = data.frame()

for (i in 1:length(usage_threshold)) {
  
  usage_lim = usage_threshold[i]
  
  print(usage_lim)
  
  estabelecimentos_iter = fread(
    paste0(
      "pix/data/cnpj_open/workdata/thresholds/estabelecimentos_unfiltered_cnaes_NOTAGS_low_usage_munic_lim_",
      usage_lim,
      ".csv"
    )
  ) %>%
    mutate(cnpj_8dig = as.character(str_pad(x1, 8, side = "left", pad = "0")))
  
  repeated_cnpj = estabelecimentos_iter %>%
    group_by(cnpj_8dig) %>%
    filter(n() > 1) %>%
    ungroup() %>% 
    select(cnpj_8dig) %>% 
    as.vector() %>% 
    unlist()
  
  estabelecimentos_iter_filter = estabelecimentos_iter %>% 
    filter(!cnpj_8dig %in% repeated_cnpj) %>% 
    filter(!is.na(x23)) %>% 
    filter(!is.na(x22)) %>% 
    filter(str_length(x23) >= 8) %>% 
    filter(str_length(x22) == 2) %>% 
    filter(substr(x23, 1, 1) %in% c(9,8,7,6)) %>% 
    mutate(cnpj_full = paste0(
      str_pad(x1, 8, side = "left", pad = "0"),
      str_pad(x2, 4, side = "left", pad = "0"),
      str_pad(x3, 2, side = "left", pad = "0")
    )) %>% 
    mutate(number_full = paste0("whatsapp:+55", x22, "9", x23))%>% 
    select(number_full, x5, cnpj_full, x12, x21) %>% 
    rename(number = number_full, name = x5, caseid = cnpj_full, sector = x12, munic = x21)
  
  repeated_contact = estabelecimentos_iter_filter %>%
    group_by(number) %>%
    filter(n() > 1) %>%
    ungroup() %>% 
    select(number) %>% 
    as.vector() %>% 
    unlist()
  
  estabelecimentos_iter_filter_unique_contacts = estabelecimentos_iter_filter %>% 
    filter(!number %in% repeated_contact)
  
  estabelecimentos_list = c(estabelecimentos_list, list(estabelecimentos_iter_filter_unique_contacts))
  
  pix_usage_col = pay_methods_usage_bcb_pix %>% 
    select(max_usage_pix) %>% 
    filter(max_usage_pix <= usage_lim) %>% 
    as.vector() %>% 
    unlist()
  
  stats_row = data.frame(
    threshold = usage_lim,
    obs = nrow(estabelecimentos_iter_filter_unique_contacts),
    avg_pix_usage = round(mean(pix_usage_col, na.rm = T),2),
    unique_cnaes = length(unique(estabelecimentos_iter_filter_unique_contacts$sector)),
    unique_munics = length(unique(estabelecimentos_iter_filter_unique_contacts$munic))
  )
  
  stats_df = bind_rows(stats_df, stats_row)
  
}

save_path_stats = "pix/results/tables/"

colnames(stats_df) = c("Pix Usage Threshold", "Total Obs.", "Avg. Pix Usage", "# Unique Sectors", "# Unique Munic.")

stats_df %>%
  kable() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Contacts Data Statistics - MEIs and MEs - No CNAE Filters" = 5), bold = TRUE) %>%
  # add_header_above(c("Contacts Data Statistics - MEIs and MEs - With CNAE Filters" = 5), bold = TRUE) %>%
  row_spec(0, bold = TRUE) %>% 
  # footnote(general = "Terms used to eliminate sectors: atacad, laboratório, fabricação, cultivo, produção, criação, extração, pesca, abate, coleta, metalurgia, fundição, horticultura, apicultura, beneficiamento.") %>%
  footnote(general = "Includes all sector codes (CNAEs) provided in the Central Bank's data.") %>%
  save_kable(file = paste0(save_path_stats,"Contacts Data Statistics - MEIs and MEs - No CNAE Filters", ".png"), self_contained = T)

stats_df %>%
  kable() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Contacts Data Statistics - MEIs and MEs - No CNAE Filters" = 5), bold = TRUE) %>%
  # add_header_above(c("Contacts Data Statistics - MEIs and MEs - With CNAE Filters" = 5), bold = TRUE) %>%
  row_spec(0, bold = TRUE) %>% 
  # footnote(general = "Terms used to eliminate sectors: atacad, laboratório, fabricação, cultivo, produção, criação, extração, pesca, abate, coleta, metalurgia, fundição, horticultura, apicultura, beneficiamento.") %>%
  footnote(general = "Includes all sector codes (CNAEs) provided in the Central Bank's data.")
