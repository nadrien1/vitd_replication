
# Author: Will Simmons
# Replication of main study results from Adrien et al. (2020)
# Script: Creates Table 2
# Date: 2021-03-23
#
# packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(gtsummary)
library(gt)

# global ------------------------------------------------------------------

options(gtsummary.as_gt.addl_cmds = "gt::tab_options(table.font.size = 'small', data_row.padding = gt::px(1))")

# data import -------------------------------------------------------------

tbl1_dat <- readRDS(here("data/derived/tbl1_dat.rds"))
bd_uv_clean_filtered <- readRDS(here("data/derived/bd_uv_clean_filtered.rds"))
bd_uv_clean <- readRDS(here("data/derived/bd_uv_clean.rds"))

# creating table ----------------------------------------------------------
# all among controls:
  # stratified by low VitD (<65.21)
  # stratified by fall/winter conception
  # stratified by low UV conception month (<3)

tbl2_dat <-
  tbl1_dat %>%
  filter(C_CC_char == "Control")

table2_low_vitd <-
  tbl2_dat %>%
  tbl_summary(by = "low_vitd") %>%
  as_gt() %>%
  tab_header(md("**TABLE 2a** | Maternal demographic characteristics from the National Birth Defects Prevention Study (1997-2011), by low dietary vitamin D (< 65.21 IU)"),
             subtitle = "Among controls") %>%
  opt_align_table_header("left")

table2_fall_winter_concep <-
  tbl2_dat %>%
  tbl_summary(by = "fwconcep") %>%
  as_gt() %>%
  tab_header(md("**TABLE 2b** | Maternal demographic characteristics from the National Birth Defects Prevention Study (1997-2011), by fall/winter conception status"),
             subtitle = "Among controls") %>%
  opt_align_table_header("left")

table2_low_uv_concep <-
  tbl2_dat %>%
  tbl_summary(by = "lowuv") %>%
  as_gt() %>%
  tab_header(md("**TABLE 2c** | Maternal demographic characteristics from the National Birth Defects Prevention Study (1997-2011), by low UV month conception status"),
             subtitle = "Among controls") %>%
  opt_align_table_header("left")

table2s <- list(table2_low_vitd, table2_fall_winter_concep, table2_low_uv_concep)
table2s <- setNames(table2s, c("table2a", "table2b", "table2c"))

# export ------------------------------------------------------------------

saveRDS(table2s, here("output/table2s.rds"))
imap(table2s, ~gtsave(.x, filename = paste0(.y, ".pdf"), here("output")))
