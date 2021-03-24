
# Author: Will Simmons
# Replication of main study results from Adrien et al. (2020)
# Script: Creates Table 1
# Date: 2020-12-03
#
# packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(gtsummary)
library(gt)

# global ------------------------------------------------------------------

options(gtsummary.as_gt.addl_cmds = "gt::tab_options(table.font.size = 'small', data_row.padding = gt::px(1))")

# data import -------------------------------------------------------------

bd_uv_clean_filtered <- readRDS(here("data", "derived", "bd_uv_clean_filtered.rds"))
bd_uv_clean <- readRDS(here("data", "derived", "bd_uv_clean.rds"))

# creating table ----------------------------------------------------------

tbl1_dat <-
  bd_uv_clean_filtered %>%
  # fixing labels, etc.
  filter(C_CC %in% c(0, 1)) %>%  # excluding case/control == NA, 3, 9, etc.
  mutate(
    low_vitd = case_when(Vit_D_tert == 0 ~ 1,
                         TRUE ~ 0),
    C_CC_char = case_when(C_CC == 0 ~ "Control",
                          C_CC == 1 ~ "Case"),
    C_CC_char = fct_reorder(C_CC_char, C_CC),
    cat_smoke = case_when(cat_smoke == 1 ~ "Yes",
                          cat_smoke == 0 ~ "No"),
    cat_alc = case_when(cat_alc == 1 ~ "Yes",
                        cat_alc == 0 ~ "No"),
    anypmv = case_when(anypmv == 1 ~ "Yes",
                       anypmv == 0 ~ "No"),
    north = case_when(north == 1 ~ "Yes",
                      north == 0 ~ "No"),
    lowuv = case_when(lowuv == 1 ~ "Yes",
                      lowuv == 0 ~ "No"),
    fwconcep = case_when(fwconcep == 1 ~ "Yes",
                         fwconcep == 0 ~ "No"),
    low_vitd = case_when(low_vitd == 1 ~ "Yes",
                         low_vitd == 0 ~ "No")
  ) %>%
  select(C_CC_char,
         cat_age,
         cat_race,
         newbmi,
         cat_edu,
         cat_smoke,
         cat_alc,
         cat_parity,
         anypmv,
         north,
         lowuv,
         fwconcep,
         low_vitd) %>%
  labelled::set_variable_labels(
    C_CC_char = "Case-control status",
    cat_age = "Age at delivery in years",
    cat_race = "Race/ethnicity",
    newbmi = "Pre-pregnancy BMI",
    cat_edu = "Education in years",
    cat_smoke = "Cigarette smoking (B1- P3)",
    cat_alc = "Alcohol consumption (B1 - P3)",
    cat_parity = "Gravidity",
    anypmv = "Periconceptional multivitamin or prenatal vitamin",
    north = "Northern study center (>37N)",
    lowuv = "Low UV conception month",
    fwconcep = "Fall/winter conception",
    low_vitd = "Low dietary Vitamin D"
  )

table1 <-
  tbl1_dat %>%
  tbl_summary(by = "C_CC_char",
              type = list(everything() ~ "categorical")) %>%
  as_gt() %>%
  tab_header(md("**TABLE 1** | Maternal demographic characteristics from the National Birth Defects Prevention Study (1997-2011) by case-control status")) %>%
  gt::opt_align_table_header("left")

# export ------------------------------------------------------------------

saveRDS(table1, here("output", "table1.rds"))
saveRDS(tbl1_dat, here("data/derived/tbl1_dat.rds"))
gtsave(table1, "table1.pdf", here("output"))
