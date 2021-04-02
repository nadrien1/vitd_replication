
# Author: Will Simmons
# Replication of main study results from Adrien et al. (2020)
# Script: Confirm case/control counts
# Date: 2020-11-09
#
# packages ----------------------------------------------------------------

library(tidyverse)
library(here)

# data import -------------------------------------------------------------

bd <- read_csv(here("data", "BD9302_Analytic_VITD.csv")) %>%
  as_tibble()

bd_uv_clean_filtered <- readRDS(here("data", "derived", "bd_uv_clean_filtered.rds"))
bd_uv_clean <- readRDS(here("data", "derived", "bd_uv_clean.rds"))

# replicating case/control counts -----------------------------------------

bd_uv_clean_filtered %>% count(case)

# with Nedghie's variables
bd_uv_clean %>%
  filter(
    nedghie_missmorethan1 == 0 &  # differing MISSING counts
    nedghie_missingkcal == 0 &  # differing kcal distributions
    nedghie_kcal_ex == 0
  ) %>% count(nedghie_case)  # same as nedghie_data

# where do nedghie's variables not equal mine?
bd_uv_clean %>%
  filter(
    missmorethan1 != nedghie_missmorethan1 |
    missingkcal != nedghie_missingkcal |
    nedghie_kcal_ex != kcal_ex |
    case != nedghie_case
  ) %>%
  select(id, ends_with("than1"), ends_with("missingkcal"), ends_with("_ex"), matches("case")) %>%
  view
  # 167 unequal rows between my and Nedghie's calculated variables
    # troubleshoot my code
    # see how much is due to differences in ENERC_KCAL, VITD, case


# nedghie's cases / my NAs - what defect? ---------------------------------

bd_uv_clean_filtered %>%
  filter(is.na(case)) %>%
  select(anyheart, conotruncal, septal, rvoto, lvoto, limb, diaphragm, gastroschisis, ear,
         craniosyn, clpwwcp, cleftpalate, ntd_any, anencephaly, spinabifida, hydroceph,
         esophsummary, esophageal, anorectal, hypospadias)

nedghie_data_clean <- readRDS(here("data", "existing", "nedghie_data_clean.rds"))

mock_nedghie_data <-
  bd %>%
  left_join(
    nedghie_data_clean,
    by = c("id" = "Id")
  ) %>%
  filter(
    nedghie_missmorethan1 == 0 &
    #MISSING %in% c(0,1) &
    nedghie_missingkcal == 0 &
    #!is.na(ENERC_KCAL) &
    nedghie_kcal_ex == 0
    #kcal_ls500 != 1 & kcal_gr5000 != 1
  ) %>%
  select(C_CC, anyheart, conotruncal, septal, rvoto, lvoto, limb, diaphragm, gastroschisis, ear,
         craniosyn, clpwwcp, cleftpalate, ntd_any, anencephaly, spinabifida, hydroceph,
         esophsummary, esophageal, anorectal, hypospadias) %>%
  filter(C_CC %in% c(1,9))

colSums(mock_nedghie_data, na.rm = T)
# these sums don't look like what's in the manuscript


# notes -------------------------------------------------------------------

# difference of 7 between total of N's cases and controls (41517) and my clean dataset (41524)
# my cases (30,177) plus my NAs (173) equal N's cases minus 1 (30,349)
# difference of 6 between my controls (11,174) and Nedghie's controls (11,168)

# to make my dataset into Nedghie's (in terms of cases/controls):
  # 1. remove 6 controls, to match our controls at 11,168
  # 2. remove 1 of my NAs and add remaining 172 NAs to cases, to match our cases at 30,349
