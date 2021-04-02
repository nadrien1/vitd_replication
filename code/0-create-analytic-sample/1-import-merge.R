
# Author: Will Simmons
# Replication of main study results from Adrien et al. (2020)
# Script: Import data and merge
# Date: 2020-10-06
#
# packages ----------------------------------------------------------------

library(tidyverse)
library(here)

# data import -------------------------------------------------------------

# BD data
bd <- read_csv(here("data", "BD9302_Analytic_VITD.csv")) %>%
  as_tibble()

ineligible <- bd %>% filter(C_CC == 9) %>% pull(id)

# Nedghie's derived data - using her derived UV variable, and using to test discrepancies w/ NBDPS data

# UV data
# uv <- read_csv(here("data", "uv.csv")) %>%
#   as_tibble()

# derived UV variable - need ID and UV?
nedghie_data <- read_csv(here("data", "UVMERGEDFILE.csv")) %>%
  as_tibble()

nedghie_data_clean <- nedghie_data %>%
  select(Id, UV, nedghie_vitd = VITD, nedghie_missingkcal = missingkcal,
         nedghie_kcal_ex = kcal_ex,
         nedghie_case = case, nedghie_ENERC_KCAL = ENERC_KCAL, nedghie_MISSING = Missing,
         nedghie_missmorethan1 = missmorethan1)

nedghie_data_clean_defects <- nedghie_data %>%
  select(Id, UV, nedghie_C_CC = C_cc, nedghie_vitd = VITD, nedghie_missingkcal = missingkcal,
         nedghie_kcal_ex = kcal_ex,
         nedghie_case = case, nedghie_ENERC_KCAL = ENERC_KCAL,
         nedghie_missmorethan1 = missmorethan1, nedghie_MISSING = Missing,
         vdtert,
         anyheart, conotruncal, septal, rvoto, lvoto, limb, diaphragm, gastroschisis, ear,
         craniosyn, clpwwcp, cleftpalate, ntd_any, anencephaly, spinabifida, hydroceph,
         esophsummary, esophageal, anorectal, hypospadias)

# join --------------------------------------------------------------------

# adding derived UV variable to BD data
bd_uv <- bd %>%
  left_join(nedghie_data_clean, by = c("id" = "Id"))

# export ------------------------------------------------------------------

# creates subfolder "derived" in data folder, if it doesn't already exist
if(!dir.exists(here("data", "derived"))) {
  dir.create(here("data", "derived"))
}

saveRDS(ineligible, here("data/derived/ineligible_case_ids.rds"))
saveRDS(bd_uv, here("data", "derived", "bd_uv.rds"))
saveRDS(nedghie_data, here("data", "existing", "nedghie_data.rds"))
saveRDS(nedghie_data_clean, here("data", "existing", "nedghie_data_clean.rds"))
