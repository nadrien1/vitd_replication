
# Author: Will Simmons
# Replication of main study results from Adrien et al. (2020)
# Script: Creates Table 3 (analytic)
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

bd_uv_clean_filtered <- readRDS(here("data/derived/bd_uv_clean_filtered.rds"))
bd_uv_clean <- readRDS(here("data/derived/bd_uv_clean.rds"))

# model wrangling ---------------------------------------------------------

bd_model <-
  bd_uv_clean_filtered %>%
  select(
    # covariates
    cat_age,
    cat_race,
    newbmi,
    cat_edu,
    cat_smoke,
    cat_alc,
    cat_anypmv = anypmv,
    log_kcal,
    cat_study_center = study_center,
    # exposure
    vitd_tertile = Vit_D_tert,
    # outcomes
    dx_ntd_any = ntd_any,
    dx_anencephaly = anencephaly,
    dx_spinabifida = spinabifida,
    dx_hypospadias = hypospadias,
    dx_anyheart = anyheart,
    dx_contruncal = conotruncal,
    dx_septal = septal,
    dx_rvoto = rvoto,
    dx_lvoto = lvoto,
    dx_limb = limb,
    dx_diaphragm = diaphragm,
    dx_gastroschisis = gastroschisis,
    dx_craniosyn = craniosyn,
    dx_clpwwcp = clpwwcp,
    dx_cleftpalate = cleftpalate
  ) %>%
  mutate(across(c(starts_with("cat"), starts_with("dx_"), vitd_tertile),
                ~ as.factor(.))
  ) %>%
  mutate(
    vitd_tertile = case_when(vitd_tertile == 0 ~ "low",
                             vitd_tertile == 1 ~ "mid",
                             vitd_tertile == 2 ~ "hi"),
    vitd_tertile = relevel(as.factor(vitd_tertile), ref = "hi")
  )

# analyses ----------------------------------------------------------------

analyze <- function(data = bd_model, outcome) {

  covs <- c("cat_age",
            "cat_race",
            "newbmi",
            "cat_edu",
            "cat_smoke",
            "cat_alc",
            "cat_anypmv",
            "log_kcal",
            "cat_study_center")

  crude_formul <- reformulate("vitd_tertile", outcome)

  crude <- data %>%
    glm(data = .,
        formula = crude_formul,
        family = "binomial") %>%
    broom::tidy() %>%
    mutate(or = exp(estimate),
           low_ci = exp(estimate - 1.96*std.error),
           hi_ci = exp(estimate + 1.96*std.error)
    ) %>%
    filter(term %in% c("vitd_tertilelow", "vitd_tertilemid")) %>%
    select(term, or, low_ci, hi_ci)

  adj_formul <- reformulate(c("vitd_tertile", covs), outcome)

  adj <- data %>%
    glm(data = .,
        formula = adj_formul,
        family = "binomial") %>%
    broom::tidy() %>%
    mutate(or = exp(estimate),
           low_ci = exp(estimate - 1.96*std.error),
           hi_ci = exp(estimate + 1.96*std.error)
    ) %>%
    filter(term %in% c("vitd_tertilelow", "vitd_tertilemid")) %>%
    select(term, or, low_ci, hi_ci)

    list(crude = crude,
         adj = adj)

}

# running -----------------------------------------------------------------

dx_list <- bd_model %>% select(starts_with("dx_")) %>% names

setNames(map(dx_list, ~analyze(outcome = .x)), dx_list)
