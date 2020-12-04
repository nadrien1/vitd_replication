
# Author: Will Simmons
# Replication of main study results from Adrien et al. (2020)
# Script: Clean and wrangle data
# Date: 2020-10-06
#
# packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

# data import -------------------------------------------------------------

bd_uv <- readRDS(here("data", "derived", "bd_uv.rds"))
vitd_tertiles <- readRDS(here("data", "derived", "vitd_tertiles.rds"))

# wrangle -----------------------------------------------------------------

bd_uv_clean <-
  bd_uv %>%
  mutate(

    # First, making dates readable by R
    across(
      c(C_BDATE_STD, C_CONCEP_STD),
      ~ lubridate::mdy(.)
    ),

    # Fill missing value for est. date of conception - 2000-02-28
    C_CONCEP_STD = case_when(is.na(C_CONCEP_STD) & C_BDATE_STD == "2000-11-21" ~ lubridate::ymd("2000-02-28"),
                             TRUE ~ C_CONCEP_STD),

    # Demographics and baseline vars
    cat_age = case_when(C_agecat %in% c(1,2) ~ "<20 years old",
                        C_agecat == 3 ~ "20 - 24 years old",
                        C_agecat == 4 ~ "25 - 29 years old",
                        C_agecat == 5 ~ "30 - 34 years old",
                        C_agecat == 6 ~ "35 - 39 years old",
                        C_agecat %in% c(7,8,9) ~ "40+ years old",
                        C_agecat %in% c(888, 999) ~ NA_character_
    ),
    cat_race = case_when(C_mrace == 11 ~ "White",
                         C_mrace == 12 ~ "Black",
                         C_mrace == 13 ~ "Hispanic",
                         C_mrace %in% c(14, 15, 16) ~ "Other",
                         C_mrace == 999 ~ NA_character_
    ),
    cat_edu = case_when(c_meduc == 999 | is.na(c_meduc) ~ NA_real_,
                        TRUE ~ c_meduc2),
    cat_parity = case_when(NUMPG == 0 ~ "First birth",
                           NUMPG %in% (1:15) ~ "2+ births"
                           # else NA
    ),
    cat_smoke = na_if(C_SMOKE, 999),  # equals c_smoke, NA if 999
    cat_alc = na_if(C_ALCOHOL, 999),

    # UV index - joined from UV dataset/Nedghie's complex exposure assignment
    cat_uv = case_when(UV >= 0 & UV < 3 ~ "Low",
                       UV >= 3 & UV < 6 ~ "Moderate",
                       UV >= 6 ~ "High"
    ),

    # Nutrition
    log_vd = log(VITD),
    log_kcal = log(ENERC_KCAL),
    newbmi = case_when(C_BMINIH %in% c(1,2,3,4) ~ C_BMINIH,
                       C_BMINIH %in% c(888,999) ~ NA_real_),

    # Case/control
    case = case_when(C_CC == 9 ~ NA_real_,
                     TRUE ~ C_CC),

    # Study center
    study_center = case_when(LOCID == 10 ~ "AK",
                             LOCID == 11 ~ "CA",
                             LOCID == 12 ~ "IA",
                             LOCID == 13 ~ "MA",
                             LOCID == 14 ~ "NG",
                             LOCID == 15 ~ "NY",
                             LOCID == 16 ~ "TX",
                             LOCID == 17 ~ "GA",
                             LOCID == 18 ~ "NC",
                             LOCID == 19 ~ "UT"),

    # Vitamin D from supplementation
    anypvb3p2 = case_when(GPPD1 == 1 | GPPD2 == 1 | GPPD3 == 1 ~ 1,
                          GPPD1 == 0 & GPPD2 == 0 | GPPD3 == 0 ~ 0,
                          TRUE ~ NA_real_),
    anymvb3p2 = case_when(GMVPD1 == 1 | GMVPD2 == 1 | GMVPD3 == 1 ~ 1,
                          GMVPD1 == 0 & GMVPD2 == 0 | GMVPD3 == 0 ~ 0,
                          TRUE ~ NA_real_),
    anypmv = case_when(anypvb3p2 == 0 ~ case_when(anymvb3p2 == 0 ~ 0,
                                                  anymvb3p2 == 1 ~ 1,
                                                  is.na(anymvb3p2) ~ 0),
                       anypvb3p2 == 1 ~ case_when(anymvb3p2 == 0 ~ 1,
                                                  anymvb3p2 == 1 ~ 1,
                                                  is.na(anymvb3p2) ~ 1),
                       is.na(anypvb3p2) ~ case_when(anymvb3p2 == 0 ~ 0,
                                                  anymvb3p2 == 1 ~ 1,
                                                  is.na(anymvb3p2) ~ NA_real_)
    ),
    singlevd = case_when(vd_ExpB3P2 == 1 ~ 1,
                         vd_ExpB3P2 == 2 ~ 0,
                         vd_ExpB3P2 == 8 ~ NA_real_,
                         vd_ExpB3P2 == 0 ~ NA_real_),
    anysupp = case_when(anypmv == 0 ~ case_when(singlevd == 0 ~ 0,
                                                singlevd == 1 ~ 1,
                                                is.na(singlevd) ~ 0),
                        anypmv == 1 ~ case_when(singlevd == 0 ~ 1,
                                                singlevd == 1 ~ 1,
                                                is.na(singlevd) ~ 1),
                        is.na(anypmv) ~ case_when(singlevd == 0 ~ 0,
                                                  singlevd == 1 ~ 1,
                                                  is.na(singlevd) ~ 0)

    ),
    Vit_D_tert = case_when(VITD >= 0 & VITD < vitd_tertiles$tertiles[[1]] ~ 0,
                           VITD >= vitd_tertiles$tertiles[[1]] & VITD < vitd_tertiles$tertiles[[2]] ~ 1,
                           VITD >= vitd_tertiles$tertiles[[2]] ~ 2),
    UV_index_cat = case_when(UV >= 0 & UV < 3 ~ 0,
                             UV >= 3 & UV < 6 ~ 1,
                             UV >= 6 ~ 2),
    vdsuppnum = anypmv + singlevd,
    vdsuppnum2 = case_when(!is.na(anypmv) & singlevd == 1 ~ 2,
                           TRUE ~ vdsuppnum),
    missmorethan1 = case_when(MISSING %in% c(0, 1) ~ 0,
                              MISSING > 1 ~ 1),
    kcal_ls500 = case_when(ENERC_KCAL < 500 ~ 1,
                           ENERC_KCAL >= 500 ~ 0),
    kcal_gr5000 = case_when(ENERC_KCAL > 5000 ~ 1,
                            ENERC_KCAL <= 5000 ~ 0),
    missingkcal = case_when(is.na(ENERC_KCAL) ~ 1,
                            TRUE ~ 0),
    kcal_ex = case_when(kcal_ls500 == 1 | kcal_gr5000 == 1 ~ 1,
                        TRUE ~ 0),
    north = case_when(LOCID %in% c(10,11,16:19) ~ 0,
                      LOCID %in% c(12:15) ~ 1),
    recvd = case_when(VITD >= 600 ~ 1,
                      VITD < 600 ~ 0),
    lowuv = case_when(UV_index_cat == 0 ~ 1,
                      TRUE ~ 0),
    lowdiet = case_when(Vit_D_tert == 0 ~ 1,
                        Vit_D_tert %in% c(1,2) ~ 0),
    month_day = month(C_CONCEP_STD)*100 + day(C_CONCEP_STD),  # kind of a cheat - creates month-day var
    sconcep = case_when(month_day >= 620 & month_day <= 921 ~ 0,  # summer 6/20-9/21
                        month_day >= 320 & month_day <= 619 ~ 1,  # spring 3/20-6/19
                        month_day >= 922 & month_day <= 1220 ~ 2,  # fall 9/22-12/20
                        month_day >= 1221 | month_day <= 319 ~ 3  # winter 12/21-3/19
    ),
    fwconcep = case_when(sconcep %in% c(0,1) ~ 0,
                         sconcep %in% c(2,3) ~ 1),
    newseasons = case_when(sconcep %in% c(1,3) ~ 0,
                           sconcep %in% c(0,2) ~ 1)

  )

bd_uv_clean_filtered <-
  bd_uv_clean %>%
  filter(
    missmorethan1 == 0 &
    #MISSING %in% c(0,1) &
    missingkcal == 0 &
    #!is.na(ENERC_KCAL) &
    kcal_ex == 0
    #kcal_ls500 != 1 & kcal_gr5000 != 1
  )

# export ------------------------------------------------------------------

saveRDS(bd_uv_clean, here("data", "derived", "bd_uv_clean.rds"))
saveRDS(bd_uv_clean_filtered, here("data", "derived", "bd_uv_clean_filtered.rds"))
