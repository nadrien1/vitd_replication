
# Author: Will Simmons
# Replication of main study results from Adrien et al. (2020)
# Script: Create UV tertiles
# Date: 2020-10-18
#
# packages ----------------------------------------------------------------

library(tidyverse)
library(here)

# data import -------------------------------------------------------------

bd_uv <- readRDS(here("data", "derived", "bd_uv.rds"))

# create tertiles ---------------------------------------------------------

vitd_tertiles <- bd_uv %>%
  filter(nedghie_case == 0 &  # among controls
         nedghie_missmorethan1 == 0 &
         nedghie_ENERC_KCAL > 500 &
         nedghie_ENERC_KCAL < 5000 &
         !is.na(nedghie_ENERC_KCAL)           # ┌	tertile values used in Nedghie's code
  ) %>%                                       # |
  summarize(tertiles = quantile(nedghie_vitd, probs = c(0.333, 0.667), na.rm = T, type = 2))
                                # |
                                # └ replace with VITD to get estimate with CDC data

# export ------------------------------------------------------------------

saveRDS(vitd_tertiles, here("data", "derived", "vitd_tertiles.rds"))
