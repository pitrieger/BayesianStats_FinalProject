library(tidyverse)
library(haven)
library(here)


## load data ====
# "" applause
load(here("data", "Part1_Exploratory.RData"))
dat = BT_17_19 %>% filter(!presidency & period == 17)

# "" MP characteristics
#https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/QSFXLQ/PN31TE&version=1.0

MP = read_dta("data/mp_characteristics.dta")
MP = MP %>% filter(elecper == 17)

unique(dat$SPEAKER_raw)
