# combine TRY and BIEN datasets at individual levels
library(tidyverse)

###################### TRY ######################
# read individual data of TRY
try.ind <- read.csv("data/indi.trait_longform.csv") # long form data with individuals

# rename traits; the same as the beginning
try.ind$TraitName[try.ind$TraitName == "14_Leaf nitrogen (N) content per leaf dry mass_mg/g"] <- "leafN"
try.ind$TraitName[try.ind$TraitName == "80_Root nitrogen (N) content per root dry mass_mg/g"] <- "rootN"
try.ind$TraitName[try.ind$TraitName == "82_Root tissue density (root dry mass per root volume)_g/cm3"] <- "RTD"
try.ind$TraitName[try.ind$TraitName == "1080_Root length per root dry mass (specific root length, SRL)_cm/g"] <- "SRL"
try.ind$TraitName[try.ind$TraitName == "26_Seed dry mass_mg"] <- "seed.mass"
try.ind$TraitName[try.ind$TraitName == "4_Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density_g/cm3"] <- "SSD"
try.ind$TraitName[try.ind$TraitName == "3107_Plant height generative_m"] <- "generative.height"
try.ind$TraitName[try.ind$TraitName == "3106_Plant height vegetative_m"] <- "vegetation.height"
try.ind$TraitName[try.ind$TraitName == "47_Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)_g/g"] <- "LDMC"
try.ind$TraitName[try.ind$TraitName == "3110_Leaf area (in case of compound leaves: leaf, petiole included)_mm2"] <- "LA.inc"
try.ind$TraitName[try.ind$TraitName == "3108_Leaf area (in case of compound leaves: leaf, petiole excluded)_mm2"] <- "LA.ex"
try.ind$TraitName[try.ind$TraitName == "3112_Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)_mm2"] <- "LA.un"
try.ind$TraitName[try.ind$TraitName == "3111_Leaf area (in case of compound leaves: leaflet, petiole included)_mm2"] <- "LA.leaflet.inc"
try.ind$TraitName[try.ind$TraitName == "3109_Leaf area (in case of compound leaves: leaflet, petiole excluded)_mm2"] <- "LA.leaflet.ex"
try.ind$TraitName[try.ind$TraitName == "3113_Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)_mm2"] <- "LA.leaflet.un"
try.ind$TraitName[try.ind$TraitName == "3114_Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)_mm2"] <- "LA.un.un"
try.ind$TraitName[try.ind$TraitName == "3116_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included_mm2 mg-1"] <- "SLA.inc"
try.ind$TraitName[try.ind$TraitName == "3115_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded_mm2 mg-1"] <- "SLA.ex"
try.ind$TraitName[try.ind$TraitName == "3117_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded_mm2 mg-1"] <- "SLA.un"
try.ind$TraitName[try.ind$TraitName == "3086_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded_mm2 mg-1"] <- "SLA.all.ex"

unique(try.ind$TraitName)

# select traits
try.ind.select = try.ind %>% 
  filter(TraitName %in% c("leafN", "LDMC", "SLA.inc", "SLA.ex")) %>%
  select(AccSpeciesName, TraitName, StdValue) %>%
  rename(species = AccSpeciesName, trait_value = StdValue, 
         trait_name = TraitName)

# make the name consistent
try.ind.select$species[try.ind.select$species == "Draba verna"] <- "Erophila verna"   
try.ind.select$species[try.ind.select$species == "CAPSELLA BURSA-PASTORIS"] <- "Capsella bursa-pastoris" 

# add a new column of database name
try.ind.select$database <- "TRY"

###################### BIEN ######################
bien.ind <- read.csv("data/trait_bien_select.csv")

# rename traits in bien
bien.ind$trait_name[bien.ind$trait_name == "seed mass"] <- "seed.mass"
bien.ind$trait_name[bien.ind$trait_name == "leaf dry mass per leaf fresh mass"] <- "LDMC"
bien.ind$trait_name[bien.ind$trait_name == "leaf area per leaf dry mass"] <- "SLA"
bien.ind$trait_name[bien.ind$trait_name == "leaf area"] <- "LA"
bien.ind$trait_name[bien.ind$trait_name == "whole plant height"] <- "height"
bien.ind$trait_name[bien.ind$trait_name == "maximum whole plant height"] <- "mix.height"
bien.ind$trait_name[bien.ind$trait_name == "leaf nitrogen content per leaf dry mass"] <- "leafN"

# select traits
bien.ind.select = bien.ind %>% 
  filter(trait_name %in% c("LDMC", "SLA", "leafN")) %>%
  select(scrubbed_species_binomial, trait_name, trait_value) %>%
  rename(species = scrubbed_species_binomial)

# add a new column of database name
bien.ind.select$database <- 'BIEN'

# merge data from TRY and BIEN
combine.try.bien <- rbind(try.ind.select, bien.ind.select) %>%
  unite(trait_ID, trait_name, database, remove = F)

# check the units.
# convert g/g in TRY to mg/g
combine.indi = combine.try.bien %>% 
  mutate(trait_value = if_else(trait_ID == "LDMC_TRY", trait_value * 1000, trait_value)) 

# calculate species mean trait value; long to wide
combine.sp.wide = combine.indi %>% 
  group_by(species, trait_ID) %>% 
  summarise(species.mean.trait = mean(trait_value, na.rm = T)) %>%
  spread(trait_ID, species.mean.trait)

# fill missing values within TRY

# SLA: select 3116 (include petiole; SLA.inc), fill the gaps using the values from 3115 (SLA.ex)
sp_used.SLA.ex.TRY = combine.sp.wide %>%
  filter(is.na(SLA.inc_TRY) & !is.na(SLA.ex_TRY)) %>%
  select(species, SLA.ex_TRY)

combine.sp.fill = combine.sp.wide %>%
  mutate(SLA = if_else(is.na(SLA.inc_TRY), SLA.ex_TRY, SLA.inc_TRY)) %>%
  select(species, SLA.inc_TRY, SLA.ex_TRY, SLA, everything())

#fill the missing value in TRY using data from BIEN

# SLA
sp_used.SLA.BIEN = combine.sp.fill %>%
  filter(is.na(SLA) & !is.na(SLA_BIEN)) %>%
  select(species, SLA_BIEN) # no species

# LDMC
sp_used.LDMC.BIEN = combine.sp.fill %>%
  filter(is.na(LDMC_TRY) & !is.na(LDMC_BIEN)) %>%
  select(species, LDMC_BIEN)

combine.sp.fill = combine.sp.fill %>%
  mutate(LDMC = if_else(is.na(LDMC_TRY), LDMC_BIEN, LDMC_TRY)) %>%
  select(species, LDMC_BIEN, LDMC, everything())

# leafN
sp_used.leafN.BIEN = combine.sp.fill %>%
  filter(is.na(leafN_TRY) & !is.na(leafN_BIEN)) %>%
  select(species, leafN_BIEN)

combine.sp.fill = combine.sp.fill %>%
  mutate(leafN = if_else(is.na(leafN_TRY), leafN_BIEN, leafN_TRY)) %>%
  select(species, leafN_BIEN, leafN, everything())

# select the traits
combine.final = combine.sp.fill %>% 
  select(species, leafN, LDMC, SLA)

# species level data
write.csv(combine.final, 'data/combined.TRY.BIEN_species_level.csv', row.names = F)

# merge species used in different databases
dfs <- list(sp_used.SLA.ex.TRY, sp_used.SLA.BIEN, sp_used.LDMC.BIEN, sp_used.leafN.BIEN)

sp_used <- Reduce(function(x, y) full_join(x, y, by = "species"), dfs)

# wide to long
sp_used_long = sp_used %>% 
  pivot_longer(cols = SLA.ex_TRY:leafN_BIEN, names_to = "trait_ID", 
               values_to = "trait_value") %>%
  drop_na(trait_value)

# select individual data used to fill the gaps from TRY
combine.indi.used = combine.indi %>% semi_join(sp_used_long, by = c("species", "trait_ID"))

# select all the data used in TRY
TRY.used = combine.indi %>% filter(trait_ID %in% c("leafN_TRY", "LDMC_TRY", "SLA.inc_TRY"))

# combine the two dataframes above; outlier should be checked based on 'all.indi.used.csv'
all.indi.used <- rbind(TRY.used, combine.indi.used) %>%
  mutate(trait_name = recode(trait_name,
                             "SLA.inc" = "SLA",
                             "SLA.ex"  = "SLA"))
  
write.csv(all.indi.used, 'data/all.indi.used.csv', row.names = F)
