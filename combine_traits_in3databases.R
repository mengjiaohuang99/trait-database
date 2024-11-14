library(tidyverse)
rm(list=ls())

###################### TRY ######################
# read data: TRY
setwd("D:/RECODYN_Pro/哈维性状/TRY database/31423_27022024092721")
try.mean.sp <- read.csv("mean_traits_species.csv") # mean value of each species
colnames(try.mean.sp)

#rename traits
try.mean.sp1 = try.mean.sp %>% rename(leafN = X14_Leaf.nitrogen..N..content.per.leaf.dry.mass_mg.g,
                                      rootN = X80_Root.nitrogen..N..content.per.root.dry.mass_mg.g,
                                      RTD = X82_Root.tissue.density..root.dry.mass.per.root.volume._g.cm3,
                                      seed.mass = X26_Seed.dry.mass_mg,
                                      LDMC = X47_Leaf.dry.mass.per.leaf.fresh.mass..leaf.dry.matter.content..LDMC._g.g,
                                      vegetation.height = X3106_Plant.height.vegetative_m,
                                      SSD = X4_Stem.specific.density..SSD..stem.dry.mass.per.stem.fresh.volume..or.wood.density_g.cm3,
                                      SRL = X1080_Root.length.per.root.dry.mass..specific.root.length..SRL._cm.g,
                                      LA.ex = X3108_Leaf.area..in.case.of.compound.leaves..leaf..petiole.excluded._mm2,
                                      LA.leaflet.ex = X3109_Leaf.area..in.case.of.compound.leaves..leaflet..petiole.excluded._mm2,
                                      LA.inc = X3110_Leaf.area..in.case.of.compound.leaves..leaf..petiole.included._mm2,
                                      LA.leaflet.inc = X3111_Leaf.area..in.case.of.compound.leaves..leaflet..petiole.included._mm2,
                                      LA.un = X3112_Leaf.area..in.case.of.compound.leaves..leaf..undefined.if.petiole.in..or.excluded._mm2,
                                      LA.leaflet.un = X3113_Leaf.area..in.case.of.compound.leaves..leaflet..undefined.if.petiole.is.in..or.excluded._mm2,
                                      LA.un.un = X3114_Leaf.area..in.case.of.compound.leaves.undefined.if.leaf.or.leaflet..undefined.if.petiole.is.in..or.excluded._mm2,
                                      SLA.ex = X3115_Leaf.area.per.leaf.dry.mass..specific.leaf.area..SLA.or.1.LMA...petiole.excluded_mm2.mg.1,
                                      SLA.inc = X3116_Leaf.area.per.leaf.dry.mass..specific.leaf.area..SLA.or.1.LMA...petiole.included_mm2.mg.1,
                                      SLA.un = X3117_Leaf.area.per.leaf.dry.mass..specific.leaf.area..SLA.or.1.LMA...undefined.if.petiole.is.in..or.excluded_mm2.mg.1,
                                      SLA.all.ex = X3086_Leaf.area.per.leaf.dry.mass..specific.leaf.area..SLA.or.1.LMA..petiole..rhachis.and.midrib.excluded_mm2.mg.1,
                                      species = AccSpeciesName)
# select traits
try.mean.sp_select = try.mean.sp1 %>% select(-c(X3107_Plant.height.generative_m))

# leaf area: select include prtiole 3110 (LA.inc), fill the gaps using values from 3108 (LA.ex), 3112 (LA.un) ; see Carmona et al. 2020
try.mean.sp_select$LA <- coalesce(try.mean.sp_select$LA.inc, try.mean.sp_select$LA.ex)
try.mean.sp_select$LA <- coalesce(try.mean.sp_select$LA, try.mean.sp_select$LA.un)
# SLA: select 3116 (include petiole; SLA.inc), fill the gaps using the values from 3115
try.mean.sp_select$SLA <- coalesce(try.mean.sp_select$SLA.inc, try.mean.sp_select$SLA.ex)

#select trait again
# try.mean.sp_select1 <- try.mean.sp_select[, c(2:10, 22:23)]
try.mean.sp_select1 = try.mean.sp_select %>% select(species, leafN, rootN, RTD, seed.mass, LDMC, 
                                                    vegetation.height, SSD, SRL, LA, SLA)

###################### BIEN ######################
setwd("D:/RECODYN_Pro/哈维性状")
bien.ind <- read.csv("trait_bien_select.csv")

# mean trait value of each species
bien.mean.sp = bien.ind %>% group_by(trait_name, scrubbed_species_binomial) %>% summarise(mean.trait = mean (trait_value, na.rm = T))

#long to wide 
bien.mean.sp.wide = bien.mean.sp %>% spread(trait_name, mean.trait)

#rename colnames to be the same as TRY dataset
bien.mean.sp.rename = bien.mean.sp.wide %>% rename(leafN.bien = `leaf nitrogen content per leaf dry mass`, seed.mass.bien = `seed mass`,
                                                   SLA.bien = `leaf area per leaf dry mass`, LA.bien = `leaf area`,
                                                   LDMC.bien = `leaf dry mass per leaf fresh mass`, species = scrubbed_species_binomial)

#keep the species names the same
#change 'Draba verna' to 'Erophila verna'; change 'CAPSELLA BURSA-PASTORIS' to 'Capsella bursa-pastoris'    
try.mean.sp_select1$species[try.mean.sp_select1$species == "Draba verna"] <- "Erophila verna"   
try.mean.sp_select1$species[try.mean.sp_select1$species == "CAPSELLA BURSA-PASTORIS"] <- "Capsella bursa-pastoris" 

#merge dataframe of TRY and BIEN
combine1 = full_join(try.mean.sp_select1, bien.mean.sp.rename, by = 'species')

# check unit to be the same; ONLY LDMC are different
# transfer g/g in TRY to mg/g
combine1$LDMC <- (combine1$LDMC)*1000

#fill the missing value using data from BIEN
#fill vegetation.height with 'whole plant height'
combine1$height <- coalesce(combine1$vegetation.height, combine1$`whole plant height`)
combine1$LA <- coalesce(combine1$LA, combine1$LA.bien)
combine1$SLA <- coalesce(combine1$SLA, combine1$SLA.bien)
combine1$LDMC <- coalesce(combine1$LDMC, combine1$LDMC.bien)
combine1$leafN <- coalesce(combine1$leafN, combine1$leafN.bien)
combine1$seed.mass <- coalesce(combine1$seed.mass, combine1$seed.mass.bien)

###################### GRooT ######################
setwd("D:/RECODYN_Pro/哈维性状/GRooT database")
groot.ind <- read.csv("GrooT_selected_3roottraits.csv")
colnames(groot.ind)

# mean trait value of each species
groot.mean.sp = groot.ind %>% group_by(traitName, speciesname) %>% summarise(mean.trait = mean(traitValue, na.rm = T))

#long to wide 
groot.mean.sp.wide = groot.mean.sp %>% spread(traitName, mean.trait)

#keep the species name the same as combine1; 
#change "Lysimachia arvensis" to "Anagallis arvensis" (But no this species)
groot.mean.sp.wide$speciesname[groot.mean.sp.wide$speciesname == "Lysimachia arvensis"] <- "Anagallis arvensis" 

#rename colnames to be the same as combine1
groot.mean.sp.rename = groot.mean.sp.wide %>% rename(species = speciesname,  rootN.groot = Root_N_concentration, 
                                                     RTD.groot  = Root_tissue_density, SRL.groot  = Specific_root_length)

#merge dataframe from GRooT to combine1
combine2 = full_join(groot.mean.sp.rename, combine1, by = 'species')

#check unit; SRL in GRooT is m/g, in TRY is cm/g. convert cm/g to m/g
combine2$SRL <- (combine2$SRL)/100

#fill the missing value using data from groot
combine2$rootN <- coalesce(combine2$rootN, combine2$rootN.groot)
combine2$RTD <- coalesce(combine2$RTD, combine2$RTD.groot)
combine2$SRL <- coalesce(combine2$SRL, combine2$SRL.groot)

#select final traits
trait.final = combine2 %>% select(species, leafN, rootN, RTD, seed.mass, LDMC, SSD, SRL, LA, SLA, height) %>% arrange(species)

trait.final$species #54 species in total

write.csv(trait.final,'combined.TRY.BIEN.GRooT.csv', row.names = F)


# combine datasets at individual levels
# read individual data of TRY
setwd("D:/RECODYN_Pro/哈维性状/TRY database/31423_27022024092721")
try.ind <- read.csv("indi.trait_longform.csv") # long form data with individuals

# rename traits; the same as the beginning
try.ind$TraitName[try.ind$TraitName == "Leaf nitrogen (N) content per leaf dry mass"] <- "leafN"
try.ind$TraitName[try.ind$TraitName == "Root nitrogen (N) content per root dry mass"] <- "rootN"
try.ind$TraitName[try.ind$TraitName == "Root tissue density (root dry mass per root volume)"] <- "RTD"
try.ind$TraitName[try.ind$TraitName == "Root length per root dry mass (specific root length, SRL)"] <- "SRL"
try.ind$TraitName[try.ind$TraitName == "Seed dry mass"] <- "seed.mass"
try.ind$TraitName[try.ind$TraitName == "Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density"] <- "SSD"
try.ind$TraitName[try.ind$TraitName == "Plant height generative"] <- "generative.height"
try.ind$TraitName[try.ind$TraitName == "Plant height vegetative"] <- "vegetation.height"
try.ind$TraitName[try.ind$TraitName == "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)"] <- "LDMC"
try.ind$TraitName[try.ind$TraitName == "Leaf area (in case of compound leaves: leaf, petiole included)"] <- "LA.inc"
try.ind$TraitName[try.ind$TraitName == "Leaf area (in case of compound leaves: leaf, petiole excluded)"] <- "LA.ex"
try.ind$TraitName[try.ind$TraitName == "Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)"] <- "LA.un"
try.ind$TraitName[try.ind$TraitName == "Leaf area (in case of compound leaves: leaflet, petiole included)"] <- "LA.leaflet.inc"
try.ind$TraitName[try.ind$TraitName == "Leaf area (in case of compound leaves: leaflet, petiole excluded)"] <- "LA.leaflet.ex"
try.ind$TraitName[try.ind$TraitName == "Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)"] <- "LA.leaflet.un"
try.ind$TraitName[try.ind$TraitName == "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)"] <- "LA.un.un"
try.ind$TraitName[try.ind$TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included"] <- "SLA.inc"
try.ind$TraitName[try.ind$TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded"] <- "SLA.ex"
try.ind$TraitName[try.ind$TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded"] <- "SLA.un"
try.ind$TraitName[try.ind$TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded"] <- "SLA.all.ex"

unique(try.ind$TraitName)

# select traits
try.ind.select = try.ind %>% 
  filter(TraitName %in% c("leafN", "rootN", "RTD", "seed.mass", "SSD", "SRL", "LDMC", 
                          "vegetation.height", "LA.inc", "LA.ex", "LA.un", "SLA.inc", "SLA.ex")) %>%
  select(AccSpeciesName, TraitName, StdValue) %>%
  rename(species = AccSpeciesName, trait_value = StdValue, 
         trait_name = TraitName)

# make the name consistent
try.ind.select$species[try.ind.select$species == "Draba verna"] <- "Erophila verna"   
try.ind.select$species[try.ind.select$species == "CAPSELLA BURSA-PASTORIS"] <- "Capsella bursa-pastoris" 

# add a new column of database name
try.ind.select$database <- "TRY"

setwd("D:/RECODYN_Pro/哈维性状")
bien.ind <- read.csv("trait_bien_select.csv")
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
  filter(trait_name != "mix.height") %>%
  select(scrubbed_species_binomial, trait_name, trait_value) %>%
  rename(species = scrubbed_species_binomial)

# add a new column of database name
bien.ind.select$database <- 'BIEN'

# merge data from TRY and BIEN
combine.try.bien <- rbind(try.ind.select, bien.ind.select)

setwd("D:/RECODYN_Pro/哈维性状/GRooT database")
groot.ind <- read.csv("GrooT_selected_3roottraits.csv")
# rename traits in groot
groot.ind$traitName[groot.ind$traitName == "Root_N_concentration"] <- "rootN"
groot.ind$traitName[groot.ind$traitName == "Root_tissue_density"] <- "RTD"
groot.ind$traitName[groot.ind$traitName == "Specific_root_length"] <- "SRL"

# select traits
groot.ind.select = groot.ind %>%
  select(speciesname, traitName, traitValue) %>%
  rename(species = speciesname, trait_name = traitName, trait_value = traitValue)

#change "Lysimachia arvensis" to "Anagallis arvensis" (But no this species)
groot.ind.select$species[groot.ind.select$species == "Lysimachia arvensis"] <- "Anagallis arvensis" 

# add database column
groot.ind.select$database <- 'GROOT'

# merge groot
combine.try.bien.groot <- rbind(combine.try.bien, groot.ind.select)
# add a column combine trait name and database
combine.try.bien.groot = combine.try.bien.groot %>%
  unite(trait_ID, trait_name, database, remove = F)

# check the units.
# convert g/g in TRY to mg/g
# SRL in GRooT is m/g, in TRY is cm/g. convert TRY cm/g to m/g
combine.unit = combine.try.bien.groot %>% 
  mutate(trait_value = if_else(trait_ID == "LDMC_TRY", trait_value * 1000, trait_value)) %>%
  mutate(trait_value = if_else(trait_ID == "SRL_TRY", trait_value/100, trait_value))


# calculate species mean trait value; long to wide
combine.sp.wide = combine.unit %>% 
  group_by(species, trait_ID) %>% 
  summarise(species.mean.trait = mean(trait_value, na.rm = T)) %>%
  spread(trait_ID, species.mean.trait)

# fill missing values within TRY

# leaf area: select include petiole 3110 (LA.inc), fill the gaps using values from 3108 (LA.ex); see Carmona et al. 2020
# select the species used for filling gaps
sp_used.LA.ex_TRY = combine.sp.wide %>%
  filter(is.na(LA.inc_TRY) & !is.na(LA.ex_TRY)) %>%
  select(species, LA.ex_TRY)

combine.sp.fill = combine.sp.wide %>%
  mutate(LA = if_else(is.na(LA.inc_TRY), LA.ex_TRY, LA.inc_TRY)) %>%
  select(species, LA.inc_TRY, LA.ex_TRY, LA, everything())

# then fill the gap with 3112 (LA.un) 
sp_used.LA.un.TRY = combine.sp.fill %>%
  filter(is.na(LA) & !is.na(LA.un_TRY)) %>%
  select(species, LA.un_TRY)

combine.sp.fill = combine.sp.fill %>%
  mutate(LA = if_else(is.na(LA), LA.un_TRY, LA)) %>%
  select(species, LA.un_TRY, LA, everything())

# SLA: select 3116 (include petiole; SLA.inc), fill the gaps using the values from 3115 (SLA.ex)
sp_used.SLA.ex.TRY = combine.sp.fill %>%
  filter(is.na(SLA.inc_TRY) & !is.na(SLA.ex_TRY)) %>%
  select(species, SLA.ex_TRY)

combine.sp.fill = combine.sp.fill %>%
  mutate(SLA = if_else(is.na(SLA.inc_TRY), SLA.ex_TRY, SLA.inc_TRY)) %>%
  select(species, SLA.inc_TRY, SLA.ex_TRY, SLA, everything())

# fill the missing value of data in TRY using available data from BIEN and GROOT
sp_used.height 

#fill the missing value using data from BIEN
#fill vegetation.height with 'whole plant height'
combine1$height <- coalesce(combine1$vegetation.height, combine1$`whole plant height`)
combine1$LA <- coalesce(combine1$LA, combine1$LA.bien)
combine1$SLA <- coalesce(combine1$SLA, combine1$SLA.bien)
combine1$LDMC <- coalesce(combine1$LDMC, combine1$LDMC.bien)
combine1$leafN <- coalesce(combine1$leafN, combine1$leafN.bien)
combine1$seed.mass <- coalesce(combine1$seed.mass, combine1$seed.mass.bien)

#fill the missing value using data from groot
combine2$rootN <- coalesce(combine2$rootN, combine2$rootN.groot)
combine2$RTD <- coalesce(combine2$RTD, combine2$RTD.groot)
combine2$SRL <- coalesce(combine2$SRL, combine2$SRL.groot)