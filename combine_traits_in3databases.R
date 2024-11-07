library(tidyverse)
rm(list=ls())

###################### TRY ######################
# read data: TRY
setwd("D:/RECODYN_Pro/哈维性状/TRY database/31423_27022024092721")
try.mean.sp <- read.csv("mean_traits_species.csv") # mean value of each species
#try.ind <- read.csv("indi.trait_longform.csv") # long form data with individuals
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

# GRooT
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
