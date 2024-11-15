# trait-database

The codes aim to get trait data of the species in Arkaute from 3 database (TRY, BIEN and GRooT) and combine them together. The data used for the codes are at https://figshare.com/s/52c81906fcf42ec045f3

Please first run 'rtry_3.6.R', 'BIEN.R' and 'GrooT.R' to get individual-level trait values of each species respectively ('indi.trait_longform.csv' for TRY, 'trait_bien_select.csv' for BIEN and 'GrooT_selected_3roottraits.csv' for GRoot), then 'combine_traits_in3databases.R' to get the integrated species-level data ('combined.TRY.BIEN.GRooT_species_level.csv').

Note that outliers need to be checked based on the integrated individual-level data ('all.indi.used.csv')


