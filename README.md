# trait-database

The codes aim to get trait data of the species in Arkaute from 2 database (TRY and BIEN) and combine them together. The data used for the codes are at https://figshare.com/s/52c81906fcf42ec045f3

Please first run '01_rtry.R' and '02_BIEN.R' to get individual-level trait values of each species respectively ('indi.trait_longform.csv' for TRY and 'trait_bien_select.csv' for BIEN), then '03_combine_in2databases.R' to get the integrated species-level data ('combined.TRY.BIEN_species_level.csv').

Note that outliers need to be checked based on the integrated individual-level data ('all.indi.used.csv').


