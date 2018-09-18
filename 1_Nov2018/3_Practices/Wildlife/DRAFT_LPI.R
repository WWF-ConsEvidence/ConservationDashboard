

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Load libraries, input data, reference tables ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

pacman::p_load(devtools)

install_github("Zoological-Society-of-London/rlpi", dependencies = T)
library(rlpi)

RawLPI_data <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/LPI_data_August2018.csv', na.strings = "NULL")



# Reference tables below pulled from supplemental materials (S10-S13 Tables) of McRae et al's (2017) 
# "Diversity-Weighted LPI: Controlling for Taxonomic Bias in Global Biodiversity Indicator"

Class_weights_reftable_T_FW <- 
  data.frame(System=c(rep("Terrestrial",3), rep("Freshwater",4)),
             Class=c("Aves","Mammalia","Herps","Fish","Aves","Mammalia","Herps"),
             Afrotropical=c(0.387,0.197,0.414,0.590,0.192,0.009,0.207),
             Nearctic=c(0.376,0.249,0.373,0.565,0.203,0.013,0.217),
             Neotropical=c(0.387,0.127,0.484,0.584,0.107,0.010,0.298),
             Palearctic=c(0.433,0.249,0.316,0.592,0.211,0.015,0.179),
             IndoPacific=c(0.396,0.172,0.431,0.493,0.176,0.008,0.321))

Class_weights_reftable_M <-
  data.frame(System=rep("Marine",4),
             Class=c("Reptiles","Aves","Mammalia","Fish"),
             Arctic=c(0,0.172867,0.035011,0.792123),
             AtlanticNorthTemp=c(0.001303,0.068635,0.009774,0.920286),
             AtlanticTropSubTrop=c(0.001630,0.069353,0.006224,0.922791),
             PacificNorthTemp=c(0.000935,0.080916,0.025257,0.892890),
             TropSubTropIndoPac=c(0.005505,0.048714,0.004878,0.940901),
             SouthTempAntarctic=c(0.000957,0.054261,0.022342,0.922438))

Realm_weights_reftable_T_FW <-
  data.frame(System=c("Terrestrial","Freshwater"),
             Afrotropical=c(0.189738,0.211701),
             Nearctic=c(0.061683,0.060853),
             Neotropical=c(0.321132,0.365550),
             Palearctic=c(0.116431,0.123314),
             IndoPacific=c(0.292168,0.225576))

Realm_weights_reftable_M <-
  data.frame(Arctic=0.014541,
             AtlanticNorthTemp=0.146489,
             AtlanticTropSubTrop=0.214706,
             PacificNorthTemp=0.068026,
             TropSubTropIndoPac=0.456553,
             SouthTempAntarctic=0.099685)

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Freshwater LPI ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Freshwater LPI infiles ----

FW_filepath <- "1_Nov2018/2_FlatDataFiles/ConsDB_Input/LPI/FW"

# -- FW mammals
# INDEX VECTORS
FW_Afrotropical_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$FW_realm == "Afrotropical"

FW_Nearctic_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$FW_realm == "Nearctic"

FW_Neotropical_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$FW_realm == "Neotropical"

FW_Palearctic_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$FW_realm == "Palearctic"

FW_IndoPacific_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  (RawLPI_data$FW_realm == "Indo-Malayan" | RawLPI_data$FW_realm == "Oceania" | RawLPI_data$FW_realm == "Australasia")

# INFILES
FW_Afrotropical_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Afrotropical_Mammal,
                name = paste(FW_filepath, "FW_Afrotropical_Mammal", sep = "/"))

FW_Nearctic_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Nearctic_Mammal,
                name = paste(FW_filepath, "FW_Nearctic_Mammal", sep = "/"))

FW_Neotropical_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Neotropical_Mammal,
                name = paste(FW_filepath, "FW_Neotropical_Mammal", sep = "/"))

FW_Palearctic_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Palearctic_Mammal,
                name = paste(FW_filepath, "FW_Palearctic_Mammal", sep = "/"))

FW_IndoPacific_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_IndoPacific_Mammal,
                name = paste(FW_filepath, "FW_IndoPacific_Mammal", sep = "/"))


# -- FW aves
# INDEX VECTORS
FW_Afrotropical_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$FW_realm == "Afrotropical"

FW_Nearctic_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$FW_realm == "Nearctic"

FW_Neotropical_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$FW_realm == "Neotropical"

FW_Palearctic_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$FW_realm == "Palearctic"

FW_IndoPacific_Aves <- 
  RawLPI_data$Class == "Aves" & 
  (RawLPI_data$FW_realm == "Indo-Malayan" | RawLPI_data$FW_realm == "Oceania" | RawLPI_data$FW_realm == "Australasia")

# INFILES
FW_Afrotropical_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Afrotropical_Aves,
                name = paste(FW_filepath, "FW_Afrotropical_Aves", sep = "/"))

FW_Nearctic_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Nearctic_Aves,
                name = paste(FW_filepath, "FW_Nearctic_Aves", sep = "/"))

FW_Neotropical_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Neotropical_Aves,
                name = paste(FW_filepath, "FW_Neotropical_Aves", sep = "/"))

FW_Palearctic_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Palearctic_Aves,
                name = paste(FW_filepath, "FW_Palearctic_Aves", sep = "/"))

FW_IndoPacific_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_IndoPacific_Aves,
                name = paste(FW_filepath, "FW_IndoPacific_Aves", sep = "/"))


# -- FW herps
# INDEX VECTORS
FW_Afrotropical_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  RawLPI_data$FW_realm == "Afrotropical"

FW_Nearctic_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  RawLPI_data$FW_realm == "Nearctic"

FW_Neotropical_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  RawLPI_data$FW_realm == "Neotropical"

FW_Palearctic_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  RawLPI_data$FW_realm == "Palearctic"

FW_IndoPacific_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  (RawLPI_data$FW_realm == "Indo-Malayan" | RawLPI_data$FW_realm == "Oceania" | RawLPI_data$FW_realm == "Australasia")

# INFILES
FW_Afrotropical_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Afrotropical_Herps,
                name = paste(FW_filepath, "FW_Afrotropical_Herps", sep = "/"))

FW_Nearctic_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Nearctic_Herps,
                name = paste(FW_filepath, "FW_Nearctic_Herps", sep = "/"))

FW_Neotropical_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Neotropical_Herps,
                name = paste(FW_filepath, "FW_Neotropical_Herps", sep = "/"))

FW_Palearctic_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Palearctic_Herps,
                name = paste(FW_filepath, "FW_Palearctic_Herps", sep = "/"))

FW_IndoPacific_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_IndoPacific_Herps,
                name = paste(FW_filepath, "FW_IndoPacific_Herps", sep = "/"))


# -- FW fish
# INDEX VECTORS
FW_Afrotropical_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$FW_realm == "Afrotropical"

FW_Nearctic_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$FW_realm == "Nearctic"

FW_Neotropical_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$FW_realm == "Neotropical"

FW_Palearctic_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$FW_realm == "Palearctic"

FW_IndoPacific_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  (RawLPI_data$FW_realm == "Indo-Malayan" | RawLPI_data$FW_realm == "Oceania" | RawLPI_data$FW_realm == "Australasia")

# INFILES
FW_Afrotropical_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Afrotropical_Fish,
                name = paste(FW_filepath, "FW_Afrotropical_Fish", sep = "/"))

FW_Nearctic_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Nearctic_Fish,
                name = paste(FW_filepath, "FW_Nearctic_Fish", sep = "/"))

FW_Neotropical_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Neotropical_Fish,
                name = paste(FW_filepath, "FW_Neotropical_Fish", sep = "/"))

FW_Palearctic_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_Palearctic_Fish,
                name = paste(FW_filepath, "FW_Palearctic_Fish", sep = "/"))

FW_IndoPacific_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = FW_IndoPacific_Fish,
                name = paste(FW_filepath, "FW_IndoPacific_Fish", sep = "/"))


# ---- 2.2 Overall FW infile ----

FW_infile <-
  data.frame(FileName=c(paste(FW_filepath, "FW_Afrotropical_Fish_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Afrotropical_Aves_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Afrotropical_Mammal_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Afrotropical_Herps_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Nearctic_Fish_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Nearctic_Aves_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Nearctic_Mammal_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Nearctic_Herps_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Neotropical_Fish_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Neotropical_Aves_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Neotropical_Mammal_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Neotropical_Herps_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Palearctic_Fish_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Palearctic_Aves_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Palearctic_Mammal_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_Palearctic_Herps_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_IndoPacific_Fish_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_IndoPacific_Aves_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_IndoPacific_Mammal_pops.txt", sep = "/"),
                        paste(FW_filepath, "FW_IndoPacific_Herps_pops.txt", sep = "/")),
             Group=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4)),
             Weighting=c(Class_weights_reftable_T_FW$Afrotropical[Class_weights_reftable_T_FW$System=="Freshwater"],
                         Class_weights_reftable_T_FW$Nearctic[Class_weights_reftable_T_FW$System=="Freshwater"],
                         Class_weights_reftable_T_FW$Neotropical[Class_weights_reftable_T_FW$System=="Freshwater"],
                         Class_weights_reftable_T_FW$Palearctic[Class_weights_reftable_T_FW$System=="Freshwater"],
                         Class_weights_reftable_T_FW$IndoPacific[Class_weights_reftable_T_FW$System=="Freshwater"]),
             WeightingB=c(rep(Realm_weights_reftable_T_FW$Afrotropical[Realm_weights_reftable_T_FW$System=="Freshwater"],4),
                          rep(Realm_weights_reftable_T_FW$Nearctic[Realm_weights_reftable_T_FW$System=="Freshwater"],4),
                          rep(Realm_weights_reftable_T_FW$Neotropical[Realm_weights_reftable_T_FW$System=="Freshwater"],4),
                          rep(Realm_weights_reftable_T_FW$Palearctic[Realm_weights_reftable_T_FW$System=="Freshwater"],4),
                          rep(Realm_weights_reftable_T_FW$IndoPacific[Realm_weights_reftable_T_FW$System=="Freshwater"],4)))

FW_infile_table <-
  write.table(FW_infile, file = paste(FW_filepath, "FW_infile.txt", sep = "/"), row.names=FALSE)

FW_LPI <- LPIMain(paste(FW_filepath, 'FW_infile.txt', sep = "/"), use_weightings = 1, use_weightings_B = 1)

# -- REMOVE CLUTTER (index vectors, infile names)
rm(FW_Afrotropical_Mammal,FW_Afrotropical_Aves,FW_Afrotropical_Herps,FW_Afrotropical_Fish,
   FW_Nearctic_Mammal,FW_Nearctic_Aves,FW_Nearctic_Herps,FW_Nearctic_Fish,
   FW_Neotropical_Mammal,FW_Neotropical_Aves,FW_Neotropical_Herps,FW_Neotropical_Fish,
   FW_Palearctic_Mammal,FW_Palearctic_Aves,FW_Palearctic_Herps,FW_Palearctic_Fish,
   FW_IndoPacific_Mammal,FW_IndoPacific_Aves,FW_IndoPacific_Herps,FW_IndoPacific_Fish)

rm(FW_Afrotropical_Mammal_infile,FW_Afrotropical_Aves_infile,FW_Afrotropical_Herps_infile,FW_Afrotropical_Fish_infile,
   FW_Nearctic_Mammal_infile,FW_Nearctic_Aves_infile,FW_Nearctic_Herps_infile,FW_Nearctic_Fish_infile,
   FW_Neotropical_Mammal_infile,FW_Neotropical_Aves_infile,FW_Neotropical_Herps_infile,FW_Neotropical_Fish_infile,
   FW_Palearctic_Mammal_infile,FW_Palearctic_Aves_infile,FW_Palearctic_Herps_infile,FW_Palearctic_Fish_infile,
   FW_IndoPacific_Mammal_infile,FW_IndoPacific_Aves_infile,FW_IndoPacific_Herps_infile,FW_IndoPacific_Fish_infile)


FW_data <- RawLPI_data[RawLPI_data$System=="Freshwater",]

# WRITE TO FILE
FW_LPI_output <- 
  data.frame(Year=as.numeric(row.names(FW_LPI)), FW_LPI)

write.csv(FW_LPI_output,'1_Nov2018/2_FlatDataFiles/ConsDB_Input/FW_LPI_output_2018_0910.csv',row.names=F)

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Marine LPI ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Marine LPI infiles ----

M_filepath <- "1_Nov2018/2_FlatDataFiles/ConsDB_Input/LPI/Marine"

# -- Marine mammals
# INDEX VECTORS
M_Arctic_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$M_realm == "Arctic"

M_AtlanticNorthTemp_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$M_realm == "Atlantic north temperate"

M_AtlanticTropSubTrop_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$M_realm == "Atlantic tropical and subtropical"

M_PacificNorthTemp_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$M_realm == "Pacific north temperate"

M_TropSubTropIndoPac_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$M_realm == "Tropical and subtropical Indo-Pacific"

M_SouthTempAntarctic_Mammal <-
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$M_realm == "South temperate and Antarctic"

# INFILES
M_Arctic_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_Arctic_Mammal,
                name = paste(M_filepath, "M_Arctic_Mammal", sep = "/"))

M_AtlanticNorthTemp_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_AtlanticNorthTemp_Mammal,
                name = paste(M_filepath, "M_AtlanticNorthTemp_Mammal", sep = "/"))

M_AtlanticTropSubTrop_Mammal <- 
  create_infile(RawLPI_data, 
                index_vector = M_AtlanticTropSubTrop_Mammal,
                name = paste(M_filepath, "M_AtlanticTropSubTrop_Mammal", sep = "/"))

M_PacificNorthTemp_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_PacificNorthTemp_Mammal,
                name = paste(M_filepath, "M_PacificNorthTemp_Mammal", sep = "/"))

M_TropSubTropIndoPac_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_TropSubTropIndoPac_Mammal,
                name = paste(M_filepath, "M_TropSubTropIndoPac_Mammal", sep = "/"))

M_SouthTempAntarctic_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_SouthTempAntarctic_Mammal,
                name = paste(M_filepath, "M_SouthTempAntarctic_Mammal", sep = "/"))

# -- Marine aves
# INDEX VECTORS
M_Arctic_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$M_realm == "Arctic"

M_AtlanticNorthTemp_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$M_realm == "Atlantic north temperate"

M_AtlanticTropSubTrop_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$M_realm == "Atlantic tropical and subtropical"

M_PacificNorthTemp_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$M_realm == "Pacific north temperate"

M_TropSubTropIndoPac_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$M_realm == "Tropical and subtropical Indo-Pacific"

M_SouthTempAntarctic_Aves <-
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$M_realm == "South temperate and Antarctic"

# INFILES
M_Arctic_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_Arctic_Aves,
                name = paste(M_filepath, "M_Arctic_Aves", sep = "/"))

M_AtlanticNorthTemp_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_AtlanticNorthTemp_Aves,
                name = paste(M_filepath, "M_AtlanticNorthTemp_Aves", sep = "/"))

M_AtlanticTropSubTrop_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_AtlanticTropSubTrop_Aves,
                name = paste(M_filepath, "M_AtlanticTropSubTrop_Aves", sep = "/"))

M_PacificNorthTemp_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_PacificNorthTemp_Aves,
                name = paste(M_filepath, "M_PacificNorthTemp_Aves", sep = "/"))

M_TropSubTropIndoPac_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_TropSubTropIndoPac_Aves,
                name = paste(M_filepath, "M_TropSubTropIndoPac_Aves", sep = "/"))

M_SouthTempAntarctic_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_SouthTempAntarctic_Aves,
                name = paste(M_filepath, "M_SouthTempAntarctic_Aves", sep = "/"))

# -- Marine reptiles
# INDEX VECTORS
M_Arctic_Reptiles <- 
  RawLPI_data$Class == "Reptilia" & 
  RawLPI_data$M_realm == "Arctic"

M_AtlanticNorthTemp_Reptiles <- 
  RawLPI_data$Class == "Reptilia" & 
  RawLPI_data$M_realm == "Atlantic north temperate"

M_AtlanticTropSubTrop_Reptiles <- 
  RawLPI_data$Class == "Reptilia" & 
  RawLPI_data$M_realm == "Atlantic tropical and subtropical"

M_PacificNorthTemp_Reptiles <- 
  RawLPI_data$Class == "Reptilia" & 
  RawLPI_data$M_realm == "Pacific north temperate"

M_TropSubTropIndoPac_Reptiles <- 
  RawLPI_data$Class == "Reptilia" & 
  RawLPI_data$M_realm == "Tropical and subtropical Indo-Pacific"

M_SouthTempAntarctic_Reptiles <-
  RawLPI_data$Class == "Reptilia" & 
  RawLPI_data$M_realm == "South temperate and Antarctic"

# INFILES
M_Arctic_Reptiles_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_Arctic_Reptiles,
                name = paste(M_filepath, "M_Arctic_Reptiles", sep = "/"))

M_AtlanticNorthTemp_Reptiles_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_AtlanticNorthTemp_Reptiles,
                name = paste(M_filepath, "M_AtlanticNorthTemp_Reptiles", sep = "/"))

M_AtlanticTropSubTrop_Reptiles_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_AtlanticTropSubTrop_Reptiles,
                name = paste(M_filepath, "M_AtlanticTropSubTrop_Reptiles", sep = "/"))

M_PacificNorthTemp_Reptiles_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_PacificNorthTemp_Reptiles,
                name = paste(M_filepath, "M_PacificNorthTemp_Reptiles", sep = "/"))

M_TropSubTropIndoPac_Reptiles_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_TropSubTropIndoPac_Reptiles,
                name = paste(M_filepath, "M_TropSubTropIndoPac_Reptiles", sep = "/"))

M_SouthTempAntarctic_Reptiles_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_SouthTempAntarctic_Reptiles,
                name = paste(M_filepath, "M_SouthTempAntarctic_Reptiles", sep = "/"))


# -- Marine fish
# INDEX VECTORS
M_Arctic_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$M_realm == "Arctic"

M_AtlanticNorthTemp_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$M_realm == "Atlantic north temperate"

M_AtlanticTropSubTrop_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$M_realm == "Atlantic tropical and subtropical"

M_PacificNorthTemp_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$M_realm == "Pacific north temperate"

M_TropSubTropIndoPac_Fish <- 
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$M_realm == "Tropical and subtropical Indo-Pacific"

M_SouthTempAntarctic_Fish <-
  (RawLPI_data$Class == "Actinopterygii" | RawLPI_data$Class == "Elasmobranchii" |
     RawLPI_data$Class == "Sarcopterygii" | RawLPI_data$Class == "Cephalaspidomorphi" |
     RawLPI_data$Class == "Holocephali" | RawLPI_data$Class == "Myxini" | RawLPI_data$Class == "Chondrichthyes") & 
  RawLPI_data$M_realm == "South temperate and Antarctic"

# INFILES
M_Arctic_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_Arctic_Fish,
                name = paste(M_filepath, "M_Arctic_Fish", sep = "/"))

M_AtlanticNorthTemp_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_AtlanticNorthTemp_Fish,
                name = paste(M_filepath, "M_AtlanticNorthTemp_Fish", sep = "/"))

M_AtlanticTropSubTrop_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_AtlanticTropSubTrop_Fish,
                name = paste(M_filepath, "M_AtlanticTropSubTrop_Fish", sep = "/"))

M_PacificNorthTemp_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_PacificNorthTemp_Fish,
                name = paste(M_filepath, "M_PacificNorthTemp_Fish", sep = "/"))

M_TropSubTropIndoPac_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_TropSubTropIndoPac_Fish,
                name = paste(M_filepath, "M_TropSubTropIndoPac_Fish", sep = "/"))

M_SouthTempAntarctic_Fish_infile <- 
  create_infile(RawLPI_data, 
                index_vector = M_SouthTempAntarctic_Fish,
                name = paste(M_filepath, "M_SouthTempAntarctic_Fish", sep = "/"))


# ---- 3.2 Overall Marine infile ----

M_infile <-
  data.frame(FileName=c(paste(M_filepath, "M_Arctic_Reptiles_pops.txt", sep = "/"),
                        paste(M_filepath, "M_Arctic_Aves_pops.txt", sep = "/"),
                        paste(M_filepath, "M_Arctic_Mammal_pops.txt", sep = "/"),
                        paste(M_filepath, "M_Arctic_Fish_pops.txt", sep = "/"),
                        paste(M_filepath, "M_AtlanticNorthTemp_Reptiles_pops.txt", sep = "/"),
                        paste(M_filepath, "M_AtlanticNorthTemp_Aves_pops.txt", sep = "/"),
                        paste(M_filepath, "M_AtlanticNorthTemp_Mammal_pops.txt", sep = "/"),
                        paste(M_filepath, "M_AtlanticNorthTemp_Fish_pops.txt", sep = "/"),
                        paste(M_filepath, "M_AtlanticTropSubTrop_Reptiles_pops.txt", sep = "/"),
                        paste(M_filepath, "M_AtlanticTropSubTrop_Aves_pops.txt", sep = "/"),
                        paste(M_filepath, "M_AtlanticTropSubTrop_Mammal_pops.txt", sep = "/"),
                        paste(M_filepath, "M_AtlanticTropSubTrop_Fish_pops.txt", sep = "/"),
                        paste(M_filepath, "M_PacificNorthTemp_Reptiles_pops.txt", sep = "/"),
                        paste(M_filepath, "M_PacificNorthTemp_Aves_pops.txt", sep = "/"),
                        paste(M_filepath, "M_PacificNorthTemp_Mammal_pops.txt", sep = "/"),
                        paste(M_filepath, "M_PacificNorthTemp_Fish_pops.txt", sep = "/"),
                        paste(M_filepath, "M_TropSubTropIndoPac_Reptiles_pops.txt", sep = "/"),
                        paste(M_filepath, "M_TropSubTropIndoPac_Aves_pops.txt", sep = "/"),
                        paste(M_filepath, "M_TropSubTropIndoPac_Mammal_pops.txt", sep = "/"),
                        paste(M_filepath, "M_TropSubTropIndoPac_Fish_pops.txt", sep = "/"),
                        paste(M_filepath, "M_SouthTempAntarctic_Reptiles_pops.txt", sep = "/"),
                        paste(M_filepath, "M_SouthTempAntarctic_Aves_pops.txt", sep = "/"),
                        paste(M_filepath, "M_SouthTempAntarctic_Mammal_pops.txt", sep = "/"),
                        paste(M_filepath, "M_SouthTempAntarctic_Fish_pops.txt", sep = "/")),
             Group=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4)),
             Weighting=c(Class_weights_reftable_M$Arctic,
                         Class_weights_reftable_M$AtlanticNorthTemp,
                         Class_weights_reftable_M$AtlanticTropSubTrop,
                         Class_weights_reftable_M$PacificNorthTemp,
                         Class_weights_reftable_M$TropSubTropIndoPac,
                         Class_weights_reftable_M$SouthTempAntarctic),
             WeightingB=c(rep(Realm_weights_reftable_M$Arctic,4),
                          rep(Realm_weights_reftable_M$AtlanticNorthTemp,4),
                          rep(Realm_weights_reftable_M$AtlanticTropSubTrop,4),
                          rep(Realm_weights_reftable_M$PacificNorthTemp,4),
                          rep(Realm_weights_reftable_M$TropSubTropIndoPac,4),
                          rep(Realm_weights_reftable_M$SouthTempAntarctic,4)))

M_infile_table <-
  write.table(M_infile, file = paste(M_filepath, "M_infile.txt", sep = "/"), row.names=FALSE)

M_LPI <- LPIMain(paste(M_filepath, 'M_infile.txt', sep = "/"), use_weightings = 1, use_weightings_B = 1)


# -- REMOVE CLUTTER (index vectors, infile names)
rm(M_Arctic_Mammal,M_Arctic_Aves,M_Arctic_Reptiles,M_Arctic_Fish,
   M_AtlanticNorthTemp_Mammal,M_AtlanticNorthTemp_Aves,M_AtlanticNorthTemp_Reptiles,M_AtlanticNorthTemp_Fish,
   M_AtlanticTropSubTrop_Mammal,M_AtlanticTropSubTrop_Aves,M_AtlanticTropSubTrop_Reptiles,M_AtlanticTropSubTrop_Fish,
   M_PacificNorthTemp_Mammal,M_PacificNorthTemp_Aves,M_PacificNorthTemp_Reptiles,M_PacificNorthTemp_Fish,
   M_TropSubTropIndoPac_Mammal,M_TropSubTropIndoPac_Aves,M_TropSubTropIndoPac_Reptiles,M_TropSubTropIndoPac_Fish,
   M_SouthTempAntarctic_Mammal,M_SouthTempAntarctic_Aves,M_SouthTempAntarctic_Reptiles,M_SouthTempAntarctic_Fish)

rm(M_Arctic_Mammal_infile,M_Arctic_Aves_infile,M_Arctic_Reptiles_infile,M_Arctic_Fish_infile,
   M_AtlanticNorthTemp_Mammal_infile,M_AtlanticNorthTemp_Aves_infile,M_AtlanticNorthTemp_Reptiles_infile,M_AtlanticNorthTemp_Fish_infile,
   M_AtlanticTropSubTrop_Mammal_infile,M_AtlanticTropSubTrop_Aves_infile,M_AtlanticTropSubTrop_Reptiles_infile,M_AtlanticTropSubTrop_Fish_infile,
   M_PacificNorthTemp_Mammal_infile,M_PacificNorthTemp_Aves_infile,M_PacificNorthTemp_Reptiles_infile,M_PacificNorthTemp_Fish_infile,
   M_TropSubTropIndoPac_Mammal_infile,M_TropSubTropIndoPac_Aves_infile,M_TropSubTropIndoPac_Reptiles_infile,M_TropSubTropIndoPac_Fish_infile,
   M_SouthTempAntarctic_Mammal_infile,M_SouthTempAntarctic_Aves_infile,M_SouthTempAntarctic_Reptiles_infile,M_SouthTempAntarctic_Fish_infile)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Terrestrial LPI ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Terrestrial LPI infiles ----

T_filepath <- "1_Nov2018/2_FlatDataFiles/ConsDB_Input/LPI/Terr"

# -- Terr mammals
# INDEX VECTORS
T_Afrotropical_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$T_realm == "Afrotropical"

T_Nearctic_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$T_realm == "Nearctic"

T_Neotropical_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$T_realm == "Neotropical"

T_Palearctic_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  RawLPI_data$T_realm == "Palearctic"

T_IndoPacific_Mammal <- 
  RawLPI_data$Class == "Mammalia" & 
  (RawLPI_data$T_realm == "Indo-Malayan" | RawLPI_data$T_realm == "Oceania" | RawLPI_data$T_realm == "Australasia")

# INFILES
T_Afrotropical_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Afrotropical_Mammal,
                name = paste(T_filepath, "T_Afrotropical_Mammal", sep = "/"))

T_Nearctic_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Nearctic_Mammal,
                name = paste(T_filepath, "T_Nearctic_Mammal", sep = "/"))

T_Neotropical_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Neotropical_Mammal,
                name = paste(T_filepath, "T_Neotropical_Mammal", sep = "/"))

T_Palearctic_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Palearctic_Mammal,
                name = paste(T_filepath, "T_Palearctic_Mammal", sep = "/"))

T_IndoPacific_Mammal_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_IndoPacific_Mammal,
                name = paste(T_filepath, "T_IndoPacific_Mammal", sep = "/"))


# -- Terr aves
# INDEX VECTORS
T_Afrotropical_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$T_realm == "Afrotropical"

T_Nearctic_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$T_realm == "Nearctic"

T_Neotropical_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$T_realm == "Neotropical"

T_Palearctic_Aves <- 
  RawLPI_data$Class == "Aves" & 
  RawLPI_data$T_realm == "Palearctic"

T_IndoPacific_Aves <- 
  RawLPI_data$Class == "Aves" & 
  (RawLPI_data$T_realm == "Indo-Malayan" | RawLPI_data$T_realm == "Oceania" | RawLPI_data$T_realm == "Australasia")

# INFILES
T_Afrotropical_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Afrotropical_Aves,
                name = paste(T_filepath, "T_Afrotropical_Aves", sep = "/"))

T_Nearctic_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Nearctic_Aves,
                name = paste(T_filepath, "T_Nearctic_Aves", sep = "/"))

T_Neotropical_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Neotropical_Aves,
                name = paste(T_filepath, "T_Neotropical_Aves", sep = "/"))

T_Palearctic_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Palearctic_Aves,
                name = paste(T_filepath, "T_Palearctic_Aves", sep = "/"))

T_IndoPacific_Aves_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_IndoPacific_Aves,
                name = paste(T_filepath, "T_IndoPacific_Aves", sep = "/"))


# -- Terr herps
# INDEX VECTORS
T_Afrotropical_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  RawLPI_data$T_realm == "Afrotropical"

T_Nearctic_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  RawLPI_data$T_realm == "Nearctic"

T_Neotropical_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  RawLPI_data$T_realm == "Neotropical"

T_Palearctic_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  RawLPI_data$T_realm == "Palearctic"

T_IndoPacific_Herps <- 
  (RawLPI_data$Class == "Reptilia" | RawLPI_data$Class == "Amphibia") & 
  (RawLPI_data$T_realm == "Indo-Malayan" | RawLPI_data$T_realm == "Oceania" | RawLPI_data$T_realm == "Australasia")

# INFILES
T_Afrotropical_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Afrotropical_Herps,
                name = paste(T_filepath, "T_Afrotropical_Herps", sep = "/"))

T_Nearctic_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Nearctic_Herps,
                name = paste(T_filepath, "T_Nearctic_Herps", sep = "/"))

T_Neotropical_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Neotropical_Herps,
                name = paste(T_filepath, "T_Neotropical_Herps", sep = "/"))

T_Palearctic_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_Palearctic_Herps,
                name = paste(T_filepath, "T_Palearctic_Herps", sep = "/"))

T_IndoPacific_Herps_infile <- 
  create_infile(RawLPI_data, 
                index_vector = T_IndoPacific_Herps,
                name = paste(T_filepath, "T_IndoPacific_Herps", sep = "/"))


# ---- 2.2 Overall Terr infile ----

T_infile <-
  data.frame(FileName=c(paste(T_filepath, "T_Afrotropical_Aves_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Afrotropical_Mammal_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Afrotropical_Herps_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Nearctic_Aves_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Nearctic_Mammal_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Nearctic_Herps_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Neotropical_Aves_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Neotropical_Mammal_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Neotropical_Herps_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Palearctic_Aves_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Palearctic_Mammal_pops.txt", sep = "/"),
                        paste(T_filepath, "T_Palearctic_Herps_pops.txt", sep = "/"),
                        paste(T_filepath, "T_IndoPacific_Aves_pops.txt", sep = "/"),
                        paste(T_filepath, "T_IndoPacific_Mammal_pops.txt", sep = "/"),
                        paste(T_filepath, "T_IndoPacific_Herps_pops.txt", sep = "/")),
             Group=c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3)),
             Weighting=c(Class_weights_reftable_T_FW$Afrotropical[Class_weights_reftable_T_FW$System=="Terrestrial"],
                         Class_weights_reftable_T_FW$Nearctic[Class_weights_reftable_T_FW$System=="Terrestrial"],
                         Class_weights_reftable_T_FW$Neotropical[Class_weights_reftable_T_FW$System=="Terrestrial"],
                         Class_weights_reftable_T_FW$Palearctic[Class_weights_reftable_T_FW$System=="Terrestrial"],
                         Class_weights_reftable_T_FW$IndoPacific[Class_weights_reftable_T_FW$System=="Terrestrial"]),
             WeightingB=c(rep(Realm_weights_reftable_T_FW$Afrotropical[Realm_weights_reftable_T_FW$System=="Terrestrial"],3),
                          rep(Realm_weights_reftable_T_FW$Nearctic[Realm_weights_reftable_T_FW$System=="Terrestrial"],3),
                          rep(Realm_weights_reftable_T_FW$Neotropical[Realm_weights_reftable_T_FW$System=="Terrestrial"],3),
                          rep(Realm_weights_reftable_T_FW$Palearctic[Realm_weights_reftable_T_FW$System=="Terrestrial"],3),
                          rep(Realm_weights_reftable_T_FW$IndoPacific[Realm_weights_reftable_T_FW$System=="Terrestrial"],3)))

T_infile_table <-
  write.table(T_infile, file = paste(T_filepath, "T_infile.txt", sep = "/"), row.names=FALSE)

T_LPI <- LPIMain(paste(T_filepath, 'T_infile.txt', sep = "/"), use_weightings = 1, use_weightings_B = 1)


# -- REMOVE CLUTTER (index vectors, infile names)
rm(T_Afrotropical_Mammal,T_Afrotropical_Aves,T_Afrotropical_Herps,
   T_Nearctic_Mammal,T_Nearctic_Aves,T_Nearctic_Herps,
   T_Neotropical_Mammal,T_Neotropical_Aves,T_Neotropical_Herps,
   T_Palearctic_Mammal,T_Palearctic_Aves,T_Palearctic_Herps,
   T_IndoPacific_Mammal,T_IndoPacific_Aves,T_IndoPacific_Herps)

rm(T_Afrotropical_Mammal_infile,T_Afrotropical_Aves_infile,T_Afrotropical_Herps_infile,
   T_Nearctic_Mammal_infile,T_Nearctic_Aves_infile,T_Nearctic_Herps_infile,
   T_Neotropical_Mammal_infile,T_Neotropical_Aves_infile,T_Neotropical_Herps_infile,
   T_Palearctic_Mammal_infile,T_Palearctic_Aves_infile,T_Palearctic_Herps_infile,
   T_IndoPacific_Mammal_infile,T_IndoPacific_Aves_infile,T_IndoPacific_Herps_infile)
