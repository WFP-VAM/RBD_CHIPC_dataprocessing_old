library(readxl)
library(tidyverse)
library(readr)

#open fs sub indicators
FSsubindicators <- read_excel("C:/RBD_CHIPC_dataprocessing/data/raw/CH_2020/November 2020/Matricefiles/Cadre_Harmonis_Nov_2020.xlsx")

#keep only adm2 name and food sec variable
FSsubindicatorsadm2 <- FSsubindicators %>% select(country = ADMIN0Name, adm2 = ADMIN2Name,FCG_Poor, FCG_Borderline, FCG_Acceptable, FCG_finalphase,
                                                  HDDS_Phase1, HDDS_Phase2, HDDS_Phase3, HDDS_Phase4, HDDS_Phase5, HDDS_finalphase,
                                                  HHS_Phase1, HHS_Phase2, HHS_Phase3, HHS_Phase4, HHS_Phase5, HHS_finalphase,
                                                  NoStrategies, StressStrategies, CrisisStrategies, EmergencyStrategies, LhHCSCat_finalphase,
                                                  rCSI_Phase1, rCSI_Phase2, rCSI_Phase3, rCSI_finalphase)

FSsubindicatorsadm1 <- FSsubindicators %>% select(country = ADMIN0Name, adm1 = ADMIN1Name, FCG_Poor, FCG_Borderline, FCG_Acceptable, FCG_finalphase,
                                                  HDDS_Phase1, HDDS_Phase2, HDDS_Phase3, HDDS_Phase4, HDDS_Phase5, HDDS_finalphase,
                                                  HHS_Phase1, HHS_Phase2, HHS_Phase3, HHS_Phase4, HHS_Phase5, HHS_finalphase,
                                                  NoStrategies, StressStrategies, CrisisStrategies, EmergencyStrategies, LhHCSCat_finalphase,
                                                  rCSI_Phase1, rCSI_Phase2, rCSI_Phase3, rCSI_finalphase)

#pullup country by country - Burkina
bfa_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/BFA/bfa_nov2020_cur.csv")
FSsubindicatorsadm2bfa <- FSsubindicatorsadm2 %>% filter(country == "Burkina Faso") 
FSsubindicatorsadm2bfa <- FSsubindicatorsadm2bfa %>% select(-country)
#join 
sux1 <- anti_join(bfa_nov2020_cur, FSsubindicatorsadm2bfa, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2bfa, bfa_nov2020_cur,  by = "adm2")
FSsubindicatorsadm2bfa <- FSsubindicatorsadm2bfa %>% mutate(adm2=recode(adm2, "Komonjdjari" = "Komondjari"))
bfa_nov2020_cur <- left_join(bfa_nov2020_cur, FSsubindicatorsadm2bfa, by = "adm2")
#write
bfa_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/BFA/bfa_nov2020_cur.csv")

#pullup country by country - Cameroon
cmr_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/CMR/cmr_nov2020_cur.csv")
FSsubindicatorsadm2cmr <- FSsubindicatorsadm2 %>% filter(country == "Cameroon") %>% select(-country)
#join 
sux1 <- anti_join(cmr_nov2020_cur, FSsubindicatorsadm2cmr, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2cmr, cmr_nov2020_cur,  by = "adm2")
cmr_nov2020_cur <- left_join(cmr_nov2020_cur, FSsubindicatorsadm2cmr, by = "adm2")
#write
cmr_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/CMR/cmr_nov2020_cur.csv")


#pullup country by country - Chad
tcd_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/TCD/tcd_nov2020_cur.csv")
FSsubindicatorsadm2tcd <- FSsubindicatorsadm2 %>% filter(country == "Chad") %>% select(-country)
#join 
sux1 <- anti_join(tcd_nov2020_cur, FSsubindicatorsadm2tcd, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2tcd, tcd_nov2020_cur,  by = "adm2")
tcd_nov2020_cur <- left_join(tcd_nov2020_cur, FSsubindicatorsadm2tcd, by = "adm2")
#write
tcd_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tcd/tcd_nov2020_cur.csv")

#pullup country by country - gmb
gmb_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gmb/gmb_nov2020_cur.csv")
FSsubindicatorsadm1gmb <- FSsubindicatorsadm1 %>% filter(country == "Cote d'Ivoire") %>% select(-country)
#join 
sux1 <- anti_join(gmb_nov2020_cur, FSsubindicatorsadm1gmb, by = "adm1")
sux2 <- anti_join(FSsubindicatorsadm1gmb, gmb_nov2020_cur,  by = "adm1")
gmb_nov2020_cur <- left_join(gmb_nov2020_cur, FSsubindicatorsadm1gmb, by = "adm1")
#write
gmb_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gmb/gmb_nov2020_cur.csv")


#pullup country by country - gambia
gmb_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gmb/gmb_nov2020_cur.csv")
FSsubindicatorsadm1gmb <- FSsubindicatorsadm1 %>% filter(country == "Gambia") %>% select(-country)
#join 
sux1 <- anti_join(gmb_nov2020_cur, FSsubindicatorsadm1gmb, by = "adm1")
sux2 <- anti_join(FSsubindicatorsadm1gmb, gmb_nov2020_cur,  by = "adm1")
gmb_nov2020_cur <- left_join(gmb_nov2020_cur, FSsubindicatorsadm1gmb, by = "adm1")
#write
gmb_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gmb/gmb_nov2020_cur.csv")

#pullup country by country - ghana
gha_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gha/gha_nov2020_cur.csv")
FSsubindicatorsadm1gha <- FSsubindicatorsadm1 %>% filter(country == "Ghana") %>% select(-country)
#join 
sux1 <- anti_join(gha_nov2020_cur, FSsubindicatorsadm1gha, by = "adm1")
sux2 <- anti_join(FSsubindicatorsadm1gha, gha_nov2020_cur,  by = "adm1")
gha_nov2020_cur <- left_join(gha_nov2020_cur, FSsubindicatorsadm1gha, by = "adm1")
#write
gha_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gha/gha_nov2020_cur.csv")

#pullup country by country - liberia
lbr_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/lbr/lbr_nov2020_cur.csv")
FSsubindicatorsadm1lbr <- FSsubindicatorsadm1 %>% filter(country == "Liberia") %>% select(-country)
#join 
sux1 <- anti_join(lbr_nov2020_cur, FSsubindicatorsadm1lbr, by = "adm1")
sux2 <- anti_join(FSsubindicatorsadm1lbr, lbr_nov2020_cur,  by = "adm1")
lbr_nov2020_cur <- left_join(lbr_nov2020_cur, FSsubindicatorsadm1lbr, by = "adm1")
#write
lbr_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/lbr/lbr_nov2020_cur.csv")

#pullup country by country - Mali
mli_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/MLI/mli_nov2020_cur.csv", 
                            locale = locale(date_names = "fr", encoding = "WINDOWS-1252"))
FSsubindicatorsadm2mli <- FSsubindicatorsadm2 %>% filter(country == "Mali") %>% select(-country)
#join 
FSsubindicatorsadm2mli <- FSsubindicatorsadm2mli %>% mutate(adm2=recode(adm2, "Abeibara" = "Abéibara",
                                                                        "Bafoulabe" = "Bafoulabé", 
                                                                        "Baroueli" = "Barouéli", 
                                                                        "Diema" = "Diéma", 
                                                                        "Dioila" = "Dioïla", 
                                                                        "Dire" = "Diré", 
                                                                        "Djenne" = "Djenné",
                                                                        "Gourma-Rharous" = "Gourma Rharous", 
                                                                        "Kenieba" = "Kéniéba",
                                                                        "Kolondieba" = "Kolondiéba",
                                                                        "Menaka" = "Ménaka",
                                                                        "Niafunke" = "Niafunké", 
                                                                        "Segou" = "Ségou", 
                                                                        "Tin-Essako" = "Tin Essako",
                                                                        "Yelimane" = "Yélimané"))
sux1 <- anti_join(mli_nov2020_cur, FSsubindicatorsadm2mli, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2mli, mli_nov2020_cur,  by = "adm2")                                                                        
mli_nov2020_cur <- left_join(mli_nov2020_cur, FSsubindicatorsadm2mli, by = "adm2")
#write
mli_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mli/mli_nov2020_cur.csv")


#pullup country by country - Mauritania
mrt_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mrt/mrt_nov2020_cur.csv")
FSsubindicatorsadm2mrt <- FSsubindicatorsadm2 %>% filter(country == "Mauritania") %>% select(-country)
#join 
sux1 <- anti_join(mrt_nov2020_cur, FSsubindicatorsadm2mrt, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2mrt, mrt_nov2020_cur,  by = "adm2")
mrt_nov2020_cur <- left_join(mrt_nov2020_cur, FSsubindicatorsadm2mrt, by = "adm2")
#write
mrt_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mrt/mrt_nov2020_cur.csv")

#pullup country by country - Mauritania
ner_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/ner/ner_nov2020_cur.csv")
FSsubindicatorsadm2ner <- FSsubindicatorsadm2 %>% filter(country == "Niger") %>% select(-country)
#join 
FSsubindicatorsadm2ner <- FSsubindicatorsadm2ner %>% mutate(adm2=recode(adm2, "Abala" = "Abala AL",
                                                                        "Ayerou" = "Ayerou AL", 
                                                                        "Banibangou" = "Banibangou AL", 
                                                                        "Bankilare" = "Bankilare AL", 
                                                                        "Bosso" = "Bosso AL", 
                                                                        "Tassara" = "Tassara AL", 
                                                                        "Tillia" = "Tillia AL",
                                                                        "Torodi" = "Torodi AL"))
sux1 <- anti_join(ner_nov2020_cur, FSsubindicatorsadm2ner, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2ner, ner_nov2020_cur,  by = "adm2")
ner_nov2020_cur <- left_join(ner_nov2020_cur, FSsubindicatorsadm2ner, by = "adm2")
#write
ner_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/ner/ner_nov2020_cur.csv")

#skip Nigeria

#Senegal
sen_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sen/sen_nov2020_cur.csv")
FSsubindicatorsadm2sen <- FSsubindicatorsadm2 %>% filter(country == "Senegal") %>% select(-country)
#join 
FSsubindicatorsadm2sen <- FSsubindicatorsadm2sen %>% mutate(adm2=recode(adm2, "Birkelane" = "MBirkelane"))
sux1 <- anti_join(sen_nov2020_cur, FSsubindicatorsadm2sen, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2sen, sen_nov2020_cur,  by = "adm2")
sen_nov2020_cur <- left_join(sen_nov2020_cur, FSsubindicatorsadm2sen, by = "adm2")
#write
sen_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sen/sen_nov2020_cur.csv")

#pullup country by country - sierra leone
sle_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sle/sle_nov2020_cur.csv")
FSsubindicatorsadm2sle <- FSsubindicatorsadm2 %>% filter(country == "Sierra Leone") %>% select(-country)
#join 
sux1 <- anti_join(sle_nov2020_cur, FSsubindicatorsadm2sle, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2sle, sle_nov2020_cur,  by = "adm2")
sle_nov2020_cur <- left_join(sle_nov2020_cur, FSsubindicatorsadm2sle, by = "adm2")
#write
sle_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sle/sle_nov2020_cur.csv")

#pullup country by country - togo
tgo_nov2020_cur <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tgo/tgo_nov2020_cur.csv")
FSsubindicatorsadm2tgo <- FSsubindicatorsadm2 %>% filter(country == "Togo") %>% select(-country)
#join 
sux1 <- anti_join(tgo_nov2020_cur, FSsubindicatorsadm2tgo, by = "adm2")
sux2 <- anti_join(FSsubindicatorsadm2tgo, tgo_nov2020_cur,  by = "adm2")
tgo_nov2020_cur <- left_join(tgo_nov2020_cur, FSsubindicatorsadm2tgo, by = "adm2")
#write
tgo_nov2020_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tgo/tgo_nov2020_cur.csv")

#Nigeria
#should find a better way to import all these files - with purrr
file_path <- "C:\\RBD_CHIPC_dataprocessing\\data\\raw\\CH_2020\\November 2020\\Matricefiles\\Inter_Matrix"
file_path %>% list.files()

Matrice_intermediaire_Taraba <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire Taraba.xlsx")
Matrice_intermediaire_Adamawa <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Adamawa.xlsx")
Matrice_intermediaire_Bauchi <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Bauchi.xlsx")
Matrice_intermediaire_Benue <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Benue.xlsx")
Matrice_intermediaire_Borno <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Borno.xlsx")
Matrice_intermediaire_FCT <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_FCT.xlsx")
Matrice_intermediaire_Gombe <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Gombe.xlsx")
Matrice_intermediaire_Jigawa <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Jigawa.xlsx")
Matrice_intermediaire_Kaduna <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Kaduna.xlsx")
Matrice_intermediaire_Kano <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Kano.xlsx")
Matrice_intermediaire_Katsina <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Katsina.xlsx")
Matrice_intermediaire_Kebbi <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Kebbi.xlsx")
Matrice_intermediaire_Niger <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Niger.xlsx")
Matrice_intermediaire_Plateau <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Plateau.xlsx")
Matrice_intermediaire_Sokoto <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Sokoto.xlsx")
Matrice_intermediaire_Yobe <- read_excel("data/raw/CH_2020/November 2020/MatriceFiles/Inter_Matrix/Matrice_intermediaire_Yobe.xlsx")

Matrice_intermediaire_Nigeria <- bind_rows(Matrice_intermediaire_Taraba, Matrice_intermediaire_Adamawa, Matrice_intermediaire_Bauchi,
                                           Matrice_intermediaire_Benue, Matrice_intermediaire_Borno, Matrice_intermediaire_FCT,
                                           Matrice_intermediaire_Gombe, Matrice_intermediaire_Jigawa, Matrice_intermediaire_Kaduna,
                                           Matrice_intermediaire_Kano, Matrice_intermediaire_Katsina, Matrice_intermediaire_Kebbi,
                                           Matrice_intermediaire_Niger, Matrice_intermediaire_Plateau, Matrice_intermediaire_Sokoto,
                                           Matrice_intermediaire_Yobe)


