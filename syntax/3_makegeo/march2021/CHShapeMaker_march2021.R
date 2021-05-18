library(tidyverse)
library(sf)
library(googlesheets4)
library(rgdal)
library(janitor)
library(rmapshaper)
library(readxl)
library(mapview)
library(roperators)



#add CH data
cadre_harmonise_caf_ipc <- read_excel("data/processed/cadre_harmonise_caf_ipc.xlsx", guess_max = 21474836)
#split into current and projected analyzed at adm1 level and analyzed at adm2 level
#current
cadre_harmonise_caf_ipc_mar2021current_adm1 <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "current") %>% filter(is.na(adm2_name) & is.na(adm2_pcod2))  
cadre_harmonise_caf_ipc_mar2021current_adm2 <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "current") %>% filter(!is.na(adm2_name) & !is.na(adm2_pcod2)) 
#projected
cadre_harmonise_caf_ipc_mar2021projected_adm1 <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "projected") %>% filter(is.na(adm2_name) & is.na(adm2_pcod2))  
cadre_harmonise_caf_ipc_mar2021projected_adm2 <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "projected") %>% filter(!is.na(adm2_name) & !is.na(adm2_pcod2))

#create list of countries analyzed in march 2021
wca_mar2021 <- c("Benin","Burkina Faso","Cameroon","Central African Republic","Chad","Côte d'Ivoire","Gambia","Ghana","Guinea","Guinea Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leone","Togo")

#import shapefiles at adm1, adm2, adm3 - all files taken from HDX website  
#adm0 
wca_shp0 <- read_sf("data\\geo\\adm0\\wca_admbnda_adm0_ocha_18022021.shp") 
wca_shp0_filtered <- wca_shp0 %>% filter(admin0Name %in% wca_mar2021)
#adm1
wca_shp1 <- read_sf("data\\geo\\adm1\\wca_admbnda_adm1_ocha_18022021.shp") 
wca_shp1_filtered <- wca_shp1 %>% rename(adm1_pcod2 = admin1Pcod) %>% filter(admin0Name %in% wca_mar2021)
#adm2 and get rid of special areas in Mali / Niger / Liberia / Mauritania so they can be created/added seperately
wca_shp2 <- read_sf("data\\geo\\adm2\\wca_admbnda_adm2_ocha_18022021.shp") %>%  filter(admin0Name %in% wca_mar2021)
#some adm2 and adm1 pcodes in Benin have mistakes in them
wca_shp2 <- wca_shp2 %>% mutate(admin2Pcod = case_when(
  admin2Pcod == "BE0301" ~ "BJ0301",
  admin2Pcod == "BE0304" ~ "BJ0304",
  admin2Pcod == "BE0305" ~ "BJ0305",       
  admin2Pcod == "BE0800" ~ "BJ0800",  
  admin2Pcod == "BE0904" ~ "BJ0904",
  admin2Pcod == "BE1009" ~ "BJ1009",
  admin2Pcod == "BE1003" ~ "BJ1003",
  TRUE ~ as.character(admin2Pcod)),
                              admin1Pcod = case_when(
  admin1Pcod == "BE03" ~ "BJ03",
  admin1Pcod == "BE08" ~ "BJ08",
  admin1Pcod == "BE09" ~ "BJ09",
  admin1Pcod == "BE10" ~ "BJ10",
  TRUE ~ as.character(admin1Pcod)))
wca_shp2_filtered_curr <- wca_shp2  %>% rename(adm1_pcod2 = admin1Pcod, adm2_pcod2 = admin2Pcod) %>%
  filter(admin2Name %ni% c("Teyarett","Ksar","TevraghZeina","Toujounine","Sebkha","El Mina","Dar Naim","Arafat","Riad","Nouadhibou","Chami", #Mauritania
                           "Greater Monrovia","Commonwealth1","Careysburg","St. Paul River","Todee", #Liberia
                           "Bamako", #Mali
                           "Dogondoutchi", "Guidan Roumdji","Madarounfa","Tahoua","Filingué","Ouallam","Say","Téra","Tillabéri")) %>%
                           filter(adm2_pcod2 != "ML0505") #Koro, Mali (because there are two Koros)
wca_shp2_filtered_proj <- wca_shp2_filtered_curr  %>% filter(admin2Name != "Bangui") #because arrondisements of Bangui only analyzed for projected


#create special areas
#niger
ner_adm3 <- read_sf("data/geo/adm3/NER/NER_adm03_feb2018.shp")
#mali
mli_adm3 <- read_sf("data/geo/adm3/MLI/mli_admbnda_adm3_1m_dnct_20190802.shp")
#CAR
CAR_adm3 <- read_sf("data/geo/adm3/CAF/caf_admbnda_adm3_200k_sigcaf_reach_itos_v2.shp")

#Make special areas
#Niger - creating special adm2 codes using accesible vs limited acess adm3 areas Niger
ner_adm2 <- ner_adm3 %>%  
  rename(adm2_pcod2 = adm_02) %>% 
  filter(adm2_pcod2 %in% c("Dogondoutchi", "Guidan Roumdji","Madarounfa","Tahoua","Filingué","Ouallam","Say","Téra","Tillabéri")) %>% 
  mutate(adm2_pcod2 = case_when(
  adm_03 %in% c("Matankari", "Dogondoutchi", "Dan-Kassari", "Kiéché") ~ "NE0303_a", #Dogondoutchi - same as Nov 2020
  adm_03 %in% c("Soucoucoutane", "Dogonkiria") ~ "NE0303_al",
  adm_03 %in% c("Chadakori", "Saé Saboua") ~ "NE0405_a_032021", #Guidan Roumdji - changed from Nov 2020
  adm_03 %in% c("Guidan Roumdji", "Guidan Sori", "Tibiri") ~ "NE0405_al_032021",
  adm_03 %in% c("Dan-Issa", "Djiratawa") ~ "NE0406_a_032021", #Madarounfa - changed from Nov 2020
  adm_03 %in% c("Gabi","Safo", "Madarounfa","Sarkin Yamma") ~ "NE0406_al_032021",
  adm_03 %in% c("Kalfou", "Barmou", "Bambeye", "Affala") ~ "NE0509_a_032021", #Tahoua changed from March 2019
  adm_03 %in% c("Tebaram","Takanamat") ~ "NE0509_al_032021",
  adm_03 %in% c("Filingué","Kourfeye Centre") ~ "NE0606_a", #Filingue- first time split into accessible non accessible
  adm_03 %in% c("Imanan", "Tondikandia") ~ "NE0606_al",
  adm_03 %in% c("Simiri") ~ "NE0609_a_032021", #Ouallam
  adm_03 %in% c("Dingazi", "Ouallam","Tondikiwindi") ~ "NE0609_al_032021",
  adm_03 %in% c("Say","Ouro Guéladjo") ~ "NE0610_a", #Say
  adm_03 %in% c("Tamou") ~ "NE0610_al",
  adm_03 %in% c("Kokorou","Méhana","Téra") ~ "NE0611_a_032021", #Tera - changed since nov2020
  adm_03 %in% c("Diagourou","Gorouol") ~"NE0611_al_032021",
  adm_03 %in% c("Anzourou") ~ "NE0612_al", #Tillaberi - same as before
  adm_03 %in% c("Dessa", "Sinder", "Sakoïra", "Bibiyergou", "Tillabéri", "Kourteye") ~ "NE0612_a"))
#now add adm1
ner_adm2 <- ner_adm2 %>% mutate(adm1_pcod2 = case_when(
  adm2_pcod2 %in% c("NE0303_a","NE0303_al") ~ "NE03",
  adm2_pcod2 %in% c("NE0405_a_032021","NE0405_al_032021") ~ "NE04",
  adm2_pcod2 %in% c("NE0406_a_032021","NE0406_al_032021") ~ "NE04",
  adm2_pcod2 %in% c("NE0509_a_032021","NE0509_al_032021") ~ "NE05",
  adm2_pcod2 %in% c("NE0606_a","NE0606_al") ~ "NE06",
  adm2_pcod2 %in% c("NE0609_a_032021","NE0609_al_032021") ~ "NE06",
  adm2_pcod2 %in% c("NE0610_a","NE0610_al") ~ "NE06",
  adm2_pcod2 %in% c("NE0611_a_032021","NE0611_al_032021") ~ "NE06",
  adm2_pcod2 %in% c("NE0612_al","NE0612_a") ~ "NE06"))
#combine together at adm2 level
ner_adm2  <- ner_adm2  %>%  
  group_by(adm1_pcod2, adm2_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() 

##Mali - creating special adm2 codes using accesible vs limited acess adm3 areas and Bamako communes - Mali
mli_adm3 <- mli_adm3 %>% filter(admin2Name %in% c("Koro","Bamako")) 
mli_adm2 <- mli_adm3 %>%  mutate(adm2_pcod2 = case_when(
    admin3Name  %in% c("Koro","Barapireli","Diougani","Dougouténé I","Dougouténé II","Koporo Pen","Koporokendie Na","Pel Maoude","Youdiou") ~ "ML0505_a",
    admin3Name %in% c("Bamba","Bondo","Diankabou","Dinangourou","Kassa","Madougou","Yoro") ~ "ML0505_al",
    TRUE ~ as.character(admin3Pcod)))
#now add adm1
#now add adm1
mli_adm2 <- mli_adm2 %>% mutate(adm1_pcod2 = case_when(
  adm2_pcod2 %in% c("ML0505_a","ML0505_al") ~ "ML05",
  TRUE ~ "ML09"))
#combine together at adm2 level
mli_adm2 <- mli_adm2 %>%  
group_by(adm1_pcod2, adm2_pcod2) %>%
summarise(Shape_Area = sum(Shape_Area)) %>%
ungroup() 
###Mauritania - nouakchott and chami/noudibhou
mrt_special <- wca_shp2 %>% filter(admin1Name %in% c("Nouakchott","Dakhlet-Nouadhibou")) %>%
  mutate(adm2_pcod2 = case_when(
    admin2Name %in% c("Arafat","El Mina","Riad") ~ "MRT010_sud",
    admin2Name %in% c("Ksar","Sebkha","TevraghZeina") ~ "MRT010_ouest",
    admin2Name %in% c("Dar Naim","Teyarett","Toujounine") ~ "MRT010_nord",
    admin2Name %in% c("Nouadhibou","Chami") ~ "MR0401"))
#now add adm1
mrt_special <- mrt_special %>% mutate(adm1_pcod2 = case_when(
  adm2_pcod2 %in% c("MRT010_sud","MRT010_ouest","MRT010_nord") ~ "MR10",
  TRUE ~ "MR04"))
#create shape area 
mrt_special$Shape_Area <- st_area(mrt_special)
#combine together at adm2 level
mrt_adm2 <- mrt_special  %>%  
  group_by(adm1_pcod2, adm2_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() %>% select(-Shape_Area)
###Liberia
lbr_montesserado <- wca_shp2 %>% filter(admin1Name == "Montserrado") %>% mutate(adm2_pcod2 = case_when(
    admin2Name %in% c("Greater Monrovia") ~ "LBR011_u",
    admin2Name %in% c("Careysburg"," Commonwealth1","St. Paul River","Todee") ~ "LBR011_r"))
#now add adm1
lbr_montesserado <- lbr_montesserado %>% mutate(adm1_pcod2 = "LR11")
#create shape area 
lbr_montesserado$Shape_Area <- st_area(lbr_montesserado)    
#combine together at adm2 level
lbr_adm2 <- lbr_montesserado  %>%  
  group_by(adm1_pcod2, adm2_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() %>% select(-Shape_Area)
###Bangui - only for projected because 
car_adm2 <- CAR_adm3 %>% filter(admin2Name == "Bangui") %>% mutate(adm2_pcod2 = case_when(
  admin3Name == "Arrondissement 1" ~ "CF7111",
  admin3Name == "Arrondissement 2" ~ "CF7112",
  admin3Name == "Arrondissement 3" ~ "CF7113",
  admin3Name == "Arrondissement 4" ~ "CF7114",
  admin3Name == "Arrondissement 5" ~ "CF7115",
  admin3Name == "Arrondissement 6" ~ "CF7116",
  admin3Name == "Arrondissement 7" ~ "CF7117",
  admin3Name == "Arrondissement 8" ~ "CF7118"))
#now add adm1
car_adm2  <- car_adm2 %>% mutate(adm1_pcod2 = "CF71")
#combine together at adm2 level
car_adm2 <- car_adm2  %>%  
  group_by(adm1_pcod2, adm2_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() %>% select(-Shape_Area)


#add together all adm2 + Niger/Mali/Mauritania/
wca_adm2_CH_curr <- bind_rows(wca_shp2_filtered_curr, ner_adm2, mli_adm2, mrt_adm2, lbr_adm2) 
wca_adm2_CH_proj <- bind_rows(wca_shp2_filtered_proj, ner_adm2, mli_adm2, mrt_adm2, lbr_adm2, car_adm2) 
# adm2
wca_adm2_CH_curr <- inner_join(wca_adm2_CH_curr, cadre_harmonise_caf_ipc_mar2021current_adm2,  by = c("adm1_pcod2","adm2_pcod2"))
wca_adm2_CH_proj <- inner_join(wca_adm2_CH_proj, cadre_harmonise_caf_ipc_mar2021projected_adm2,  by = c("adm1_pcod2","adm2_pcod2"))
# adm1
wca_adm1_CH_curr <- inner_join(wca_shp1_filtered, cadre_harmonise_caf_ipc_mar2021current_adm1, by = c("adm1_pcod2"))
wca_adm1_CH_proj <- inner_join(wca_shp1_filtered, cadre_harmonise_caf_ipc_mar2021projected_adm1, by = c("adm1_pcod2"))
#put together adm1 and amd2
wca_CHIPC_mar2021_current <- bind_rows(wca_adm1_CH_curr, wca_adm2_CH_curr)
wca_CHIPC_mar2021_projected <- bind_rows(wca_adm1_CH_proj, wca_adm2_CH_proj)

#get rid of some variables
wca_CHIPC_mar2021_current <- wca_CHIPC_mar2021_current %>% select(-admin0Name, -admin0Pcod, -admin1Name, -date, -adm0_gaulcode, -region, -adm1_gaulcode, -adm1_pcod3, -adm2_gaulcode, -adm2_pcod3, 
                                                                  -notes,	-Comments,	-DataSource,	-status,	-admin2Name,	-Shape_Area)

wca_CHIPC_mar2021_projected <- wca_CHIPC_mar2021_projected %>% select(-admin0Name, -admin0Pcod, -admin1Name, -date, -adm0_gaulcode, -region, -adm1_gaulcode, -adm1_pcod3, -adm2_gaulcode, -adm2_pcod3, 
                                                                 -notes,	-Comments,	-DataSource,	-status,	-admin2Name,	-Shape_Area)


#test mapping
#CH color codes
CH_colors = c("1" = "#c6ffc7", "2" = "#ffe718", "3" = "#e88400", "4" = "#e02d00", "5" = "#5e0803")
#finalphases
mapcurrentmar2021_phase_class <- ggplot()  +geom_sf(data = wca_CHIPC_mar2021, mapping = aes(fill = as.factor(currentmar2021_phase_class), color = NULL)) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) final area phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
mapprojectedmar2021_phase_class <- wca_CHIPC_mar2021 %>% ggplot() +geom_sf(aes(fill = as.factor(projectedmar2021_phase_class))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projected) final area phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
#fc
mapcurrentmar2021_foodconsumption_phase <- wca_ch %>% ggplot() +geom_sf(aes(fill = as.factor(currentmar2021_foodconsumption_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="food consumption outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) food consumption outcome phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
mapprojectedmar2021_foodconsumption_phase <- wca_ch %>% ggplot() +geom_sf(aes(fill = as.factor(projectedmar2021_foodconsumption_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="food consumption outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projected) food consumption outcome phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
#livelihoods
mapcurrentmar2021_livelihoods_phase <- wca_ch %>% ggplot() +geom_sf(aes(fill = as.factor(currentmar2021_livelihoods_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="livelihoods outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) livelihood outcome phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
mapprojectedmar2021_livelihoods_phase <- wca_ch %>% ggplot() +geom_sf(aes(fill = as.factor(projectedmar2021_livelihoods_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="livelihoods outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projected) livelihood outcome phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
#nutrition
mapcurrentmar2021_nutrition_phase <- wca_ch %>% ggplot() +geom_sf(aes(fill = as.factor(currentmar2021_nutrition_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="nutrition outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) nutrition outcome phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
mapprojectedmar2021_nutrition_phase <- wca_ch %>% ggplot() +geom_sf(aes(fill = as.factor(projectedmar2021_nutrition_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="nutrition outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projected) nutrition outcome phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
#mortality
mapcurrentmar2021_mortality_phase <- wca_ch %>% ggplot() +geom_sf(aes(fill = as.factor(currentmar2021_mortality_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="mortality outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) mortality outcome phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)
mapprojectedmar2021_mortality_phase <- wca_ch %>% ggplot() +geom_sf(aes(fill = as.factor(projectedmar2021_mortality_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="mortality outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projected) mortality outcome phasing") +geom_sf(data=wca_shp0all,  lwd=1, fill=NA)

#finalphases
mapcurrentmar2021_phase_class <- wca_adm2_niger %>% ggplot() +geom_sf(mapping = aes(fill = as.factor(currentmar2021_phase_class), color = NULL)) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) final area phasing") 


#fix and reduce size
wca_CHIPC_mar2021_current <- st_make_valid(wca_CHIPC_mar2021_current)
wca_CHIPC_mar2021_current <- rmapshaper::ms_simplify(wca_CHIPC_mar2021_current, keep_shapes = TRUE) # simplify polygons
### Save it as .gpkg file
st_write(wca_CHIPC_mar2021_current, "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_mar2021_current.gpkg", driver="GPKG", append = T) 
#fix and reduce size
wca_CHIPC_mar2021_projected <- st_make_valid(wca_CHIPC_mar2021_projected)
wca_CHIPC_mar2021_projected <- rmapshaper::ms_simplify(wca_CHIPC_mar2021_projected, keep_shapes = TRUE) # simplify polygons
### Save it as .gpkg file
st_write(wca_CHIPC_mar2021_projected, "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_mar2021_projected.gpkg", driver="GPKG", append = T) 




##get adm0
#add Adm0 level to use as outline in viz
wca_shp0 <- read_sf("data\\geo\\GIS2\\bnd_adm0_gaul_revised vam_cod_20190304.shp") 
wca_shp0 <- wca_shp0 %>% filter(Adm0_Name != "Sao Tome and Principe" & Adm0_Name != "Cape Verde")
#fix and reduce size
wca_shp0all <- st_make_valid(wca_shp0all)
wca_shp0all  <- rmapshaper::ms_simplify(wca_shp0all , keep_shapes = TRUE) # simplify polygons
### Save it as .gpkg file
st_write(wca_shp0all , "data\\geo\\finalized_CHgeofiles\\wca_shp0all.gpkg", driver="GPKG", append = T) 

