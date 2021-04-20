library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(writexl)
library(fs)
library(stringdist)
library(googledrive)
library(googlesheets4)

## File template
file_regex <- "^\\w{3}_(mar|nov|jun)\\d{4}_(cur|proj)\\.csv"

##
files <- dir_ls("data/csv", type = "file", recurse = TRUE)

## Check if all files follow the patterns
stopifnot(all(str_detect(basename(files), file_regex)))
files[which(!str_detect(basename(files), file_regex))]

## Remove June
#files <- files[!str_detect(files, "jun")]

exercise_code <- function(file) {
  case_when(
    str_detect(file, "nov") ~ 1L,
    str_detect(file, "mar") ~ 2L,
    str_detect(file, "jun") ~ 3L,
    TRUE ~ 0L
  )
}


exercise_label <- function(file) {
  case_when(
    str_detect(file, "nov") ~ "Sep-Dec",
    str_detect(file, "mar") ~ "Jan-May",
    str_detect(file, "jun") ~ "Jun-Aug",
    TRUE ~ ""
  )
}

exercise_year <- function(file)
  as.integer(str_extract(file, "\\d{4}"))

reference_code <- function(file) {
  if_else(str_detect(file, "proj"), if_else(str_detect(file, "nov2014"), 2L, 3L), exercise_code(file))
}

reference_label <- function(file) {
  if_else(str_detect(file, "proj"), if_else(str_detect(file, "nov2014"), "Jan-May", "Jun-Aug"), exercise_label(file))
}

reference_year <- function(file) {
  ex_year <- exercise_year(file)
  ex_code <- exercise_code(file)
  if_else(str_detect(file, "proj") & ex_code == 1L, ex_year + 1L, ex_year)
}


guess_file_encoding <- function(file) {
  guess_encoding(file) %>%
    slice(1) %>%
    pull(encoding)
}

dd <- map(files, function(l) {
  df <- suppressMessages(read_csv(l, locale = locale(encoding = guess_file_encoding(l))))
  df$exercise_year <- exercise_year(l)
  df$exercise_code <- exercise_code(l)
  df$reference_year <- reference_year(l)
  df$reference_code <- reference_code(l)
  df$exercise_label <- exercise_label(l)
  df$reference_label <- reference_label(l)
  df$chtype <- if_else(str_detect(l, "proj"), "projected", "current")
  df
})

## Bind all data
df <- bind_rows(dd)

## Remove accent and replace them by non ascii version
df$country <- iconv(str_to_title(df$country), from = "UTF-8", to = "ASCII//TRANSLIT")
df$country[str_which(df$country, regex("ivoire", ignore_case = TRUE))] <- "Cote d'Ivoire"
df$region <- iconv(str_to_title(df$region), from = "UTF-8",to = "ASCII//TRANSLIT")
df$adm1 <- iconv(str_to_title(df$adm1), from = "UTF-8", to = "ASCII//TRANSLIT")
df$adm2 <- iconv(str_to_title(df$adm2), from = "UTF-8", to = "ASCII//TRANSLIT")


### Rename columns 
all <- df %>%
  filter(!is.na(classif)) %>%
  select(adm0_name = country, region, adm1_name = adm1, adm2_name = adm2,
         population = pop, phase_class = classif, phase1:phase35,
         chtype, exercise_code, exercise_label, exercise_year, reference_code, reference_label, reference_year, 
         foodconsumption_phase, livelihoods_phase,	nutrition_phase,	mortality_phase)
         #FCG_Poor,	FCG_Borderline,	FCG_Acceptable,	FCG_finalphase,	HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,	HDDS_finalphase,	HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,
         #HHS_finalphase,	NoStrategies,	StressStrategies,	CrisisStrategies,	EmergencyStrategies,	LhHCSCat_finalphase,	rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3,	rCSI_finalphase

## Add geo dictionnary
url <- "https://docs.google.com/spreadsheets/d/1S9OPO-x8YUQbpJ06mrbOUDgZgQroJkbtwPjYM-9Eo6g"
geo_dict <- read_sheet(url, sheet = "distinct_dict", col_types = "c")
glimpse(geo_dict)

## Adm0 check
setdiff(unique(geo_dict$adm0_name), unique(all$adm0_name))
setdiff(unique(all$adm0_name), unique(geo_dict$adm0_name))

## Adm0.5 check
setdiff(unique(all$region), unique(geo_dict$region))
setdiff(unique(geo_dict$region), unique(all$region))

## Adm1 check
setdiff(unique(all$adm1_name), unique(geo_dict$adm1_name))
setdiff(unique(geo_dict$adm1_name), unique(all$adm1_name))

## Adm2 check
setdiff(unique(all$adm2_name), unique(geo_dict$adm2_name))
setdiff(unique(geo_dict$adm2_name), unique(all$adm2_name))

## Inner join with the geo_dict table
all_matched <- left_join(all, geo_dict) %>%
  select(-region, -adm1_name, -adm2_name,
         adm0_name, adm0_gaulcode = adm0_code, adm0_pcod3, adm0_pcod2,
         region = region_namechanged, adm1_name = adm1_namechanged, adm1_gaulcode = adm1_code, adm1_pcod3, adm1_pcod2,
         adm2_name = adm2_namechanged, adm2_gaulcode = adm2_code, adm2_pcod3, adm2_pcod2) %>%
  distinct()

## Check if some admin name
all_non_matched <- distinct(anti_join(all, geo_dict))
all_non_matched

### Reorganise columuns for export
all_matched <- select(all_matched,
                      adm0_name, adm0_gaulcode, adm0_pcod3, adm0_pcod2, region, adm1_name, adm1_gaulcode, adm1_pcod3, adm1_pcod2, adm2_name, adm2_gaulcode, adm2_pcod3, adm2_pcod2, everything())

## Clean names, start with Caps
all_matched <- distinct(all_matched)
all_matched <- mutate_if(all_matched, is.numeric, ~ as.integer(round(.x)))
all_matched$phase_class[all_matched$phase_class == 0] <- NA
all_matched <- distinct(all_matched)

### Save 2014-2018 Sahel/Nigeria + 2017+ for the rest (Coastal Countries)
sahel_countries_pcod3 <- c("BFA", "MLI", "MRT", "NER", "NGA", "SEN", "TCD")
sahel_nigeria <- filter(all_matched,
                        adm0_pcod3 %in% sahel_countries_pcod3)
rest <- filter(all_matched,
               !adm0_pcod3 %in% sahel_countries_pcod3,
               exercise_year >= 2017)
final <- bind_rows(sahel_nigeria, rest)

###
adm0_fiche_comm <- read_excel("data/fiche_comm/adm0_fiche_comm.xlsx")
glimpse(adm0_fiche_comm)
### Make nameng of reference label and exercise label coherent on #

## all_matched %>%
##     group_by(adm0_name, chtype, exercise_year, exercise_code, exercise_label, reference_year, reference_code, reference_label) %>%
##     summarise_at(vars(phase1:phase5), sum, na.rm = TRUE) %>%
##     ungroup() %>%
##     filter(exercise_year == 2018, exercise_label == "Sep-Dec", adm0_name == "Mali") %>%
##     View()

df_orig <- all_matched %>%
  group_by(adm0_name, chtype, exercise_year, exercise_code, exercise_label, reference_year, reference_code, reference_label) %>%
  summarise(total_phase35 = sum(phase35, na.rm = TRUE)) %>%
  mutate(total_phase35_data = round(total_phase35)) %>%
  select(-total_phase35) %>%
  ungroup()

###
df_fiche <- adm0_fiche_comm %>%
  group_by(adm0_name, chtype, exercise_year, exercise_code, reference_year, reference_code) %>%
  summarise(total_phase35 = sum(phase35, na.rm = TRUE)) %>%
  mutate(total_phase35_fiche = round(total_phase35)) %>%
  select(-total_phase35) %>%
  ungroup() %>%
  mutate(chtype = tolower(chtype))

compar_df <- left_join(df_orig, df_fiche) %>%
  select(-reference_code, -exercise_code)

### Add percentage of discrepancy
### Check 2019 data
compar_df %>%
  mutate(diff_abs = abs(total_phase35_data - total_phase35_fiche),
         diff_perc = round(100 * (diff_abs / total_phase35_data), 2)) %>%
  View()


compar_df %>%
  mutate(diff_abs = abs(total_phase35_data - total_phase35_fiche),
         diff_perc = round(100 * (diff_abs / total_phase35_data), 2)) %>%
  write_xlsx("data/processed/compar_fiche.xlsx")

##CH_data only
cadre_harmonise <- final %>% 
  select(-matches("district|adm3")) %>%
  filter(adm0_pcod3 != "CAF") 

cadre_harmonise <- cadre_harmonise %>% mutate_at(vars(ends_with("gaulcode")), funs(as.numeric))

cadre_harmonise %>%  write_xlsx("data/processed/cadre_harmonise.xlsx")

##CAR IPC data only - new is broken down at adm2 level where old is only adm0 from old IPC reports
caf_ipc_new <- final %>% select(-matches("district|adm3")) %>% filter(adm0_pcod3 == "CAF") 
caf_ipc_new <- caf_ipc_new %>% mutate_at(vars(ends_with("gaulcode")), funs(as.numeric))

caf_ipc_old <- read_excel("data/raw/IPC_CAF/adm0_CAF_IPC_reports.xlsx") %>% filter(UseThisPeriod == "Y") %>% 
  mutate(exercise_label = case_when(
  exercise_label == "Septembre - Decembre" ~ "Sep-Dec",
  exercise_label == "January - Mai" ~ "Jan-May",
  exercise_label == "Juin - Aout" ~ "Jun-Aug"),
  reference_label = case_when(
  reference_label == "Septembre - Decembre" ~ "Sep-Dec",
  reference_label == "January - Mai" ~ "Jan-May",
  reference_label == "Juin - Aout" ~ "Jun-Aug"))

caf_ipc_old <- caf_ipc_old %>% mutate(adm0_pcod2 = "CF", status = "outofcamp")

caf_ipc <- bind_rows(caf_ipc_new, caf_ipc_old)

caf_ipc <- caf_ipc %>% mutate_at(vars(ends_with("gaulcode")), funs(as.numeric))

write_xlsx(caf_ipc, "data/processed/caf_ipc.xlsx")

#combine CH and CAR IPC data

cadre_harmonise_caf_ipc <- bind_rows(cadre_harmonise, caf_ipc) %>% select(-UseThisPeriod)

#join in the Usethisperiod

usethisperiod_fromfiche <- adm0_fiche_comm %>% select(adm0_name, exercise_label, exercise_year, chtype, reference_label, reference_year)

cadre_harmonise_caf_ipc %>%  write_xlsx("data/processed/cadre_harmonise_caf_ipc.xlsx")




