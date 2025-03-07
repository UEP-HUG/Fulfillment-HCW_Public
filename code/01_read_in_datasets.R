pacman::p_load(
  tidyverse,
  here,
  readxl,
  conflicted
)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)


# Read in datasets ####

## Inclusion ####
### All ####
inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-09-20-1819_ALLincsVs0123_ALLparticipants.rds") |> 
  # Remove the V5_inclusion people, as these are bus_santé people and there are some duplicates
  filter(Origin != "V5_inclusion") |>
  filter(!testeur) |>  # remove data produced by testers - I think Nick already did this
  filter(!str_starts(codbar, "T"))  |>  # Remove any people with codbar beginning with T (also testers)
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

latest_inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds")

### Birthdates variable to merge with others ####
birthdates <- inclusion |> select(codbar, birthdate)

### KIDS inclusion ####
inc_kids <- readRDS("P:/ODS/DMCPRU/UEPDATA/SEROCoV-KIDS/99_data/6_database/00_cleaned_augmented_database/old_kids_database/2024-01-24_KIDS_inclusion_nov21_202401231106.rds")
inc_kids <- inc_kids |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### KIDS - Parents' inclusion ####
incl_parents <- readRDS("P:/ODS/DMCPRU/UEPDATA/SEROCoV-KIDS/99_data/6_database/00_cleaned_augmented_database/2023-11-20_Parents_inclusion_202310240901.rds")|> 
  mutate(
    parent1_date_soumission = as_date(parent1_date_soumission),
    parent1_age = time_length(parent1_date_soumission - parent1_birthdate, "years"))



## General Health ####
### 2021 ####
gh_21 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-09-21-1217_SC19_general_health_31.05.2021-202309201254_ALL_cleaned_sym.rds") |> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### 2022 ####
gh_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-09-21-1208_SC19_general_health_v1_02_2022-202309201254_ALL.rds") |> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### 2023 ####
gh_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-09-21-1151_general_health_v2_03_2023-202309201254_ALL.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### 2024 ####
gh_24 <- data.table::fread("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/Raw datasets/SC19_general_health_V3_03_2024-202404230941.csv")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

## Sommeil ####
### 2023 ####
sommeil_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-09-21-1120_SC_sleep_health_06_23-202309201254_ALL.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

## Mental health ####
mental_21 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-09-21-1138_SC19_mental_health_06_2021-202309201254_ALL.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

## Health Behavior ####
### 2022 ####
hb_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-09-21-1126_SC19_health_behaviour_05_2022-202309201254_ALL.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

## Santé Travail ####
### 2022 ####
st_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2023-09-21-1145_SanteTravail_ALLparticipants.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"),
    profession = case_when(profession == "Techni^cien informatique" ~ "Technicien informatique",.default = profession)
  )

### 2023 ####

st_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/papers_database/SP_professional_fulfillment_Nov24_ANUP/santé_travail_11_2023-202401221247.rds")|> # preliminary version from Sergeui
  left_join(birthdates) |>
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years")) |> 
  group_by(participant_id) |> filter(n() < 2) |> ungroup() # remove duplicate entries

# Last submission date ####
date_last_submission <- read_csv("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/rapport_sugar_date_dernière_soumission_all18+_mzab_2024.02.15.csv") |> 
  rename(participant_id = `Participant ID`,date_inclusion = `Date de réponse au questionnaire inclusion`,
         date_last_submission = `Date de dernière soumission (hors résultat)`) |> 
  mutate(date_inclusion = dmy(date_inclusion),
         date_last_submission = dmy(date_last_submission),
         beyond_inclusion = date_last_submission > date_inclusion
  )

# Professions ####
professions <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/2024-06-26-1506_ISCO_fuzzy_recoded_occupations.rds")

# ## Monthly questionnaires ####
monthly <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/00_Archive_not_coded/2021_monthlies_feb2june.rds")|>
  mutate(date_soumission = as_date(date_sub))
# length(unique(monthly$codbar))

monthly_20 <- read_csv("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/papers_database/SP_professional_fulfillment_Nov24_ANUP/SC19_mensuel_v0_08.2020-202404050826.csv", skip = 1) |> 
  janitor::clean_names() |> 
  mutate(date_soumission = as_date(date_soumission))
