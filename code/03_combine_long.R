pacman::p_load(here)
source(here("code","02_select_variables.R"))

### ### ### ###
# Create an empty long dataset for each timepoint ####
### ### ### ###

# use the inclusion file as the base file, merge with last submission dataset
dat_master_empty <- left_join(dat_inclusion, date_last_submission, 
                              by = "participant_id") |> 
  # filter(beyond_inclusion) |> # filter out those who only completed inclusion Q
  select(codbar) |> 
  # Create empty columns for relevant datasets that we want to include
  mutate(
    inclusion = NA,
    Monthly_Nov_20 = NA,
    Monthly_Dec_20 = NA,
    Monthly_Jan_21 = NA,
    Monthly_Feb_21 = NA,
    Monthly_Mar_21 = NA,
    Monthly_Apr_21 = NA,
    Monthly_May_21 = NA,
    Monthly_Jun_21 = NA,
    gh_21 = NA,
    mental_21 = NA,
    gh_22 = NA,
    st_22 = NA,
    gh_23 = NA,
    sommeil_23 = NA,
    st_23 = NA,
    gh_24 = NA
  ) |> 
  pivot_longer(
    cols = -c(codbar),
    names_to = "questionnaire",
    values_to = "date_soumission"
  )

### ### ###
# Combine the timepoints into a Master dataset ####
### ### ###

# Join with inclusion variable
dat_master <- dat_master_empty |> 
  left_join(dat_inclusion,
            by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

# Join with Monthlies
dat_master <- left_join(dat_master, dat_monthly,
            by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join Inclusion with General Health 2021 ####
dat_master <- left_join(dat_master, dat_gh_21,
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join Inclusion with General Health 2022 ####
dat_master <- left_join(dat_master, dat_gh_22,
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join with General Health 2023 ####
dat_master <- left_join(dat_master, dat_gh_23,
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join with General Health 2024 ####
dat_master <- left_join(dat_master, dat_gh_24,
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join with Mental Health 2021 ####
dat_master <- left_join(dat_master, dat_mental_21,
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join with Sommeil 2023 ####
dat_master <- left_join(dat_master, dat_sommeil_23,
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join with Sante Travail 2022 ####
dat_master <- left_join(dat_master, dat_st_22, 
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join with Sante Travail 2023 ####
dat_master <- left_join(dat_master, dat_st_23, 
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

## Join with cleaned professions dataset
dat_master <- left_join(dat_master, dat_professions |> rename(date_soumission_profession = date_soumission), 
                        by = join_by("codbar" == "codbar", "questionnaire" == "questionnaire"))

