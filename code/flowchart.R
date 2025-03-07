pacman::p_load(here)
source(here("code","02_select_variables.R"))
# invited_gh24 <- read_csv(here("data", "gh24_invites_population_anup_2024_08_14_15_43.csv")) |> rename(participant_id = ID.) |> select(participant_id)

invited_gh23 <- read_csv("P:/ODS/DMCPRU/UEPDATA/Pour_user/Pour_Anshu/invites_quests_2023/invites_q_suivisante_2023_ulao_2024_08_15_09_57.csv") |> rename(codbar = `Code barre`)

invited_st23 <- read_csv("P:/ODS/DMCPRU/UEPDATA/Pour_user/Pour_Anshu/invites_quests_2023/invites_q_santetravail_2023_ulao_2024_08_15_09_57.csv") |> rename(codbar = `Code barre`)

specchio_consented <- read_csv("P:/ODS/DMCPRU/UEPDATA/Pour_user/Pour_Anshu/invites_quests_2023/specchio_consent_anup_2024_08_15_17_09.csv") |> 
  rename(codbar = `Code barre`) # 6,801

# Create full dataset to then go through filtering steps ####
full_eligible <- left_join(dat_inclusion, date_last_submission)

# Restrict to those invited to GH23 or ST23, to establish upper limit of denominator
# dat_inclusion_filtered <- inner_join(dat_inclusion_filtered, invited_st23) # 8,612
# dat_inclusion_filtered <- inner_join(dat_inclusion_filtered, invited_gh23) # 10,805
full_eligible <- inner_join(full_eligible, specchio_consented) # 6,289

dat_monthly_wide <- dat_monthly_21 |> 
  mutate(questionnaire = str_remove_all(questionnaire, "Monthly_|_21")) |> 
  select(codbar, questionnaire, PHQ2_score.monthly) |>  
  pivot_wider(names_from = questionnaire, values_from = PHQ2_score.monthly, names_glue = "PHQ2_score_{questionnaire}")

full_eligible <- full_eligible |> 
  left_join(dat_monthly_wide) |> 
  left_join(dat_gh_22 |> select(codbar, starts_with("PHQ2_score"))) |> 
  left_join(dat_gh_23 |> select(codbar, starts_with("PHQ2_score"), date_soumission.gh_23)) |> 
  left_join(dat_sommeil_23 |> select(codbar, starts_with("PHQ2_score"))) |> 
  left_join(dat_st_23 |> select(codbar, starts_with("PHQ2_score"), PFI_score.st_23, date_soumission.st_23)) |> 
  mutate(
    age.st23 = time_length(ymd("2023-11-15") - birthdate.inc, "years"),
    across(starts_with("PHQ2_score"), function(x)(!is.na(x))),
    has_PHQ2_measure = as.integer(rowSums((across(starts_with("PHQ2_score"))))) # column for number of PHQ2 measures
  ) |> 
  left_join(
    dat_professions |> 
      filter(!is.na(ISCO_label_full),
             ISCO_label_full != "Recoding impossible") |> 
      group_by(codbar) |> 
      # Slice by max to get the latest occupation timepoint
      slice_max(order_by = date_soumission, n = 1) |> 
      ungroup(), by = join_by("codbar" == "codbar")
  )

# single <- dat_professions |> 
#   filter(!is.na(ISCO_label_full),
#          ISCO_label_full != "Recoding impossible") |> 
#   group_by(codbar) |> 
#   # Slice by max to get the latest occupation timepoint
#   slice_max(order_by = date_soumission, n = 1) |> 
#   ungroup()
# b <- anti_join(single, dat_inclusion_filtered)
# 
# c <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds")
# c <- c |> filter(codbar == "1810885")


# Invited to GH_24 to estab

# Age
full_eligible |> count(age.inc >= 18)
full_eligible |> count(age.inc < 65)
full_eligible <- full_eligible |> filter(age.inc < 65 & age.inc >= 18)
# full_eligible |> count(age.st23 < 65)
# dat_inclusion_filtered <- dat_inclusion_filtered |> filter(age.st23 < 65)

# # Participated beyond an inclusion questionnaire
# dat_inclusion_filtered |> count(beyond_inclusion)
# dat_inclusion_filtered <- dat_inclusion_filtered |> filter(beyond_inclusion)

# Retired
full_eligible |> count(work_situation_rec_en.inc == "Retired")
full_eligible <- full_eligible |> filter(work_situation_rec_en.inc != "Retired" | is.na(work_situation_rec_en.inc))

# At least one measure of occupational grouping
full_eligible |> count(!is.na(occupation_type))
dat_inclusion_filtered <- full_eligible |> filter(!is.na(occupation_type))

# At least one measure of PHQ2
dat_inclusion_filtered |> count(has_PHQ2_measure > 0)
dat_inclusion_filtered <- dat_inclusion_filtered |> filter(has_PHQ2_measure > 0)

# Filter not NA for education
dat_inclusion_filtered |> count(education_rec_en.inc == "Other")
dat_inclusion_filtered <- dat_inclusion_filtered |> filter(!is.na(education_rec_en.inc), education_rec_en.inc != "Other")
#
# dat_inclusion_filtered |> count(!is.na(hh_livewith.inc))
# dat_inclusion_filtered <- dat_inclusion_filtered |> filter(!is.na(hh_livewith.inc))


filtered_ids_all <- dat_inclusion_filtered |> select(codbar)

# Restrict to Sample of Nov 2023
dat_inclusion_filtered |> count(!is.na(date_soumission.st_23))

dat_nov23_filtered <- dat_inclusion_filtered |> filter(!is.na(date_soumission.st_23))
dat_nov23_filtered |> group_by(is.na(PFI_score.st_23)) |> count(PHQ2_score.st_23)
filtered_ids_nov23 <- dat_nov23_filtered |> 
  # filter(!is.na(PFI_score.st_23)) |> 
  select(codbar)

full_eligible_merged <- left_join(full_eligible, filtered_ids_all |> mutate(analyzed = TRUE)) |> 
  left_join(dat_nov23_filtered |> filter(!is.na(PFI_score.st_23)) |> select(codbar) |> mutate(analyzed_nov23 = TRUE)) |> 
  mutate(
    analyzed = case_when(analyzed ~ "Analyzed", .default = "Not analyzed"),
    analyzed_nov23 = case_when(analyzed_nov23 ~ "Analyzed", .default = "Not analyzed"))
full_eligible_merged |> count(analyzed)
full_eligible_merged |> count(analyzed_nov23)

rm(list = setdiff(ls(), c("filtered_ids_all", "filtered_ids_nov23", "full_eligible_merged")))

### ###
### After running this file, run the prep_st_23 file to see that 213 participants were economically inactive at this time point and also excluded (no PFI score for them)