pacman::p_load(
  conflicted,
  here,
  tidyverse)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# source(here("code","02_select_variables.R"))
# source(here("code","04_clean_long_dataset.R"))

# HCW <- read_xlsx(here("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/indices_key_HCW.xlsx"), sheet = "HCW_WHO") |> 
#   select(ISCO, HCW_subgroup)

source(here("code", "flowchart.R")) # Source the flowchart codefile to get final list of participants after filters are applied

## Read in the fuzzy matches dataset that needs to be cleaned ####
if (file.exists(here("data", "long_cleaned.rds"))) {
  dat_master_final <- readRDS(here("data", "long_cleaned.rds"))
} else {
  print("You need to uncomment line 21 in the prep_st23.R file")
}
# source(here("code", "04b_long_clean_fuzzy_classifications.R")) # uncomment this if long_cleaned.rds dataset is not yet generated

dat_st <- dat_master_final |> 
  inner_join(filtered_ids_nov23) |> 
  group_by(codbar) |> arrange(codbar, questionnaire) |> tidyr::fill(ends_with(".st_22"), .direction = "down") |> ungroup() |>
  # filter(questionnaire %in% c("st_22", "st_23")) |> 
  filter(questionnaire == "st_23") #|>
# filter(worked.st_23 == "Oui") |>
# filter(!is.na(date_soumission)) |> 
# filter(age < 65) #|>
# filter(!is.na(occupation_type.single)) |>
# filter(!is.na(hh_livewith_rec_en.inc)) |> 
# filter(education_rec_en.inc != "Other")
# filter(!is.na(burnout_score)) |> 
# select(-ends_with(".single"))

# # Get a single 
# dat_single <- dat_master_final |> 
#   # remove those where no occupation classification is possible
#   filter(!is.na(ISCO_label_full),
#          ISCO_label_full != "Recoding impossible") |> 
#   group_by(codbar) |> slice_max(order_by = date_soumission, n=1) |> ungroup()|> 
#   select(codbar, questionnaire, occupation_type, job_sector_harmonized, years_of_service, starts_with(c("ISCO_", "isco_"))) |> 
#   rename_with(~ paste(., "single", sep = "."), !matches(c("codbar")))

dat <- dat_st |> 
  # left_join(dat_st, dat_single) |> 
  # filter(age < 65 & age >= 25) #|> 
  # left_join(HCW, by = join_by("isco_full" == "ISCO")) |> 
  # filter(actively_working == "Yes") |> 
  mutate(
    age = time_length(date_soumission.st_23-birthdate.inc, "years"),
      age_cat = factor(case_when(
        age >= 18 & age < 25 ~ "18-24",
        age >= 25 & age < 35 ~ "25-34",
        age >= 35 & age < 45 ~ "35-44",
        age >= 45 & age < 55 ~ "45-54",
        age >= 55 & age < 65 ~ "55-64")),
    
    employed.st_23 = case_when(
      is.na(work_situation) ~ NA,
      str_detect(work_situation, "employed|independent") ~ TRUE,
      .default = FALSE),
    
    work_situation_rec = factor(case_when(
      is.na(work_situation) ~ "Other economically inactive",
      work_situation %in% c("employed", "employed ; short_leave", "employed ; volunteer",
                            "employed ; job_seeker", "employed ; student", "employed ; disability_insurance",
                            "employed ; long_leave", "employed ; parental_leave") ~ "Salaried",
      work_situation %in% c("self_employed", "self_employed ; job_seeker", "self_employed ; volunteer",
                            "self_employed ; Other") ~ "Independent",
      work_situation %in% c("employed ; self_employed", "employed ; self_employed ; volunteer",
                            "employed ; self_employed ; short_leave") ~ "Other economically active",
      work_situation %in% c("short_leave") ~ "Other economically active",
      work_situation %in% c("parental_leave") ~ "Other economically inactive",
      work_situation %in% c("long_leave") ~ "Other economically inactive",
      work_situation %in% c("retired") ~ "Other economically inactive",
      work_situation %in% c("disability_insurance") ~ "Other economically inactive",
      work_situation %in% c("job_seeker", "job_seeker ; Other", "job_seeker ; volunteer ; Other") ~ "Other economically inactive",
      work_situation %in% c("student") ~ "Other economically inactive",
      work_situation %in% c("volunteer") ~ "Other economically inactive",
      work_situation %in% c("choice") ~ "Other economically inactive",
      work_situation %in% c("unpaid_leave") ~ "Other economically inactive",
      str_detect(work_situation, "employed|independent") ~ "Other economically active",
      work_situation %in% c("Other") ~ "Other economically inactive",
      .default = "Other economically inactive")
      , levels = c("Salaried", "Independent", "Other economically active", "Other economically inactive")
    ),
    # work_situation_rec_infreq = fct_relevel(
    #   fct_infreq(fct_lump_n(factor(work_situation_rec), n = 10)),
    #   "Other", after = Inf)
    
    Other_subgroup.single = fct_relevel(factor(case_when(occupation_type.single == "General workforce" ~ ISCO_label_1.single, .default = NA)), "Professionals"),
    Other_subgroup_long.single = fct_relevel(factor(case_when(occupation_type.single == "General workforce" ~ ISCO_label_2.single, .default = NA)), "Business and administration associate professionals")
  ) |> 
  filter(!is.na(occupation_type.single)) |>
  # filter(work_situation_rec != "Other economically inactive") |> 
  droplevels()

rm(dat_st, dat_master_final
   # , dat_single
)

# dat_pre <- inner_join(dat_inclusion, dat_st_23, by = join_by("codbar" == "codbar")) |> 
#   left_join(dat_st_22) |> 
#   left_join(professions |> filter(source == "st_23"))
# 
# dat <- dat_pre |> 
#   mutate(
#     actively_working = case_when(actively_working ~ "Yes", 
#                                  actively_working == FALSE ~ "No", 
#                                  .default = NA),
#     burn_out = factor(case_when(
#       burn_out_logical.st_23 ~ "Yes",
#       burn_out_logical.st_23 == FALSE ~ "No",
#       .default = NA),
#       levels = c("No", "Yes")),
#     burn_out_numeric = as.numeric(burn_out)-1,
#     employed = factor(case_when(
#       status_employed.st_23|status_self_employed.st_23 ~ "Yes",
#       .default = "No"),
#       levels = c("Yes", "No"))
#   )
