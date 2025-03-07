# Generate a master dataset that merges all Specchio datasets

pacman::p_load(here)

# Read in the datasets file
source(here("code","01_read_in_datasets.R"))
source(here("code", "scale_functions.R"))
conflicted::conflicts_prefer(dplyr::between)
conflicted::conflicts_prefer(lubridate::month)
conflicted::conflicts_prefer(lubridate::year)


### ### ###
# Select only variables of interest, to make minimal datasets ####
### ### ###

## Inclusion - All ####
dat_inclusion <- inclusion |> 
  # Filter for only participants in one of the relevant studies
  filter(serocov_pop | pop_pilote | serocov_work | sp2_novdec_2020 | sp3_juin_2021 | sp4_avril_2022) |> 
  select(participant_id,  codbar, 
         serocov_pop, pop_pilote, serocov_work, sp2_novdec_2020, sp3_juin_2021, sp4_avril_2022,
         date_soumission, age, birthdate, sex_en, serocov_work, work_pilote,
         hh_income_cat_en, starts_with("child_age_y"),nat:nat_lang_other, Swiss_nat,
         chronic_disease, Pre_existing_MH_condition, Pre_existing_physical_condition,
         education, education_other, education_rec_en, 
         occupation_cat_en, work_situation_rec_en, work_rata, hh_income_cat_en,
         health_general,mental_state, finance_situation, smoking_rec_en, alcohol,
         hh_livewith, hh_livewith_rec_fr, relation,
         smoker) |> 
  rowwise() %>%
  mutate(num_young_children = sum(c_across(starts_with("child_age_y")) >= 2008, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # Income
    hh_income_cat_en = factor(hh_income_cat_en, levels = c("High", "Middle", "Low", "Don't know/don't wish to answer")),
    # Health
    health_general_en = factor(case_match(health_general,
                                          "Très bonne" ~ "Very good",
                                          "Bonne" ~ "Good",
                                          "Moyenne" ~ "Average",
                                          "Mauvaise" ~ "Poor",
                                          "Très mauvaise" ~ "Very poor"), 
                               levels=c("Very good", "Good", "Average", "Poor", "Very poor")),
    
    # Alcohol
    alcohol_en = fct_recode(alcohol,
                            "Never" = "Jamais",
                            "Occasionally" = "Occasionnellement",
                            "Once a week" = "Une fois par semaine",
                            "A few times a week" = "Quelques fois par semaine",
                            "Daily" = "Une fois par jour",
                            "Several times a day" = "Plusieurs fois par jour"
    ),
    
    # Relevel male as reference
    sex_en = fct_relevel(sex_en, "Male"),
    
    # Work situation releveled
    work_situation_rec_en = factor(work_situation_rec_en, levels = c("Salaried", "Freelance/sole trader", "Retired", 
                                                                     "Unemployed", "Other economically inactive")),
    
    # Migration status
    parents_ch = fct_recode(parents_ch, "Aucun de mes deux parents n'était de nationalité suisse" = "Aucun de mes deux parents était de nationalité suisse"),
    
    swiss_ethn = fct_relevel(factor(case_when(
      is.na(Swiss_nat) ~ NA,
      nat == "Suisse"|nat_other == "SUISSE"|nat2 == "Suisse"|nat2_other == "SUISSE" ~ "Swiss national",
      is.na(ethn) ~ NA,
      ethn == "Européenne - Caucasienne" ~ "Foreigner: European-Caucasian",
      .default = "Foreigner: Other")), "Swiss national"),
    
    swiss_national = fct_relevel(factor(case_when(
      is.na(Swiss_nat) ~ NA,
      nat == "Suisse"|nat_other == "SUISSE"|nat2 == "Suisse"|nat2_other == "SUISSE" ~ "Swiss",
      .default = "Non-Swiss")), "Swiss"),
    
    migration_status = factor(case_when(
      swiss_ethn == "Swiss national" & parents_ch %in% c("Mes deux parents étaient de nationalité suisse", "Un de mes deux parents était de nationalité suisse") ~ "Swiss at birth",
      swiss_ethn == "Swiss national" & parents_ch %in% c("Aucun de mes deux parents n'était de nationalité suisse") ~ "Swiss by naturalization",
      swiss_ethn == "Swiss national" & is.na(parents_ch) & birth_country == "Suisse" ~ "Swiss at birth",
      swiss_ethn == "Swiss national" & is.na(parents_ch) ~ "Swiss by naturalization",
      .default = "Non-Swiss"),
      levels = c("Swiss at birth", "Swiss by naturalization", "Non-Swiss")
    ),
    
    migration_status_ethn = factor(case_when(
      swiss_ethn == "Swiss national" & parents_ch %in% c("Mes deux parents étaient de nationalité suisse", "Un de mes deux parents était de nationalité suisse") ~ "Swiss at birth",
      swiss_ethn == "Swiss national" & parents_ch %in% c("Aucun de mes deux parents n'était de nationalité suisse") ~ "Swiss by naturalization",
      swiss_ethn == "Swiss national" & is.na(parents_ch) & birth_country == "Suisse" ~ "Swiss at birth",
      swiss_ethn == "Swiss national" & is.na(parents_ch) ~ "Swiss by naturalization",
      .default = swiss_ethn),
      levels = c("Swiss at birth", "Swiss by naturalization", "Foreigner: European-Caucasian", "Foreigner: Other")
    ),
    
    health_dich = case_when(
      health_general %in% c("Très bonne", "Bonne") ~ "Good",
      health_general %in% c("Moyenne", "Mauvaise", "Très mauvaise") ~ "Not good",
      .default = NA
    ),
    
    Pre_existing_MH_condition = factor(case_when(
      is.na(Pre_existing_MH_condition) ~ NA,
      Pre_existing_MH_condition ~ "Yes",
      .default = "No"), levels = c("No", "Yes")),
    
    Pre_existing_physical_condition = factor(case_when(
      is.na(Pre_existing_physical_condition) ~ NA,
      Pre_existing_physical_condition ~ "Yes",
      .default = "No"), levels = c("No", "Yes")),
    
    hh_income_cat_en = factor(hh_income_cat_en, levels = c("High", "Middle", "Low", "Don't know/don't wish to answer")),
    finance_situation_en = factor(case_match(
      finance_situation,
      "Je suis à l’aise, l’argent n’est pas une source d’inquiétude et il m'est facile d’épargner" ~ "Comfortable",
      "Mes revenus permettent de couvrir mes dépenses et de pallier d’éventuels imprévus mineurs" ~ "Can cover expenses",
      "Je dois faire attention à mes dépenses et un imprévu pourrait me mettre en difficulté financière" ~ "Cautious / struggling",
      "Je n’arrive pas à couvrir mes besoins avec mon revenu et j'ai besoin d’un soutien externe pour fonctionner (endettement, crédits, aides financières diverses)" ~ "Cautious / struggling",
      "Je ne souhaite pas répondre" ~ "Don't want to answer"
    ),
    levels = c("Comfortable", "Can cover expenses", "Cautious / struggling", "Don't want to answer")),
    num_young_children_dich = factor(case_when(
      num_young_children == 0 ~ "No young children",
      num_young_children > 0 ~ "Has young children",
      .default = NA),
      levels = c("No young children", "Has young children"))
  ) |> 
  rename_with(~ paste(., "inc", sep = "."), !matches(c("codbar", "participant_id"))) |> 
  mutate(questionnaire = "inclusion")

# ## Inclusion - KIDS ####  --> Still need to combine this into the master_specchio dataset
# dat_inc_kids <- inc_kids |> 
#   select(parent1_codbar, date_soumission) |> 
#   rename_with(~ paste(., "inc_kids", sep = "."), !matches(c("codbar")))

## General Health ####
### 2021 ####
dat_gh_21 <- gh_21 |> 
  select(codbar, date_soumission, gh_health, gh_morale, gh_cov_long, gh_cov_long_cured) |> 
  rename_with(~ paste(., "gh_21", sep = "."), !matches(c("codbar"))) |> 
  mutate(questionnaire = "gh_21")

### 2022 ####
dat_gh_22 <- gh_22 |> 
  select(codbar, date_soumission, age, events_job_loss, events_burn_out, events_other_text, health, morale, GAD2_score, PHQ2_score, starts_with("points_mh"), UCLA_score, oslo_score, h_event_long_covid, who_score, who_interpretation) |> 
  rename_with(~ paste(., "gh_22", sep = "."), !matches(c("codbar"))) |> 
  mutate(questionnaire = "gh_22")

### 2023 ####
dat_gh_23 <- gh_23 |> 
  select(codbar, date_soumission, age, events_job_loss, events_retirement, events_moving, events_other_text, d_psycho_professional_burnout, d_psycho_personnal_burnout, 
         health, morale, GAD2_score, PHQ2_score, starts_with("points_problem"), UCLA_score, oslo_score, long_covid, h_event_long_covid,
         who_score, who_interpretation) |> 
  rename_with(~ paste(., "gh_23", sep = "."), !matches(c("codbar"))) |> 
  mutate(questionnaire = "gh_23")

### 2024 ####
dat_gh_24 <- gh_24 |>
  select(codbar, date_soumission, age, starts_with("g4h_feeling"), starts_with("g4h_problem"), g4h_health) |> 
  mutate(
    # Health
    health = factor(case_match(g4h_health, 5L ~ "Très bonne", 4L ~ "Bonne", 3L ~ "Moyenne", 2L ~ "Mauvaise", 1L ~ "Très mauvaise"),
                    levels = c("Très bonne", "Bonne", "Moyenne", "Mauvaise", "Très mauvaise")),
    # WHO
    across(starts_with("g4h_feeling"), function(x) (5-(x-1))), # subtract 1 and reverse the scale (responses coded 1-6, with "Never" coded as 6, while it should be 0-5 with "Never" coded as 0
    who_score = 4L * as.integer(rowSums((across(starts_with("g4h_feeling"))))),
    who_interpretation = factor(case_when(
      who_score <= 28 ~  "<=28: Higher risk of depression",
      who_score <= 50 ~  "29-50: 'Screening diagnosis' of depression",
      who_score > 50 ~ "> 50: Lower risk of depression",
    ), levels = c("<=28: Higher risk of depression",
                  "29-50: 'Screening diagnosis' of depression",
                  "> 50: Lower risk of depression")),
    
    # GAD2 and PHQ2
    across(starts_with("g4h_problem"), function(x) (replace(x, x %in% 100, 0))), # replace values of 100 with 0
    GAD2_score = as.integer(rowSums(across(c("g4h_problem_nervous_anxious", "g4h_problem_stop_being_worried")))),
    GAD2_interpretation = factor(case_when(
      GAD2_score < 3 ~ "<3: Doesn't suggest anxiety",
      GAD2_score >=3 ~ ">=3: Suggests further diagnostic evaluation for generalized anxiety disorder is warranted"
    ), levels = c("<3: Doesn't suggest anxiety", ">=3: Suggests further diagnostic evaluation for generalized anxiety disorder is warranted")),
    PHQ2_score = as.integer(rowSums(across(c("g4h_problem_little_interest_pleasure", "g4h_problem_sad_depressed_hopeless")))),
    PHQ2_interpretation = factor(case_when(
      PHQ2_score < 3 ~ "<3: Depression unlikely",
      PHQ2_score >=3 ~ ">=3: Major depressive disorder is likely"
    ), levels = c("<3: Depression unlikely", ">=3: Major depressive disorder is likely"))
  ) |> 
  rename_with(~ paste(., "gh_24", sep = "."), !matches(c("codbar"))) |>
  mutate(questionnaire = "gh_24")

## Sommeil ####
### 2023 ####
dat_sommeil_23 <- sommeil_23 |> 
  select(codbar, date_soumission, 
         # age, situation_night_work, situation_shift_work, address, screen_time_work, 
         GAD2_score, PHQ2_score, starts_with("points_mh"), PSQI_score) |> 
  rename_with(~ paste(., "sommeil_23", sep = "."), !matches(c("codbar"))) |> 
  mutate(questionnaire = "sommeil_23")

## Mental health: 2021 ####
dat_mental_21 <- mental_21 |> 
  select(codbar, date_soumission, age, UCLA_score, oslo_score, starts_with("mh_sentiment")) |> 
  mutate(
    across(starts_with("mh_sentiment"), as.integer), # convert from character to integer
    across(starts_with("mh_sentiment"), ~.-1L), # subtract 1 (responses coded 1-6, should be 0-5)
    who_score = 4L * as.integer(rowSums((across(starts_with("mh_sentiment"))))),
    who_interpretation = factor(case_when(
      who_score <= 28 ~  "<=28: Higher risk of depression",
      who_score <= 50 ~  "29-50: 'Screening diagnosis' of depression",
      who_score > 50 ~ "> 50: Lower risk of depression",
    ), levels = c("<=28: Higher risk of depression",
                  "29-50: 'Screening diagnosis' of depression",
                  "> 50: Lower risk of depression"))
  ) |> 
  rename_with(~ paste(., "mental_21", sep = "."), !matches(c("codbar"))) |> 
  mutate(questionnaire = "mental_21")

# ## Health Behavior: 2022 ####
# dat_hb_22 <- hb_22 |> 
#   select(codbar, date_soumission, age, job_phys_activity, job_phys_activity_other) |> 
#   rename_with(~ paste(., "hb_22", sep = "."), !matches(c("codbar"))) |> 
#   mutate(questionnaire = "hb_22")

# Santé Travail ####
### 2022 ####
dat_st_22 <- st_22 |> 
  select(codbar, date_soumission,  age, burn_out, employed:not_employed_comment,
         work_situation, work_situation_other,
         commute_work_from_home:sedentary_work_other, protective_equipment:protection_other,
         burn_out:burn_out_result_other, pandemic_change:impact_pandemic_comment, burnout_score,
         work_sick
  ) |> 
  mutate(
    work_sick = factor(case_when(work_sick == "Oui" ~ "Yes",
                                 work_sick == "Non" ~ "No",
                                 work_sick == "Je n’ai pas été malade au cours des 12 derniers mois" ~ "Not sick past 12 months",
                                 .default = NA),
                       levels = c("Not sick past 12 months", "No", "Yes")),
    burn_out_logical = case_when(
      burn_out == "Oui" ~ TRUE,
      burn_out == "Non" ~ FALSE,
      .default = NA),
    burnout_interp = factor(case_when(between(burnout_score, 0, 17) ~ "Mild",
                                      between(burnout_score, 18, 29) ~ "Moderate",
                                      burnout_score >= 30 ~ "Severe"),
                            levels = c("Mild", "Moderate", "Severe"))
  ) |> 
  rename_with(~ paste(., "st_22", sep = "."), !matches(c("codbar"))) |> 
  mutate(questionnaire = "st_22")

### 2023 ####
dat_st_23 <- st_23 |> 
  select(codbar, date_soumission, age, worked:workplace_size,years_of_service:move_work_other_text, -c(job_sector_commerce:job_sector_99),health, starts_with("maslach"), burnout, burnout_consult, work_life_balance, starts_with("problem"), starts_with("karasek"), starts_with("eri_"), work_sick,
         starts_with("pfi_scale")) |> 
  mutate(
    work_sick = factor(case_when(work_sick == "Oui" ~ "Yes",
                                 work_sick == "Non" ~ "No",
                                 work_sick == "Je n’ai pas été malade au cours des 12 derniers mois" ~ "Not sick past 12 months",
                                 .default = NA),
                       levels = c("Not sick past 12 months", "No", "Yes")),
    # GAD2 and PHQ2
    across(starts_with("problem_"), assign_PHQ4_points, .names = "points_{.col}"),
    GAD2_score = as.integer(rowSums(across(c("points_problem_nervous_anxious", "points_problem_stop_being_worried")))),
    GAD2_interpretation = factor(case_when(
      GAD2_score < 3 ~ "<3: Doesn't suggest anxiety",
      GAD2_score >=3 ~ ">=3: Suggests further diagnostic evaluation for generalized anxiety disorder is warranted"
    ), levels = c("<3: Doesn't suggest anxiety", ">=3: Suggests further diagnostic evaluation for generalized anxiety disorder is warranted")),
    PHQ2_score = as.integer(rowSums(across(c("points_problem_little_interest_pleasure", "points_problem_sad_depressed_hopeless")))),
    PHQ2_interpretation = factor(case_when(
      PHQ2_score < 3 ~ "<3: Depression unlikely",
      PHQ2_score >=3 ~ ">=3: Major depressive disorder is likely"
    ), levels = c("<3: Depression unlikely", ">=3: Major depressive disorder is likely")),
    
    # MBI Emotional Exhaustion
    across(starts_with("maslach_"), cq_assign_burnout_points, .names = "points_{.col}"),
    burnout_score = as.integer(rowSums(across(starts_with("points_maslach_")))),
    burnout_interp = factor(case_when(between(burnout_score, 0, 17) ~ "Mild",
                                      between(burnout_score, 18, 29) ~ "Moderate",
                                      burnout_score >= 30 ~ "Severe"),
                            levels = c("Mild", "Moderate", "Severe")),
    burn_out_logical = case_when(
      burnout == "Oui" ~ TRUE,
      burnout == "Non" ~ FALSE,
      .default = NA),
    
    diagnosed_burnout = case_when(
      is.na(burnout) ~ NA,
      burnout == "Oui" & burnout_consult == "Oui avec un diagnostic de burn-out par un professionnel de santé" ~ "Yes",
      .default = "No"),
    
    # Karasek
    across(starts_with("karasek_"), cq_assign_Karasek_points, .names = "points_{.col}"),
    ## Karasek - Demands
    ### invert and rescale
    points_karasek_demand_1_time = 5-points_karasek_demand_1_time,
    ### Calculate scale
    demands_karasek = as.integer(rowSums(across(starts_with("points_karasek_demand")))),
    ## Karasek - Autonomy
    ### invert and rescale
    points_karasek_autonomy_1_decision = 4*points_karasek_autonomy_1_decision,
    points_karasek_autonomy_1_few_freedoms = 4*(5-points_karasek_autonomy_1_few_freedoms),
    points_karasek_autonomy_1_can_influence = 4*points_karasek_autonomy_1_can_influence,
    points_karasek_autonomy_1_repetitive_tasks = 2*(5-points_karasek_autonomy_1_repetitive_tasks),
    points_karasek_autonomy_2_high_skill = 2*points_karasek_autonomy_2_high_skill,
    points_karasek_autonomy_2_varied_actitivy = 2*points_karasek_autonomy_2_varied_actitivy,
    points_karasek_autonomy_2_learn_new_things = 2*points_karasek_autonomy_2_learn_new_things,
    points_karasek_autonomy_2_creativity = 2*points_karasek_autonomy_2_creativity, 
    points_karasek_autonomy_2_develop_skills = 2*points_karasek_autonomy_2_develop_skills,
    ### Calculate scale
    autonomy_karasek = as.integer(rowSums(across(starts_with("points_karasek_autonomy")))),
    # Karasek - Social Support (no inverting / rescaling)
    social_support_karasek = as.integer(rowSums(across(starts_with("points_karasek_support")))),
    
    # Job strain / Isostrain
    work_type_karasek = case_when(
      demands_karasek > 20 & autonomy_karasek >= 71 ~ "Dynamic (active)",
      demands_karasek > 20 & autonomy_karasek < 71 ~ "Job strain",
      demands_karasek <= 20 & autonomy_karasek < 71 ~ "Passive",
      demands_karasek <= 20 & autonomy_karasek >= 71 ~ "Relaxed",
      .default = NA
    ),
    job_strain_karasek = case_when(
      is.na(demands_karasek) | is.na(autonomy_karasek) ~ NA,
      demands_karasek > 20 & autonomy_karasek < 71 ~ TRUE,
      .default = FALSE
    ),
    isostrain_karasek = case_when(
      is.na(job_strain_karasek) ~ NA,
      job_strain_karasek & social_support_karasek < 24 ~ TRUE,
      .default = FALSE
    ),
    
    # Siegrist
    across(starts_with("eri_"), cq_assign_Karasek_points, .names = "points_{.col}"),
    ## Efforts
    efforts_siegrist = as.integer(rowSums(across(starts_with("points_eri_effort")))),
    ## Rewards
    ### Reverse coding
    points_eri_reward_1_low_promotion = 5-points_eri_reward_1_low_promotion,
    points_eri_reward_1_undesirable_change = 5-points_eri_reward_1_undesirable_change,
    points_eri_reward_1_job_security = 5-points_eri_reward_1_job_security,
    ### Calculate Scale
    rewards_siegrist = as.integer(rowSums(across(starts_with("points_eri_reward")))),
    ### Calculate subscales
    rewards_esteem_siegrist = as.integer(points_eri_reward_1_supervisor_respect+points_eri_reward_2_respect),
    rewards_security_siegrist = as.integer(points_eri_reward_1_undesirable_change+points_eri_reward_1_job_security),
    rewards_promotion_siegrist = as.integer(points_eri_reward_1_low_promotion+points_eri_reward_2_adequate_promotion+points_eri_reward_2_adequate_salary),
    ## Overcommitment
    ### Reverse coding
    points_eri_overinvestment_2_relax = 5-points_eri_overinvestment_2_relax,
    ### Calculate scale
    overcommitment_siegrist = as.integer(rowSums(across(starts_with("points_eri_overinvestment")))),
    
    # Efforts-Rewards ratio
    ER_ratio = efforts_siegrist / (rewards_siegrist * (3/7)),
    ER_ratio_new = (efforts_siegrist / rewards_siegrist) * (7/3),
    
    # PFI
    across(starts_with("pfi_scale_"), assign_PFI_points, .names = "points_{.col}"),
    PFI_score = rowMeans(across(starts_with("points_pfi_scale_"))),
    PFI_fulfillment = factor(case_when(PFI_score >= 3 ~ "Yes (PFI >= 3)",
                                       PFI_score < 3 ~ "No (PFI < 3)",
                                       .default = NA),
                             levels = c("No (PFI < 3)", "Yes (PFI >= 3)"))
  ) |> 
  rename_with(~ paste(., "st_23", sep = "."), !matches(c("codbar"))) |> 
  mutate(questionnaire = "st_23")

# Cleaned professions ####
dat_professions <- professions |> 
  select(codbar, source, date_soumission, master_profession_original, ISCO_label_full:ISCO_label_1, complementary_info, management, confidence, isco_full:isco_1, 
         years_of_service, work_situation, actively_working, occupation_type, HCW_subgroup, key_subgroup, job_sector, job_sector_harmonized, sector_self_reported) |> 
  mutate(questionnaire = case_when(source %in% c("Inclusion", "Work", "inc_kids") ~ "inclusion", .default = source)) |> 
  relocate(questionnaire, .after = source)
# Split the dataset by source so we can get one profession for Inclusion filled in from inc_kids and Work
a_prof = dat_professions |> filter(source %in% c("st_22", "st_23"))
b_prof = dat_professions |> filter(source %in% c("Inclusion", "inc_kids", "Work")) |> 
  mutate(source = factor(source, levels = c("Inclusion", "inc_kids", "Work"))) |> 
  group_by(codbar) |> 
  slice_min(order_by = source, n = 1)
# Recombine again into dat_professions
dat_professions <- rbind(a_prof, b_prof) |> 
  group_by(codbar) |> 
  arrange(codbar, date_soumission) |> 
  tidyr::fill(years_of_service, .direction = "up") |> 
  tidyr::fill(years_of_service, .direction = "down") |> 
  ungroup() |> 
  mutate(years_of_service = factor(years_of_service, 
                                   levels = c("10 ans ou plus", "De plus de 5 ans à 10 ans", 
                                              "De plus d’1 an à 5 ans", "De 6 mois à 1 an", "Moins de 6 mois")))

# Monthlies ####
## 2020-2021 ####
dat_monthly_20 <- monthly_20 |> select(codbar, date_soumission, m_loneliness:m_mh_negative_emotions) |> 
  mutate(
    Month = factor(as.character(month(date_soumission, label = TRUE, locale = "en_GB.UTF-8"))
                   # , levels = c("Nov", "Dec", "Jan", "Mar", "Apr")
    ),
    Year = str_sub(year(date_soumission),start = 3, end = 4),
    
    # Points from numbers (minus 1 to start scale from 0)
    points_mh_low_interest = m_mh_low_interest - 1,
    points_mh_depression = m_mh_depression - 1,
    points_mh_nervous = m_mh_nervous - 1,
    points_mh_negative_emotions = m_mh_negative_emotions - 1,
    
    # GAD2 and PHQ2
    GAD2_score = as.integer(rowSums(across(c("points_mh_nervous", "points_mh_negative_emotions")))),
    GAD2_interpretation = factor(case_when(
      GAD2_score < 3 ~ "<3: Doesn't suggest anxiety",
      GAD2_score >=3 ~ ">=3: Suggests further diagnostic evaluation for generalized anxiety disorder is warranted"
    ), levels = c("<3: Doesn't suggest anxiety", ">=3: Suggests further diagnostic evaluation for generalized anxiety disorder is warranted")),
    
    PHQ2_score = as.integer(rowSums(across(c("points_mh_low_interest", "points_mh_depression")))),
    PHQ2_interpretation = factor(case_when(
      PHQ2_score < 3 ~ "<3: Depression unlikely",
      PHQ2_score >=3 ~ ">=3: Major depressive disorder is likely"
    ), levels = c("<3: Depression unlikely", ">=3: Major depressive disorder is likely"))
  ) |> 
  mutate(questionnaire = paste0("Monthly_", Month, "_", Year)) |> 
  select(codbar, date_soumission, starts_with("points_"), ends_with(c("_score", "_interpretation")), questionnaire) |> 
  rename_with(~ paste(., "monthly", sep = "."), !matches(c("codbar"))) |> 
  rename(questionnaire = questionnaire.monthly)

## 2021 ####
dat_monthly_21 <- monthly |>
  select(codbar, date_soumission, loneliness:mh_negative_emotions) |>
  arrange(codbar, date_soumission) |> 
  mutate(
    Month = factor(as.character(month(date_soumission, label = TRUE, locale = "en_GB.UTF-8"))
                   # ,levels = c("Feb", "Mar", "Apr", "May", "Jun")
    ),
    # GAD2 and PHQ2
    across(starts_with("mh_"), assign_PHQ4_points, .names = "points_{.col}"),
    GAD2_score = as.integer(rowSums(across(c("points_mh_nervous", "points_mh_negative_emotions")))),
    GAD2_interpretation = factor(case_when(
      GAD2_score < 3 ~ "<3: Doesn't suggest anxiety",
      GAD2_score >=3 ~ ">=3: Suggests further diagnostic evaluation for generalized anxiety disorder is warranted"
    ), levels = c("<3: Doesn't suggest anxiety", ">=3: Suggests further diagnostic evaluation for generalized anxiety disorder is warranted")),
    PHQ2_score = as.integer(rowSums(across(c("points_mh_low_interest", "points_mh_depression")))),
    PHQ2_interpretation = factor(case_when(
      PHQ2_score < 3 ~ "<3: Depression unlikely",
      PHQ2_score >=3 ~ ">=3: Major depressive disorder is likely"
    ), levels = c("<3: Depression unlikely", ">=3: Major depressive disorder is likely")),
  ) |> 
  mutate(questionnaire = paste0("Monthly_",Month,"_21")) |> 
  select(codbar, date_soumission, starts_with("points_"), ends_with(c("_score", "_interpretation")), questionnaire) |> 
  rename_with(~ paste(., "monthly", sep = "."), !matches(c("codbar"))) |> 
  rename(questionnaire = questionnaire.monthly)

dat_monthly <- rbind(dat_monthly_20, dat_monthly_21)
