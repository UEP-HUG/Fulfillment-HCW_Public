pacman::p_load(here)
source(here("code","03_combine_long.R"))

# ### ### ###
# # Keep only Master Dataset ####
# ### ### ###
# rm(list=setdiff(ls(), c("dat_master")))

# List of participants who completed a Specchio inclusion questionnaire before Feb 2021 (arbitrary date --> to choose)
filter_participants <- dat_inclusion |> 
  # filter(date_soumission.inc < as_date("2021-02-01")) |> # filter out participants who filled inclusion after Jan 2021
  # filter(str_detect(work_situation_rec_en.inc, "Salaried|Freelance")) |> # Filter out people not working at baseline
  select(codbar) |> inner_join(dat_st_23 |> select(codbar))# filter only for participants who completed the st_23 questionnaire


dat_master_clean <- dat_master |> 
  relocate(master_profession_original, .after = questionnaire) |> 
  relocate(source, .after = questionnaire) |> 
  rename(profession_source = source) |> 
  mutate(
    # harmonized health variable for each time point
    health = case_when(
      questionnaire == "inclusion" ~ health_general.inc,
      questionnaire == "gh_21" ~ gh_health.gh_21,
      questionnaire == "gh_22" ~ health.gh_22,
      questionnaire == "gh_23" ~ health.gh_23,
      questionnaire == "gh_24" ~ health.gh_24,
      questionnaire == "st_23" ~ health.st_23,
      .default = NA),
    health_numeric = abs(as.numeric(health) -5), # Invert so that low score is low self-rated health
    health_dich = case_when(
      health %in% c("Très bonne", "Bonne") ~ "Good",
      health %in% c("Moyenne", "Mauvaise", "Très mauvaise") ~ "Not good",
      .default = NA
    ),
    # harmonized morale variables
    morale = case_when(
      questionnaire == "inclusion" ~ mental_state.inc,
      questionnaire == "gh_21" ~ gh_morale.gh_21,
      questionnaire == "gh_22" ~ morale.gh_22,
      questionnaire == "gh_23" ~ morale.gh_23,
      .default = NA),
    morale_numeric = abs(as.numeric(morale) -5), # Invert so that low score is low self-rated health
    morale_dich = case_when(
      morale %in% c("Très bon", "Bon") ~ "Good",
      morale %in% c("Moyen", "Mauvais", "Très mauvais") ~ "Not good",
      .default = NA
    ),
    # harmonized submission date for each time point
    date_soumission = case_when(
      questionnaire == "inclusion" ~ date_soumission.inc,
      str_detect(questionnaire, "Monthly_") ~ date_soumission.monthly,
      questionnaire == "gh_21" ~ date_soumission.gh_21,
      questionnaire == "gh_22" ~ date_soumission.gh_22,
      questionnaire == "gh_23" ~ date_soumission.gh_23,
      questionnaire == "gh_24" ~ date_soumission.gh_24,
      questionnaire == "mental_21" ~ date_soumission.mental_21,
      questionnaire == "sommeil_23" ~ date_soumission.sommeil_23,
      questionnaire == "st_22" ~ date_soumission.st_22,
      questionnaire == "st_23" ~ date_soumission.st_23,
      .default = NA),
    # harmonized diagnosed burnout
    burn_out = case_when(
      questionnaire == "gh_22" ~ events_burn_out.gh_22,
      questionnaire == "st_22" ~ burn_out_logical.st_22,
      questionnaire == "st_23" ~ burn_out_logical.st_23,
      .default = NA
    ),
    burn_out = factor(case_when(
      burn_out ~ "Yes",
      burn_out == FALSE ~ "No",
      .default = NA),
      levels = c("No", "Yes")),
    burn_out_numeric = as.numeric(burn_out)-1,
    
    # harmonized Maslach burnout score
    burnout_score = case_when(
      questionnaire == "st_22" ~ burnout_score.st_22,
      questionnaire == "st_23" ~ burnout_score.st_23,
      .default = NA
    ),
    burnout_interp = factor(case_when(between(burnout_score, 0, 29) ~ "Mild/Moderate",
                                      burnout_score >= 30 ~ "Severe"),
                            levels = c("Mild/Moderate", "Severe")),
    # Harmonized GAD2
    GAD2_score = case_when(
      str_detect(questionnaire, "Monthly_") ~ GAD2_score.monthly,
      questionnaire == "gh_22" ~ GAD2_score.gh_22,
      questionnaire == "gh_23" ~ GAD2_score.gh_23,
      questionnaire == "gh_24" ~ GAD2_score.gh_24,
      questionnaire == "sommeil_23" ~ GAD2_score.sommeil_23,
      questionnaire == "st_23" ~ GAD2_score.st_23,
      .default = NA
    ),
    GAD2_anxiety = factor(case_when(GAD2_score >= 3 ~ "Yes", GAD2_score <3 ~ "No", .default = NA),
                          levels = c("No", "Yes")),
    GAD2_anxiety_binary = case_when(GAD2_anxiety == "Yes" ~ 1,
                                    GAD2_anxiety == "No" ~ 0,
                                    .default = NA),

    # Harmonized PHQ2
    PHQ2_score = case_when(
      str_detect(questionnaire, "Monthly_") ~ PHQ2_score.monthly,
      questionnaire == "gh_22" ~ PHQ2_score.gh_22,
      questionnaire == "gh_23" ~ PHQ2_score.gh_23,
      questionnaire == "gh_24" ~ PHQ2_score.gh_24,
      questionnaire == "sommeil_23" ~ PHQ2_score.sommeil_23,
      questionnaire == "st_23" ~ PHQ2_score.st_23,
      .default = NA
    ),
    PHQ2_depression = factor(case_when(PHQ2_score >= 3 ~ "Yes", PHQ2_score <3 ~ "No", .default = NA),
                             levels = c("No", "Yes")),
    PHQ2_depression_binary = case_when(PHQ2_depression == "Yes" ~ 1,
                                       PHQ2_depression == "No" ~ 0,
                                       .default = NA),
    
    # Longitudinal variable grouping the component points
    GAD2_points_nervous = case_when(
      str_detect(questionnaire, "Monthly_") ~ points_mh_nervous.monthly,
      questionnaire == "gh_22" ~ points_mh_nervous.gh_22,
      questionnaire == "gh_23" ~ points_problem_nervous_anxious.gh_23,
      questionnaire == "gh_24" ~ g4h_problem_nervous_anxious.gh_24,
      questionnaire == "sommeil_23" ~ points_mh_nervous.sommeil_23,
      questionnaire == "st_23" ~ points_problem_nervous_anxious.st_23,
      .default = NA
    ),
    
    GAD2_points_negative_emotions = case_when(
      str_detect(questionnaire, "Monthly_") ~ points_mh_negative_emotions.monthly,
      questionnaire == "gh_22" ~ points_mh_negative_emotions.gh_22,
      questionnaire == "gh_23" ~ points_problem_stop_being_worried.gh_23,
      questionnaire == "gh_24" ~ g4h_problem_stop_being_worried.gh_24,
      questionnaire == "sommeil_23" ~ points_mh_negative_emotions.sommeil_23,
      questionnaire == "st_23" ~ points_problem_stop_being_worried.st_23,
      .default = NA
    ),
    
    PHQ2_points_low_interest = case_when(
      str_detect(questionnaire, "Monthly_") ~ points_mh_low_interest.monthly,
      questionnaire == "gh_22" ~ points_mh_low_interest.gh_22,
      questionnaire == "gh_23" ~ points_problem_little_interest_pleasure.gh_23,
      questionnaire == "gh_24" ~ g4h_problem_little_interest_pleasure.gh_24,
      questionnaire == "sommeil_23" ~ points_mh_interest.sommeil_23,
      questionnaire == "st_23" ~ points_problem_little_interest_pleasure.st_23,
      .default = NA
    ),
    
    PHQ2_points_depression = case_when(
      str_detect(questionnaire, "Monthly_") ~ points_mh_depression.monthly,
      questionnaire == "gh_22" ~ points_mh_depression.gh_22,
      questionnaire == "gh_23" ~ points_problem_sad_depressed_hopeless.gh_23,
      questionnaire == "gh_24" ~ g4h_problem_sad_depressed_hopeless.gh_24,
      questionnaire == "sommeil_23" ~ points_mh_depression.sommeil_23,
      questionnaire == "st_23" ~ points_problem_sad_depressed_hopeless.st_23,
      .default = NA
    ),
    
    # PHQ4 variable for total psychological distress
    PHQ4_score = as.integer(rowSums(across(c("GAD2_points_nervous", "GAD2_points_negative_emotions",
                                             "PHQ2_points_low_interest", "PHQ2_points_depression")))),
    
    PHQ4_cat = factor(case_when(
      PHQ4_score %in% c(0,1,2) ~ "None",
      PHQ4_score %in% c(3,4,5) ~ "Mild",
      PHQ4_score %in% c(6,7,8) ~ "Moderate", 
      PHQ4_score %in% c(9,10,11,12) ~ "Severe",
      .default = NA  
    ), levels = c("None", "Mild", "Moderate", "Severe")),
    
    # Harmonized WHO
    who_score = case_when(
      questionnaire == "gh_22" ~ who_score.gh_22,
      questionnaire == "gh_23" ~ who_score.gh_23,
      questionnaire == "gh_24" ~ who_score.gh_24,
      questionnaire == "mental_21" ~ who_score.mental_21,
      .default = NA
    ),
    who_interpretation = factor(case_when(
      who_score <= 28 ~  "<=28: Higher risk of depression",
      who_score <= 50 ~  "29-50: 'Screening diagnosis' of depression",
      who_score > 50 ~ "> 50: Lower risk of depression",
    ), levels = c("<=28: Higher risk of depression",
                  "29-50: 'Screening diagnosis' of depression",
                  "> 50: Lower risk of depression")),
    
    # Relevel the questionnaire according to the order of time
    questionnaire = factor(questionnaire, levels = c(
      "inclusion", 
      "Monthly_Nov_20", "Monthly_Dec_20", "Monthly_Jan_21", 
      "Monthly_Feb_21", "Monthly_Mar_21", "Monthly_Apr_21", 
      "gh_21", "Monthly_May_21", "Monthly_Jun_21", 
      "mental_21", "gh_22", "st_22", "gh_23", "sommeil_23", "st_23", "gh_24")),
    actively_working = case_when(actively_working ~ "Yes", actively_working == FALSE ~ "No", .default = NA),
    
    ## Relationship status recode into English
    relation.inc = case_match(relation.inc,
                              "En couple, non marié-e" ~ "In relationship (unmarried)",
                              "Marié-e ou en partenariat enregistré" ~ "Married / civil partnership",
                              "Divorcé-e ou séparé-e" ~ "Divorced / separated",
                              "Célibataire" ~ "Single",
                              "Veuf-ve" ~ "Widowed",
                              "Other" ~ "Other"
    ),
    
    ## Living situation --> not considering age of children
    hh_livewith_rec_en.inc = factor(case_match(hh_livewith.inc,
                                               "En couple, sans enfant" ~ "Couple without children",
                                               "En cohabitation, avec d'autres personnes (famille, amis, collocataires, etc.)" ~ "With other adults",
                                               "Seul-e" ~ "Alone",
                                               "En couple, avec vos enfants ou ceux de votre conjoint-e" ~ "Couple with children",
                                               "En parent seul, avec vos enfants" ~ "Single with children"),
                                    levels = c("Couple without children", "Couple with children", 
                                               "Single with children", "With other adults", "Alone")
    ),
    ## Living situation simplified
    hh_livewith_en.inc = factor(case_match(hh_livewith.inc,
                                               "En couple, sans enfant" ~ "With partner, without kids",
                                               "En cohabitation, avec d'autres personnes (famille, amis, collocataires, etc.)" ~ "Cohabitation",
                                               "Seul-e" ~ "Alone",
                                               "En couple, avec vos enfants ou ceux de votre conjoint-e" ~ "With partner and kids",
                                               "En parent seul, avec vos enfants" ~ "Single parent"),
                                    levels = c("With partner, without kids", "With partner and kids", 
                                               "Cohabitation", "Single parent", "Alone")
    ),
    
    # Single parent (differentiate young vs older children)
    single_parent.inc = factor(case_when(
      is.na(hh_livewith_rec_en.inc) ~ NA,
      hh_livewith_rec_en.inc == "Single with children" & num_young_children_dich.inc == "Has young children" ~ "Yes, with young children (<16)",
      hh_livewith_rec_en.inc == "Single with children" & num_young_children_dich.inc == "No young children" ~ "Yes, with older children (16+)",
      .default = "Not a single parent"),
      levels = c("Not a single parent", "Yes, with older children (16+)", "Yes, with young children (<16)")),
    
    living_situation_rec.inc = factor(case_when(
      is.na(hh_livewith_rec_en.inc) ~ NA,
      hh_livewith_rec_en.inc %in% c("Couple without children", "With other adults") ~ "With other adults",
      hh_livewith_rec_en.inc == "Couple with children" & num_young_children_dich.inc == "Has young children" ~ "Couple with young children (<16)",
      hh_livewith_rec_en.inc == "Couple with children" & num_young_children_dich.inc == "No young children" ~ "Couple with older children (16+)",
      hh_livewith_rec_en.inc == "Single with children" & num_young_children_dich.inc == "Has young children" ~ "Single with young children (<16)",
      hh_livewith_rec_en.inc == "Single with children" & num_young_children_dich.inc == "No young children" ~ "Single with older children (16+)",
      hh_livewith_rec_en.inc == "Alone" ~ "Alone",
      .default = "With other adults"),
      levels = c("Couple with older children (16+)", "Couple with young children (<16)", 
                 "Single with older children (16+)", "Single with young children (<16)", "With other adults", "Alone")),
    
    ## Living situation taking into account age of children
    hh_livewith_rec_en_children.inc = factor(case_when(
      hh_livewith_rec_en.inc == "Couple with children" & num_young_children_dich.inc == "Has young children" ~ "Couple with young children (<16)",
      hh_livewith_rec_en.inc == "Couple with children" & num_young_children_dich.inc == "No young children" ~ "Couple with older children (16+)",
      hh_livewith_rec_en.inc == "Single with children" & num_young_children_dich.inc == "Has young children" ~ "Single with young children (<16)",
      hh_livewith_rec_en.inc == "Single with children" & num_young_children_dich.inc == "No young children" ~ "Single with older children (16+)",
      .default = hh_livewith_rec_en.inc),
      levels = c("Couple without children", "Couple with young children (<16)", "Couple with older children (16+)", 
                 "Single with young children (<16)", "Single with older children (16+)", "With other adults", "Alone")),
    
    # Translate job_sector_harmonized to English
    # Shorten and relevel the job_sector labels
    job_sector_harmonized_en = str_to_sentence(str_remove(job_sector_harmonized, "Secteur de la|Secteur de l’|Secteur des")),
    job_sector_harmonized_en = str_replace(job_sector_harmonized_en, " \\s*\\([^\\)]+\\)", ""),
    job_sector_harmonized_en = str_wrap(job_sector_harmonized_en, width = 60),  # wrap sector so that it's better displayed
    job_sector_harmonized_en = relevel(factor(job_sector_harmonized_en), ref = "Administration publique"),
    
    # Translate job sector
    job_sector_harmonized_en = factor(case_match(job_sector_harmonized_en,
                                      "Administration publique" ~ "Public administration"
                                      ,"Activités juridiques, comptabilité, secrétariat" ~ "Legal, accounting, secretarial"
                                      ,"Agriculture, sylviculture, horticulture, entretien des\nespaces verts, élevage, pêche, etc." ~ "Agriculture, forestry, livestock, fishing"
                                      ,"Ambassade, organisation internationale" ~ "Embassy / international organisations"
                                      ,"Arts, spectacle, musée, bibliothèque" ~ "Arts, museums, libraries"
                                      ,"Banques, assurances" ~ "Banking, insurance"
                                      ,"Bureaux d’études, recherche et développement, architecture"  ~ "Design, R&D, architecture"
                                      ,"Commerce" ~ "Trade"
                                      ,"Construction ou de la rénovation" ~ "Construction, renovation"
                                      ,"Enseignement, recherche" ~ "Education, research"
                                      ,"Extraction de matières premières" ~ "Extraction of raw materials"
                                      ,"Hébergement et restauration" ~ "Accommodation and catering"
                                      ,"Immobilier, agences" ~ "Service agencies (e.g. real estate, travel)"
                                      ,"Industrie, de la fabrication de biens" ~ "Industry, goods manufacturing"
                                      ,"Information et communication" ~ "Information and communication"
                                      ,"Autre" ~ "Other"
                                      ,"Petite enfance" ~ "Early childhood (care, education)"
                                      ,"Production ou de la distribution" ~ "Production or distribution"
                                      ,"Santé, social, médico-social" ~ "Healthcare and social services"
                                      ,"Sécurité, secours" ~ "Security, rescue"
                                      ,"Services à la personne" ~ "Personal services"
                                      ,"Services de conseils" ~ "Consulting"
                                      ,"Services domestiques" ~ "Domestic services"
                                      ,"Transports et de l’entreposage" ~ "Transportation and storage"
                                      ,.default = NA
    )),
    job_sector_harmonized_en = fct_relevel(job_sector_harmonized_en, c("Public administration", "Healthcare and social services"), after = 0),
    
    job_sector_harmonized_fr_short = fct_recode(
      job_sector_harmonized_en,
      "Administration, économie, et activités juridiques" = "Public administration"
      ,"Administration, économie, et activités juridiques" = "Legal, accounting, secretarial"
      ,"Agriculture" = "Agriculture, forestry, livestock, fishing"
      ,"Administration, économie, et activités juridiques" = "Embassy / international organisations"
      ,"Arts, journalisme, services de communication" = "Arts, museums, libraries"
      ,"Administration, économie, et activités juridiques" = "Banking, insurance"
      ,"Services de l'industrie"  = "Design, R&D, architecture"
      ,"Commerce" = "Trade"
      ,"Services de l'industrie" = "Construction, renovation"
      ,"Enseignement" = "Education, research"
      ,"Services de l'industrie" = "Extraction of raw materials"
      ,"Commerce" = "Accommodation and catering"
      ,"Commerce" = "Service agencies (e.g. real estate, travel)"
      ,"Services de l'industrie" = "Industry, goods manufacturing"
      ,"Arts, journalisme, services de communication" = "Information and communication"
      ,"Autre" = "Other"
      ,"Enseignement" = "Early childhood (care, education)"
      ,"Production ou de la distribution" = "Production or distribution"
      ,"Santé" = "Healthcare and social services"
      ,"Sécurité, secours" = "Security, rescue"
      ,"Services à la personne et domestiques" = "Personal services"
      ,"Arts, journalisme, services de communication" = "Consulting"
      ,"Services à la personne et domestiques" = "Domestic services"
      ,"Transports" = "Transportation and storage"
    ),
    
    job_sector_harmonized_en_short = fct_recode(
      job_sector_harmonized_fr_short,
      "Administration, economics, and legal activities" = "Administration, économie, et activités juridiques",
      "Health" = "Santé",
      "Commerce" = "Commerce",                                         
      "Agriculture" = "Agriculture",
      "Arts, journalism, and communication services" = "Arts, journalisme, services de communication",
      "Industrial services" = "Services de l'industrie",
      "Personal and domestic services" = "Services à la personne et domestiques",
      "Education" = "Enseignement",
      "Other" = "Autre",
      "Production or distribution" = "Production ou de la distribution",
      "Security and emergency services" = "Sécurité, secours",
      "Transportation and storage" = "Transports"
    )
    
  ) |>
  # Count the number of questionnaires filled (among gh and st questionnaires)
  group_by(codbar) |> mutate(n_filled = sum(!is.na(date_soumission))) |> ungroup() |> 
  # Remove people that only filled the inclusion questionnaire and not any other questionnaires
  # filter(n_filled >1) |>
  # Count the number of profession timepoints
  group_by(codbar) |> mutate(n_profession = sum(!is.na(profession_source))) |> ungroup() |> 
  # Remove people for whom we have no profession timepoints
  # filter(n_profession > 0) |>
  # inner_join(filter_participants) |> # filter based on criteria outlined earlier
  group_by(codbar) |> arrange(codbar, questionnaire) |> tidyr::fill(ends_with(".inc"), .direction = "down") |> ungroup() |> 
  # Age at each timepoint
  mutate(age = time_length(date_soumission - birthdate.inc, "years"),
        
         # Age group of participants at Inclusion
         age_cat.inc = factor(case_when(     
           age.inc < 25 ~ "18-24",
           age.inc >= 25 & age.inc < 35 ~ "25-34",
           age.inc >= 35 & age.inc < 45 ~ "35-44",
           age.inc >= 45 & age.inc < 55 ~ "45-54",
           age.inc >= 55 & age.inc < 65 ~ "55-64",
           age.inc >= 65 & age.inc < 75 ~ "65-74",
           age.inc >= 75 ~ "75+")),
         
         # Age of participants at Nov 2020
         age_nov20 = time_length(dmy("01-11-2020") - birthdate.inc, "years"),
         # Age group of participants at Nov 2020
         age_cat_nov20 = factor(case_when(     
           age_nov20 < 25 ~ "18-24",
           age_nov20 >= 25 & age_nov20 < 35 ~ "25-34",
           age_nov20 >= 35 & age_nov20 < 45 ~ "35-44",
           age_nov20 >= 45 & age_nov20 < 55 ~ "45-54",
           age_nov20 >= 55 & age_nov20 < 65 ~ "55-64",
           age_nov20 >= 65 & age_nov20 < 75 ~ "65-74",
           age_nov20 >= 75 ~ "75+")),
         age_cat_nov20 = fct_relevel(age_cat_nov20, "25-34"),
         
         # Age of participants at 15 Nov 2023
         age_nov23 = time_length(dmy("15-11-2023") - birthdate.inc, "years"),
         # Age group of participants at Nov 2020
         age_cat_nov23 = factor(case_when(     
           age_nov23 < 25 ~ "18-24",
           age_nov23 >= 25 & age_nov23 < 35 ~ "25-34",
           age_nov23 >= 35 & age_nov23 < 45 ~ "35-44",
           age_nov23 >= 45 & age_nov23 < 55 ~ "45-54",
           age_nov23 >= 55 & age_nov23 < 65 ~ "55-64",
           age_nov23 >= 65 & age_nov23 < 75 ~ "65-74",
           age_nov23 >= 75 ~ "75+")),
         age_cat_nov23 = fct_relevel(age_cat_nov23, "25-34")
  ) |> 
  # Centering of variables
  mutate(
    # Age nov 20
    c_age_nov20 = age_nov20 - mean(age_nov20, na.rm = TRUE),
    # Age nov 23
    c_age_nov23 = age_nov23 - mean(age_nov23, na.rm = TRUE)
  ) |> 
  relocate(c(c_age_nov20, n_filled, 
             health, health_numeric, morale, morale_numeric, burn_out, burnout_score, 
             GAD2_score, GAD2_anxiety, PHQ2_score, PHQ2_depression, PHQ2_depression_binary:PHQ4_cat), .after = date_soumission) |> 
  relocate(c(n_profession), .after = profession_source) |> 
  arrange(codbar, questionnaire)

# Get single profession/sector information for participants at latest timepoint ####
single_profession <- dat_professions |> #dat_master_clean |> 
  filter(!is.na(ISCO_label_full),
         ISCO_label_full != "Recoding impossible") |> 
  mutate(
    # Translate job_sector_harmonized to English
    # Shorten and relevel the job_sector labels
    job_sector_harmonized_en = str_to_sentence(str_remove(job_sector_harmonized, "Secteur de la|Secteur de l’|Secteur des")),
    job_sector_harmonized_en = str_replace(job_sector_harmonized_en, " \\s*\\([^\\)]+\\)", ""),
    job_sector_harmonized_en = str_wrap(job_sector_harmonized_en, width = 60),  # wrap sector so that it's better displayed
    job_sector_harmonized_en = relevel(factor(job_sector_harmonized_en), ref = "Administration publique"),
    
    # Translate job sector
    job_sector_harmonized_en = factor(case_match(job_sector_harmonized_en,
                                                 "Administration publique" ~ "Public administration"
                                                 ,"Activités juridiques, comptabilité, secrétariat" ~ "Legal, accounting, secretarial"
                                                 ,"Agriculture, sylviculture, horticulture, entretien des\nespaces verts, élevage, pêche, etc." ~ "Agriculture, forestry, livestock, fishing"
                                                 ,"Ambassade, organisation internationale" ~ "Embassy / international organisations"
                                                 ,"Arts, spectacle, musée, bibliothèque" ~ "Arts, museums, libraries"
                                                 ,"Banques, assurances" ~ "Banking, insurance"
                                                 ,"Bureaux d’études, recherche et développement, architecture"  ~ "Design, R&D, architecture"
                                                 ,"Commerce" ~ "Trade"
                                                 ,"Construction ou de la rénovation" ~ "Construction, renovation"
                                                 ,"Enseignement, recherche" ~ "Education, research"
                                                 ,"Extraction de matières premières" ~ "Extraction of raw materials"
                                                 ,"Hébergement et restauration" ~ "Accommodation and catering"
                                                 ,"Immobilier, agences" ~ "Service agencies (e.g. real estate, travel)"
                                                 ,"Industrie, de la fabrication de biens" ~ "Industry, goods manufacturing"
                                                 ,"Information et communication" ~ "Information and communication"
                                                 ,"Autre" ~ "Other"
                                                 ,"Petite enfance" ~ "Early childhood (care, education)"
                                                 ,"Production ou de la distribution" ~ "Production or distribution"
                                                 ,"Santé, social, médico-social" ~ "Healthcare and social services"
                                                 ,"Sécurité, secours" ~ "Security, rescue"
                                                 ,"Services à la personne" ~ "Personal services"
                                                 ,"Services de conseils" ~ "Consulting"
                                                 ,"Services domestiques" ~ "Domestic services"
                                                 ,"Transports et de l’entreposage" ~ "Transportation and storage"
                                                 ,.default = NA
    )),
    job_sector_harmonized_en = fct_relevel(job_sector_harmonized_en, c("Public administration", "Healthcare and social services"), after = 0),
    
    job_sector_harmonized_fr_short = fct_recode(
      job_sector_harmonized_en,
      "Administration, économie, et activités juridiques" = "Public administration"
      ,"Administration, économie, et activités juridiques" = "Legal, accounting, secretarial"
      ,"Agriculture" = "Agriculture, forestry, livestock, fishing"
      ,"Administration, économie, et activités juridiques" = "Embassy / international organisations"
      ,"Arts, journalisme, services de communication" = "Arts, museums, libraries"
      ,"Administration, économie, et activités juridiques" = "Banking, insurance"
      ,"Services de l'industrie"  = "Design, R&D, architecture"
      ,"Commerce" = "Trade"
      ,"Services de l'industrie" = "Construction, renovation"
      ,"Enseignement" = "Education, research"
      ,"Services de l'industrie" = "Extraction of raw materials"
      ,"Commerce" = "Accommodation and catering"
      ,"Commerce" = "Service agencies (e.g. real estate, travel)"
      ,"Services de l'industrie" = "Industry, goods manufacturing"
      ,"Arts, journalisme, services de communication" = "Information and communication"
      ,"Autre" = "Other"
      ,"Enseignement" = "Early childhood (care, education)"
      ,"Production ou de la distribution" = "Production or distribution"
      ,"Santé" = "Healthcare and social services"
      ,"Sécurité, secours" = "Security, rescue"
      ,"Services à la personne et domestiques" = "Personal services"
      ,"Arts, journalisme, services de communication" = "Consulting"
      ,"Services à la personne et domestiques" = "Domestic services"
      ,"Transports" = "Transportation and storage"),
    job_sector_harmonized_fr_short = fct_relevel(job_sector_harmonized_fr_short, "Autre", after = Inf),
    
    job_sector_harmonized_en_short = fct_recode(
      job_sector_harmonized_fr_short,
      "Administration, economics, and legal activities" = "Administration, économie, et activités juridiques",
      "Health" = "Santé",
      "Commerce" = "Commerce",                                         
      "Agriculture" = "Agriculture",
      "Arts, journalism, and communication services" = "Arts, journalisme, services de communication",
      "Industrial services" = "Services de l'industrie",
      "Personal and domestic services" = "Services à la personne et domestiques",
      "Education" = "Enseignement",
      "Other" = "Autre",
      "Production or distribution" = "Production ou de la distribution",
      "Security and emergency services" = "Sécurité, secours",
      "Transportation and storage" = "Transports"
    )
  ) |> 
  group_by(codbar) |> 
  # Slice by max to get the latest occupation timepoint
  slice_max(order_by = date_soumission, n = 1) |> 
  ungroup() |> 
  select(codbar, source, occupation_type, HCW_subgroup, key_subgroup, job_sector, job_sector_harmonized, job_sector_harmonized_en, job_sector_harmonized_fr_short, job_sector_harmonized_en_short, years_of_service, starts_with(c("ISCO_", "isco_"))) |> 
  rename_with(~ paste(., "single", sep = "."), !matches(c("codbar")))

dat_master_final <- left_join(dat_master_clean, single_profession) |> 
  # filter(
  #   education_rec_en.inc != "Other"
  #   , work_situation_rec_en.inc != "Retired"
  #   # , sex_en.inc != "Other"
  # ) |> 
  mutate(
    # Survey recruitment source
    pop_source = NA,
    pop_source = case_when(
      serocov_work.inc == TRUE & work_pilote.inc == FALSE & serocov_pop.inc == FALSE & pop_pilote.inc == FALSE &
        sp3_juin_2021.inc == FALSE & sp2_novdec_2020.inc == FALSE & sp4_avril_2022.inc == FALSE ~ "SEROCoV-WORK",
      is.na(pop_source) & serocov_pop.inc == TRUE ~ "Population-based",
      is.na(pop_source) & sp2_novdec_2020.inc == TRUE ~ "Population-based",
      is.na(pop_source) & sp3_juin_2021.inc == TRUE ~ "Population-based",
      is.na(pop_source) & sp4_avril_2022.inc == TRUE ~ "Population-based",
      is.na(pop_source) & pop_pilote.inc == TRUE ~ "Population-based",
      is.na(pop_source) & work_pilote.inc == TRUE ~ "work_pilote",
      .default = "Other"
    )
  ) |> 
  group_by(codbar) |> fill(participant_id, .direction = "downup") |> ungroup() |> 
  relocate(pop_source, .after = participant_id) |> 
  mutate(
    occupation_type = fct_recode(occupation_type,
      "General workforce" = "Other occupation", "Healthcare worker" = "Health worker", "Other key worker" = "Key occupation"),
    occupation_type.single = fct_recode(occupation_type.single,
      "General workforce" = "Other occupation", "Healthcare worker" = "Health worker", "Other key worker" = "Key occupation")
  )

dat_master_final |> count(occupation_type.single)
dat_master_final |> filter(!is.na(date_soumission)) |> count(questionnaire)

saveRDS(dat_master_final, file = here("data", "long_cleaned.rds"))

# print(paste0("We have ", length(unique(dat_master_clean$codbar)), " participants where we have at least one profession timepoint and where they have filled out at least one of Santé Général (2022 or 2023) or Santé-Travail (2022 or 2023)"))
# 
# a <- dat_master_clean |> group_by(codbar) |> mutate(n = sum(!is.na(date_soumission))) |> 
#   relocate(n, .after = date_soumission)
#   filter(!is.na(master_profession_original)) |> 
#   group_by(codbar) |> filter(n()>1) |> 
#   relocate(c(ISCO_label_2, ISCO_label_full, management, complementary_info), .after = master_profession_original) |> 
#   relocate(health, .after = date_soumission)
# 
# b <- dat_master_clean |> 
#   filter(is.na(job_sector_harmonized),
#          !is.na(master_profession_original)
#          )