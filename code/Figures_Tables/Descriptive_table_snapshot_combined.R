
#Table combined with p-value symbols

pacman::p_load(
  tidyverse,
  gtsummary,
  forestplot,
  here,
  forcats,
  # data.table,
  # devEMF, # print figures into a vectorized format
  flextable,
  gt
  )

# Section that needs your input -------------------------------------------

# update dat to use your dataset
source(here("code", "prep_st23.R"))
dat_original <- dat
# Update outcome levels for easier readability
dat <- dat_original %>% mutate(
  # burn_out = factor(case_when(
  #   burn_out == 1 ~ "Yes",
  #   burn_out == 0 ~ "No",
  #   .default = NA), levels = c("No", "Yes")),
  # burnout_interp_dich30 = factor(case_when(
  #   burnout_interp_dich30 == "Severe" ~ "Yes",
  #   burnout_interp_dich30 == "Not Severe" ~ "No",
  #   .default = NA), levels = c("No", "Yes"))
  # sex_en.inc = case_match(sex_en.inc, "Female" ~ "Yes", "Male" ~ "No"),
  PFI_score_dich = factor(case_when(PFI_score.st_23 >= 3 ~ "High (>=3)",
                                    PFI_score.st_23 < 3 ~ "Low (<3)"),
                          levels = c("Low (<3)", "High (>=3)")),
  num_young_children_dich.inc = case_match(num_young_children_dich.inc,
                                           "No young children" ~ "No",
                                           "Has young children" ~ "Yes"),
  health_dich_notgood = case_match(health_dich,
                                   "Good" ~ "No", "Not good" ~ "Yes"),
  health_general_good.inc = case_match(health_general.inc,
                                       c("Très bonne", "Bonne") ~ "Yes",
                                       c("Moyenne", "Mauvaise", "Très mauvaise") ~ "No"),
  health_dich_good = case_match(health_dich,
                                c("Good") ~ "Yes",
                                c("Not good") ~ "No"),
  burnout.st_23 = case_match(burnout.st_23,
                             "Oui" ~ "Yes", "Non" ~ "No"),
  years_of_service.st_23 = case_match(
    years_of_service.st_23,
    "Moins de 6 mois" ~ "< 6 months",
    "De 6 mois à moins d’1 an" ~ "6-12 months",
    "De 1 an à moins de 5 ans" ~ "1-5 years",
    "De 5 ans à moins de 10 ans" ~ "5-10 years",
    "10 ans ou plus" ~ "10+ years")
) #|> filter(work_situation_rec_en.inc != "Retired")

clean_labels <- enframe(c(
  'Occupation type' = 'occupation_type.single'                                  
  , '*Age group' = 'age_cat'                                               
  , '*Sex: female' = 'sex_en.inc'                                                   
  , '*Education' = 'education_rec_en.inc'
  , 'Occupational status' = 'work_situation_rec_en.inc'
  # , 'Profession' = 'occupation_cat_en.inc'
  , 'Household income' = 'hh_income_cat_en.inc'
  # , '*Has young children' = 'num_young_children_dich.inc'
  # , 'Single parent' = 'single_parent.inc'
  # , 'Living situation' = 'hh_livewith_rec_en_children.inc'
  , 'Living situation' = 'hh_livewith_en.inc'
  # , 'Financial security' = 'finance_situation_en.inc'
  , 'Migration status' = 'swiss_national.inc'
  # , 'Migration status' = 'migration_status.inc'
  , 'Mental health condition' = 'Pre_existing_MH_condition.inc'  
  , 'Physical condition' = 'Pre_existing_physical_condition.inc'
  # , ' Recruitment source' = "pop_source"
  
  # , 'Work situation (2023)' = 'work_situation_rec'                        
  # , 'Years of service (2023)' = 'years_of_service.st_23'                   
  # , 'Demands (Karasek)' = 'demands_karasek.st_23'                          
  # , 'Autonomy (Karasek)' = 'autonomy_karasek.st_23'                       
  # , 'Social support (Karasek)' = 'social_support_karasek.st_23'            
  # , 'Work type (Karasek)' = 'work_type_karasek.st_23'                     
  # , 'Isostrain: yes (Karasek)' = 'isostrain_karasek.st_23'                      
  # , 'Efforts (Siegrist)' = 'efforts_siegrist.st_23'                            
  # , 'Rewards (Siegrist)' = 'rewards_siegrist.st_23'                             
  # , 'Efforts-Rewards ratio' = 'ER_ratio.st_23'                            
  # , 'Overcommitment (Siegrist)' = 'overcommitment_siegrist.st_23'               
  # , 'Rewards: esteem' = 'rewards_esteem_siegrist.st_23'          
  # , 'Rewards: job security' = 'rewards_security_siegrist.st_23'   
  # , 'Rewards: job promotion' = 'rewards_promotion_siegrist.st_23'
  # , 'Worked while sick?' = 'work_sick.st_23' 
  # , "Good health (inclusion)" = "health_general_good.inc"
  # , "(Very) good health (Nov '23)" = "health_dich_good"
  , 'PHQ2 score (depression)' = 'PHQ2_score'
  # , 'PHQ2: probable depression' = 'PHQ2_depression'                       
  , 'GAD2 score (anxiety)' = 'GAD2_score'
  # , 'GAD2: anxiety disorder ' = 'GAD2_anxiety'       
  # , 'Professional fulfillment score' = 'PFI_score.st_23'
  # , 'EE-MBI score' = 'burnout_score.st_23'                          
  # , 'Emotional exhaustion (2023)' = 'burnout_interp.st_23'                
  # , 'EE-MBI score (2022)' = 'burnout_score.st_22'                          
  # , 'Emotional exhaustion (2022)' = 'burnout_interp.st_22'                
  # , 'Diagnosed burnout: yes' = 'burnout.st_23'                           
  # , 'Diagnosed burnout (2022)' = 'burn_out.st_22'   
  , 'Professional fulfillment (PFI)' = 'PFI_score_dich'   
)) |> 
  rename(clean_name = name, var_label = value) |> 
  mutate(in_regression = case_when(str_detect(clean_name, "[*]")~ TRUE,
                                   .default = FALSE),
         clean_name = str_remove(clean_name,"[*]")
  );covariates <- clean_labels$var_label

## By Occupational grouping ####
table_occupation <- dat %>% 
  filter(!is.na(PFI_score.st_23)) |> 
  # mutate(occupation_type = forcats::fct_na_value_to_level(occupation_type, level = "Missing")) |>
  select(all_of(covariates), -PFI_score_dich) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>% 
  droplevels() %>%
  tbl_summary(
    by = occupation_type.single,
    percent = "col",                  # calculate percent column-wise
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    type = c(
      # rewards_esteem_siegrist.st_23, rewards_security_siegrist.st_23, 
      PHQ2_score, GAD2_score
      ) ~ "continuous",
    digits = list(all_continuous() ~ 2
                  , all_categorical() ~ c(0,1)
    ),
    missing = "no",
    missing_text = "Missing"    # how missing values should display
  ) %>% 
  # add_n() %>%
  add_p(
    test = all_categorical() ~ "chisq.test",
    pvalue_fun = function(x) style_pvalue(x, digits = 3),
    test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
  ) %>%
  # add_q() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Occupational grouping**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}")) %>% 
  # add_overall() %>%
  # modify_header(stat_0 ~ "**Overall**") |>
  bold_labels();table_occupation

## Add clean names
clean_table <- table_occupation$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name) 

table_occupation$table_body <- clean_table ; rm(clean_table)
# table_occupation <- table_occupation %>%
#   modify_column_hide(columns = c(n))

table_occupation %>% as_flex_table()

## By PFI ####
table_PFI <- dat %>% 
  filter(!is.na(PFI_score.st_23)) |> 
  # mutate(occupation_type = forcats::fct_na_value_to_level(occupation_type, level = "Missing")) |>
  select(all_of(covariates)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>% 
  droplevels() %>%
  tbl_summary(
    by = PFI_score_dich,
    percent = "col",                  # calculate percent column-wise
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    type = c(
      # rewards_esteem_siegrist.st_23, rewards_security_siegrist.st_23, 
      PHQ2_score, GAD2_score
      ) ~ "continuous",
    digits = list(all_continuous() ~ 2
                  , all_categorical() ~ c(0,1)
    ),
    missing = "no",
    missing_text = "Missing"    # how missing values should display
  ) %>% 
  # add_n() %>%
  add_p(
    test = all_categorical() ~ "chisq.test",
    pvalue_fun = function(x) style_pvalue(x, digits = 3),
    test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
  ) %>%
  # add_q() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Professional fulfillment (PFI)**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}")) %>% 
  # add_overall() %>%
  # modify_header(stat_0 ~ "**Overall**") |> 
  bold_labels();table_PFI

## Add clean names
clean_table <- table_PFI$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name) 

table_PFI$table_body <- clean_table ; rm(clean_table)
# table_PFI <- table_PFI %>% modify_column_hide(columns = c(n))

table_PFI %>% as_flex_table()

## Combined ####
table_combined <- tbl_merge(tbls = list(table_occupation, table_PFI),
                            tab_spanner = c(
                              "Occupational grouping",
                              "Professional fulfillment (PFI)")
)
table_combined |> as_flex_table()


a <- table_combined$table_body %>%           # Crude way to ID the group labels and for later coloring
  rowid_to_column() %>% 
  filter(row_type == "label")
groupnames <- (a$rowid)

rename_table_cols <- table_combined$table_body #|> 
# mutate(label = case_when(
#   row_type == "label" & is.na(n_1) ~ paste0(label, " (N=", max(n_1, na.rm = TRUE),")"),
#   row_type == "label"~ paste0(label, " (N=", n_1,")"),
#   .default = label)
# )

table_combined$table_body <- rename_table_cols

table_final <- table_combined %>% 
  # modify_column_hide(columns = c(n_1, n_2)) |> 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  add_footer_lines("n (%) ; mean (SD). % may not add up to 100% due to rounding.") %>%
  footnote(i = 2, j = 5, 
           value = as_paragraph(" Pearson's Chi-squared test; Kruskal-Wallis rank sum test"),
           ref_symbols = c("a"),
           part = "header") %>% 
  footnote(i = 2, j = 8, 
           value = as_paragraph(" Pearson's Chi-squared test; Wilcoxon rank sum test"),
           ref_symbols = c("b"),
           part = "header") %>% 
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>%
  # autofit() %>%
  # set_table_properties(width = .1, layout = "autofit") %>%
  vline(j = c(1,5), border = fp_border_default(color = "grey")) %>%
  # hline(i = c(42, 46,53, 58), border = fp_border_default(color = "black")) %>% # lines to indicate variable themes
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>% 
  # # Fontsize 7
  height_all(height = 0, part = "body", unit = "cm") %>%
  fontsize(size = 7, part = "all") %>% 
  width(j = 1, width = 4.5, unit = "cm") %>% 
  # width(j = 2, width = 1.1, unit = "cm") %>% 
  width(j = c(2:4, 6:7), width = 2.1, unit = "cm") %>% 
  # width(j = c(3:4), width = 1.9, unit = "cm") %>% 
  width(j = c(5, 8), width = 1.2, unit = "cm") %>%
  line_spacing(space = 0.1, part = "body") ; table_final
  # padding(padding.right = 0) |>
  # valign(valign = "bottom", part = "body") ; table_final

# Save as docx ####
save_as_docx(
  "Table 2: Participant characteristics" = table_final, path = here("output", "publication figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Characteristics_snapshot_combined.docx")))
