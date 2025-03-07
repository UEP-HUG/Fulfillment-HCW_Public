# Descriptive tables for the publication
# Load packages and dataset ####
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  gt,
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  # devEMF,       # Save vectorized figures as EMF
  lubridate,     # manipulate date objects
  forcats
)
# conflicted::conflicts_prefer(dplyr::filter)
# conflicted::conflicts_prefer(dplyr::select)

source(here("code", "prep_st23.R"))
dat <- dat |> 
  mutate(num_young_children_dich.inc = case_match(num_young_children_dich.inc,
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
         )

b <- dat |> group_by(work_situation) |> count(years_of_service) |> filter(!str_detect(work_situation, "employed"))
table(dat$work_situation_rec, dat$years_of_service)
# dat <- dat |> filter(occupation_type == "Health worker")

clean_labels <- enframe(c(
  'Occupation type' = 'occupation_type.single'                                  
  , '*Age group' = 'age_cat'                                               
  , '*Sex' = 'sex_en.inc'                                                   
  , '*Education' = 'education_rec_en.inc'                                  
  , '*Has young children' = 'num_young_children_dich.inc'                   
  , 'Household income' = 'hh_income_cat_en.inc'                           
  # , 'Financial security' = 'finance_situation_en.inc'
  , 'Migration status' = 'migration_status.inc'
  , 'Mental health condition' = 'Pre_existing_MH_condition.inc'  
  , 'Physical condition' = 'Pre_existing_physical_condition.inc'
  # , ' Recruitment source' = "pop_source"
  
  , 'Work situation (2023)' = 'work_situation_rec'                        
  # , 'Years of service (2023)' = 'years_of_service.st_23'                   
  , 'Demands (Karasek)' = 'demands_karasek.st_23'                          
  , 'Autonomy (Karasek)' = 'autonomy_karasek.st_23'                       
  , 'Social support (Karasek)' = 'social_support_karasek.st_23'            
  # , 'Work type (Karasek)' = 'work_type_karasek.st_23'                     
  , 'Isostrain (Karasek)' = 'isostrain_karasek.st_23'                      
  , 'Efforts (ERI)' = 'efforts_siegrist.st_23'                            
  , 'Rewards (ERI)' = 'rewards_siegrist.st_23'                             
  , 'Efforts-Rewards ratio' = 'ER_ratio.st_23'                            
  , 'Overcommitment (ERI)' = 'overcommitment_siegrist.st_23'               
  , 'Rewards subscale: esteem' = 'rewards_esteem_siegrist.st_23'          
  , 'Rewards subscale: job security' = 'rewards_security_siegrist.st_23'   
  , 'Rewards subscale: job promotion' = 'rewards_promotion_siegrist.st_23'
  # , 'Worked while sick?' = 'work_sick.st_23' 
  , "Good health (inclusion)" = "health_general_good.inc"
  , "Good health (Nov '23)" = "health_dich_good"
  , 'PHQ2 score' = 'PHQ2_score'                                            
  # , 'PHQ2: probable depression' = 'PHQ2_depression'                       
  , 'GAD2 score' = 'GAD2_score'                                            
  # , 'GAD2: anxiety disorder ' = 'GAD2_anxiety'       
  , 'Professional fulfillment (PFI)' = 'PFI_score.st_23'                  
  , 'EE-MBI score' = 'burnout_score.st_23'                          
  # , 'Emotional exhaustion (2023)' = 'burnout_interp.st_23'                
  # , 'EE-MBI score (2022)' = 'burnout_score.st_22'                          
  # , 'Emotional exhaustion (2022)' = 'burnout_interp.st_22'                
  , 'Diagnosed burnout' = 'burnout.st_23'                           
  # , 'Diagnosed burnout (2022)' = 'burn_out.st_22'       
)) |> 
  rename(clean_name = name, var_label = value) |> 
  mutate(in_regression = case_when(str_detect(clean_name, "[*]")~ TRUE,
                                   .default = FALSE),
         clean_name = str_remove(clean_name,"[*]")
         );covariates <- clean_labels$var_label


### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Section that does NOT necessarily need your input ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###


## Overall values ####
table_overall <- dat %>% select(all_of(covariates)) %>% select(-occupation_type.single) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>%
  droplevels() %>% 
  tbl_summary(
    percent = "col",                         # calculate percent column-wise
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    type = c(rewards_esteem_siegrist.st_23, rewards_security_siegrist.st_23, PHQ2_score, GAD2_score) ~ "continuous",
    digits = list(all_continuous() ~ 2
                  , all_categorical() ~ c(0,1)
                  ),
    missing = "no",
    missing_text = "Missing"    # how missing values should display
  ) %>%
  add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Overall**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  modify_spanning_header(everything() ~ NA);table_overall
## Add clean names
clean_table <- table_overall$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name) 

table_overall$table_body <- clean_table ; rm(clean_table)
table_overall <-  table_overall %>% modify_column_hide(columns = c(stat_0)) # remove overall column
table_overall  %>% as_flex_table()

## By occupation type ####
table_occupation_type <- dat %>% 
  # mutate(occupation_type = forcats::fct_na_value_to_level(occupation_type, level = "Missing")) |>
  select(all_of(covariates)) %>% 
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
    type = c(rewards_esteem_siegrist.st_23, rewards_security_siegrist.st_23, PHQ2_score, GAD2_score) ~ "continuous",
    digits = list(all_continuous() ~ 2
                  , all_categorical() ~ c(0,1)
    ),
    missing = "no",
    missing_text = "Missing"    # how missing values should display
  ) %>% 
  add_n() %>%
  add_p(
    test = all_categorical() ~ "chisq.test",
    pvalue_fun = function(x) style_pvalue(x, digits = 3),
    test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
  ) %>%
  # add_q() %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Occupation type**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}")) %>% 
  add_overall() %>%
  modify_header(stat_0 ~ "**Overall**");table_occupation_type
  
## Add clean names
clean_table <- table_occupation_type$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name) 

table_occupation_type$table_body <- clean_table ; rm(clean_table)
table_occupation_type <- table_occupation_type %>% 
  modify_column_hide(columns = c(n))

table_occupation_type %>% as_flex_table()

## Combine for main table - TW group only ####
# !! Try to get it so that only the levels have a different color, and the labels are white !!
table_combined <-
  tbl_merge(
    tbls = list(table_overall, 
                table_occupation_type),
    tab_spanner = c(NA, 
                    "**Occupation type**")
  ) ; table_combined %>% as_flex_table()
a <- table_combined$table_body %>%           # Crude way to ID the group labels and for later coloring
  rowid_to_column() %>% 
  filter(row_type == "label")
groupnames <- (a$rowid)

table_combined2 <- table_combined %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  add_footer_lines("n (%) ; mean (SD). % may not add up to 100% due to rounding.") %>%
  footnote(i = 2, j = 7, 
           value = as_paragraph(" Kruskal-Wallis rank sum test; Pearson's Chi-squared test"),
           ref_symbols = c("a"),
           part = "header") %>% 
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>%
  # autofit() %>%
  # set_table_properties(width = .1, layout = "autofit") %>%
  vline(j = c(3), border = fp_border_default(color = "grey")) %>%
  hline(i = c(25,30,34,41), border = fp_border_default(color = "black")) %>% # lines to indicate variable themes
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>% 
  # # Fontsize 7
  height_all(height = 0, part = "body", unit = "cm") %>%
  fontsize(size = 7, part = "all") %>% 
  width(j = 1, width = 4.4, unit = "cm") %>% 
  width(j = 2, width = 1.1, unit = "cm") %>% 
  width(j = c(3:6), width = 2.2, unit = "cm") %>% 
  width(j = c(7), width = 1.2, unit = "cm") %>%
  line_spacing(space = 0.1, part = "body") %>%
  valign(valign = "bottom", part = "body") ; table_combined2

# Save as docx ####
save_as_docx(
  "Table 1: Participant summary" = table_combined2, path = here("output", "publication figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Exposure table_main_Health workers_subgroup.docx")))
