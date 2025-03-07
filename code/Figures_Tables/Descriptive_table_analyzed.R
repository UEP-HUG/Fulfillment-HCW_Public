pacman::p_load(
  here,
  gt,
  gtsummary,
  flextable
)
source(here("code","flowchart.R"))
dat_enter <- readRDS(here("data", "long_cleaned.rds")) #|> group_by(codbar) |> slice_head(n = 1) |> ungroup()

dat <- dat_enter |> inner_join(full_eligible_merged |> select(codbar, analyzed, analyzed_nov23)) |> 
  group_by(codbar) |> slice_head(n = 1) |> ungroup()

# define standard myflextable function 
myflextable = function(data, ...) {
  set_flextable_defaults(na_str = "NA", theme_fun = theme_booktabs, font.size = 12, padding.bottom = 1, padding.top = 1)
  x = flextable(data, ...)
  x = colformat_int(x, big.mark = "")
  x = colformat_double(x, big.mark = "", 
                       digits = 2,
                       na_str = "NA")
  return(x)
}

# Update some variables in dataset for easier table presentation
table_data <- dat |> 
  mutate(
    num_young_children_dich.inc = case_match(num_young_children_dich.inc	,
                                             "No young children" ~ "No", "Has young children" ~ "Yes"),
    health_general_good.inc = case_match(health_general.inc,
                                         c("Très bonne", "Bonne") ~ "Yes",
                                         c("Moyenne", "Mauvaise", "Très mauvaise") ~ "No"),
    education_rec_en.inc = case_when(education_rec_en.inc == "Other" ~ NA, .default = education_rec_en.inc)
    )

# Choose variables to include in the table
clean_labels <- enframe(c(
  'Longitudinal analyses' = 'analyzed'
  , 'Cross-sectional analyses' = 'analyzed_nov23'
  # 'Questionnaire' = 'questionnaire'
  # , 'Occupation type' = 'occupation_type.single' 
  , 'Age in years' = "age.inc"
  , '*Age group' = 'age_cat.inc'
  , '*Sex' = 'sex_en.inc'                                                   
  , '*Education' = 'education_rec_en.inc'
  , 'Occupational status' = 'work_situation_rec_en.inc'
  # , 'Profession' = 'occupation_cat_en.inc'
  , 'Household income' = 'hh_income_cat_en.inc'
  # , '*Has young children' = 'num_young_children_dich.inc'
  # , 'Living situation' = 'living_situation_rec.inc'
  , 'Living situation' = 'hh_livewith_en.inc'
  , 'Smoking' = 'smoking_rec_en.inc'
  , 'Alcohol' = 'alcohol_en.inc'
  # , 'Household income' = 'hh_income_cat_en.inc'                           
  # , 'Financial security' = 'finance_situation_en.inc'
  , 'Migration status' = 'swiss_national.inc'
  # , 'Migration status' = 'migration_status.inc'
  , 'Pre-existing mental health condition' = 'Pre_existing_MH_condition.inc'  
  , 'Pre-existing physical condition' = 'Pre_existing_physical_condition.inc'
  # , 'Self-rated health' = 'health_general_en.inc'
  , 'Self-rated health: good' = 'health_general_good.inc'
  # , 'Occupational grouping' = 'occupation_type.single' 
)) |> 
  rename(clean_name = name, var_label = value) |> 
  mutate(in_regression = case_when(str_detect(clean_name, "[*]")~ TRUE,
                                   .default = FALSE),
         clean_name = str_remove(clean_name,"[*]")
  );covariates <- clean_labels$var_label

## Longitudinal ####
table_longitudinal_raw <- table_data %>% 
  # mutate(occupation_type = forcats::fct_na_value_to_level(occupation_type, level = "Missing")) |>
  select(all_of(covariates), -analyzed_nov23) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>% 
  droplevels() %>%
  tbl_summary(
    by = analyzed,
    percent = "column",                  # calculate percent column-wise
    # type = c(PHQ2_score, GAD2_score) ~ "continuous",
    # type = c(num_young_children_dich.inc) ~ "dichotomous",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1
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
  # modify_spanning_header(all_stat_cols() ~ "**Questionnaire date**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("{level}","\nN = {n}")) %>% 
  # add_overall() %>%
  modify_header(label = "Characteristic\n(at inclusion)") |> 
  bold_labels();table_longitudinal_raw

## Add clean names
clean_table <- table_longitudinal_raw$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name)

table_longitudinal_raw$table_body <- clean_table ; rm(clean_table)
# table_longitudinal_raw <- table_longitudinal_raw %>%
#   modify_column_hide(columns = c(n))

## Cross-sectional ####
table_november_raw <- table_data %>% 
  # mutate(occupation_type = forcats::fct_na_value_to_level(occupation_type, level = "Missing")) |>
  select(all_of(covariates), -analyzed) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>% 
  droplevels() %>%
  tbl_summary(
    by = analyzed_nov23,
    percent = "column",                  # calculate percent column-wise
    # type = c(PHQ2_score, GAD2_score) ~ "continuous",
    # type = c(num_young_children_dich.inc) ~ "dichotomous",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1
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
  # modify_spanning_header(all_stat_cols() ~ "**Questionnaire date**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("{level}","\nN = {n}")) %>% 
  # add_overall() %>%
  modify_header(label = "Characteristic\n(at inclusion)") |> 
  bold_labels();table_november_raw

## Add clean names
clean_table <- table_november_raw$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name)

table_november_raw$table_body <- clean_table ; rm(clean_table)
# table_november_raw <- table_november_raw %>%
#   modify_column_hide(columns = c(n))

full_table <- tbl_merge(tbls = list(table_longitudinal_raw, table_november_raw))

a <- full_table$table_body %>%           # Crude way to ID the group labels and for later coloring
  rowid_to_column() %>% 
  filter(row_type == "label")
groupnames <- (a$rowid)

table_full <- full_table %>% 
  modify_footnote(update = everything() ~ NA) %>%
  # modify_header(stat_1 = '**Longitudinal analyses**', stat_2 = '**Cross-sectional analyses**') |> 
  as_flex_table() %>%
  add_header_row(values = c("", "Longitudinal analyses", "Longitudinal analyses", "Longitudinal analyses", "Cross-sectional analyses", "Cross-sectional analyses", "Cross-sectional analyses"), top = TRUE) |>
  merge_h(part = "header") |>
  add_footer_lines("n (%) ; mean (SD). % may not add up to 100% due to rounding.") %>%
  footnote(i = 3, j = c(4,7),
           value = as_paragraph(" Wilcoxon rank sum test; Pearson's Chi-squared test"),
           ref_symbols = c("a"),
           part = "header") %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>%
  # autofit() %>%
  # set_table_properties(width = .1, layout = "autofit") %>%
  vline(j = c(1,4), border = fp_border_default(color = "grey")) %>%
  # hline(i = c(28,39,49,57,60,67,72), border = fp_border_default(color = "black")) %>% # lines to indicate variable themes
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>% 
  # # Fontsize 7
  height_all(height = 0, part = "body", unit = "cm") %>%
  fontsize(size = 8, part = "all") %>% 
  width(j = 1, width = 5.3, unit = "cm") %>% 
  width(j = c(2,5), width = 2.2, unit = "cm") %>% 
  width(j = c(3,6), width = 2.2, unit = "cm") %>%
  width(j = c(4,7), width = 1.3, unit = "cm") %>%
  line_spacing(space = 0.1, part = "body") %>%
  valign(valign = "bottom", part = "body");table_full

# Save as docx ####
# library(officer)
# sect_properties <- prop_section(
#   page_size = page_size(
#     orient = "landscape"
#     # width = 8.3, height = 11.7
#   ),
#   type = "continuous",
#   page_margins = page_mar()
# )

save_as_docx(
  "Table X: Participant summary_specchio_analytical sample" = table_full, path = here("output", "publication figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Characteristics_specchio_analyzed.docx"))
  # , pr_section = sect_properties
)
