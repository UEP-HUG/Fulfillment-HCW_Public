# First run "Longitudinal_analyses_final.Rmd", then can run this to create the clean table for the growth model estimates

clean_names_me <- c(
  levels(dat_longitudinal$occupation_type.single), levels(dat_longitudinal$sex_en.inc), 
  levels(dat_longitudinal$education_rec_en.inc), 
  "c_age_feb_21",
  levels(dat_longitudinal$Pre_existing_MH_condition.inc), levels(dat_longitudinal$Pre_existing_physical_condition.inc), 
  "Swiss", "Non-Swiss"
  # levels(dat_longitudinal$swiss_national.inc)
);clean_names_me
clean_names_mec <- c(clean_names_me, "n-") |> str_c(collapse = "|")

clean_variable <- enframe(c(
  "(Intercept)" = "(Intercept)",
  "Time 1" = "Time1_Q", 
  "Time 2" = "Time2_Q",  
  "Occupational grouping" = "occupation_type.single"     ,    
  "Sex" = "sex_en.inc"                            ,   
  "Age in years" = "c_age_feb21"                                   ,  
  "Education" = "education_rec_en.inc"                    ,
  "Mental health condition" = "Pre_existing_MH_condition.inc"                ,
  "Comorbidity" = "Pre_existing_physical_condition.inc"         ,
  "Nationality" = "swiss_national.inc"                     ,
  "Time 1: Sex" = "Time1_Q:sex_en.inc"                        ,
  "Time 2: Sex" = "Time2_Q:sex_en.inc"                        ,
  "Time 1: Age" = "Time1_Q:c_age_feb21"                            ,
  "Time 2: Age" = "Time2_Q:c_age_feb21"                             ,
  "Time 1: Education" = "Time1_Q:education_rec_en.inc"           ,
  "Time 1: Education" = "Time2_Q:education_rec_en.inc"           ,
  "Time 1: Occupational grouping" = "Time1_Q:occupation_type.single",
  "Time 2: Occupational grouping" = "Time2_Q:occupation_type.single" 
)) |> 
  rename(variable = value)

clean_full <- tibble(term = clean_names_me,
                     variable = c("occupation_type.single", "occupation_type.single", "occupation_type.single",
                                  "sex_en.inc", "sex_en.inc", "sex_en.inc",
                                  "education_rec_en.inc","education_rec_en.inc", "education_rec_en.inc",
                                  "c_age_feb21",
                                  "Pre_existing_MH_condition.inc", "Pre_existing_MH_condition.inc",
                                  "Pre_existing_physical_condition.inc", "Pre_existing_physical_condition.inc",
                                  "swiss_national.inc", "swiss_national.inc"))

PHQ_full_table_2 <- clean_full |> 
  left_join(PHQ_full_table |> mutate(
    variable = str_remove_all(term,clean_names_mec),
    term = case_when(term == variable ~ term,
                     .default = str_remove(term, variable)))) |>
  left_join(clean_variable) |> 
  relocate(name)

PHQ_full_table_4 <- left_join(clean_full, PHQ_full_table_2) |> left_join(clean_variable)


pacman::p_load(gt, flextable)

alt_names <- tibble(
  # variable = c(
  #   rep("Associations at baseline", 3),
  #   rep("Interaction effects for Time 1", 3),
  #   rep("Interaction effects for Time 2", 3)
  # ),
  variable = c(
    "Effects at baseline", "", "",
    "Interaction effects for Time 1", "", "",
    "Interaction effects for Time 2", "", ""
  ),
  level = rep(c(NA, "     Healthcare worker", "     Other key worker"), 3)
)

PHQ_full_alt <- bind_cols(alt_names, PHQ_full_table |> filter(str_detect(term, "occupation_type")) |> 
  add_row(term = NA,.before = 1) |> 
  add_row(term = NA,.before = 4) |> 
  add_row(term = NA,.before = 7)
) |> mutate(
  confint = case_when(is.na(estimate) ~ NA, 
                      .default = paste0(format(round(estimate,2), digits = 2), " [", format(round(conf.low,2), digits = 2), ", ", format(round(conf.high,2), digits = 2), "]")
  )) |> 
  select(variable, level, confint, p.value) |> 
  mutate(level = replace_na(level, ""),
         confint = replace_na(confint, ""),
         p.value = replace_na(p.value, "")) |> 
  mutate(variable = case_when(variable == "" ~ level, .default = variable)) |> select(-level)

GAD_full_alt <- bind_cols(alt_names, GAD_full_table |> filter(str_detect(term, "occupation_type")) |> 
                            add_row(term = NA,.before = 1) |> 
                            add_row(term = NA,.before = 4) |> 
                            add_row(term = NA,.before = 7)
) |> mutate(
  confint = case_when(is.na(estimate) ~ NA, 
                      .default = paste0(format(round(estimate,2), digits = 2), " [", format(round(conf.low,2), digits = 2), ", ", format(round(conf.high,2), digits = 2), "]")
  )) |> 
  select(variable, level, confint, p.value) |> 
  mutate(level = replace_na(level, ""),
         confint = replace_na(confint, ""),
         p.value = replace_na(p.value, "")) |> 
  mutate(variable = case_when(variable == "" ~ level, .default = variable)) |> select(-level)

combined_alt <- bind_cols(PHQ_full_alt, GAD_full_alt |> select(-variable))

combined_alt |> flextable() |> 
  add_header_row(values = c("", "Depressive symptoms (PHQ2)", "Depressive symptoms (PHQ2)", "Anxiety symptoms (GAD2)", "Anxiety symptoms (GAD2)"), top = TRUE) |>
  merge_h(part = "header") |>
  set_header_labels(values = c("", "Coefficient [95% CI]", "P", "Coefficient [95% CI]", "P")) |> 
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>%
  vline(j = c(3), border = fp_border_default(color = "grey")) %>%
  add_footer_lines("Reference group: General workforce") %>%
  bg(i = c(1,4,7), part = "body", bg = "#EFEFEF") |> 
  padding(i=c(2,3,5,6,8,9), j=1, padding.left=20) |> 
  padding(i=c(3,6,9), j=2, padding.left=5) |>
  width(j = 1, width = 5.8, unit = "cm") %>% 
  width(j = c(2,4), width = 5, unit = "cm") |> 
  width(j = c(3,5), width = 2, unit = "cm")


save_as_docx(
  "Table X: Participant summary_specchio_analytical sample" = combined_alt_table, path = here("output", "publication figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Piecewise_summary_table.docx"))
  # , pr_section = sect_properties
)
