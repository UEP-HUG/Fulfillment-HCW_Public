# Scale functions

# PHQ2 and GAD2
assign_PHQ4_points = function(.z) {
  PHQ4p = case_when(.z == "Pas du tout" ~ 0L,
                    .z == "Plusieurs jours" ~ 1L,
                    .z == "Plus de la moitié du temps" ~ 2L,
                    .z == "Presque chaque jour" ~ 3L
  )
  return(PHQ4p)
}

# PFI
assign_PFI_points = function(.z) {
  PFIp = case_when(.z == "Pas du tout vrai" ~ 0L,
                    .z == "Très peu vrai" ~ 1L,
                    .z == "Plutôt vrai" ~ 2L,
                    .z == "Très vrai" ~ 3L,
                    .z == "Tout à fait vrai" ~ 4L
  )
  return(PFIp)
}



#' @title Burnout scale used in Santé travail questionnaire
#' @details ne que les items nécessaires à la mesure du score d'épuisement professionnel
#' soit les questions 1, 2, 3, 6, 8, 13, 14, 16 et 20 de l'échelle de Maslach
#' @scoring Pour chaque question, les scores possibles s'étalaient de 0 (jamais) à 6 (quasiment tous les jours).
#' En additionnant les scores sur chaque item (0 à 6), le score maximum possible était donc de 54.
#' Les seuils d'interprétation des scores sont généralement les suivants:
#' •	Score de 0 à 17: épuisement faible;
#' •	Score de 18 à 29: épuisement modéré;
#' •	Score de 30+: épuisement fort
#'
#' @param .z Numeric vector of participant choices
#' 
#' @usage dat %>% mutate(across(starts_with("burn_out_scale_"), cq_assign_burnout_points, .names = "points_{.col}"))
#' @return An integer vector of points from 1 to 6
#' @export
#'
cq_assign_burnout_points = function(.z) {
  burnoutp = case_when(.z == "Jamais" ~ 0L,
                       .z == "Quelques fois par an, au moins" ~ 1L,
                       .z == "Une fois par mois au moins" ~ 2L,
                       .z == "Quelques fois par mois" ~ 3L,
                       .z == "Une fois par semaine" ~ 4L,
                       .z == "Quelques fois par semaine" ~ 5L,
                       .z == "Chaque jour" ~ 6L
  )
  return(burnoutp)
}

#' @title Points for Karasek scale used in Santé travail questionnaire
#' @details Only soutien social professionnel part is used in the Q because otherwise it will be too long.
#' But here the points scoring is done for all Karasek questions as applicable to all.
#' @scoring Echelle de Likert en 4 points
#'
#' @param .z Character vector of participant choices
#' 
#' @usage dat %>% mutate(across(matches("karasek_scale_"), cq_assign_Karasek_points, .names = "points_{.col}"))
#' @return An integer vector of points from 1 to 4
#' @export
#'
cq_assign_Karasek_points = function(.z) {
  Karasekp = case_when(.z == "Pas du tout d’accord" ~ 1L,
                       .z == "Pas d’accord" ~ 2L,
                       .z == "D’accord" ~ 3L,
                       .z == "Tout à fait d’accord" ~ 4L
  )
  return(Karasekp)
}

# Create function to assign points to the WHO scale responses
# WHO-5 well being
# https://www.karger.com/Article/Fulltext/376585
assign_who_points = function(.z) {
  whop = case_when(.z == "Tout le temps" ~ 5L,
                   .z == "La plupart du temps" ~ 4L,
                   .z == "Plus de la moitié du temps" ~ 3L,
                   .z == "Moins de la moitié du temps" ~ 2L,
                   .z == "De temps en temps" ~ 1L,
                   .z == "Jamais" ~ 0L
  )
  return(whop)
}