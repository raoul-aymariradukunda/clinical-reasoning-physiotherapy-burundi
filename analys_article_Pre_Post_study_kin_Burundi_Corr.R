library(tidyverse)   # manipulation des données et graphiques
library(readxl)      # lecture des fichiers Excel
library(janitor)     # nettoyage des noms de variables
library(gtsummary)   # tableaux descriptifs (Tableaux I et II)
library(rstatix)     # taille d'effet du test de Wilcoxon
library(flextable)   # mise en forme des tableaux
library(officer)     # export Word
library(coin)        # Z du test de Wilcoxon signé-rang (taille d'effet r)

# --- Mise en forme (anglais, point décimal) -----------------------------------
fmt   <- function(x, d = 1) formatC(x, format = "f", digits = d)
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))

theme_gtsummary_language("en")


# 1. Importation et préparation des données ------------------------------------

pre  <- read_excel("Pre_test_Le_raisonnement_clinique_en_Kinesitherapie_Burundi_reponses.xlsx") %>%
  clean_names()

post <- read_excel("Post_test_Le_raisonnement_clinique_en_Kinesitherapie_Musculosquelettique_Burundi_reponses.xlsx") %>%
  clean_names() %>%
  filter(str_detect(consentement_a_participer, "confirme"))  # participants consentants

cat("Pre-test:", nrow(pre), "| Post-test (consenting):", nrow(post), "\n")

# Réponses Likert ("4 = D'accord" -> 4)
likert_num <- function(x) as.integer(str_extract(as.character(x), "^[1-5]"))

items_ptse_pre   <- str_subset(names(pre),  "ptse")
items_sacrr_pre  <- str_subset(names(pre),  "sacrr")
items_ptse_post  <- str_subset(names(post), "ptse")
items_sacrr_post <- str_subset(names(post), "sacrr")

stopifnot(length(items_ptse_pre)  == 5,  length(items_ptse_post)  == 5,
          length(items_sacrr_pre) == 26, length(items_sacrr_post) == 26)

pre  <- pre  %>% mutate(across(c(all_of(items_ptse_pre),  all_of(items_sacrr_pre)),  likert_num))
post <- post %>% mutate(across(c(all_of(items_ptse_post), all_of(items_sacrr_post)), likert_num))

# Scores totaux : PTSE (5-25) et SACRR (26-130)
pre <- pre %>%
  mutate(PTSE_total  = rowSums(across(all_of(items_ptse_pre))),
         SACRR_total = rowSums(across(all_of(items_sacrr_pre))))

post <- post %>%
  mutate(PTSE_total  = rowSums(across(all_of(items_ptse_post))),
         SACRR_total = rowSums(across(all_of(items_sacrr_post))))


# 2. Nettoyage de l'âge --------------------------------------------------------
annee_ref <- 2025

calcul_age <- function(x) {
  x <- as.character(x) %>%
    str_trim() %>%
    str_remove("^'") %>%
    str_remove_all("(?i)\\s*ans?\\b") %>%
    str_replace_all("[-.\\s]", "/")
  
  annee_naiss <- if_else(str_detect(x, "^(\\d{1,2}/)?\\d{1,2}/\\d{4}$|^\\d{4}$"),
                         as.numeric(str_extract(x, "\\d{4}$")), NA_real_)
  age_direct  <- suppressWarnings(as.numeric(x))
  
  case_when(
    !is.na(annee_naiss)                      ~ annee_ref - annee_naiss,
    !is.na(age_direct) & age_direct <= 120   ~ age_direct,
    TRUE                                     ~ NA_real_
  )
}

pre <- pre %>% mutate(age_ans = calcul_age(age_ans))
summary(pre$age_ans)   # contrôle : aucune valeur manquante attendue


# 3. Recodage des variables socio-démographiques -------------------------------

pre <- pre %>%
  mutate(
    region = fct_recode(region_ou_ville_de_pratique,
                        "Bujumbura"   = "Bujumbura mairie",
                        "Bujumbura"   = "CIBITOKE",
                        "Bujumbura"   = "Gihanga/bubanza",
                        "Bujumbura"   = "Tous les domaines de la kiné",
                        "Gitega"      = "CNAR GITEGA",
                        "Gitega"      = "Karusi",
                        "Butanyerera" = "Kirundo",
                        "Butanyerera" = "Ngozi",
                        "Butanyerera" = "NGOZI",
                        "Burunga"     = "Makamba",
                        "Burunga"     = "Matana"
    ) %>% fct_relevel("Bujumbura", "Gitega", "Butanyerera", "Burunga"),
    
    # Libellés en anglais pour les tableaux
    diplome = factor(diplome_universitaire_le_plus_eleve_obtenu) %>%
      fct_recode("Bachelor's degree (Licence)" = "Baccalauréat/Licence"),
    sexe = factor(sexe, levels = c("Masculin", "Féminin")) %>%
      fct_recode("Male" = "Masculin", "Female" = "Féminin"),
    
    experience_detail = factor(annees_dexperience_professionnelle,
                               levels = c("1 an", "2 ans", "3 ans", "4 ans", "6 ans", "7 ans", "9 ans")) %>%
      fct_recode("1 year" = "1 an", "2 years" = "2 ans", "3 years" = "3 ans",
                 "4 years" = "4 ans", "6 years" = "6 ans", "7 years" = "7 ans",
                 "9 years" = "9 ans"),
    
    experience = fct_collapse(experience_detail,
                              "1-3 years" = c("1 year", "2 years", "3 years"),
                              "4-6 years" = c("4 years", "6 years"),
                              ">6 years"  = c("7 years", "9 years")),
    
    emploi = factor(cadre_demploi_actuel,
                    levels = c("Autre", "Clinique ou centre privé", "Hopital ou clinique publique")) %>%
      fct_recode("Other" = "Autre",
                 "Private clinic or centre" = "Clinique ou centre privé",
                 "Public hospital or clinic" = "Hopital ou clinique publique"),
    
    formation_rc = factor(
      formation_anterieure_sur_le_raisonnement_clinique_ou_sujets_connexes,
      levels = c("Ateliers/Séminaires", "Aucune", "Cours formels")) %>%
      fct_recode("Workshops/seminars" = "Ateliers/Séminaires",
                 "None" = "Aucune",
                 "Formal courses" = "Cours formels"),
    
    supervision = factor(
      avez_vous_deja_supervise_un_e_etudiant_e_en_kinesitherapie_en_stage,
      levels = c("Oui", "Non")) %>%
      fct_recode("Yes" = "Oui", "No" = "Non")
  )

# Vérification des regroupements (aucun NA attendu)
count(pre, region_ou_ville_de_pratique, region)
sapply(pre %>% select(region, sexe, experience, emploi, formation_rc, supervision),
       function(v) sum(is.na(v)))


# 4. Appariement pré / post ----------------------------------------------------

lignes_pre  <- c(1, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 20)
lignes_post <- c(14, 5, 6, 16, 3, 13, 15, 1, 8, 7, 10, 4, 9, 2, 11, 12)

stopifnot(length(lignes_pre) == 16, length(lignes_post) == 16,
          !anyDuplicated(lignes_pre), !anyDuplicated(lignes_post))

pre <- pre %>%
  mutate(groupe = if_else(row_number() %in% lignes_pre,
                          "Retained", "Lost to follow-up") %>%
           factor(levels = c("Retained", "Lost to follow-up")))

# Une ligne par participant apparié
paires <- tibble(
  id         = seq_along(lignes_pre),
  PTSE_pre   = pre$PTSE_total[lignes_pre],
  PTSE_post  = post$PTSE_total[lignes_post],
  SACRR_pre  = pre$SACRR_total[lignes_pre],
  SACRR_post = post$SACRR_total[lignes_post]
) %>%
  mutate(PTSE_diff  = PTSE_post  - PTSE_pre,
         SACRR_diff = SACRR_post - SACRR_pre)


# 5. Tests de normalité --------------------------------------------------------
normalite <- tibble(
  Variable = c("PTSE pre", "PTSE post", "SACRR pre", "SACRR post",
               "PTSE change score", "SACRR change score"),
  x = list(paires$PTSE_pre, paires$PTSE_post, paires$SACRR_pre, paires$SACRR_post,
           paires$PTSE_diff, paires$SACRR_diff)
) %>%
  mutate(sw = map(x, shapiro.test),
         W  = map_dbl(sw, "statistic"),
         p  = map_dbl(sw, "p.value")) %>%
  select(Variable, W, p)

normalite

# Contrôle graphique (Q-Q plots) des scores de changement
png("QQplots_change_scores.png", width = 1800, height = 900, res = 220)
par(mfrow = c(1, 2))
qqnorm(paires$PTSE_diff,  main = "PTSE change score");  qqline(paires$PTSE_diff)
qqnorm(paires$SACRR_diff, main = "SACRR change score"); qqline(paires$SACRR_diff)
dev.off()
par(mfrow = c(1, 1))


# 6. Tableau I : caractéristiques des participants (pré-test, n = 20) ----------

tab1 <- pre %>%
  select(age_ans, sexe, diplome, experience,
         emploi, region, formation_rc, supervision) %>%
  tbl_summary(
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p})"),
    digits    = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    label = list(
      age_ans ~ "Age (years)",
      sexe    ~ "Sex",
      diplome      ~ "Highest degree obtained",
      experience   ~ "Years of professional experience",
      emploi       ~ "Current employment setting",
      region       ~ "Region of practice",
      formation_rc ~ "Previous training in clinical reasoning",
      supervision  ~ "Supervision of physiotherapy students"
    ),
    missing = "no"
  ) %>%
  modify_header(label ~ "**Variable**", all_stat_cols() ~ "**n (%) or mean (SD)**")

tab1


# 7. Tableau II : retenus (n = 16) vs perdus de vue (n = 4) --------------------
# Wilcoxon-Mann-Whitney (variables continues), Fisher (catégorielles).

tab2 <- pre %>%
  select(groupe, PTSE_total, SACRR_total, sexe, experience_detail, emploi, age_ans) %>%
  tbl_summary(
    by = groupe,
    statistic = list(all_continuous()  ~ "{mean} ± {sd} ({median})",
                     all_categorical() ~ "{n} ({p})"),
    digits    = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    label = list(
      PTSE_total        ~ "Baseline PTSE score (5-25)",
      SACRR_total       ~ "Baseline SACRR score (26-130)",
      sexe              ~ "Sex",
      experience_detail ~ "Years of professional experience",
      emploi            ~ "Current employment setting",
      age_ans           ~ "Age (years)"
    ),
    missing = "no"
  ) %>%
  add_p(test = list(all_continuous()  ~ "wilcox.test",
                    all_categorical() ~ "fisher.test"),
        pvalue_fun = ~ style_pvalue(.x, digits = 1)) %>%
  modify_header(label ~ "**Characteristic**",
                stat_1 ~ "**Retained (n = 16)**",
                stat_2 ~ "**Lost to follow-up (n = 4)**",
                p.value ~ "**p**")

tab2


# 8. Fonctions communes (taille d'effet, changement, test) ---------------------

# r de Rosenthal = Z / sqrt(N), N = nombre de paires, avec IC 95 % bootstrap (signe conserve).
# Z vient de coin::wilcoxsign_test (même méthode que rstatix). Version robuste :
# si un échantillon bootstrap ne contient aucune différence non nulle, r = 0
# (évite l'erreur "all pairwise differences equal zero").
# NB : vérifier que r correspond bien à N = 16 paires et indiquer ce N dans Methods.
z_r <- function(x_pre, x_post) {
  if (all(x_post == x_pre)) return(0)
  z <- tryCatch(
    coin::statistic(coin::wilcoxsign_test(x_post ~ x_pre, distribution = "asymptotic")),
    error = function(e) NA_real_
  )
  as.numeric(z) / sqrt(length(x_pre))     # signe conserve : positif = hausse apres la formation
}

effet_r <- function(pre_x, post_x, B = 1000) {
  set.seed(2025)
  r_hat <- z_r(pre_x, post_x)
  idx   <- seq_along(pre_x)
  b <- replicate(B, { i <- sample(idx, replace = TRUE); z_r(pre_x[i], post_x[i]) })
  tibble(r      = r_hat,
         r_low  = unname(quantile(b, .025, na.rm = TRUE)),
         r_high = unname(quantile(b, .975, na.rm = TRUE)))
}

# Changement moyen (post - pré) avec IC 95 % bootstrap percentile
change_ci <- function(pre_x, post_x, B = 2000) {
  set.seed(2025)
  d <- post_x - pre_x
  b <- replicate(B, mean(sample(d, length(d), replace = TRUE)))
  tibble(chg = mean(d),
         chg_low  = unname(quantile(b, .025)),
         chg_high = unname(quantile(b, .975)))
}

# Une paire de vecteurs -> test + changement + taille d'effet
analyse_paire <- function(x_pre, x_post) {
  test <- suppressWarnings(wilcox.test(x_post, x_pre, paired = TRUE, exact = FALSE))
  bind_cols(
    tibble(n         = length(x_pre),
           n_nonzero = sum(x_post != x_pre),   # paires réellement utilisées par Wilcoxon
           W         = unname(test$statistic), # = "V" dans R
           p         = test$p.value),
    change_ci(x_pre, x_post),
    effet_r(x_pre, x_post)
  )
}

med_iqr <- function(x) paste0(median(x), " (", quantile(x, .25), "-", quantile(x, .75), ")")


# 9. Critères de jugement principaux : scores totaux ---------------------------

res_principal <- bind_rows(
  bind_cols(tibble(Score = "PTSE total (5-25)",
                   Pre   = med_iqr(paires$PTSE_pre),
                   Post  = med_iqr(paires$PTSE_post)),
            analyse_paire(paires$PTSE_pre, paires$PTSE_post)),
  bind_cols(tibble(Score = "SACRR total (26-130)",
                   Pre   = med_iqr(paires$SACRR_pre),
                   Post  = med_iqr(paires$SACRR_post)),
            analyse_paire(paires$SACRR_pre, paires$SACRR_post))
)

res_principal   # médianes (IQR), W, p, n non nul, changement (IC), r (IC)

# 9b. Précisions descriptives et analyse de sensibilité ------------------------

# Direction des changements individuels (scores totaux)
direction <- tibble(
  Score      = c("PTSE total", "SACRR total"),
  Increased  = c(sum(paires$PTSE_diff > 0),  sum(paires$SACRR_diff > 0)),
  Unchanged  = c(sum(paires$PTSE_diff == 0), sum(paires$SACRR_diff == 0)),
  Decreased  = c(sum(paires$PTSE_diff < 0),  sum(paires$SACRR_diff < 0))
)
direction

# Analyse de sensibilité : test t apparié (le SACRR a des changements normaux,
# Shapiro-Wilk p > 0.05 ; le PTSE est fourni à titre de comparaison)
t_ptse  <- t.test(paires$PTSE_post,  paires$PTSE_pre,  paired = TRUE)
t_sacrr <- t.test(paires$SACRR_post, paires$SACRR_pre, paired = TRUE)

sensibilite <- tibble(
  Score    = c("PTSE total", "SACRR total"),
  t        = c(unname(t_ptse$statistic), unname(t_sacrr$statistic)),
  df       = c(unname(t_ptse$parameter), unname(t_sacrr$parameter)),
  p        = c(t_ptse$p.value, t_sacrr$p.value),
  mean_chg = c(unname(t_ptse$estimate), unname(t_sacrr$estimate)),
  ci_low   = c(t_ptse$conf.int[1],  t_sacrr$conf.int[1]),
  ci_high  = c(t_ptse$conf.int[2],  t_sacrr$conf.int[2]),
  # d de Cohen pour échantillons appariés (dz)
  dz       = c(mean(paires$PTSE_diff)  / sd(paires$PTSE_diff),
               mean(paires$SACRR_diff) / sd(paires$SACRR_diff))
)
sensibilite

# Effet plafond : proportion de participants au score maximal (post-test)
plafond <- tibble(
  Score = c("PTSE total (max 25)", "SACRR total (max 130)"),
  n_max_pre  = c(sum(paires$PTSE_pre  == 25), sum(paires$SACRR_pre  == 130)),
  n_max_post = c(sum(paires$PTSE_post == 25), sum(paires$SACRR_post == 130)),
  n_ge90pct_post = c(sum(paires$PTSE_post >= 0.9 * 25), sum(paires$SACRR_post >= 0.9 * 130))
)
plafond


# 10. Figure en ANNEXE : scores individuels pré/post (sans p ni r) -------------

figure_paires <- function(pre_x, post_x, ylab) {
  d <- tibble(id = rep(seq_along(pre_x), 2),
              time = factor(rep(c("Pre-test", "Post-test"), each = length(pre_x)),
                            levels = c("Pre-test", "Post-test")),
              score = c(pre_x, post_x))
  ggplot(d, aes(time, score, group = id)) +
    geom_line(alpha = 0.5, colour = "gray40") +
    geom_point(size = 2, colour = "gray20") +
    stat_summary(aes(group = 1), fun = median, geom = "point",
                 shape = 18, size = 5, colour = "#C0392B") +
    labs(x = NULL, y = ylab) +
    theme_classic(base_size = 12)
}

fig_ptse  <- figure_paires(paires$PTSE_pre,  paires$PTSE_post,  "PTSE total score (5-25)")
fig_sacrr <- figure_paires(paires$SACRR_pre, paires$SACRR_post, "SACRR total score (26-130)")

figure <- cowplot::plot_grid(fig_ptse, fig_sacrr, ncol = 2, labels = c("A", "B"))
figure
ggsave("Supplementary_Figure_paired_scores.png", figure, width = 9, height = 5, dpi = 300)


# 11. Analyses exploratoires par item (Tableaux III et IV) ---------------------
# Wilcoxon apparié par item + taille d'effet + correction BH,
# appliquée séparément à chaque questionnaire.

analyse_items <- function(cols_pre, cols_post) {
  map2_dfr(cols_pre, cols_post, function(c_pre, c_post) {
    x_pre  <- pre[[c_pre]][lignes_pre]
    x_post <- post[[c_post]][lignes_post]
    bind_cols(
      tibble(Item = c_pre,
             Pre  = paste0(fmt(mean(x_pre), 2),  " (", fmt(sd(x_pre), 2),  ")"),
             Post = paste0(fmt(mean(x_post), 2), " (", fmt(sd(x_post), 2), ")")),
      analyse_paire(x_pre, x_post)
    )
  }) %>%
    mutate(q = p.adjust(p, method = "BH"), .after = p) %>%
    mutate(`N°` = row_number(), .before = 1)
}

items_ptse  <- analyse_items(items_ptse_pre,  items_ptse_post)
items_sacrr <- analyse_items(items_sacrr_pre, items_sacrr_post)

# Contrôles : p < 0.05 vs q < 0.05
c(PTSE_p = sum(items_ptse$p < 0.05),   PTSE_q = sum(items_ptse$q < 0.05))    # attendu 3 / 3
c(SACRR_p = sum(items_sacrr$p < 0.05), SACRR_q = sum(items_sacrr$q < 0.05))  # attendu 22 / 22 (item 18 limite)

# Libellés exacts des items (anglais), à la place des noms de variables
labels_ptse <- c(
  "I am confident in my ability to know when to perform specific tests in physical therapy practice.",
  "I will know when it is appropriate to refer a patient or a problem to another healthcare professional.",
  "In a general physical therapy context, I am confident in my ability to not miss a primary medical condition.",
  "I believe I can manage general problems encountered in physical therapy.",
  "In a general physical therapy context, when facing a difficult case, I am confident that I can make the right decisions.")

labels_sacrr <- c(
  "I identify the limits of my knowledge in a clinical situation.",
  "I recognize when I need to seek advice or supervision.",
  "I consider the patient's values and preferences in my clinical decisions.",
  "I take time to reflect on my clinical practice after seeing a patient.",
  "I use my past experiences to guide my clinical decisions.",
  "I am able to justify my clinical decisions with sound reasoning.",
  "I make clinical decisions even in uncertain situations.",
  "I know when to adjust my reasoning based on new information.",
  "I identify my biases or preconceived ideas that may affect my clinical decisions.",
  "I am open to critical feedback on my clinical decisions.",
  "I reflect on what I could have done differently after a patient encounter.",
  "I actively seek scientific evidence to guide my clinical practice.",
  "I change my treatment approach when it does not produce the expected outcomes.",
  "I am able to integrate different sources of information to understand a clinical situation.",
  "I use reflection to learn from my mistakes.",
  "I consider the patient's social and emotional factors in my clinical reasoning.",
  "I use tools or models to structure my clinical reasoning.",
  "I distinguish between objective facts and clinical assumptions.",
  "I consider the patient's context (living environment, resources, etc.) in my decisions.",
  "I regularly review my decisions in light of the patient's progress.",
  "I am able to explain my decisions to other healthcare professionals.",
  "I strive to recognize and correct my clinical errors.",
  "I ask critical questions about my own practice.",
  "I feel comfortable discussing clinical cases with my colleagues.",
  "I incorporate patient feedback into my clinical reasoning.",
  "I actively engage in my continuing professional development.")

stopifnot(length(labels_ptse) == 5, length(labels_sacrr) == 26)
items_ptse$Item  <- labels_ptse
items_sacrr$Item <- labels_sacrr

# Contrôles du signe de r : tous les items augmentent, donc r doit être >= 0
summary(items_ptse$r); summary(items_sacrr$r)
# Si toutes les valeurs sont négatives, le signe est inversé :
# dans z_r(), remplacer as.numeric(z) par -as.numeric(z).
# Les IC des items non significatifs (PTSE 5 ; SACRR 7, 10, 11, 24) doivent inclure 0 ou s'en approcher.
items_sacrr %>% filter(`N°` %in% c(7, 10, 11, 24)) %>% select(`N°`, p, q, r, r_low, r_high)
items_ptse  %>% filter(`N°` == 5) %>% select(`N°`, p, q, r, r_low, r_high)

tableau_items <- function(df) {
  gras <- which(df$q < 0.05)
  df %>%
    transmute(`N°`, Item, n,
              `Pre-test\nMean (SD)`   = Pre,
              `Post-test\nMean (SD)`  = Post,
              `Mean change\n(95% CI)` = paste0(fmt(chg, 2), " (", fmt(chg_low, 2), "; ", fmt(chg_high, 2), ")"),
              W   = fmt(W, 1),
              p   = fmt_p(p),
              q   = fmt_p(q),
              `r (95% CI)` = paste0(fmt(r, 2), " (", fmt(r_low, 2), "; ", fmt(r_high, 2), ")")) %>%
    flextable() %>%
    bold(i = gras, j = "q") %>%
    bold(part = "header") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    autofit()
}

mise_en_forme <- function(ft) {
  ft %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    autofit()
}


# 12. Export Word --------------------------------------------------------------

tab_principal <- res_principal %>%
  transmute(Score,
            n,
            `Non-zero pairs` = n_nonzero,
            `Pre-test\nMedian (IQR)`  = Pre,
            `Post-test\nMedian (IQR)` = Post,
            `Mean change\n(95% CI)` = paste0(fmt(chg, 2), " (", fmt(chg_low, 2), "; ", fmt(chg_high, 2), ")"),
            W = fmt(W, 1),
            p = fmt_p(p),
            `r (95% CI)` = paste0(fmt(r, 2), " (", fmt(r_low, 2), "; ", fmt(r_high, 2), ")")) %>%
  flextable()

tab_normalite <- normalite %>%
  mutate(W = fmt(W, 3), p = fmt_p(p)) %>%
  flextable()

doc <- read_docx() %>%
  body_add_par("Table I. Sociodemographic characteristics of participants (baseline, n = 20)") %>%
  body_add_flextable(mise_en_forme(as_flex_table(tab1))) %>%
  body_add_break() %>%
  
  body_add_par("Table II. Baseline characteristics of retained participants (n = 16) and those lost to follow-up (n = 4)") %>%
  body_add_flextable(mise_en_forme(as_flex_table(tab2))) %>%
  body_add_break() %>%
  
  body_add_par("Table S1. Shapiro-Wilk tests") %>%
  body_add_flextable(mise_en_forme(tab_normalite)) %>%
  body_add_break() %>%
  
  body_add_par("Table S2. Sensitivity analysis: paired t-test on total scores") %>%
  body_add_flextable(mise_en_forme(flextable(sensibilite %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))) %>%
  body_add_break() %>%
  
  body_add_par("Table S3. Primary outcomes: total PTSE and SACRR scores before and after the training") %>%
  body_add_flextable(mise_en_forme(tab_principal)) %>%
  body_add_par("Paired Wilcoxon signed-rank test. W = test statistic; r = Rosenthal's r (Z/sqrt(N), N = 16 pairs) with bootstrap 95% CI; mean change = post minus pre with bootstrap 95% CI.") %>%
  body_add_break() %>%
  
  body_add_par("Table III. Changes in PTSE item scores following the training (exploratory analysis)") %>%
  body_add_flextable(tableau_items(items_ptse)) %>%
  body_add_par("Exploratory analysis. Paired Wilcoxon signed-rank test. p = unadjusted p-value; q = Benjamini-Hochberg adjusted value (5 items). r = Rosenthal's r with bootstrap 95% CI (wide, and possibly including zero, for items with few non-zero paired changes). Bold: q < 0.05.") %>%
  body_add_break() %>%
  
  body_add_par("Table IV. Changes in SACRR item scores following the training (exploratory analysis)") %>%
  body_add_flextable(tableau_items(items_sacrr)) %>%
  body_add_par("Exploratory analysis. Paired Wilcoxon signed-rank test. p = unadjusted p-value; q = Benjamini-Hochberg adjusted value (26 items). r = Rosenthal's r with bootstrap 95% CI. Bold: q < 0.05.")

print(doc, target = "Tables_I_to_IV_revised.docx")

write_csv(res_principal, "Primary_results.csv")
write_csv(bind_rows(items_ptse, items_sacrr, .id = "Scale"), "Item_level_results.csv")
write_csv(normalite, "Normality_tests.csv")
write_csv(direction, "Direction_of_changes.csv")
write_csv(sensibilite, "Sensitivity_paired_t_test.csv")
write_csv(plafond, "Ceiling_effects.csv")

sessionInfo()  