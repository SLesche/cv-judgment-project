################################################################################
# 1. BIBLIOTHEKEN & DATEN LADEN
################################################################################
library(tidyverse)
library(readxl)

raw_data <- read_excel("data/data_cv_bewertung_2026-07-28_15-48.xlsx")
df <- raw_data

################################################################################
# 2. DATENBEREINIGUNG (ZEIT & ABLENKUNG)
################################################################################
block_prefixes <- sprintf("BG%02d", 1:25)

count_resumes <- function(row, prefixes) {
  completed_blocks <- 0
  for (prefix in prefixes) {
    block_cols <- grep(paste0("^", prefix), names(row), value = TRUE)
    if (length(block_cols) > 0 && any(!is.na(row[block_cols]))) {
      completed_blocks <- completed_blocks + 1
    }
  }
  return(completed_blocks)
}

df$anzahl_lebenslaeufe <- apply(df, 1, count_resumes, prefixes = block_prefixes)

df <- df %>% mutate(TIME_SUM = as.numeric(TIME_SUM))
referenz_gruppe <- df %>% filter(anzahl_lebenslaeufe == 5)
median_5_ll <- median(referenz_gruppe$TIME_SUM, na.rm = TRUE)
cut_off_wert <- median_5_ll / 2

time_cleaned_data <- df %>% filter(TIME_SUM >= cut_off_wert, anzahl_lebenslaeufe <= 15)

final_cleaned_data <- time_cleaned_data %>%
  mutate(FR16 = as.numeric(FR16)) %>% 
  filter(FR16 <= 3 | is.na(FR16))

if (any(final_cleaned_data$CASE == "Interview-Nummer (fortlaufend)", na.rm = TRUE)) {
  final_cleaned_data <- final_cleaned_data %>% 
    filter(CASE != "Interview-Nummer (fortlaufend)")
}

################################################################################
# 3. WIDE TO LONG TRANSFORMATION & MERGING
################################################################################

# --- Schritt 1: CV Slider ---
cv_long <- final_cleaned_data %>%
  select(CASE, FR07, FR16, FR17, FR19, FR21, FR21_04, IC01, SD01, SD02_01, SD10, SD10_09, SD19, SD19_03, starts_with("CV")) %>%
  mutate(across(c(FR07, FR16, FR17, FR19, IC01, SD01, SD10, SD19), as.numeric)) %>%
  mutate(across(c(FR21_04, SD10_09, SD19_03), as.character)) %>%
  pivot_longer(cols = starts_with("CV"), names_to = "raw_variable", values_to = "wert") %>%
  mutate(wert = as.numeric(wert)) %>%
  filter(!is.na(wert)) %>%
  extract(raw_variable, into = c("typ", "lebenslauf_id", "merkmal_id"), regex = "([A-Z]+)(\\d+)_(\\d+.*)") %>%
  filter(!is.na(merkmal_id)) %>%
  mutate(merkmal_name = case_when(
    merkmal_id == "01" ~ "geschaetzte_Extraversion",
    merkmal_id == "02" ~ "geschaetzter_Neurotizismus",
    merkmal_id == "03" ~ "geschaetzte_Gewissenhaftigkeit",
    merkmal_id == "04" ~ "geschaetzte_Vertraeglichkeit",
    merkmal_id == "05" ~ "geschaetzte_Offenheit",
    merkmal_id == "06" ~ "geschaetzer_IQ",
    TRUE ~ "unbekannt"
  )) %>%
  select(-merkmal_id, -typ) %>%
  pivot_wider(names_from = merkmal_name, values_from = wert) %>%
  mutate(lebenslauf_id = as.numeric(lebenslauf_id))

average_raters_per_cv <- cv_long %>% 
  count(lebenslauf_id) %>% 
  pull(n) %>% 
  mean()

# --- Schritt 2: BG Checkboxen ---
bg_long <- final_cleaned_data %>%
  select(CASE, starts_with("BG")) %>% 
  pivot_longer(cols = starts_with("BG"), names_to = "raw_variable", values_to = "kriterium_wert") %>%
  filter(!is.na(kriterium_wert)) %>%
  extract(raw_variable, into = c("typ", "lebenslauf_id", "kriterium_id"), regex = "([A-Z]+)(\\d+)_(\\d+.*)") %>%
  filter(!is.na(kriterium_id)) %>%
  mutate(kriterium_name = paste0("Kriterium_", kriterium_id)) %>%
  select(-kriterium_id, -typ) %>%
  pivot_wider(names_from = "kriterium_name", values_from = "kriterium_wert") %>%
  mutate(lebenslauf_id = as.numeric(lebenslauf_id))

analysedatensatz <- left_join(cv_long, bg_long, by = c("CASE", "lebenslauf_id"))

# --- Schritt 3: Wahre Werte einlesen & mappen ---
true_values <- read_excel("empra/cv_scores.xlsx")
colnames(true_values) <- as.character(true_values[1, ])

true_values_clean <- true_values[-1, ] %>%
  select(
    lebenslauf_id,
    age = age,
    education = education,
    # work = work,
    wahre_Extraversion = extraversion,
    wahre_Vertraeglichkeit = agreeableness,
    wahre_Gewissenhaftigkeit = conscientiousness,
    wahrer_Neurotizismus = neuroticism,
    wahre_Offenheit = openness,
    wahre_Intelligenz = Total_sum
  ) %>%
  mutate(
    education = case_when(
      education == "Realschulsabschluss" ~ 1,
      education == "Ausbildung" ~ 2,
      education == "Abitur" ~ 3,
      education == "Bachelor" ~ 4,
      education == "Master" ~ 5,
      education == "Promotion" ~ 6,
    )
  ) %>% 
  mutate(across(everything(), as.numeric))

# Zusammenführung des Hauptdatensatzes
final_data <- left_join(analysedatensatz, true_values_clean, by = "lebenslauf_id")

# --- Schritt 4: Transformation der Schätzer-Skala (0-100 zu 1-5) ---
final_data <- final_data %>%
  mutate(across(
    .cols = c(starts_with("geschaetzte_"), starts_with("geschaetzer_")),
    .fns = ~ (.x / 25) + 1
  ))

# --- Schritt 5: Spalten umbenennen ---
final_data <- final_data %>%
  rename(
    Big5_Erfahrung                      = FR07,
    Aufmerksamkeit                      = FR16,
    Englischkenntnisse                  = FR17,
    Auswahlprozesse                     = FR19,
    Ansicht                             = FR21,
    Ansicht_Sonstiges                   = FR21_04,
    Informed_Consent                    = IC01,
    Geschlecht                          = SD01,
    Alter                               = SD02_01,
    Formale_Bildung                     = SD10,
    Formale_Bildung_Sonstiges           = SD10_09,
    Studienwahl                         = SD19,
    Studienwahl_Sonstiges               = SD19_03,
    
    Kriterium_Ausbildungs_Studienwahl   = Kriterium_01,
    Kriterium_Geografische_Stationen    = Kriterium_02,
    Kriterium_Berufliche_Taetigkeiten   = Kriterium_03,
    Kriterium_Engagement_Ausserhalb     = Kriterium_04,
    Kriterium_Freizeitinteressen_Hobbys = Kriterium_05,
    Kriterium_Sprach_IT_Kompetenzen     = Kriterium_06,
    Kriterium_Weiterbildungen_Zertifikate = Kriterium_07,
    Kriterium_Stabilitaet_Kontinuitaet  = Kriterium_08,
    Kriterium_Berufliche_Verantwortung  = Kriterium_09,
    Kriterium_Struktur_Form_Lebenslauf  = Kriterium_10,
    Kriterium_Sonstiges                 = Kriterium_11,
    Kriterium_Sonstiges_Text            = Kriterium_11a
  ) %>%
  # Umwandlungs-Sicherheits-Check für spätere mathematische Analysen
  mutate(
    Alter = as.numeric(Alter),
    across(starts_with("wahre_"), as.numeric),
    wahrer_Neurotizismus = as.numeric(wahrer_Neurotizismus)
  )

################################################################################
# 4. KRITERIENVERWENDUNG & VISUALISIERUNG
################################################################################

# Dummy-Codierung für Kriterien (0 = nicht gewählt, 1 = gewählt)
# Das Text-Freitextfeld 'Kriterium_Sonstiges_Text' wird hier explizit ausgeschlossen!
final_data <- final_data %>%
  mutate(across(starts_with("Kriterium_") & !Kriterium_Sonstiges_Text, ~ as.numeric(.) - 1))

kriterien_paper_data <- final_data %>%
  select(starts_with("Kriterium_") & !Kriterium_Sonstiges_Text) %>%
  summarise(across(everything(), ~ round(mean(. == 1, na.rm = TRUE), 2)*100)) %>%
  pivot_longer(cols = everything(), names_to = "technischer_name", values_to = "prozentsatz") %>%
  mutate(Inhaltliches_Kriterium = case_when(
    technischer_name == "Kriterium_Ausbildungs_Studienwahl"   ~ "Ausbildungs- & Studienwahl",
    technischer_name == "Kriterium_Geografische_Stationen"    ~ "Wahl geografischer Stationen",
    technischer_name == "Kriterium_Berufliche_Taetigkeiten"   ~ "Berufliche Tätigkeiten & Rollenprofile",
    technischer_name == "Kriterium_Engagement_Ausserhalb"     ~ "Engagement außerhalb von Beruf/Studium",
    technischer_name == "Kriterium_Freizeitinteressen_Hobbys" ~ "Freizeitinteressen & Hobbys",
    technischer_name == "Kriterium_Sprach_IT_Kompetenzen"     ~ "Sprach- & IT-Kompetenzen",
    technischer_name == "Kriterium_Weiterbildungen_Zertifikate" ~ "Weiterbildungen & Zertifikate",
    technischer_name == "Kriterium_Stabilitaet_Kontinuitaet"  ~ "Stabilität & Kontinuität des Werdegangs",
    technischer_name == "Kriterium_Berufliche_Verantwortung"  ~ "Entwicklung beruflicher Verantwortung",
    technischer_name == "Kriterium_Struktur_Form_Lebenslauf"  ~ "Struktur & Form des Lebenslaufs",
    technischer_name == "Kriterium_Sonstiges"                 ~ "Sonstiges",
    TRUE ~ technischer_name
  ))

begründungen_plot <- ggplot(kriterien_paper_data, aes(x = prozentsatz, y = reorder(Inhaltliches_Kriterium, prozentsatz))) +
  geom_col(fill = "steelblue4", width = 0.7) +
  geom_text(aes(label = paste(prozentsatz, "%")), hjust = -0.2, size = 3.5, color = "black") +
  expand_limits(x = max(kriterien_paper_data$prozentsatz) * 1.1) +
  labs(
    title = "Anteil der genutzten Kriterien bei der Persönlichkeitseinschätzung",
    subtitle = paste0("Gesamtstichprobe (N Zeilen = ", nrow(final_data), ")"),
    x = "Relative Häufigkeit der Nennungen",
    y = "Beurteiltes Kriterium im Lebenslauf"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(color = "black")
  )  
################################################################################
# 5. ÜBERBLICK ÜBER DEMOGRAFISCHE DATEN (Personenebene)
################################################################################
demo_sample <- final_data %>% distinct(CASE, .keep_all = TRUE)

print("--- Altersverteilung ---")
demo_sample %>% summarise(N = n(), Mittelwert = mean(Alter, na.rm = TRUE), SD = sd(Alter, na.rm = TRUE), Min = min(Alter, na.rm = TRUE), Max = max(Alter, na.rm = TRUE))

print("--- Geschlechtsverteilung ---")
demo_sample %>% count(Geschlecht) %>% mutate(Prozent = n / sum(n) * 100)

print("--- Studienwahl Codes ---")
demo_sample %>% count(Studienwahl, Studienwahl_Sonstiges)

print("--- Formale Bildung ---")
demo_sample %>% count(Formale_Bildung)

print("--- Big5 Erfahrung ---")
demo_sample %>% count(Big5_Erfahrung)

print("--- Erfahrung mit Auswahlprozessen ---")
demo_sample %>% count(Auswahlprozesse)

################################################################################
# 6. ICC-BERECHNUNGEN (Konsistenz)
################################################################################
library(psych)

merkmale <- c("geschaetzte_Extraversion", "geschaetzter_Neurotizismus", "geschaetzte_Gewissenhaftigkeit", "geschaetzte_Vertraeglichkeit", "geschaetzte_Offenheit", "geschaetzer_IQ")

# --- Globale ICCs ---
icc_ergebnisse <- list()
for (merkmal in merkmale) {
  rater_matrix <- final_data %>%
    select(CASE, lebenslauf_id, !!sym(merkmal)) %>%
    pivot_wider(names_from = CASE, values_from = !!sym(merkmal)) %>%
    select(-lebenslauf_id)
  
  icc_berechnung <- ICC(rater_matrix, missing = TRUE)
  icc_ergebnisse[[merkmal]] <- icc_berechnung$results
}
icc_average <- function(icc2, k) {
  (k * icc2) / (1 + (k - 1) * icc2)
}

required_raters <- function(icc2, target = .90) {
  ceiling(target * (1 - icc2) /
            (icc2 * (1 - target)))
}
# ==============================================================================
# ERGEBNISSE ANZEIGEN
# ==============================================================================

# ICC-Tabelle für die Extraversion
print("--- ERGEBNIS EXTRAVERSION ---")
print(icc_ergebnisse[["geschaetzte_Extraversion"]])

icc_average(icc_ergebnisse[["geschaetzte_Extraversion"]]$ICC[2], average_raters_per_cv)
required_raters(icc_ergebnisse[["geschaetzte_Extraversion"]]$ICC[2], 0.90)

# ICC-Tabelle für den Neurotizismus
print("--- ERGEBNIS NEUROTIZISMUS ---")
print(icc_ergebnisse[["geschaetzter_Neurotizismus"]])
icc_average(icc_ergebnisse[["geschaetzter_Neurotizismus"]]$ICC[2], average_raters_per_cv)
required_raters(icc_ergebnisse[["geschaetzter_Neurotizismus"]]$ICC[2], 0.90)

# ICC-Tabelle für die Gewissenhaftigkeit
print("--- ERGEBNIS Gewissenhaftigkeit ---")
print(icc_ergebnisse[["geschaetzte_Gewissenhaftigkeit"]])
icc_average(icc_ergebnisse[["geschaetzte_Gewissenhaftigkeit"]]$ICC[2], average_raters_per_cv)
required_raters(icc_ergebnisse[["geschaetzte_Gewissenhaftigkeit"]]$ICC[2], 0.90)

# ICC-Tabelle für die Verträglichkeit
print("--- ERGEBNIS VERTRÄGLICHKEIT ---")
print(icc_ergebnisse[["geschaetzte_Vertraeglichkeit"]])
icc_average(icc_ergebnisse[["geschaetzte_Vertraeglichkeit"]]$ICC[2], average_raters_per_cv)
required_raters(icc_ergebnisse[["geschaetzte_Vertraeglichkeit"]]$ICC[2], 0.90)

# ICC-Tabelle für die Offenheit
print("--- ERGEBNIS OFFENHEIT ---")
print(icc_ergebnisse[["geschaetzte_Offenheit"]])
icc_average(icc_ergebnisse[["geschaetzte_Offenheit"]]$ICC[2], average_raters_per_cv)
required_raters(icc_ergebnisse[["geschaetzte_Offenheit"]]$ICC[2], 0.90)

# ICC-Tabelle für den IQ
print("--- ERGEBNIS IQ ---")
print(icc_ergebnisse[["geschaetzer_IQ"]])
icc_average(icc_ergebnisse[["geschaetzer_IQ"]]$ICC[2], average_raters_per_cv)
required_raters(icc_ergebnisse[["geschaetzer_IQ"]]$ICC[2], 0.90)

# # --- ICCs für Gruppenvergleiche (Beispiel: Extraversion) ---
# # Hinweis: Gemäß Kommentar ist Code 2 = Psychologie
# icc_data <- final_data %>%
#   mutate(Fachgruppe = if_else(Studienwahl == 2, "Psychologie", "Anderes Fach"))
# 
# # Psychologie
# matrix_psy <- icc_data %>% filter(Fachgruppe == "Psychologie") %>% select(CASE, lebenslauf_id, geschaetzte_Extraversion) %>% pivot_wider(names_from = CASE, values_from = geschaetzte_Extraversion) %>% select(-lebenslauf_id)
# print("--- ICC EXTRAVERSION (PSYCHOLOGIE) ---"); print(ICC(matrix_psy, missing = TRUE)$results)
# 
# # Andere Fächer
# matrix_andere <- icc_data %>% filter(Fachgruppe == "Anderes Fach") %>% select(CASE, lebenslauf_id, geschaetzte_Extraversion) %>% pivot_wider(names_from = CASE, values_from = geschaetzte_Extraversion) %>% select(-lebenslauf_id)
# print("--- ICC EXTRAVERSION (ANDERES FACH) ---"); print(ICC(matrix_andere, missing = TRUE)$results)
# 
# # Big5-Erfahrung (Code 1 = Ja)
# icc_big5_data <- final_data %>% mutate(Big5_Gruppe = if_else(Big5_Erfahrung == 1, "Mit Erfahrung", "Ohne Erfahrung"))
# 
# matrix_big5_ja <- icc_big5_data %>% filter(Big5_Gruppe == "Mit Erfahrung") %>% select(CASE, lebenslauf_id, geschaetzte_Extraversion) %>% pivot_wider(names_from = CASE, values_from = geschaetzte_Extraversion) %>% select(-lebenslauf_id)
# print("--- ICC EXTRAVERSION: MIT BIG5-ERFAHRUNG ---"); print(ICC(matrix_big5_ja, missing = TRUE)$results)
# 
# matrix_big5_nein <- icc_big5_data %>% filter(Big5_Gruppe == "Ohne Erfahrung") %>% select(CASE, lebenslauf_id, geschaetzte_Extraversion) %>% pivot_wider(names_from = CASE, values_from = geschaetzte_Extraversion) %>% select(-lebenslauf_id)
# print("--- ICC EXTRAVERSION: OHNE BIG5-ERFAHRUNG ---"); print(ICC(matrix_big5_nein, missing = TRUE)$results)

################################################################################
# 7. KORRELATIONEN (Akkuratheit)
################################################################################

# --- Globale Akkuratheit (Signifikanztests) ---
print("--- Signifikanztests Globale Korrelationen ---")
accuracy_cor_extra <- cor.test(final_data$geschaetzte_Extraversion, final_data$wahre_Extraversion)
accuracy_cor_vertr <- cor.test(final_data$geschaetzte_Vertraeglichkeit, final_data$wahre_Vertraeglichkeit)
accuracy_cor_gewiss <- cor.test(final_data$geschaetzte_Gewissenhaftigkeit, final_data$wahre_Gewissenhaftigkeit)
accuracy_cor_neuro <- cor.test(final_data$geschaetzter_Neurotizismus, final_data$wahrer_Neurotizismus)
accuracy_cor_offen <- cor.test(final_data$geschaetzte_Offenheit, final_data$wahre_Offenheit)
accuracy_cor_iq <- cor.test(final_data$geschaetzer_IQ, final_data$wahre_Intelligenz)

# # --- Akkuratheitsvergleich nach Gruppen ---
# print("--- Korrelationen nach Fachgruppe (2 = Psychologie) ---")
# final_data %>%
#   mutate(Fachgruppe = if_else(Studienwahl == 2, "Psychologie", "Anderes Fach")) %>% 
#   group_by(Fachgruppe) %>% 
#   summarise(
#     N_Urteile = n(),
#     r_Extraversion = cor(geschaetzte_Extraversion, wahre_Extraversion, use = "complete.obs"),
#     r_Vertraeglichkeit = cor(geschaetzte_Vertraeglichkeit, wahre_Vertraeglichkeit, use = "complete.obs"),
#     r_Gewissenhaftigkeit = cor(geschaetzte_Gewissenhaftigkeit, wahre_Gewissenhaftigkeit, use = "complete.obs"),
#     r_Neurotizismus = cor(geschaetzter_Neurotizismus, wahrer_Neurotizismus, use = "complete.obs"),
#     r_Offenheit = cor(geschaetzte_Offenheit, wahre_Offenheit, use = "complete.obs"),
#     r_IQ = cor(geschaetzer_IQ, wahre_Intelligenz, use = "complete.obs")
#   )
# 
# print("--- Korrelationen nach Geschlecht ---")
# final_data %>%
#   group_by(Geschlecht) %>% 
#   summarise(
#     N_Urteile = n(),
#     r_Extraversion       = cor(geschaetzte_Extraversion, wahre_Extraversion, use = "complete.obs"),
#     r_Vertraeglichkeit   = cor(geschaetzte_Vertraeglichkeit, wahre_Vertraeglichkeit, use = "complete.obs"),
#     r_Gewissenhaftigkeit = cor(geschaetzte_Gewissenhaftigkeit, wahre_Gewissenhaftigkeit, use = "complete.obs"),
#     r_Neurotizismus      = cor(geschaetzter_Neurotizismus, wahrer_Neurotizismus, use = "complete.obs"),
#     r_Offenheit          = cor(geschaetzte_Offenheit, wahre_Offenheit, use = "complete.obs"),
#     r_IQ                 = cor(geschaetzer_IQ, wahre_Intelligenz, use = "complete.obs")
#   )

average_data_per_cv <- final_data %>%
  group_by(lebenslauf_id, education, age, wahre_Extraversion, wahre_Vertraeglichkeit, wahre_Gewissenhaftigkeit, wahrer_Neurotizismus, wahre_Offenheit, wahre_Intelligenz) %>% 
  summarise(
    N_Urteile = n(),
    # education = unique(education),
    mean_Extraversion       = mean(geschaetzte_Extraversion, na.rm = TRUE),
    mean_Vertraeglichkeit   = mean(geschaetzte_Vertraeglichkeit, na.rm = TRUE),
    mean_Gewissenhaftigkeit = mean(geschaetzte_Gewissenhaftigkeit, na.rm = TRUE),
    mean_Neurotizismus      = mean(geschaetzter_Neurotizismus, na.rm = TRUE),
    mean_Offenheit          = mean(geschaetzte_Offenheit, na.rm = TRUE),
    mean_IQ                 = mean(geschaetzer_IQ, na.rm = TRUE)
  ) %>% 
  ungroup() 

average_data_cors <- average_data_per_cv %>% 
  summarise(
    r_Extraversion       = cor(mean_Extraversion, wahre_Extraversion, use = "complete.obs"),
    r_Vertraeglichkeit   = cor(mean_Vertraeglichkeit, wahre_Vertraeglichkeit, use = "complete.obs"),
    r_Gewissenhaftigkeit = cor(mean_Gewissenhaftigkeit, wahre_Gewissenhaftigkeit, use = "complete.obs"),
    r_Neurotizismus      = cor(mean_Neurotizismus, wahrer_Neurotizismus, use = "complete.obs"),
    r_Offenheit          = cor(mean_Offenheit, wahre_Offenheit, use = "complete.obs"),
    r_IQ                 = cor(mean_IQ, wahre_Intelligenz, use = "complete.obs")
  )

m1 <- lm(wahre_Intelligenz ~ education, data = average_data_per_cv)

m2 <- lm(wahre_Intelligenz ~ mean_IQ + education, data = average_data_per_cv)

model_comp <- anova(m1, m2)

################################################################################
# DESKRIPTIVE STATISTIKEN DER BEWERTETEN LEBENS LÄUFE (WAHRE WERTE)
################################################################################

print("--- Wahre Ausprägungen der Big 5 & IQ der Lebensläufe ---")

# 1. Datensatz auf die Ebene der Lebensläufe reduzieren (jeder LL nur 1x)
lebenslauf_sample <- final_data %>% 
  distinct(lebenslauf_id, .keep_all = TRUE)

# 2. Mittelwerte und Standardabweichungen berechnen
lebenslauf_deskriptiv <- lebenslauf_sample %>% 
  summarise(
    Anzahl_Lebenslaeufe = n(),
    # Intelligenz / IQ
    M_Intelligenz = mean(wahre_Intelligenz, na.rm = TRUE),
    SD_Intelligenz = sd(wahre_Intelligenz, na.rm = TRUE)
  )

# Ergebnis anzeigen
print(lebenslauf_deskriptiv)

lebenslauf_tabelle <- lebenslauf_sample %>%
  select(starts_with("wahre_"), wahrer_Neurotizismus) %>%
  pivot_longer(cols = everything(), names_to = "Merkmal", values_to = "Wert") %>%
  group_by(Merkmal) %>%
  summarise(
    Mittelwert = round(mean(Wert, na.rm = TRUE), 2),
    SD         = round(sd(Wert, na.rm = TRUE), 2),
    Min        = round(min(Wert, na.rm = TRUE), 2),
    Max        = round(max(Wert, na.rm = TRUE), 2)
  )

print(lebenslauf_tabelle)
# Mean Score of 111 (SD = 22.6) corresponds to IQ of 106 (see Sadus et al., 2026)