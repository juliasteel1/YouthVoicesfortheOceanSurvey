# YouthVoicesfortheOceanSurvey

# Julia Steel 
# Youth Voices for the Ocean Survey Analysis 
#September 2026

#-------------------------------------------------------
# Download Packages 
#-------------------------------------------------------

required_pkgs <- c("readxl","dplyr","tidyr","stringr","ggplot2",
                   "forcats","MASS","broom","janitor","brant","purrr","car")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

filter <- dplyr::filter
select <- dplyr::select
rename <- dplyr::rename

theme_clean <- theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA))
country_colors <- c("Scotland" = "#2196F3", "South Africa" = "#FF9800")

#---------------------------------------------------
# Data Preparation & Cleaning 
#---------------------------------------------------

xlsx_path  <- "YVO Survey .xlsx"
sheet_name <- "Main"
stopifnot("Excel file not found - check getwd() and exact filename" = file.exists(xlsx_path))

raw_fresh <- readxl::read_excel(xlsx_path, sheet = sheet_name)
cat("Loaded raw file:", nrow(raw_fresh), "rows x", ncol(raw_fresh), "cols\n")

new_names <- c(
  "consent","country","age_eligible","last_coast_visit","ocean_connection",
  "community_influence","how_learn_ocean","how_learn_other","top_ocean_concerns",
  "concerns_other","relevant_issues","interest_decisions","heard_of_mechanisms",
  "mechanisms_other","participated_before","participation_type","easier_to_share",
  "age_group","gender","employment_status","travel_time_coast","area_type",
  "area_other","land_cover","contact_research","contact_prize"
)
stopifnot(length(new_names) == 26)
names(raw_fresh)[1:26] <- new_names

survey <- raw_fresh %>% mutate(row_id = row_number())

survey_clean <- survey %>%
  mutate(across(c(consent, age_eligible, country), ~ str_squish(as.character(.)))) %>%
  filter(consent == "Yes", age_eligible == "Yes")
cat("After eligibility filter:", nrow(survey_clean), "rows\n")

#After eligibility filter: 261 rows

connection_levels <- c("Not connected at all","A little connected","Somewhat connected",
                       "Very connected","Extremely connected")
interest_levels    <- c("Not interested at all","A little interested","Somewhat interested",
                        "Very interested","Extremely interested")
travel_levels      <- c("Under 10 minutes","10-15 minutes","15-25 minutes","25-30 minutes",
                        "30-60 minutes","Over 60 minutes","I'm unable to get to the coast")
participation_levels <- c("No", "I'm not sure", "Yes")

survey_clean <- survey_clean %>%
  mutate(
    ocean_connection = str_squish(str_replace_all(as.character(ocean_connection), "\u00a0", " ")),
    ocean_connection = factor(ocean_connection, levels = connection_levels, ordered = TRUE),
    interest_decisions = str_squish(str_replace_all(as.character(interest_decisions), "\u00a0", " ")),
    interest_decisions = factor(interest_decisions, levels = interest_levels, ordered = TRUE),
    travel_time_coast = str_squish(str_replace_all(as.character(travel_time_coast), "\u00a0", " ")),
    travel_time_coast = factor(travel_time_coast, levels = travel_levels, ordered = TRUE),
    participated_before = str_squish(str_replace_all(as.character(participated_before), "\u00a0", " ")),
    participated_before = factor(participated_before, levels = participation_levels),
    gender = str_squish(as.character(gender)),
    age_group = str_squish(as.character(age_group)),
    employment_status = str_squish(as.character(employment_status))
  )

# --- area_type: FIXED VERSION (every raw category explicitly assigned
#     before factor() is called, so nothing silently becomes NA) ---
survey_clean <- survey_clean %>%
  mutate(
    area_type_raw = str_squish(as.character(area_type)),
    area_lower    = str_to_lower(area_type_raw),
    area_type = case_when(
      str_detect(area_lower, "city")                                          ~ "City",
      str_detect(area_lower, "suburb")                                         ~ "Suburb",
      str_detect(area_lower, "township")                                       ~ "Rural",
      str_detect(area_lower, "settlement")                                     ~ "Rural",
      str_detect(area_lower, "village")                                        ~ "Town / Village",
      str_detect(area_lower, "town") & !str_detect(area_lower, "township")     ~ "Town / Village",
      str_detect(area_lower, "rural")                                          ~ "Rural",
      TRUE ~ NA_character_
    ),
    area_type = factor(area_type, levels = c("City","Suburb","Town / Village","Rural"))
  ) %>%
  select(-area_lower)

cat("\n--- CHECKPOINT: cleaned variables ---\n")
print(table(survey_clean$ocean_connection, useNA = "ifany"))

#Not connected at all   A little connected   Somewhat connected       Very connected  Extremely connected 
#7                   24                   54                   98                   78 

print(table(survey_clean$interest_decisions, useNA = "ifany"))

#Not interested at all   A little interested   Somewhat interested       Very interested  Extremely interested 
#4                    19                    66                    81                    91 

print(table(survey_clean$area_type, useNA = "ifany"))
#City         Suburb Town / Village          Rural           <NA> 
#  132             38             72             17              2 

na_area_check <- survey_clean %>% filter(!is.na(area_type_raw), is.na(area_type)) %>% distinct(area_type_raw)
cat("\nRaw area_type values still mapping to NA (check these are genuinely ambiguous):\n")
print(na_area_check)
# A tibble: 2 × 1
#area_type_raw      
#<chr>              
#  1 I don't know       
#2 Whatever Orkney is!


# --- Expand multi-select columns into 0/1 dummy variables ---
expand_multiselect <- function(data, col, prefix) {
  split_long <- data %>%
    select(row_id, all_of(col)) %>%
    filter(!is.na(.data[[col]])) %>%
    separate_rows(all_of(col), sep = ";") %>%
    mutate(across(all_of(col), str_squish)) %>%
    filter(.data[[col]] != "")
  
  lookup <- split_long %>%
    distinct(.data[[col]]) %>%
    rename(option = 1) %>%
    mutate(clean = paste0(prefix, "_", janitor::make_clean_names(option)))
  
  wide <- split_long %>%
    rename(option = all_of(col)) %>%
    left_join(lookup, by = "option") %>%
    distinct(row_id, clean) %>%
    mutate(present = 1L) %>%
    pivot_wider(names_from = clean, values_from = present, values_fill = 0L)
  
  new_cols <- setdiff(names(wide), "row_id")
  data %>% left_join(wide, by = "row_id") %>%
    mutate(across(all_of(new_cols), ~ coalesce(., 0L)))
}

survey_clean <- survey_clean %>%
  expand_multiselect("how_learn_ocean",     "learn") %>%
  expand_multiselect("heard_of_mechanisms", "mechanism") %>%
  expand_multiselect("land_cover",          "land")

# --- Fold learning-pathway write-ins into existing categories ---
survey_clean <- survey_clean %>%
  mutate(
    learn_school_or_university_education = case_when(
      learn_school_or_university_education == 1 ~ 1L,
      coalesce(learn_past_school_education, 0L) == 1 ~ 1L,
      coalesce(learn_got_a_ph_d_in_oceanography, 0L) == 1 ~ 1L,
      coalesce(learn_books_in_my_school_library_and_passed_down_through_my_family, 0L) == 1 ~ 1L,
      TRUE ~ 0L
    )
  )

learn_exclude_from_test <- c(
  "learn_past_school_education", "learn_got_a_ph_d_in_oceanography",
  "learn_books_in_my_school_library_and_passed_down_through_my_family",
  "learn_i_dont_know", "learn_i_dont_learn_about_the_ocean", "learn_hobby_scuba_diving"
)

survey_anon <- survey_clean %>%
  select(-contact_research, -contact_prize, -how_learn_other, -concerns_other,
         -mechanisms_other, -area_other)

cat("\nFinal analysis dataset: survey_anon,", nrow(survey_anon), "rows,", ncol(survey_anon), "columns\n")
write.csv(survey_anon, "YVO_survey_cleaned_final.csv", row.names = FALSE)
#Final analysis dataset: survey_anon, 261 rows, 73 columns

# Derived numeric versions used throughout
survey_anon <- survey_anon %>%
  mutate(conn_num = as.numeric(ocean_connection),
         interest_num = as.numeric(interest_decisions))

# Column-name helpers used throughout the rest of the script
learn_cols       <- names(survey_anon)[str_starts(names(survey_anon), "learn_")]
learn_cols_clean <- setdiff(learn_cols, learn_exclude_from_test)
mechanism_cols   <- names(survey_anon)[startsWith(names(survey_anon), "mechanism_")]

#----------------------------------------------------
# Research Question 1: Learning Pathways 
#----------------------------------------------------

cat("\n\n========== RQ1: Learning Pathways ==========\n")

rq1 <- survey_anon %>%
  select(all_of(learn_cols_clean)) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "pathway", values_to = "count") %>%
  mutate(pct = round(count / nrow(survey_anon) * 100, 1),
         pathway = str_remove(pathway, "^learn_") %>% str_replace_all("_", " ") %>% str_to_sentence()) %>%
  arrange(desc(count))
print(rq1)
# A tibble: 8 × 3
#pathway                                           count   pct
#<chr>                                             <int> <dbl>
#  1 School or university education                      195  74.7
#2 Social media or online                              184  70.5
#3 Visiting the coast                                  182  69.7
#4 Film or television                                  156  59.8
#5 Environmental conservation organisations            139  53.3
#6 Work volunteering                                   118  45.2
#7 Conservations with friends family in my community   109  41.8
#8 Cultural practices or tradition                      60  23  

ggsave("RQ1_learning_pathways.png",
       ggplot(rq1, aes(x = pct, y = fct_reorder(pathway, pct), fill = pathway)) +
         geom_col() + geom_text(aes(label = paste0(pct, "%")), hjust = -0.2, size = 3.5) +
         scale_fill_manual(values = colorRampPalette(c("#E3F2FD", "#0D47A1"))(nrow(rq1))) +
         scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
         labs(title = "How do young people learn about the ocean?", x = "% of respondents", y = NULL) +
         theme_minimal(base_size = 12) + theme(panel.grid = element_blank(), legend.position = "none"),
       width = 8, height = 5, dpi = 300, bg = "white")

learn_country_tests <- bind_rows(lapply(learn_cols_clean, function(col) {
  tab <- table(survey_anon$country, survey_anon[[col]])
  use_fisher <- any(tab < 5)
  test <- if (use_fisher) fisher.test(tab) else chisq.test(tab)
  tibble(pathway = str_remove(col, "^learn_") %>% str_replace_all("_", " ") %>% str_to_sentence(),
         statistic = if ("statistic" %in% names(test)) unname(test$statistic) else NA_real_,
         p_value = test$p.value, test_used = if (use_fisher) "Fisher's exact" else "Chi-square")
})) %>% mutate(p_adj = p.adjust(p_value, method = "BH")) %>% arrange(p_value)
cat("\n--- Learning pathways: country comparison (BH-adjusted) ---\n"); print(learn_country_tests)

--- Learning pathways: country comparison (BH-adjusted) ---
  # A tibble: 8 × 5
#  pathway                                           statistic p_value test_used   p_adj
#<chr>                                                 <dbl>   <dbl> <chr>       <dbl>
#  1 Work volunteering                                  9.22e+ 0 0.00239 Chi-square 0.0192
#2 School or university education                     1.75e+ 0 0.185   Chi-square 0.741 
#3 Social media or online                             6.43e- 1 0.423   Chi-square 0.858 
#4 Film or television                                 6.26e- 1 0.429   Chi-square 0.858 
#5 Conservations with friends family in my community  1.51e- 1 0.697   Chi-square 1.00  
#6 Environmental conservation organisations           2.69e- 2 0.870   Chi-square 1.00  
#7 Visiting the coast                                 7.96e- 3 0.929   Chi-square 1.00  
#8 Cultural practices or tradition                    5.33e-31 1.00    Chi-square 1.00  

# ---------------------------------------------------
# Research Question 2: Connection Descriptive 
# ---------------------------------------------------
cat("\n\n========== RQ2: Ocean Connection ==========\n")
rq2 <- survey_anon %>% count(ocean_connection, .drop = FALSE) %>% mutate(pct = round(n / sum(n) * 100, 1))
print(rq2)

# A tibble: 5 × 3
#ocean_connection         n   pct
#<ord>                <int> <dbl>
#  1 Not connected at all     7   2.7
#2 A little connected      24   9.2
#3 Somewhat connected      54  20.7
#4 Very connected          98  37.5
#5 Extremely connected     78  29.9

ggsave("RQ2_ocean_connection.png",
       ggplot(rq2, aes(x = ocean_connection, y = pct, fill = ocean_connection)) +
         geom_col() + geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3.5) +
         scale_fill_manual(values = colorRampPalette(c("#E3F2FD", "#0D47A1"))(nrow(rq2))) +
         labs(title = "How connected do young people feel to the ocean?", x = NULL, y = "% of respondents") +
         theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 20, hjust = 1),
                                               panel.grid = element_blank(), legend.position = "none"),
       width = 7, height = 5, dpi = 300, bg = "white")

#Ocean connection - between country comparison 

survey_anon <- survey_anon %>%
  mutate(extremely_connected = ocean_connection == "Extremely connected")

# Descriptive: % "Extremely connected" by country
survey_anon %>%
  filter(!is.na(ocean_connection), !is.na(country)) %>%
  count(country, extremely_connected) %>%
  group_by(country) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  filter(extremely_connected == TRUE)

# A tibble: 2 × 4
# Groups:   country [2]
#country      extremely_connected     n   pct
#<chr>        <lgl>               <int> <dbl>
#  1 Scotland     TRUE                 38  24.1
#2 South Africa TRUE                   40  38.8

# Inferential test
tab_extreme <- table(survey_anon$country, survey_anon$extremely_connected)
print(tab_extreme)
#FALSE TRUE
#Scotland       120   38
#South Africa    63   40

use_fisher <- any(tab_extreme < 5)
if (use_fisher) {
  cat("\n--- Fisher's exact: 'Extremely connected' by country ---\n")
  print(fisher.test(tab_extreme))
} else {
  cat("\n--- Chi-square: 'Extremely connected' by country ---\n")
  print(chisq.test(tab_extreme))
}
#--- Chi-square: 'Extremely connected' by country ---
  
 # Pearson's Chi-squared test with Yates' continuity correction

#data:  tab_extreme
#X-squared = 5.8177, df = 1, p-value = 0.01587


#For methods: does any cell in your 2×2 table have fewer than 5 respondents?
#  If yes → uses Fisher's exact test
#If no → uses Pearson's chi-square test with Yate's continuity correction 

#need to check direction of country significance 
survey_anon %>%
  filter(!is.na(ocean_connection), !is.na(country)) %>%
  group_by(country) %>%
  summarise(median = connection_levels[median(as.integer(ocean_connection))], n = n())
# A tibble: 2 × 3
#country      median             n
#<chr>        <chr>          <int>
#1 Scotland     Very connected   158
#2 South Africa Very connected   103

survey_anon %>%
  filter(!is.na(country)) %>%
  count(country, extremely_connected) %>%
  group_by(country) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  filter(extremely_connected == TRUE)

# A tibble: 2 × 4
# Groups:   country [2]
#country      extremely_connected     n   pct
#<chr>        <lgl>               <int> <dbl>
#1 Scotland     TRUE                   38  24.1
#2 South Africa TRUE                   40  38.8

#----------------------------------------------------
# Research Question 3: Place-based factors & ocean connection
#----------------------------------------------------

cat("\n\n========== RQ3: Place-based factors ==========\n")

#Bivariate Analysis 

# ------------------------------------------------------------
# Clean the land-cover column list
# ------------------------------------------------------------
# WHY: the raw multi-select expansion includes the original text
# column and rare one-off free-text write-ins (n < 3) that cannot
# support a meaningful statistical comparison. These were excluded
# before any testing
# ------------------------------------------------------------

land_cols_all <- names(survey_anon)[str_starts(names(survey_anon), "land_")]

land_freq_check <- survey_anon %>%
  select(all_of(land_cols_all)) %>%
  select(-any_of("land_cover")) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n")

land_cols_clean <- land_freq_check %>%
  filter(!column %in% "land_none_of_the_above", n >= 3) %>%
  pull(column)

cat("Clean land columns retained for RQ3:\n"); print(land_cols_clean)

survey_anon <- survey_anon %>%
  mutate(land_n = rowSums(select(., all_of(land_cols_clean)), na.rm = TRUE),
         conn_num = as.numeric(ocean_connection))

# ------------------------------------------------------------
# Bivariate screening
# ------------------------------------------------------------
# WHY these specific tests:
#   - travel_time_coast is ordinal -> Spearman's rank correlation
#     (respects the natural ordering; Kruskal-Wallis would discard it)
#   - area_type is unordered categorical (4 groups) -> Kruskal-Wallis
#   - each land-cover type is a binary yes/no indicator -> Mann-Whitney U,
#     one test per type, with Benjamini-Hochberg correction across
#     the simultaneous tests to control the false discovery rate
# ------------------------------------------------------------

cat("\n========== STEP 2: Bivariate screening ==========\n")

cat("\nSpearman: travel_time_coast vs ocean_connection\n")
print(cor.test(as.integer(survey_anon$travel_time_coast), as.integer(survey_anon$ocean_connection), method = "spearman"))

#Spearman's rank correlation rho

#data:  as.integer(survey_anon$travel_time_coast) and as.integer(survey_anon$ocean_connection)
#S = 2431831, p-value = 0.000435
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.2310859 

cat("\nKruskal-Wallis: area_type vs ocean_connection\n")
print(kruskal.test(conn_num ~ area_type, data = survey_anon))
#Kruskal-Wallis rank sum test

#data:  conn_num by area_type
#Kruskal-Wallis chi-squared = 5.3366, df = 3, p-value = 0.1487

land_results <- bind_rows(lapply(land_cols_clean, function(col) {
  grps <- split(survey_anon$conn_num, survey_anon[[col]])
  if (length(grps) < 2 || any(sapply(grps, length) < 2)) return(NULL)
  test <- wilcox.test(grps[["1"]], grps[["0"]], exact = FALSE)
  tibble(variable = col,
         land_characteristic = str_remove(col, "^land_") %>% str_replace_all("_", " ") %>% str_to_sentence(),
         n_yes = length(grps[["1"]]), W = unname(test$statistic), p_value = round(test$p.value, 4))
})) %>%
  mutate(p_adj = p.adjust(p_value, method = "BH")) %>%
  arrange(p_value)

cat("\n--- Mann-Whitney U: each land characteristic vs connectedness (BH-adjusted) ---\n")
print(land_results)

#> print(land_results)
# A tibble: 13 × 6
#variable                         land_characteristic         n_yes      W p_value  p_adj
#<chr>                            <chr>                       <int>  <dbl>   <dbl>  <dbl>
#  1 land_coast_beach                 Coast beach                   132 11196   0      0     
#2 land_estuary                     Estuary                        27  4218.  0.0028 0.013 
#3 land_wetland                     Wetland                        45  6152.  0.0033 0.013 
#4 land_conservation_area           Conservation area              51  6684.  0.004  0.013 
#5 land_grassland                   Grassland                      57  6993   0.0143 0.0372
#6 land_floodplain                  Floodplain                      9  1530.  0.0629 0.136 
#7 land_agricultural_land           Agricultural land              72  7541   0.157  0.291 
#8 land_river_stream_canal          River stream canal            131  7821   0.233  0.379 
#9 land_mountain                    Mountain                        3   282.  0.402  0.529 
#10 land_woodland                    Woodland                       92  7322.  0.417  0.529 
#11 land_forest_commercial_or_native Forest commercial or native    66  6820.  0.448  0.529 
#12 land_mining_area                 Mining area                     6   847   0.640  0.694 
#13 land_lack_loch                   Lack loch                      40  4484.  0.881  0.881 

sig_land_filtered <- land_results %>% filter(p_adj < .05) %>% pull(variable)
sig_land_filtered <- sig_land_filtered[sapply(sig_land_filtered, function(col) sum(survey_anon[[col]], na.rm = TRUE) >= 20)]
cat("\nLand types entering RQ3 modelling (p_adj < .05, n >= 20):\n")
print(sig_land_filtered)

# ------------------------------------------------------------
# Build the shared modelling dataset
# ------------------------------------------------------------

rq3_data <- survey_anon %>%
  select(ocean_connection, travel_time_coast, area_type, land_n, all_of(sig_land_filtered)) %>%
  drop_na() %>%
  mutate(travel_time_coast = droplevels(travel_time_coast),
         area_type = droplevels(area_type))

cat("\nSample size for RQ3 modelling:", nrow(rq3_data), "\n")


# ------------------------------------------------------------
# STEP 4: Solo models - each place-based factor type alone
# ------------------------------------------------------------
# WHY: before combining factors, each is tested alone against the
# null model, to establish which single TYPE of place-based factor
# (travel time, urban/rural, or land-cover) is the strongest
# standalone predictor of connectedness.
# ------------------------------------------------------------

cat("\n========== STEP 4: Solo models (each factor type alone) ==========\n")

n0 <- polr(ocean_connection ~ 1, data = rq3_data, Hess = TRUE)
n1 <- polr(ocean_connection ~ travel_time_coast, data = rq3_data, Hess = TRUE)
n2 <- polr(ocean_connection ~ area_type, data = rq3_data, Hess = TRUE)

n_land_only <- NULL
if (length(sig_land_filtered) > 0) {
  n_land_only <- polr(as.formula(paste("ocean_connection ~", paste(sig_land_filtered, collapse = " + "))),
                      data = rq3_data, Hess = TRUE)
}

cat("\n--- AIC: solo models vs null ---\n")
if (!is.null(n_land_only)) print(AIC(n0, n1, n2, n_land_only)) else print(AIC(n0, n1, n2))
#df      AIC
#n0           4 611.6135
#n1           6 600.9929
#n2           7 609.2730
#n_land_only  9 591.5172

cat("\n--- LRT: each solo model vs null ---\n")
cat("\nTravel time alone vs null:\n");    print(anova(n0, n1))

#Travel time alone vs null:
#  Likelihood ratio tests of ordinal regression models

#Response: ocean_connection
#Model Resid. df Resid. Dev   Test    Df LR stat.      Pr(Chi)
#1                 1       222   603.6135                                   
#2 travel_time_coast       220   588.9929 1 vs 2     2 14.62064 0.0006686047

cat("\nArea type alone vs null:\n");      print(anova(n0, n2))

#Area type alone vs null:
#  Likelihood ratio tests of ordinal regression models

#Response: ocean_connection
#Model Resid. df Resid. Dev   Test    Df LR stat.    Pr(Chi)
#1         1       222   603.6135                                 
#2 area_type       219   595.2730 1 vs 2     3 8.340595 0.03947291

if (!is.null(n_land_only)) { cat("\nLand types alone vs null:\n"); print(anova(n0, n_land_only)) }

#Land types alone vs null:
#  Likelihood ratio tests of ordinal regression models

#Response: ocean_connection
#Model Resid. df Resid. Dev   Test    Df LR stat.      Pr(Chi)
#1                                                                                        1       222   603.6135                                   
#2 land_coast_beach + land_estuary + land_wetland + land_conservation_area + land_grassland       217   573.5172 1 vs 2     5 30.09633 1.411847e-05


# ------------------------------------------------------------
# Combined models - building up from land types
# ------------------------------------------------------------
# WHY start from land types rather than travel time: Step 4
# established land-cover characteristics as the strongest solo
# predictor, so subsequent models build outward from that base to
# test whether travel time and/or area_type add anything further.
# ------------------------------------------------------------

cat("\n========== STEP 5: Combined models ==========\n")

n3 <- polr(ocean_connection ~ travel_time_coast + area_type, data = rq3_data, Hess = TRUE)

n_new <- NULL; n5 <- NULL
if (length(sig_land_filtered) > 0) {
  n_new <- polr(as.formula(paste("ocean_connection ~ travel_time_coast +", paste(sig_land_filtered, collapse = " + "))),
                data = rq3_data, Hess = TRUE)
  n5 <- polr(as.formula(paste("ocean_connection ~ travel_time_coast + area_type +", paste(sig_land_filtered, collapse = " + "))),
             data = rq3_data, Hess = TRUE)
}

cat("\n--- AIC: full comparison across all models ---\n")
if (!is.null(n5)) {
  print(AIC(n0, n1, n2, n_land_only, n3, n_new, n5))
} else {
  print(AIC(n0, n1, n2, n3))
}

#df      AIC
#n0           4 611.6135
#n1           6 600.9929
#n2           7 609.2730
#n_land_only  9 591.5172
#n3           9 602.8328
#n_new       11 591.7688
#n5          14 589.1908

cat("\n--- LRT: does travel time add to land types alone? ---\n")
if (!is.null(n_new)) print(anova(n_land_only, n_new))

#Likelihood ratio tests of ordinal regression models

#Response: ocean_connection
#Model Resid. df Resid. Dev   Test    Df LR stat.   Pr(Chi)
#1                     land_coast_beach + land_estuary + land_wetland + land_conservation_area + land_grassland       217   573.5172                                
#2 travel_time_coast + land_coast_beach + land_estuary + land_wetland + land_conservation_area + land_grassland       215   569.7688 1 vs 2     2 3.748414 0.1534766

cat("\n--- LRT: do travel time + area_type together add to land types alone? ---\n")
if (!is.null(n5)) print(anova(n_land_only, n5))

#Likelihood ratio tests of ordinal regression models

#Response: ocean_connection
#Model Resid. df Resid. Dev   Test    Df LR stat.    Pr(Chi)
#1                                 land_coast_beach + land_estuary + land_wetland + land_conservation_area + land_grassland       217   573.5172                                 
#2 travel_time_coast + area_type + land_coast_beach + land_estuary + land_wetland + land_conservation_area + land_grassland       212   561.1908 1 vs 2     5 12.32647 0.03057816

cat("\n--- LRT: does area_type specifically add, once land types + travel time are known? ---\n")
if (!is.null(n5)) print(anova(n_new, n5))

#Likelihood ratio tests of ordinal regression models

#Response: ocean_connection
#Model Resid. df Resid. Dev   Test    Df LR stat.    Pr(Chi)
#1             travel_time_coast + land_coast_beach + land_estuary + land_wetland + land_conservation_area + land_grassland       215   569.7688                                 
#2 travel_time_coast + area_type + land_coast_beach + land_estuary + land_wetland + land_conservation_area + land_grassland       212   561.1908 1 vs 2     3 8.578053 0.03546021
 
cat("\n--- LRT: does area_type alone add to travel-time-only model? (for context) ---\n")
print(anova(n1, n3))

#Likelihood ratio tests of ordinal regression models

#Response: ocean_connection
#Model Resid. df Resid. Dev   Test    Df LR stat.   Pr(Chi)
#1             travel_time_coast       220   588.9929                                
#2 travel_time_coast + area_type       217   584.8328 1 vs 2     3 4.160077 0.2446894

# ------------------------------------------------------------
# STEP 6: Multicollinearity check (VIF)
# ------------------------------------------------------------
# WHY: confirms travel time, area_type, and the 5 land types in
# the final combined model (n5) are sufficiently distinct from
# each other that their individual coefficients are stable and
# trustworthy, despite testing several related place-based
# predictors together (Dormann et al., 2013).
# ------------------------------------------------------------

cat("\n========== STEP 6: VIF check on final model predictors ==========\n")
if (!is.null(n5)) {
  vif_model_rq3 <- lm(as.formula(paste("as.numeric(ocean_connection) ~ travel_time_coast + area_type +",
                                       paste(sig_land_filtered, collapse = " + "))),
                      data = rq3_data)
  print(vif(vif_model_rq3))
}

#GVIF Df GVIF^(1/(2*Df))
#travel_time_coast      1.821733  2        1.161773
#area_type              1.338076  3        1.049736
#land_coast_beach       1.604051  1        1.266511
#land_estuary           1.183095  1        1.087702
#land_wetland           1.286713  1        1.134334
#land_conservation_area 1.265837  1        1.125094
#land_grassland         1.093626  1        1.045766

# ------------------------------------------------------------
# Select and report the best-supported model
# ------------------------------------------------------------
# WHY report AIC + LRT + VIF + Brant together, and interpret with
# appropriate caution: AIC identifies the best-supported model
# AMONG THOSE TESTED, not a definitively "true" model
# (Burnham & Anderson, 2002).
# ------------------------------------------------------------

cat("\n========== STEP 7: Best-supported model ==========\n")

# EDIT this line once you've reviewed the AIC table in Step 5
n_best_rq3 <- if (!is.null(n5)) n5 else n3

n_best_coef  <- coef(summary(n_best_rq3))
n_best_pvals <- pnorm(abs(n_best_coef[, "t value"]), lower.tail = FALSE) * 2
n_best_results <- tibble(
  Variable    = rownames(n_best_coef),
  Coefficient = round(n_best_coef[, "Value"], 3),
  SE          = round(n_best_coef[, "Std. Error"], 3),
  t_value     = round(n_best_coef[, "t value"], 3),
  p_value     = round(n_best_pvals, 4),
  sig         = case_when(n_best_pvals < .001 ~ "***", n_best_pvals < .01 ~ "**", n_best_pvals < .05 ~ "*", TRUE ~ "")
)
cat("\n--- Coefficients ---\n"); print(n_best_results)

#--- Coefficients ---
  # A tibble: 14 × 6
#  Variable                                Coefficient    SE t_value p_value sig  
#<chr>                                         <dbl> <dbl>   <dbl>   <dbl> <chr>
#  1 travel_time_coast.L                          -0.213 0.288  -0.74   0.459  ""   
#2 travel_time_coast.Q                           0.236 0.221   1.07   0.286  ""   
#3 area_typeSuburb                               1.07  0.394   2.72   0.0065 "**" 
#4 area_typeTown / Village                       0.439 0.323   1.36   0.174  ""   
#5 area_typeRural                                0.739 0.584   1.26   0.206  ""   
#6 land_coast_beach                              0.848 0.324   2.62   0.0089 "**" 
#7 land_estuary                                  0.428 0.441   0.969  0.333  ""   
#8 land_wetland                                  0.423 0.378   1.12   0.263  ""   
#9 land_conservation_area                        0.366 0.342   1.07   0.284  ""   
#10 land_grassland                                0.707 0.337   2.10   0.0358 "*"  
#11 Not connected at all|A little connected      -2.96  0.505  -5.86   0      "***"
#12 A little connected|Somewhat connected        -1.3   0.32   -4.07   0      "***"
#13 Somewhat connected|Very connected             0.136 0.29    0.469  0.639  ""   
#14 Very connected|Extremely connected            2.04  0.322   6.33   0      "***"

cat("\n--- Odds ratios with 95% CIs ---\n")
print(exp(cbind(OR = coef(n_best_rq3), confint(n_best_rq3))))

#OR     2.5 %   97.5 %
#  travel_time_coast.L     0.8081211 0.4591156 1.422714
#travel_time_coast.Q     1.2658938 0.8209159 1.954849
#area_typeSuburb         2.9232465 1.3661632 6.431610
#area_typeTown / Village 1.5511395 0.8247243 2.931331
#area_typeRural          2.0929698 0.6779593 6.855174
#land_coast_beach        2.3349392 1.2423920 4.435582
#land_estuary            1.5337123 0.6519240 3.708207
#land_wetland            1.5271047 0.7311122 3.237773
#land_conservation_area  1.4421164 0.7384898 2.833847
#land_grassland          2.0272062 1.0537491 3.957972

cat("\n--- Brant test (proportional odds assumption) ---\n")
print(brant(n_best_rq3))

#------------------------------------------------------------ 
#  Test for			X2	df	probability 
#------------------------------------------------------------ 
#  Omnibus				57.16	30	0
#travel_time_coast.L		2.56	3	0.47
#travel_time_coast.Q		5.98	3	0.11
#area_typeSuburb		1.48	3	0.69
#area_typeTown / Village	0.32	3	0.96
#area_typeRural			0.83	3	0.84
#land_coast_beach		0.5	3	0.92
#land_estuary			0.2	3	0.98
#land_wetland			0.71	3	0.87
#land_conservation_area	3.44	3	0.33
#land_grassland			24.28	3	0
#------------------------------------------------------------ 
#  
#  H0: Parallel Regression Assumption holds
#X2 df  probability
#Omnibus                 57.1632445 30 2.002253e-03
#travel_time_coast.L      2.5569753  3 4.650823e-01
#travel_time_coast.Q      5.9839000  3 1.123962e-01
#area_typeSuburb          1.4793643  3 6.870412e-01
#area_typeTown / Village  0.3249725  3 9.552654e-01
#area_typeRural           0.8263242  3 8.431608e-01
#land_coast_beach         0.4967382  3 9.196074e-01
#land_estuary             0.2028724  3 9.771243e-01
#land_wetland             0.7122464  3 8.703198e-01
#land_conservation_area   3.4411926  3 3.284697e-01
#land_grassland          24.2807013  3 2.182612e-05

cat("\nNOTE: if the omnibus test or any individual predictor is significant,\n")
cat("the proportional odds assumption is violated for that predictor. In an\n")
cat("earlier run, this was violated specifically for land_grassland - if that\n")
cat("recurs, report it as a limitation or fit a partial proportional odds\n")
cat("model (e.g. via ordinal::clm with a nominal= term for that predictor).\n")


# ------------------------------------------------------------
# Interpretive note on bivariate vs. combined-model divergence
# ------------------------------------------------------------
# WHY this note matters: some predictors' significance changes
# between the bivariate (Step 2) and combined (Step 5-7) results.
# This is an expected feature of multivariate modelling, not an
# error - see the interpretive guide printed below.
# ------------------------------------------------------------

cat("\n========== STEP 8: Interpretive note ==========\n")
cat("
If travel_time_coast was significant bivariately (Step 2) but not in the
final combined model (Step 7), this is consistent with CONFOUNDING: travel
time's apparent effect may be substantially explained by its correlation
with specific land-cover types (Baron & Kenny, 1986).
 
If area_type was NOT significant bivariately (Step 2) but IS significant
in the final combined model (Step 7), this is consistent with a
SUPPRESSION EFFECT: area_type's true relationship with connectedness may
only become visible once land-cover heterogeneity is held constant
(Tzelgov & Henik, 1991).
 
Both patterns are expected, legitimate features of multivariate modelling
and should be reported as substantive findings, not discrepancies to
reconcile away.
")

cat("\n\n========== RQ3 MODELLING COMPLETE ==========\n")
cat("Remember to update 'n_best_rq3' in Step 7 based on the actual AIC\n")
cat("table (Step 5) and LRT results from your own run.\n")

#Given your VIF check already confirmed the 5 land types aren't meaningfully correlated with each other, 
#don't need to run individual models for e.g. estuary, river, conservation area, etc

#0   1
#0 126 108
#1   3  24

# Check overlap between estuary/wetland/conservation area and coast/beach
table(survey_anon$land_estuary, survey_anon$land_coast_beach)
#0   1
#0 126 108
#1   3  24
table(survey_anon$land_wetland, survey_anon$land_coast_beach)
#0   1
#0 111 105
#1  18  27
table(survey_anon$land_conservation_area, survey_anon$land_coast_beach)
#0   1
#0 110 100
#1  19  32

# Does travel time predict which land types someone has nearby?
kruskal.test(as.integer(travel_time_coast) ~ land_coast_beach, data = survey_anon)
#Kruskal-Wallis rank sum test

#data:  as.integer(travel_time_coast) by land_coast_beach
#Kruskal-Wallis chi-squared = 69.741, df = 1, p-value < 2.2e-16

kruskal.test(as.integer(travel_time_coast) ~ land_estuary, data = survey_anon)
#Kruskal-Wallis rank sum test

#data:  as.integer(travel_time_coast) by land_estuary
#Kruskal-Wallis chi-squared = 6.8141, df = 1, p-value = 0.009044

#CONFOUNDING FACTORS!!!
#> kruskal.test(as.integer(travel_time_coast) ~ land_coast_beach, data = survey_anon
#Kruskal-Wallis rank sum test

#data:  as.integer(travel_time_coast) by land_coast_beach
#Kruskal-Wallis chi-squared = 69.741, df = 1, p-value < 2.2e-16

#> kruskal.test(as.integer(travel_time_coast) ~ land_estuary, data = survey_anon)

#Kruskal-Wallis rank sum test

#data:  as.integer(travel_time_coast) by land_estuary
#Kruskal-Wallis chi-squared = 6.8141, df = 1, p-value = 0.009044

######proximity to coast is confounding with travel time:

#-----------------------------------------
#Running a diagnostic tests 
#-----------------------------------------

# Model without coast/beach, with travel time
sig_land_no_coast <- setdiff(sig_land_filtered, "land_coast_beach")

n_no_coast <- polr(as.formula(paste("ocean_connection ~ travel_time_coast + area_type +",
                                    paste(sig_land_no_coast, collapse = " + "))),
                   data = rq3_data, Hess = TRUE)

summary(n_no_coast)
#Call:
#  polr(formula = as.formula(paste("ocean_connection ~ travel_time_coast + area_type +", 
#                                  paste(sig_land_no_coast, collapse = " + "))), data = rq3_data, 
#       Hess = TRUE)

#Coefficients:
#  Value Std. Error t value
#travel_time_coast.L     -0.6248     0.2411 -2.5913
#travel_time_coast.Q      0.2386     0.2203  1.0830
#area_typeSuburb          0.8493     0.3753  2.2630
#area_typeTown / Village  0.2981     0.3182  0.9368
#area_typeRural           0.4822     0.5756  0.8378
#land_estuary             0.5900     0.4356  1.3544
#land_wetland             0.4218     0.3805  1.1088
#land_conservation_area   0.3834     0.3420  1.1211
#land_grassland           0.7864     0.3362  2.3388

#Intercepts:
#  Value   Std. Error t value
#Not connected at all|A little connected -3.4395  0.4724    -7.2804
#A little connected|Somewhat connected   -1.7929  0.2615    -6.8565
#Somewhat connected|Very connected       -0.3883  0.2093    -1.8558
#Very connected|Extremely connected       1.4706  0.2314     6.3548

#Residual Deviance: 568.1498 
#AIC: 594.1498 

# Pull travel time's p-value specifically for easy comparison
no_coast_coef <- coef(summary(n_no_coast))
no_coast_pvals <- pnorm(abs(no_coast_coef[, "t value"]), lower.tail = FALSE) * 2
tibble(Variable = rownames(no_coast_coef), p_value = round(no_coast_pvals, 4))

# A tibble: 13 × 2
#Variable                                p_value
#<chr>                                     <dbl>
#  1 travel_time_coast.L                      0.0096
#2 travel_time_coast.Q                      0.279 
#3 area_typeSuburb                          0.0236
#4 area_typeTown / Village                  0.349 
#5 area_typeRural                           0.402 
#6 land_estuary                             0.176 
#7 land_wetland                             0.268 
#8 land_conservation_area                   0.262 
#9 land_grassland                           0.0193
#10 Not connected at all|A little connected  0     
#11 A little connected|Somewhat connected    0     
#12 Somewhat connected|Very connected        0.0635
#13 Very connected|Extremely connected       0  

# Compare AIC to your original n5, to see the cost of dropping coast/beach
AIC(n5, n_no_coast)
#df      AIC
#n5         14 589.1908
#n_no_coast 13 594.1498

#Travel time (linear), WITH coast/beach:    p = .459  (not significant)
#Travel time (linear), WITHOUT coast/beach: p = .0096 (significant again)
#coast/beach was specifically absorbing travel time's explanatory power
#so what if we remove beach/coast from land-based characteristics and re-run the modelling 
#isolates each remaining land type completely, removing both coast/beach and the other 
#land types as competitors, to see which ones can stand on their own once given a clear field.

land_types_to_test <- c("land_estuary", "land_wetland", "land_conservation_area", "land_grassland")

individual_no_coast_models <- bind_rows(lapply(land_types_to_test, function(land_var) {
  model <- polr(as.formula(paste("ocean_connection ~ travel_time_coast + area_type +", land_var)),
                data = rq3_data, Hess = TRUE)
  
  coef_table <- coef(summary(model))
  pvals <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2
  
  land_row <- coef_table[land_var, ]
  land_pval <- pvals[land_var]
  travel_row <- coef_table["travel_time_coast.L", ]
  travel_pval <- pvals["travel_time_coast.L"]
  
  tibble(
    land_characteristic = str_remove(land_var, "^land_") %>% str_replace_all("_", " ") %>% str_to_sentence(),
    land_coefficient = round(land_row["Value"], 3),
    land_p_value = round(land_pval, 4),
    travel_time_coefficient = round(travel_row["Value"], 3),
    travel_time_p_value = round(travel_pval, 4),
    model_AIC = round(AIC(model), 2)
  )
}))

print(individual_no_coast_models)

# A tibble: 4 × 6
#land_characteristic land_coefficient land_p_value travel_time_coefficient travel_time_p_value model_AIC
#<chr>                          <dbl>        <dbl>                   <dbl>               <dbl>     <dbl>
#  1 Estuary                        0.838       0.0406                  -0.662              0.0053      601.
#2 Wetland                        0.903       0.0081                  -0.703              0.003       598.
#3 Conservation area              0.751       0.0167                  -0.716              0.0025      599.
#4 Grassland                      0.948       0.0033                  -0.709              0.0027      596.
#> print(individual_no_coast_models)

#I want to consider ALL land-types bc I don't think its enough rationale to go by bivariate significance 

exists("land_cols_clean_rq3")
exists("land_cols_clean")
exists("land_cols_all")
exists("sig_land_filtered")

print(land_cols_clean)
length(land_cols_clean)

all_land_no_coast <- setdiff(all_land_cols, "land_coast_beach")

rq3_data_all_land_no_coast <- survey_anon %>%
  select(ocean_connection, travel_time_coast, area_type, all_of(all_land_no_coast)) %>%
  drop_na() %>%
  mutate(travel_time_coast = droplevels(travel_time_coast),
         area_type = droplevels(area_type))

cat("\nSample size:", nrow(rq3_data_all_land_no_coast), "\n")

n_all_land_no_coast <- polr(as.formula(paste("ocean_connection ~ travel_time_coast + area_type +",
                                             paste(all_land_no_coast, collapse = " + "))),
                            data = rq3_data_all_land_no_coast, Hess = TRUE)

no_coast_coef <- coef(summary(n_all_land_no_coast))
no_coast_pvals <- pnorm(abs(no_coast_coef[, "t value"]), lower.tail = FALSE) * 2
tibble(Variable = rownames(no_coast_coef),
       Coefficient = round(no_coast_coef[, "Value"], 3),
       p_value = round(no_coast_pvals, 4),
       sig = case_when(no_coast_pvals < .001 ~ "***", no_coast_pvals < .01 ~ "**",
                       no_coast_pvals < .05 ~ "*", TRUE ~ "")) %>%
  print(n = 20)

# A tibble: 21 × 4
#Variable                                Coefficient p_value sig  
#<chr>                                         <dbl>   <dbl> <chr>
#  1 travel_time_coast.L                          -0.612  0.0125 "*"  
#2 travel_time_coast.Q                           0.194  0.382  ""   
#3 area_typeSuburb                               0.78   0.0424 "*"  
#4 area_typeTown / Village                       0.276  0.416  ""   
#5 area_typeRural                                0.325  0.598  ""   
#6 land_forest_commercial_or_native              0.238  0.465  ""   
#7 land_woodland                                -0.225  0.459  ""   
#8 land_river_stream_canal                      -0.386  0.18   ""   
#9 land_lack_loch                                0.131  0.751  ""   
#10 land_agricultural_land                        0.005  0.990  ""   
#11 land_grassland                                0.723  0.0694 ""   
#12 land_mining_area                              0.634  0.499  ""   
#13 land_conservation_area                        0.499  0.163  ""   
#14 land_estuary                                  0.568  0.214  ""   
#15 land_wetland                                  0.376  0.337  ""   
#16 land_floodplain                               1.05   0.205  ""   
#17 land_mountain                                -1.34   0.196  ""   
#18 Not connected at all|A little connected      -3.7    0      "***"
#19 A little connected|Somewhat connected        -2.04   0      "***"
#20 Somewhat connected|Very connected            -0.604  0.0177 "*"  

# Compare AIC to the version WITH coast/beach
AIC(n_all_land, n_all_land_no_coast)
#df      AIC
#n_all_land          22 599.0424
#n_all_land_no_coast 21 603.5427
# Correlation matrix among just the 4 remaining significant types
cor(rq3_data %>% select(land_estuary, land_wetland, land_conservation_area, land_grassland))
#land_estuary land_wetland land_conservation_area land_grassland
#land_estuary             1.00000000    0.2688117              0.2654797     0.03285278
#land_wetland             0.26881168    1.0000000              0.3702491     0.24045260
#land_conservation_area   0.26547970    0.3702491              1.0000000     0.16775424
#land_grassland           0.03285278    0.2404526              0.1677542     1.00000000

# -----------------------------------------------------------
# Research Question 4: Awareness of governance mechanisms (descriptive)
# -----------------------------------------------------------

cat("\n\n========== RQ4: Awareness ==========\n")
rq4 <- survey_anon %>%
  select(all_of(mechanism_cols)) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "mechanism", values_to = "count") %>%
  mutate(pct = round(count / nrow(survey_anon) * 100, 1),
         mechanism = str_remove(mechanism, "^mechanism_") %>% str_replace_all("_", " ") %>% str_to_sentence()) %>%
  arrange(desc(count))
print(rq4)
#mechanism                                              count   pct
#<chr>                                                  <int> <dbl>
#  1 Marine protected areas                                   220  84.3
#2 Community led conservation projects or campaigns         203  77.8
#3 Fisheries management                                     180  69  
#4 Marine planning or marine spatial planning               143  54.8
#5 High seas treaties                                       129  49.4
#6 Marine development consultations                         100  38.3
#7 None of the above                                         17   6.5
#8 Policy change consultations                                1   0.4
#9 Un convention on the conservation of migratory species     1   0.4

ggsave("RQ4_awareness_mechanisms.png",
       ggplot(rq4, aes(x = pct, y = fct_reorder(mechanism, pct), fill = mechanism)) +
         geom_col() + geom_text(aes(label = paste0(pct, "%")), hjust = -0.2, size = 3.5) +
         scale_fill_manual(values = colorRampPalette(c("#E3F2FD", "#0D47A1"))(nrow(rq4))) +
         scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
         labs(title = "Awareness of ocean decision-making processes", x = "% of respondents", y = NULL) +
         theme_minimal(base_size = 12) + theme(panel.grid = element_blank(), legend.position = "none"),
       width = 9, height = 5, dpi = 300, bg = "white")

mechanism_country_tests <- bind_rows(lapply(mechanism_cols, function(col) {
  tab <- table(survey_anon$country, survey_anon[[col]])
  use_fisher <- any(tab < 5)
  test <- if (use_fisher) fisher.test(tab) else chisq.test(tab)
  tibble(mechanism = str_remove(col, "^mechanism_") %>% str_replace_all("_", " ") %>% str_to_sentence(),
         statistic = if ("statistic" %in% names(test)) unname(test$statistic) else NA_real_,
         p_value = test$p.value, test_used = if (use_fisher) "Fisher's exact" else "Chi-square")
})) %>% mutate(p_adj = p.adjust(p_value, method = "BH")) %>% arrange(p_value)

cat("\n--- Awareness mechanisms: country comparison (BH-adjusted) ---\n")
print(mechanism_country_tests)
mechanism                                              statistic p_value test_used      p_adj
#<chr>                                                      <dbl>   <dbl> <chr>          <dbl>
#  1 Marine development consultations                        1.67e+ 0   0.196 Chi-square     0.901
#2 Marine protected areas                                  1.64e+ 0   0.200 Chi-square     0.901
#3 Fisheries management                                    9.36e- 1   0.333 Chi-square     0.963
#4 High seas treaties                                      4.31e- 1   0.511 Chi-square     0.963
#5 None of the above                                       3.85e- 1   0.535 Chi-square     0.963
#6 Community led conservation projects or campaigns        6.20e-31   1.000 Chi-square     1    
#7 Marine planning or marine spatial planning              0          1     Chi-square     1    
#8 Policy change consultations                            NA          1     Fisher's exact 1    
#9 Un convention on the conservation of migratory species NA          1     Fisher's exact 1   

# --------------------------------------------------------------
# Research Question 5 - What predicts interest in participation?
# Hierarchical (blockwise) modelling, per supervisor's suggestion
# --------------------------------------------------------------
cat("\n\n========== RQ5: Interest in Participation ==========\n")

# --- Step 1: derived variables ---
awareness_n_tbl <- survey_anon %>%
  select(row_id, all_of(mechanism_cols), -any_of("mechanism_none_of_the_above")) %>%
  mutate(awareness_n = rowSums(select(., -row_id), na.rm = TRUE)) %>%
  select(row_id, awareness_n)

survey_anon <- survey_anon %>%
  left_join(awareness_n_tbl, by = "row_id") %>%
  mutate(learned_at_work = coalesce(learn_work_volunteering == 1, FALSE))

# --- Step 2: bivariate screening ---
cat("\n--- Bivariate screening ---\n")
cat("\nSpearman: interest vs ocean_connection\n")
print(cor.test(as.integer(survey_anon$interest_decisions), as.integer(survey_anon$ocean_connection), method = "spearman"))
#Spearman's rank correlation rho
#data:  as.integer(survey_anon$interest_decisions) and as.integer(survey_anon$ocean_connection)
#S = 1330448, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.5510126 

cat("\nSpearman: interest vs awareness_n\n")
print(cor.test(as.integer(survey_anon$interest_decisions), survey_anon$awareness_n, method = "spearman"))
#Spearman's rank correlation rho

#data:  as.integer(survey_anon$interest_decisions) and survey_anon$awareness_n
#S = 1572997, p-value = 1.087e-15
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.4691596 

learn_interest_tests <- bind_rows(lapply(learn_cols_clean, function(col) {
  test <- wilcox.test(as.integer(interest_decisions) ~ survey_anon[[col]], data = survey_anon)
  tibble(pathway = str_remove(col, "^learn_") %>% str_replace_all("_", " ") %>% str_to_sentence(),
         W = unname(test$statistic), p_value = round(test$p.value, 4))
})) %>% mutate(p_adj = p.adjust(p_value, method = "BH")) %>% arrange(p_value)
cat("\n--- Each learning pathway vs interest (BH-adjusted) ---\n"); print(learn_interest_tests)

#--- Each learning pathway vs interest (BH-adjusted) ---
#  # A tibble: 8 × 4
#  pathway                                               W p_value    p_adj
#<chr>                                             <dbl>   <dbl>    <dbl>
#  1 Work volunteering                                 4427   0      0       
#2 Environmental conservation organisations          5550   0      0       
#3 Cultural practices or tradition                   4060.  0.0001 0.000267
#4 Conservations with friends family in my community 6872.  0.014  0.028   
#5 Visiting the coast                                5984.  0.0244 0.0390  
#6 School or university education                    5476.  0.0581 0.0775  
#7 Film or television                                7241   0.0966 0.110   
#8 Social media or online                            6920.  0.759  0.759  

land_interest_tests <- bind_rows(lapply(land_cols_clean_rq3, function(col) {
  grps <- split(survey_anon$interest_num, survey_anon[[col]])
  if (length(grps) < 2 || any(sapply(grps, length) < 2)) return(NULL)
  test <- wilcox.test(as.integer(interest_decisions) ~ survey_anon[[col]], data = survey_anon)
  tibble(variable = col, land_characteristic = str_remove(col, "^land_") %>% str_replace_all("_", " ") %>% str_to_sentence(),
         W = unname(test$statistic), p_value = round(test$p.value, 4))
})) %>% mutate(p_adj = p.adjust(p_value, method = "BH")) %>% arrange(p_value)
cat("\n--- Each land characteristic vs interest (BH-adjusted) ---\n"); print(land_interest_tests)

sig_land_cols_interest <- land_interest_tests %>% filter(p_adj < .05) %>% pull(variable)

sig_land_cols_interest <- sig_land_cols_interest[sapply(sig_land_cols_interest, function(col) sum(survey_anon[[col]], na.rm = TRUE) >= 20)]
cat("\nLand columns entering RQ5 modelling:\n"); print(sig_land_cols_interest)

df_gender <- survey_anon %>% filter(gender %in% c("Male", "Female"))
cat("\nMann-Whitney: interest by gender\n"); print(wilcox.test(as.integer(interest_decisions) ~ gender, data = df_gender))

#Mann-Whitney: interest by gender

#Wilcoxon rank sum test with continuity correction

#data:  as.integer(interest_decisions) by gender
#W = 7502.5, p-value = 0.02213
#alternative hypothesis: true location shift is not equal to 0

cat("\nKruskal-Wallis: interest across age_group\n"); print(kruskal.test(as.integer(interest_decisions) ~ age_group, data = survey_anon))
#Kruskal-Wallis: interest across age_group

#Kruskal-Wallis rank sum test

#data:  as.integer(interest_decisions) by age_group
#Kruskal-Wallis chi-squared = 2.9647, df = 2, p-value = 0.2271

cat("\nKruskal-Wallis: interest across employment_status\n"); print(kruskal.test(as.integer(interest_decisions) ~ employment_status, data = survey_anon))

#Kruskal-Wallis: interest across employment_status

#Kruskal-Wallis rank sum test

#data:  as.integer(interest_decisions) by employment_status
#Kruskal-Wallis chi-squared = 17.657, df = 20, p-value = 0.61

cat("\nSpearman: interest vs travel_time_coast\n")
print(cor.test(as.integer(survey_anon$interest_decisions), as.integer(survey_anon$travel_time_coast), method = "spearman"))

#Spearman's rank correlation rho

#data:  as.integer(survey_anon$interest_decisions) and as.integer(survey_anon$travel_time_coast)
#S = 2061499, p-value = 0.5123
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#        rho 
#-0.04360987 

cat("\nKruskal-Wallis: interest across area_type\n"); print(kruskal.test(as.integer(interest_decisions) ~ area_type, data = survey_anon))

#Kruskal-Wallis: interest across area_type

#Kruskal-Wallis rank sum test

#data:  as.integer(interest_decisions) by area_type
#Kruskal-Wallis chi-squared = 4.4327, df = 3, p-value = 0.2184

cat("\nMann-Whitney: interest by country\n"); print(wilcox.test(as.integer(interest_decisions) ~ country, data = survey_anon))

#Mann-Whitney: interest by country

#Wilcoxon rank sum test with continuity correction

#data:  as.integer(interest_decisions) by country
#W = 6100, p-value = 0.0003448
#alternative hypothesis: true location shift is not equal to 0

# --- Step 3: shared modelling dataset ---

names(rq5_data)          # does it include age_group, employment_status, country?
table(rq5_data$area_type)

c("age_group", "employment_status", "country", "gender") %in% names(survey_anon)   # all should be TRUE

rq5_data <- survey_anon %>%
  select(interest_decisions, ocean_connection, awareness_n, learned_at_work,
         travel_time_coast, area_type, gender, age_group, employment_status,
         country) %>%
  drop_na() %>%
  mutate(
    gender = factor(if_else(gender %in% c("Male","Female"), gender, "Other/prefer not to say")),
    age_group = factor(age_group),
    employment_status = factor(employment_status),
    travel_time_coast = droplevels(travel_time_coast),
    area_type = droplevels(area_type)
  )

nrow(rq5_data)           # expect 226
names(rq5_data)          # confirm all columns now present

b0 <- polr(interest_decisions ~ 1, data = rq5_data, Hess = TRUE)
b1 <- polr(interest_decisions ~ ocean_connection + awareness_n, data = rq5_data, Hess = TRUE)
b2 <- polr(interest_decisions ~ ocean_connection + awareness_n + learned_at_work, data = rq5_data, Hess = TRUE)
b3 <- polr(interest_decisions ~ ocean_connection + awareness_n + learned_at_work + travel_time_coast + area_type, data = rq5_data, Hess = TRUE)
b4 <- update(b3, . ~ . + gender + age_group + employment_status)
b5 <- update(b4, . ~ . + country)

AIC(b0, b1, b2, b3, b4, b5)

#> AIC(b0, b1, b2, b3, b4, b5)
#df      AIC
#b0  4 606.8670
#b1  9 501.5186
#b2 10 497.4957
#b3 15 504.0847
#b4 38 524.0528
#b5 39 522.2945

vif_formula <- update(b3_formula, as.numeric(interest_decisions) ~ . + gender + age_group + employment_status + country - interest_decisions)
cat("\n--- RQ5: VIF check (fullest block) ---\n")
print(vif(lm(vif_formula, data = rq5_data)))
#> print(vif(lm(vif_formula, data = rq5_data)))
#GVIF Df GVIF^(1/(2*Df))
#ocean_connection  2.827105  4        1.138722
#awareness_n       1.611335  1        1.269384
#learned_at_work   1.456801  1        1.206980
#travel_time_coast 1.614758  2        1.127267
#area_type         2.864326  3        1.191710
#gender            1.403145  2        1.088368
#age_group         1.745801  2        1.149473
#employment_status 7.070512 19        1.052820
#country           1.688378  1        1.299376
 
# --- Step 5: report best-supported model ---
best_model_rq5 <- b1   # <-- confirm against your printed AIC/LRT results above
cat("\n--- RQ5 best model: coefficients ---\n")
rq5_coef <- coef(summary(best_model_rq5)); rq5_pvals <- pnorm(abs(rq5_coef[, "t value"]), lower.tail = FALSE) * 2
print(tibble(Variable = rownames(rq5_coef), Coefficient = round(rq5_coef[, "Value"], 3),
             SE = round(rq5_coef[, "Std. Error"], 3), p_value = round(rq5_pvals, 4),
             sig = case_when(rq5_pvals < .001 ~ "***", rq5_pvals < .01 ~ "**", rq5_pvals < .05 ~ "*", TRUE ~ "")))

# A tibble: 9 × 5
#Variable                                  Coefficient    SE p_value sig  
#<chr>                                           <dbl> <dbl>   <dbl> <chr>
#  1 ocean_connection.L                              3.05  0.676  0      "***"
#2 ocean_connection.Q                              0.462 0.562  0.410  ""   
#3 ocean_connection.C                              0.075 0.43   0.862  ""   
#4 ocean_connection^4                             -0.208 0.323  0.520  ""   
#5 awareness_n                                     0.397 0.079  0      "***"
#6 Not interested at all|A little interested      -3.10  0.626  0      "***"
#7 A little interested|Somewhat interested        -0.776 0.363  0.0324 "*"  
#8 Somewhat interested|Very interested             1.27  0.362  0.0005 "***"
#9 Very interested|Extremely interested            3.23  0.41   0      "***"

cat("\n--- RQ5 best model: odds ratios ---\n"); print(exp(cbind(OR = coef(best_model_rq5), confint(best_model_rq5))))
cat("\n--- RQ5 best model: Brant test ---\n"); print(brant(best_model_rq5))

# ----------------------------------------------------------------------
# PART G: Country comparisons (overall)
# ----------------------------------------------------------------------

cat("\n\n========== Country Comparisons ==========\n")
cat("\n--- Mann-Whitney U: ocean_connection by country ---\n")
print(wilcox.test(as.integer(ocean_connection) ~ country, data = survey_anon))
#Wilcoxon rank sum test with continuity correction

#data:  as.integer(ocean_connection) by country
#W = 6523, p-value = 0.004563
#alternative hypothesis: true location shift is not equal to 0

cat("\n--- Mann-Whitney U: interest_decisions by country ---\n")
print(wilcox.test(as.integer(interest_decisions) ~ country, data = survey_anon))
#Wilcoxon rank sum test with continuity correction
#data:  as.integer(interest_decisions) by country
#W = 6100, p-value = 0.0003448
#alternative hypothesis: true location shift is not equal to 0

cat("\n--- Chi-square: participation by country ---\n")
print(chisq.test(table(survey_anon$country, survey_anon$participated_before)))
#Pearson's Chi-squared test

#data:  table(survey_anon$country, survey_anon$participated_before)
#X-squared = 3.095, df = 2, p-value = 0.2128

cat("\n\n========== ALL ANALYSES COMPLETE ==========\n")
cat("Reminder: confirm n_best_rq3 and best_model_rq5 against the printed\n")
cat("AIC and LRT results above before finalising your write-up.\n")

#need to do country comparison between "extremely interest" between countries for interest 
survey_anon <- survey_anon %>%
  mutate(extremely_interested = interest_decisions == "Extremely interested")

# Descriptive: % "Extremely interested" by country
survey_anon %>%
  filter(!is.na(interest_decisions), !is.na(country)) %>%
  count(country, extremely_interested) %>%
  group_by(country) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  filter(extremely_interested == TRUE)

# A tibble: 2 × 4
# Groups:   country [2]
#country      extremely_interested     n   pct
#<chr>        <lgl>                <int> <dbl>
#  1 Scotland     TRUE                    43  27.2
#2 South Africa TRUE                    48  46.6

# Inferential test
tab_extreme_interest <- table(survey_anon$country, survey_anon$extremely_interested)
print(tab_extreme_interest)
#              FALSE TRUE
#Scotland       115   43
#South Africa    55   48

use_fisher <- any(tab_extreme_interest < 5)
if (use_fisher) {
  cat("\n--- Fisher's exact: 'Extremely interested' by country ---\n")
  print(fisher.test(tab_extreme_interest))
} else {
  cat("\n--- Chi-square: 'Extremely interested' by country ---\n")
  print(chisq.test(tab_extreme_interest))
}

#--- Chi-square: 'Extremely interested' by country ---
  
#  Pearson's Chi-squared test with Yates' continuity correction

#data:  tab_extreme_interest
#X-squared = 9.4834, df = 1, p-value = 0.002073

#Every block added after b2 (place-based, demographics, country) either increases 
#AIC or fails to reach significance. This means your best-supported, most parsimonious model 
#is:BEST MODEL: interest_decisions ~ ocean_connection + awareness_n + learned_at_work

best_model_rq5 <- b2

rq5_coef <- coef(summary(best_model_rq5)); rq5_pvals <- pnorm(abs(rq5_coef[, "t value"]), lower.tail = FALSE) * 2
print(tibble(Variable = rownames(rq5_coef), Coefficient = round(rq5_coef[, "Value"], 3),
             SE = round(rq5_coef[, "Std. Error"], 3), p_value = round(rq5_pvals, 4),
             sig = case_when(rq5_pvals < .001 ~ "***", rq5_pvals < .01 ~ "**", rq5_pvals < .05 ~ "*", TRUE ~ "")))

print(exp(cbind(OR = coef(best_model_rq5), confint(best_model_rq5))))
print(brant(best_model_rq5))

# A tibble: 10 × 5
#Variable                                  Coefficient    SE p_value sig  
#<chr>                                           <dbl> <dbl>   <dbl> <chr>
#  1 ocean_connection.L                              2.86  0.687  0      "***"
#2 ocean_connection.Q                              0.458 0.567  0.420  ""   
#3 ocean_connection.C                              0.019 0.432  0.964  ""   
#4 ocean_connection^4                             -0.149 0.323  0.645  ""   
#5 awareness_n                                     0.339 0.082  0      "***"
#6 learned_at_workTRUE                             0.723 0.295  0.0142 "*"  
#7 Not interested at all|A little interested      -3.08  0.626  0      "***"
#8 A little interested|Somewhat interested        -0.758 0.363  0.0371 "*"  
#9 Somewhat interested|Very interested             1.30  0.363  0.0003 "***"
#10 Very interested|Extremely interested            3.32  0.414  0      "***"
#> print(exp(cbind(OR = coef(best_model_rq5), confint(best_model_rq5))))

#OR     2.5 %    97.5 %
#ocean_connection.L  17.4284351 4.7289254 71.691501
#ocean_connection.Q   1.5801669 0.4964769  4.708682
#ocean_connection.C   1.0195709 0.4394013  2.410904
#ocean_connection^4   0.8616972 0.4554316  1.622288
#awareness_n          1.4042260 1.1989972  1.651699
#learned_at_workTRUE  2.0615801 1.1570174  3.686376
#Final model summary (b2)

#Your best-supported model shows:
  
#Connectedness: strong, significant linear effect (OR = 21.03 for the linear trend — a large effect)
#Awareness: significant (OR = 1.49 per additional mechanism known)
#Brant test: fully supports the proportional odds assumption (all p > .6, well above .05)

#------------------------------------------------------------
# Demographics
#------------------------------------------------------------
cat("\n\n========== Sample Demographics ==========\n")

# --- Country ---
demo_country <- survey_anon %>%
  count(country) %>%
  mutate(pct = round(100 * n / sum(n), 1))
cat("\n--- Country ---\n"); print(demo_country)

#  # A tibble: 2 × 3
#  country          n   pct
#<chr>        <int> <dbl>
#1 Scotland       158  60.5
#2 South Africa   103  39.5

# --- Gender ---
demo_gender <- survey_anon %>%
  filter(!is.na(gender), gender != "") %>%
  count(gender) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
cat("\n--- Gender ---\n"); print(demo_gender)
# A tibble: 5 × 3
#gender                n   pct
#<chr>             <int> <dbl>
# 1 Female              182  69.7
#2 Male                 70  26.8
#3 Non-binary            6   2.3
#4 Prefer not to say     2   0.8
#5 Trans man             1   0.4

# --- Age group ---
demo_age <- survey_anon %>%
  filter(!is.na(age_group), age_group != "") %>%
  count(age_group) %>%
  mutate(pct = round(100 * n / sum(n), 1))
cat("\n--- Age group ---\n"); print(demo_age)

# A tibble: 3 × 3
#age_group     n   pct
#<chr>     <int> <dbl>
#1 18-23        80  30.7
#2 24-30       136  52.1
#3 30-35        45  17.2

# --- Employment status ---
demo_employment <- survey_anon %>%
  filter(!is.na(employment_status), employment_status != "") %>%
  count(employment_status) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
cat("\n--- Employment status ---\n"); print(demo_employment)

# A tibble: 21 × 3
#employment_status                                                             n   pct
#<chr>                                                                     <int> <dbl>
#  1 Working (full and part-time)                                                 64  24.5
#2 Studying                                                                     58  22.2
#3 Working (full and part-time) ;                                               39  14.9
#4 Studying;                                                                    37  14.2
#5 Volunteering/internship/placement                                            11   4.2
#6 Studying;Working (full and part-time)                                        10   3.8
#7 Studying;Working (full and part-time) ;                                       7   2.7
#8 Studying;Volunteering/internship/placement                                    6   2.3
#9 Studying;Working (full and part-time) ;Volunteering/internship/placement;     4   1.5
#10 Volunteering/internship/placement;                                            4   1.5

# --- Gender x Country (descriptive cross-tab) ---
demo_gender_country <- survey_anon %>%
  filter(!is.na(gender), gender != "", !is.na(country)) %>%
  count(country, gender) %>%
  group_by(country) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()
cat("\n--- Gender by country ---\n"); print(demo_gender_country)

# A tibble: 9 × 4
#country      gender                n   pct
#<chr>        <chr>             <int> <dbl>
#1 Scotland     Female              105  66.5
#2 Scotland     Male                 47  29.7
#3 Scotland     Non-binary            4   2.5
#4 Scotland     Prefer not to say     1   0.6
#5 Scotland     Trans man             1   0.6
#6 South Africa Female               77  74.8
#7 South Africa Male                 23  22.3
#8 South Africa Non-binary            2   1.9
#9 South Africa Prefer not to say     1   1 

# --- Age group x Country (descriptive cross-tab) ---
demo_age_country <- survey_anon %>%
  filter(!is.na(age_group), age_group != "", !is.na(country)) %>%
  count(country, age_group) %>%
  group_by(country) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()
cat("\n--- Age group by country ---\n"); print(demo_age_country)

# A tibble: 6 × 4
#country      age_group     n   pct
#<chr>        <chr>     <int> <dbl>
#1 Scotland     18-23        43  27.2
#2 Scotland     24-30        88  55.7
#3 Scotland     30-35        27  17.1
#4 South Africa 18-23        37  35.9
#5 South Africa 24-30        48  46.6
#6 South Africa 30-35        18  17.5

# --- Combined demographics summary table (one row per characteristic) ---
demo_summary <- bind_rows(
  demo_country    %>% rename(category = country) %>% mutate(characteristic = "Country"),
  demo_gender     %>% rename(category = gender) %>% mutate(characteristic = "Gender"),
  demo_age        %>% rename(category = age_group) %>% mutate(characteristic = "Age group"),
  demo_employment %>% rename(category = employment_status) %>% mutate(characteristic = "Employment status")
) %>%
  select(characteristic, category, n, pct)
cat("\n--- Combined demographics summary table ---\n"); print(demo_summary, n = 40)

# --- Visualisation: gender and age group by country ---
ggsave("demographics_gender_by_country.png",
       ggplot(demo_gender_country, aes(x = fct_reorder(gender, pct), y = pct, fill = country)) +
         geom_col(position = "dodge") + coord_flip() +
         scale_fill_manual(values = country_colors) +
         labs(title = "Gender identity by country", x = NULL, y = "% of respondents (within country)", fill = "Country") +
         theme_clean,
       width = 7, height = 5, dpi = 300, bg = "white")

ggsave("demographics_age_by_country.png",
       ggplot(demo_age_country, aes(x = age_group, y = pct, fill = country)) +
         geom_col(position = "dodge") +
         geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
         scale_fill_manual(values = country_colors) +
         labs(title = "Age group by country", x = "Age group", y = "% of respondents (within country)", fill = "Country") +
         theme_clean,
       width = 7, height = 5, dpi = 300, bg = "white")

cat("Saved: demographics_gender_by_country.png, demographics_age_by_country.png\n")
