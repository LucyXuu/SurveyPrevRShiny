#######################################
###functions to process

# =============================================================================
# 2. Helper Function - Standardize Column Names
# =============================================================================

standardize_columns <- function(df, type) {
  # type: "bh", "wm", "ch"
  
  # Cluster
  cand_cluster <- c("HH1", "cluster", "clusterno")
  hit <- intersect(cand_cluster, names(df))
  if (length(hit) > 0) names(df)[names(df) == hit[1]] <- "cluster"
  
  # Household
  cand_hh <- c("HH2", "householdID", "hhno")
  hit <- intersect(cand_hh, names(df))
  if (length(hit) > 0) names(df)[names(df) == hit[1]] <- "householdID"
  
  # Area (urban/rural)
  if ("HH6" %in% names(df)) {
    if (length(unique(df$HH6)) > 2 & "hh6a" %in% names(df)) {
      df$strata <- ifelse(df$hh6a == 1, "urban", "rural")
    } else {
      df$strata <- ifelse(df$HH6 == 1, "urban", "rural")
    }
  }
  
  # Weights
  if (type == "bh" & "wmweight" %in% names(df)) df$weight <- df$wmweight
  if (type == "wm" & "wmweight" %in% names(df)) df$weight <- df$wmweight
  if (type == "ch" & "chweight" %in% names(df)) df$weight <- df$chweight
  
  return(df)
}

# =============================================================================
# 3. Process NMR (Neonatal Mortality Rate)
# =============================================================================

process_NMR <- function(bh) {
  bh <- standardize_columns(bh, "bh")
  
  # Variables (harmonize names)
  cand_alive <- c("BH5")
  cand_age_death <- c("BH9C")
  cand_birth_cmc <- c("BH4C")
  cand_int_cmc <- c("WDOI")
  
  # Subset variables
  bh <- bh %>% 
    select(cluster, householdID, strata, weight,
           all_of(c(cand_alive, cand_age_death, cand_birth_cmc, cand_int_cmc)))
  
  names(bh)[names(bh) %in% cand_alive] <- "alive"
  names(bh)[names(bh) %in% cand_age_death] <- "age_death"
  names(bh)[names(bh) %in% cand_birth_cmc] <- "birth_cmc"
  names(bh)[names(bh) %in% cand_int_cmc] <- "int_cmc"
  
  # Restrict to births in last 5 years (61 months)
  bh <- bh %>%
    filter(birth_cmc >= int_cmc - 61 & birth_cmc <= int_cmc - 1)
  
  # Define outcome: 1 if neonatal death (died at age 0 months), 0 otherwise
  bh$value <- ifelse(!is.na(bh$age_death) & bh$age_death == 0, 1, 0)
  
  bh %>% select(cluster, householdID, weight, strata, value)
}

# =============================================================================
# 4. Process ANC (Antenatal Care - 4+ visits)
# =============================================================================

process_ANC <- function(wm) {
  wm <- standardize_columns(wm, "wm")
  
  # --- Birth recode (CM17 vs CM13)
  if ("CM17" %in% names(wm)) {
    births_var <- "CM17"
  } else if ("CM13" %in% names(wm)) {
    births_var <- "CM13"
    # Convert Y/N or 0/1 into numeric
    if (is.character(wm[[births_var]])) {
      wm[[births_var]] <- ifelse(wm[[births_var]] %in% c("Y", "1"), 1, 0)
    }
  } else {
    stop("No birth recode variable (CM17 or CM13) found.")
  }
  
  # --- ANC visits (MN3 vs MN5)
  # Nigeria 2016–17 has MN3, and MN5 there is "has immunization card"
  if ("MN3" %in% names(wm)) {
    anc_var <- "MN3"
  } else if ("MN5" %in% names(wm)) {
    anc_var <- "MN5"
  } else {
    stop("No ANC variable (MN3 or MN5) found.")
  }
  
  # --- Process: Filter for births in last 2 years, define ANC 4+ indicator
  wm <- wm %>%
    filter(!!sym(births_var) == 1) %>%
    mutate(
      anc_visits = as.numeric(!!sym(anc_var)),
      value = case_when(
        !is.na(anc_visits) & anc_visits >= 4 & anc_visits < 98 ~ 1,
        !is.na(anc_visits) ~ 0,
        TRUE ~ 0
      )
    )
  
  # --- Return standardized structure
  wm %>%
    select(cluster, householdID, weight, strata, value)
}

# =============================================================================
# 5. Process DTP3 (Pentavalent 3 vaccination coverage)
# =============================================================================

process_DTP3 <- function(ch) {
  ch <- standardize_columns(ch, "ch")
  
  # Harmonize vaccine variables
  cand_age <- c("CAGE_6")
  cand_vax_day <- c("IM6PENTA3D", "IM3PENTA3D")
  cand_vax_ever <- c("IM20", "IM12A")
  cand_vax_times <- c("IM21", "IM12B")
  
  age_var <- cand_age[cand_age %in% names(ch)][1]
  vax_day <- cand_vax_day[cand_vax_day %in% names(ch)][1]
  vax_ever <- cand_vax_ever[cand_vax_ever %in% names(ch)][1]
  vax_times <- cand_vax_times[cand_vax_times %in% names(ch)][1]
  
  # Filter for children aged 12-23 months (CAGE_6 == 3)
  # Define vaccination status
  ch <- ch %>%
    filter((!!sym(age_var)) == 3) %>%
    mutate(value = case_when(
      # Valid vaccination date recorded
      (!!sym(vax_day)) %in% c(1:31, 44) ~ 1,
      # Mother reported vaccination and 3+ doses
      (!!sym(vax_ever)) == 1 & (!!sym(vax_times)) >= 3 & (!!sym(vax_times)) < 8 ~ 1,
      TRUE ~ 0
    ))
  
  ch %>% select(cluster, householdID, weight, strata, value)
}
