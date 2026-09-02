###############################################################
### setup mics metadata
###############################################################

ref_tab_mics <- data.frame(matrix(nrow = 18))

ref_tab_mics$Chap_abbrev <- c("CH", "CH", "CH", "CM", "MH", "MH", "MH", "CH", "MH", "MH", "CH", "CH", "CH", "CH", "CH", "CH", "CH", "MH")

ref_tab_mics$ID <- c("CH_VACC_C_DP1", "CH_VACC_C_DP3", "CH_VACC_C_BCG", "CM_ECMR_C_NNF",
                     "RH_ANCN_W_N4P",
                     "RH_DELP_C_DHF",
                     "RH_PCCT_C_DY2",
                     "CH_DIAT_C_TRT",
                     "RH_ANCN_W_N01",
                     "RH_DELA_C_SKP",
                     "CH_VACC_C_BAS", "CH_VACC_C_PCV", "CH_VACC_C_OP3",
                     "CH_VACC_C_OP0", "CH_VACC_C_IPV", "CH_VACC_C_MSL", "CH_VACC_C_YFI", "RH_PCCT_C_DY2"
)

ref_tab_mics$ch <- c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
ref_tab_mics$wm <- c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
ref_tab_mics$bh <- c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
ref_tab_mics$notNGExclusive <- c(FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
ref_tab_mics$Description <- c("Children age 12-23 months who had received third dose of DTP1 vaccine",
                              "Children age 12-23 months who had received third dose of DTP3 vaccine",
                              "Children age 12-23 months who had recieved BCG containing vaccine",
                              "Probability of dying within the first month of life",
                              "Antenatal visits for pregnancy: 4+ visits",
                              "Place of delivery: Health facility",
                              "Newborn's first postnatal checkup in the first two days after birth",
                              "Children under 5 who sought treatment for diarrhea",
                              "Antenatal visits for pregnancy: 1",
                              "Assistance during delivery from a skilled provider",
                              "Children age 12-23 months who is fully immunized with basic antigens",
                              "Children age 12-23 months who had recieved third dose of Pneumococcal (Conjugate) vaccine",
                              "Children age 12-23 months who had recieved third dose of Oral Polio vaccine",
                              "Children age 12-23 months who had recieved Oral Polio vaccine",
                              "Children age 12-23 months who had recieved Interactive Polio vaccine",
                              "Children age 12-23 months who had recieved measles vaccine",
                              "Children age 12-23 months who had recieved yellow fever vaccine",
                              "Women whose most recent live-born child received a health check within 2 days of delivery")

ref_tab_mics$Full_definition <- c("Percentage of children age 12-23 months who had received DTP1 vaccine",
                                  "Percentage of children age 12-23 months who had received third dose of DTP vaccine",
                                  "Percentage of children 12-23 months who had received BCG vaccination",
                                  "Probability of dying in the first month of life in the five or ten years preceding the survey, per 1,000 live births. Estimates are given for ten year periods for all characteristics, but for five year periods only for the national total, by residence, and by sex.",
                                  "Percentage of women by number of antenatal care visits, 4 or more visits to any provider column",
                                  "Place of delivery: Health facility",
                                  "Percentage of last births in the two years preceding the survey who had their first postnatal checkup within the first two days after birth",
                                  "Children under 5 who sought treatment for diarrhea",
                                  "Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey who had 1 antenatal care visit",
                                  "Percentage of live births (or stillbirths) in the two (or three/five) years preceding the survey assisted by a skilled provider. Skilled provider includes doctor, nurse, midwife and auxiliary nurse or midwife.",
                                  "Percentage of children age 12-23 months who had received vaccines of BCG, third does of Polio and DTP, and Measles",
                                  "Percentage of children age 12-23 months who had received third dose of Pneumococcal (Conjugate) vaccine",
                                  "Percentage of children age 12-23 months who had received third dose of Oral Polio vaccine",
                                  "Percentage of children age 12-23 months who had received Oral Polio vaccine",
                                  "Percentage of children age 12-23 months who had received Interactive Polio vaccine",
                                  "Percentage of children age 12-23 months who had measles vaccine",
                                  "Percentage of children age 12-23 months who had yellow fever vaccine",
                                  "Percentage of women age 15--49 years whose most recent live-born child received a health check within 2 days of delivery (at home or facility)")

ref_tab_mics$Topic <- c("Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 05 - Survive",
                        "Chapter 06 - Thrive - Reproductive and maternal health",
                        "Chapter 06 - Thrive - Reproductive and maternal health",
                        "Chapter 06 - Thrive - Reproductive and maternal health",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 06 - Thrive - Reproductive and maternal health",
                        "Chapter 06 - Thrive - Reproductive and maternal health",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 07 - Thrive: Child health, nutrition and development",
                        "Chapter 06 - Thrive - Reproductive and maternal health")

ref_tab_mics$Title <- c("Child Health",
                        "Child Health",
                        "Child Health",
                        "Infant And Child Mortality",
                        "Maternal Health",
                        "Maternal Health",
                        "Maternal Health",
                        "Child Health",
                        "Maternal Health",
                        "Maternal Health",
                        "Child Health",
                        "Child Health",
                        "Child Health",
                        "Child Health",
                        "Child Health",
                        "Child Health",
                        "Child Health",
                        "Maternal Health")



ref_tab_mics <- ref_tab_mics[, c(-1)]


###############################################################
### extend MICS metadata with MICSprev country-specific indicators
###############################################################

## Fix a pre-existing bug: rows 7 and 18 share the ID "RH_PCCT_C_DY2".
## Row 7 is the newborn's checkup (PNCNB), row 18 the mother's (PNCMOM).
## Duplicate IDs break the ID-based lookups in mod_country_specify.R.
ref_tab_mics$ID[18] <- "RH_PCMN_W_DY2"

## New recode-requirement columns. The original table only knows bh/wm/ch;
## the new MICSprev indicators also draw on hh, hl, fs and fg recodes.
ref_tab_mics$hh <- FALSE
ref_tab_mics$hl <- FALSE
ref_tab_mics$fs <- FALSE
ref_tab_mics$fg <- FALSE

## Map app indicator IDs -> MICSprev indicator codes, and record which
## recode is the *primary* argument of the MICSprev process_*() function.
## Rows left with NA MICSprev_code have no processing function (preload-only).
ref_tab_mics$MICSprev_code  <- NA_character_
ref_tab_mics$primary_recode <- NA_character_

.map_mics <- function(tab, id, code, primary, recodes = primary) {
  i <- which(tab$ID == id)
  tab$MICSprev_code[i]  <- code
  tab$primary_recode[i] <- primary
  for (rc in recodes) tab[[rc]][i] <- TRUE
  tab
}

# Existing rows now covered by MICSprev functions:
ref_tab_mics <- .map_mics(ref_tab_mics, "RH_ANCN_W_N4P", "ANC",    "wm")
ref_tab_mics <- .map_mics(ref_tab_mics, "CM_ECMR_C_NNF", "NMR",    "bh")
ref_tab_mics <- .map_mics(ref_tab_mics, "CH_VACC_C_DP3", "DTP3",   "ch")
ref_tab_mics <- .map_mics(ref_tab_mics, "CH_VACC_C_DP1", "PENTA1", "ch")  # DTP1 == Penta1 in Nigeria MICS6
ref_tab_mics <- .map_mics(ref_tab_mics, "RH_ANCN_W_N01", "ANC1",   "wm")
ref_tab_mics <- .map_mics(ref_tab_mics, "RH_DELA_C_SKP", "SBA",    "wm")
ref_tab_mics <- .map_mics(ref_tab_mics, "RH_PCCT_C_DY2", "PNCNB",  "wm")
ref_tab_mics <- .map_mics(ref_tab_mics, "RH_PCMN_W_DY2", "PNCMOM", "wm")

## New Nigeria-only indicators from MICSprev. The app ID is simply the
## MICSprev code (unique, used only for lookups + preload file names).
## recodes[1] is the primary recode; any others are the extra recode files
## the MICSprev function needs (passed by name, e.g. bh=, wm=, hl=, ch=).
.mk_ind <- function(code, recodes, Topic, Title, Description,
                    Full_definition = Description) {
  data.frame(
    ID = code,
    ch = "ch" %in% recodes, wm = "wm" %in% recodes, bh = "bh" %in% recodes,
    hh = "hh" %in% recodes, hl = "hl" %in% recodes, fs = "fs" %in% recodes,
    fg = "fg" %in% recodes,
    notNGExclusive  = FALSE,          # Nigeria-only: hidden for other countries
    Description     = Description,
    Full_definition = Full_definition,
    Topic = Topic, Title = Title,
    MICSprev_code  = code,
    primary_recode = recodes[1],
    stringsAsFactors = FALSE
  )
}

.t_cp   <- "Chapter 08 - Protected: Child protection"
.t_ed   <- "Chapter 09 - Learn: Education and learning"
.t_nt   <- "Chapter 07 - Thrive: Child health, nutrition and development"
.t_wash <- "Chapter 10 - Live in a safe and clean environment: WASH"
.t_sp   <- "Chapter 11 - Equitable chance in life: Social policy"

ref_tab_mics_new <- rbind(
  # --- Child Protection ---
  .mk_ind("BIRTHREG",      "ch",              .t_cp, "Child Protection",
          "Children under 5 whose birth is registered",
          "Percentage of children under age 5 whose birth is registered with civil authorities"),
  .mk_ind("CHILDMARRIAGE", "wm",              .t_cp, "Child Protection",
          "Women age 20-24 first married or in union before age 18",
          "Percentage of women age 20-24 years who were first married or in union before age 18"),
  .mk_ind("CHILDLABOUR",   "fs",              .t_cp, "Child Protection",
          "Children age 5-17 engaged in child labour",
          "Percentage of children age 5-17 years engaged in child labour (economic activities and/or household chores above age-specific thresholds)"),
  .mk_ind("CHILDLABOURHAZ", "fs",             .t_cp, "Child Protection",
          "Children age 5-17 working under hazardous conditions",
          "Percentage of children age 5-17 years working under hazardous conditions"),
  .mk_ind("FGMDAUGHTER",   c("fg","bh","wm"), .t_cp, "Child Protection",
          "Daughters age 0-14 who have undergone FGM",
          "Percentage of daughters age 0-14 years who have undergone any form of female genital mutilation, as reported by mothers age 15-49"),

  # --- Education ---
  .mk_ind("OOSPRIMARY",   "hl", .t_ed, "Education",
          "Children of primary school age out of school",
          "Percentage of children of primary school age currently not attending early childhood education, primary or secondary school (out-of-school rate)"),
  .mk_ind("OOSSECONDARY", "hl", .t_ed, "Education",
          "Adolescents of secondary school age out of school",
          "Percentage of adolescents of secondary school age currently not attending primary or secondary school (out-of-school rate)"),
  .mk_ind("READING",      "fs", .t_ed, "Education",
          "Children age 7-14 with foundational reading skills",
          "Percentage of children age 7-14 years who demonstrate foundational reading skills"),
  .mk_ind("MATH",         "fs", .t_ed, "Education",
          "Children age 7-14 with foundational numeracy skills",
          "Percentage of children age 7-14 years who demonstrate foundational numeracy skills"),
  .mk_ind("NETINTAKE",    "hl", .t_ed, "Education",
          "Children of primary entry age entering grade 1 (net intake rate)",
          "Percentage of children of primary school entry age entering the first grade of primary school (net intake rate)"),
  .mk_ind("PRIMARYCOMPL", "hl", .t_ed, "Education",
          "Primary school completion rate",
          "Percentage of children age 3-5 years above primary graduation age who have completed primary school"),
  .mk_ind("LOWSECCOMPL",  "hl", .t_ed, "Education",
          "Lower secondary school completion rate",
          "Percentage of adolescents age 3-5 years above lower secondary graduation age who have completed lower secondary school"),
  .mk_ind("UPSECCOMPL",   "hl", .t_ed, "Education",
          "Upper secondary school completion rate",
          "Percentage of young people age 3-5 years above upper secondary graduation age who have completed upper secondary school"),

  # --- Nutrition ---
  .mk_ind("EBF",           "ch", .t_nt, "Nutrition",
          "Infants under 6 months exclusively breastfed",
          "Percentage of infants under 6 months of age who are exclusively breastfed"),
  .mk_ind("VITA",          "ch", .t_nt, "Nutrition",
          "Children age 6-59 months who received vitamin A supplementation",
          "Percentage of children age 6-59 months who received a vitamin A dose in the 6 months preceding the survey"),
  .mk_ind("APPROPBF",      "ch", .t_nt, "Nutrition",
          "Children age 0-23 months appropriately breastfed",
          "Percentage of children age 0-23 months appropriately breastfed for their age"),
  .mk_ind("MINACCEPTDIET", "ch", .t_nt, "Nutrition",
          "Children age 6-23 months with minimum acceptable diet",
          "Percentage of children age 6-23 months who received a minimum acceptable diet"),
  .mk_ind("FOODINSEC",     "hh", .t_nt, "Nutrition",
          "Household population with moderate or severe food insecurity",
          "Percentage of household population living in households with moderate or severe food insecurity"),

  # --- WASH ---
  .mk_ind("BASICWATER", "hh", .t_wash, "Water and Sanitation",
          "Population using basic drinking water services",
          "Percentage of household population using improved drinking water sources within 30 minutes round trip (basic service)"),
  .mk_ind("BASICSAN",   "hh", .t_wash, "Water and Sanitation",
          "Population using basic sanitation services",
          "Percentage of household population using improved sanitation facilities not shared with other households (basic service)"),
  .mk_ind("OPENDEF",    "hh", .t_wash, "Water and Sanitation",
          "Population practising open defecation",
          "Percentage of household population practising open defecation"),
  .mk_ind("WATERSUFF",  "hh", .t_wash, "Water and Sanitation",
          "Population with sufficient drinking water when needed",
          "Percentage of household population able to obtain drinking water in sufficient quantities when needed"),
  .mk_ind("MENSTRUAL",  "wm", .t_wash, "Water and Sanitation",
          "Women with appropriate menstrual hygiene",
          "Percentage of women age 15-49 using appropriate menstrual hygiene materials with a private place to wash and change"),

  # --- Social Policy ---
  .mk_ind("HANDWASH",    "hh",           .t_sp, "Social Policy",
          "Population with handwashing facility with water and soap",
          "Percentage of household population with a handwashing facility on premises where water and soap or detergent are present"),
  .mk_ind("FUNCDIFF517", "fs",           .t_sp, "Social Policy",
          "Children age 5-17 with functional difficulty",
          "Percentage of children age 5-17 years with functional difficulty in at least one domain"),
  .mk_ind("FUNCDIFF217", c("fs","ch"),   .t_sp, "Social Policy",
          "Children age 2-17 with functional difficulty",
          "Percentage of children age 2-17 years with functional difficulty in at least one domain"),
  .mk_ind("HEALTHINS",   "ch",           .t_sp, "Social Policy",
          "Children under 5 covered by health insurance",
          "Percentage of children under age 5 covered by health insurance"),
  .mk_ind("SOCTRANSFER", c("hh","hl"),   .t_sp, "Social Policy",
          "Household members receiving social transfers",
          "Percentage of household members living in households that received any type of social transfer or benefit"),
  .mk_ind("BANKACCT",    "wm",           .t_sp, "Social Policy",
          "Women age 15-49 with a bank account",
          "Percentage of women age 15-49 years who have an account at a bank or other financial institution"),
  .mk_ind("BORROWED",    "wm",           .t_sp, "Social Policy",
          "Women who borrowed money in the past year",
          "Percentage of women age 15-49 years who borrowed money in the 12 months preceding the survey")
)

## Combine (align column order first) and clean up helpers.
## Note: Chap_abbrev is intentionally dropped here; it is not used anywhere
## outside this file.
ref_tab_mics <- rbind(ref_tab_mics[, names(ref_tab_mics_new)], ref_tab_mics_new)
rm(ref_tab_mics_new, .mk_ind, .map_mics, .t_cp, .t_ed, .t_nt, .t_wash, .t_sp)


###############################################################
### load DHS meta data
###############################################################

DHS_api_timeout = F

if(FALSE){
  tryCatch({
    R.utils::withTimeout({
      
      DHS.country.meta <- rdhs::dhs_countries()
      DHS.survey.meta <- rdhs::dhs_surveys()
      DHS.dataset.meta <- rdhs::dhs_datasets()
      #Sys.sleep(20)  # Simulating a delay
      
      message('DHS API is working fine. Use most up-to-date info.')
    },
    timeout = 3)  # Timeout in seconds
  },
  TimeoutException = function(ex) {
    
    message("DHS API call timed out, using backup data.")
    DHS_api_timeout = T
    
    ### use backup
    pkg_files <- list.files('data')
    filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'
    
    load(file=paste0('data/',filtered_files))
    
    
    DHS.country.meta <- DHS.country.meta.preload
    DHS.survey.meta <- DHS.survey.meta.preload
    DHS.dataset.meta <- DHS.dataset.meta.preload
    
  },
  error = function(e) {
    
    message("Error loading 'rdhs' library: ", e$message)
    DHS_api_timeout = T
    
    ### use backup
    pkg_files <- list.files('data')
    filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'
    
    load(file=paste0('data/',filtered_files))
    
    DHS.country.meta <- DHS.country.meta.preload
    DHS.survey.meta <- DHS.survey.meta.preload
    DHS.dataset.meta <- DHS.dataset.meta.preload
    
  })
}
### use pre-stored meta data
pkg_files <- list.files('data')
filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'

load(file=paste0('data/',filtered_files))
#
# DHS.country.meta <- DHS.country.meta.preload
# DHS.country.meta[DHS.country.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
# DHS.country.meta[DHS.country.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'
#
#
# DHS.survey.meta <- DHS.survey.meta.preload
# DHS.survey.meta[DHS.survey.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
# DHS.survey.meta[DHS.survey.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'
#
# DHS.dataset.meta <- DHS.dataset.meta.preload
# DHS.dataset.meta[DHS.dataset.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
# DHS.dataset.meta[DHS.dataset.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'

#message(list.files('data'))
#message(list.files('data/GADM_shp'))
#load('data/GADM_shp/zmb_example_GADM.rda')


###############################################################
### prepare preloaded DHS meta data
###############################################################

### only executed when preparing data

if(FALSE){
  
  ### load dhs meta data
  DHS.country.listing <- rdhs::dhs_countries()
  DHS.survey.listing <- rdhs::dhs_surveys()
  DHS.dataset.listing <- rdhs::dhs_datasets()
  
  ##################################################
  ### Determine the surveys eligible for analysis
  ##################################################
  ### criteria: conducted later than 2000; is DHS standard survey (not MIS etc.); contains GPS data
  
  ### subset to >2000 and standard DHS
  DHS.survey.listing <- rdhs::dhs_surveys()
  DHS.survey.listing <- DHS.survey.listing[as.numeric(DHS.survey.listing$SurveyYear)>2000 & DHS.survey.listing$SurveyType== 'DHS',]
  
  ### check GPS data set availability
  check_GPS_avail <- function(surveyID){
    svy.tmp.list <- DHS.dataset.listing[DHS.dataset.listing$SurveyId==surveyID,]
    return( ('Geographic Data' %in% unique(svy.tmp.list$FileType)))
  }
  
  ### subset to surveys with GPS
  DHS.survey.listing$GPS_avail <-  sapply(DHS.survey.listing$SurveyId,check_GPS_avail)
  DHS.survey.listing <- DHS.survey.listing[DHS.survey.listing$GPS_avail==T,]
  
  DHS.survey.meta.preload <- DHS.survey.listing
  
  ##################################################
  ### Determine data sets according to surveys
  ##################################################
  
  DHS.dataset.meta.preload <- DHS.dataset.listing[DHS.dataset.listing$SurveyId %in%
                                                    unique(DHS.survey.meta.preload$SurveyId),]
  DHS.country.meta.preload <- DHS.country.listing[DHS.country.listing$CountryName %in%
                                                    unique(DHS.survey.meta.preload$CountryName),]
  
  #DHS.country.meta.preload <- rdhs::dhs_countries()
  #DHS.survey.meta.preload <- rdhs::dhs_surveys()
  #DHS.dataset.meta.preload <- rdhs::dhs_datasets()
  
  
  ### post process
  DHS.country.meta <- DHS.country.meta.preload
  DHS.country.meta[DHS.country.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
  DHS.country.meta[DHS.country.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'
  
  
  DHS.survey.meta <- DHS.survey.meta.preload
  DHS.survey.meta[DHS.survey.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
  DHS.survey.meta[DHS.survey.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'
  
  DHS.dataset.meta <- DHS.dataset.meta.preload
  DHS.dataset.meta[DHS.dataset.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
  DHS.dataset.meta[DHS.dataset.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'
  
  formatted_date <- format(Sys.Date(), "%m%d%Y")
  
  save(DHS.country.meta,DHS.survey.meta,DHS.dataset.meta,file=paste0('data/DHS_meta_preload_',formatted_date,'.rda'))
  
  
}

###############################################################
### prepare preloaded GADM shapefile
###############################################################


if(FALSE){
  
  all_country_names <- sort(DHS.country.meta[['CountryName']])
  
  for(country in all_country_names){
    #country = 'Ethiopia'
    #country='Armenia'
    message(paste0('working on ',country))
    
    country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==country,'ISO3_CountryCode']
    
    #country_iso3='ARM'
    dir.create(file.path('data/GADM_shp', country_iso3), showWarnings = FALSE)
    
    if(length(list.files(paste0('data/GADM_shp/', country_iso3)))==2){
      message(paste0(country,' already downloaded, skip.'))
      next}
    
    ### store fine level GADM for analysis (neighborhood structure etc.)
    one_country_GADM <- get_country_GADM(country,resolution=1)
    
    adm_region_nums <- check_gadm_levels(one_country_GADM)
    
    ### only keep admin levels that have less than 1000 regions
    model_levels <- names(adm_region_nums[,adm_region_nums<1000])
    
    country.GADM.list.fine <- one_country_GADM[model_levels]
    
    saveRDS(country.GADM.list.fine,file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))
    
    ### store smoothed GADM for display
    one_country_GADM <- get_country_GADM(country,resolution=2)
    
    adm_region_nums <- check_gadm_levels(one_country_GADM)
    model_levels <- names(adm_region_nums[,adm_region_nums<1000])
    
    country.GADM.list.smoothed <- one_country_GADM[model_levels]
    
    saveRDS(country.GADM.list.smoothed,file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))
  }
  
}





###############################################################
### process WHO shapefile
###############################################################

### WHO requested countries
WHO.app.countries <- c('Benin', 'Burkina Faso',
                       "Democratic Republic of the Congo",
                       'Rwanda', 'Senegal', 'Sierra Leone',
                       'United Republic of Tanzania','Zambia')

WHO.app.countries.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName %in% WHO.app.countries,]$ISO3_CountryCode

MICS.app.countries <- c("Nigeria", "Honduras")
WHO.app.countries.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName %in% MICS.app.countries,]$ISO3_CountryCode


### save linkage file for WHO boundaries across admin levels
if(FALSE){
  #adm2.link.all <- read.csv("/WHO_admin_names.csv", fileEncoding = "Windows-1252")
  adm2.link.all <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE %in% WHO.app.countries.ISO3,]
  save(adm2.link.all,file='WHO_shp_linkage.rda')
  
}


### function to read WHO shapefiles and constrain to WHO countries
read_WHO_shp <- function(file_path,adm_level=0){
  
  temp <- tempfile()
  unzip(file_path, exdir = temp)
  
  file_name <- paste0('GLOBAL_ADM',adm_level,'.shp')
  allPaths <- list.files(temp, #pattern =  '(GLOBAL_ADM0.shp|GLOBAL_ADM1.shp|GLOBAL_ADM2.shp)',
                         pattern = file_name,
                         full.names = TRUE, recursive = T, include.dirs = TRUE,
                         ignore.case = TRUE)
  
  if (length(allPaths) > 0) {
    shp_Paths <- allPaths[which.min(nchar(allPaths))]
  } else {
    shp_Paths <- NULL  # If no matching directories are found
    return(NULL)
  }
  
  WHO.shp = NULL
  tryCatch({
    WHO.shp <- suppressWarnings(sf::st_read(shp_Paths))
    WHO.shp <- WHO.shp[WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]
    
    WHO.shp <- sf::st_as_sf(WHO.shp)
    WHO.shp <- sf::st_transform(WHO.shp,4326)
  },error = function(e) {
    message(e$message)
    return(NULL)
  })
  
  
  
  return(WHO.shp)
  
}

### save national (only national boudaries can be stored within this version of the app)

if(FALSE){
  #natl.WHO.shp <- read_WHO_shp(adm_level=0,
  #  file_path ='C:/Users/wu-th/Downloads/Detailed_Boundary_ADM0_565521753006392799.zip')
  
  #save(natl.WHO.shp,file='natl_WHO_shp.rda')
  
  #adm1.WHO.shp <- read_WHO_shp(adm_level=1,
  #  file_path ='C:/Users/wu-th/Downloads/Detailed_Boundary_ADM1_-6406508520251409053.zip')
  
  #save(adm1.WHO.shp,file='adm1_WHO_shp.rda')
  
  #adm2.WHO.shp <- read_WHO_shp(adm_level=2,
  #  file_path = 'C:/Users/wu-th/Downloads/Detailed_Boundary_ADM2_1272841991903683246.zip')
  #save(adm2.WHO.shp,file='adm2_WHO_shp.rda')
  
  
}


### Function to produce country specific shapefile
prepare_WHO_country_shp <- function(country.ISO3,
                                    natl.WHO.shp,
                                    adm1.WHO.shp,
                                    adm2.WHO.shp){
  
  return.shp.list <- list()
  
  ### initialize
  #country.ISO3 <-  DHS.country.meta[DHS.country.meta$CountryName==country.fullname,'ISO3_CountryCode']
  country.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE==country.ISO3,]
  
  
  ### national shapefile
  natl.GUID <- unique(country.adm2.link$GUID.LEVEL.0)
  natl.GUID <-  gsub("^\\{|\\}$", "", natl.GUID)
  country.natl.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(natl.WHO.shp$GUID) %in% natl.GUID,]
  
  if(dim(country.natl.shp)[1]>0){
    
    adm0.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','GUID.LEVEL.0')])
    country.natl.shp$NAME_0 <- adm0.name.ref.list$ADM0_VIZ_NAME[1]
    
    return.shp.list[['National']]<-country.natl.shp
    
  }else{return(NULL)}
  
  
  
  ### Admin-1 shapefile
  adm1.GUID <- unique(country.adm2.link$GUID.LEVEL.1..)
  adm1.GUID <-  gsub("^\\{|\\}$", "", adm1.GUID)
  
  country.adm1.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(adm1.WHO.shp$GUID) %in% adm1.GUID,]
  
  adm1.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME','GUID.LEVEL.1..')])
  adm1.name.ref.list$GUID.LEVEL.1.. <-  gsub("^\\{|\\}$", "", adm1.name.ref.list$GUID.LEVEL.1..)
  
  adm1.match.order <- match(toupper(country.adm1.shp$GUID),adm1.name.ref.list$GUID.LEVEL.1..)
  
  
  if(dim(country.adm1.shp)[1]>0){
    
    country.adm1.shp$NAME_0 <- adm1.name.ref.list$ADM0_VIZ_NAME[adm1.match.order]
    country.adm1.shp$NAME_1 <- adm1.name.ref.list$ADM1_VIZ_NAME[adm1.match.order]
    
    ### make valid for shapefile
    if(!all(sf::st_is_valid(country.adm1.shp))){
      
      wrong_row <- which(sf::st_is_valid(country.adm1.shp)==F)
      country.adm1.shp[wrong_row,] <- sf::st_make_valid(country.adm1.shp[wrong_row,])
      country.adm1.shp <- sf::st_as_sf(country.adm1.shp)
      country.adm1.shp <- sf::st_transform(country.adm1.shp,4326)
      
      message('Fixed invalid shapefile.')
      
    }
    
    return.shp.list[['Admin-1']]<-country.adm1.shp
    
  }else{return(return.shp.list)}
  
  
  ### Admin-2 shapefile
  adm2.GUID <- unique(country.adm2.link$GLOBAL.UNIQUE.IDENTIFIER..)
  adm2.GUID <-  gsub("^\\{|\\}$", "", adm2.GUID)
  
  country.adm2.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(adm2.WHO.shp$GUID) %in% adm2.GUID,]
  
  adm2.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME',
                                                    'ADM2_VIZ_NAME','GLOBAL.UNIQUE.IDENTIFIER..')])
  
  adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER.. <-  gsub("^\\{|\\}$", "", adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)
  
  adm2.match.order <- match(toupper(country.adm2.shp$GUID),adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)
  
  
  if(dim(country.adm2.shp)[1]>0){
    
    country.adm2.shp$NAME_0 <- adm2.name.ref.list$ADM0_VIZ_NAME[adm2.match.order]
    country.adm2.shp$NAME_1 <- adm2.name.ref.list$ADM1_VIZ_NAME[adm2.match.order]
    country.adm2.shp$NAME_2 <- adm2.name.ref.list$ADM2_VIZ_NAME[adm2.match.order]
    
    ### make valid for shapefile
    if(!all(sf::st_is_valid(country.adm2.shp))){
      
      wrong_row <- which(sf::st_is_valid(country.adm2.shp)==F)
      country.adm2.shp[wrong_row,] <- sf::st_make_valid(country.adm2.shp[wrong_row,])
      country.adm2.shp <- sf::st_as_sf(country.adm2.shp)
      country.adm2.shp <- sf::st_transform(country.adm2.shp,4326)
      
      message('Fixed invalid shapefile.')
      
    }
    
    ### fix wrong shapefile for congo, overlapping
    if(country.ISO3 =='COD'){
      
      country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry <-
        sf::st_difference(country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry ,
                          country.adm2.shp[country.adm2.shp$ADM2_NAME=='BILI2',]$geometry)
      
      message('Fixed overlapping shapefile.')
      
    }
    
    return.shp.list[['Admin-2']]<-country.adm2.shp
    
    
  }else{return(return.shp.list)}
  
  
  
  
  ### return shapefile list
  return(return.shp.list)
  
}


if(FALSE){
  tmp.cod.shp.list <- prepare_WHO_country_shp(country.fullname='Democratic Republic of the Congo',
                                              natl.WHO.shp=natl.WHO.shp,
                                              adm1.WHO.shp=adm1.WHO.shp,
                                              adm2.WHO.shp=adm2.WHO.shp)
  
}



###

# download link: https://gis-who.hub.arcgis.com/pages/detailedboundary





###############################################################
### prepare the WHO shapefiles for online version
###############################################################

if(FALSE){
  
  
  ### load linkage meta data
  #adm2.link.all <- read.csv("C:/Users/wu-th/Dropbox/YunhanJon/saeforhealth/WHO_admin_names.csv", fileEncoding = "Windows-1252")
  adm2.link.all <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE %in% WHO.app.countries.ISO3,]
  
  
  #######################
  ### process national
  #######################
  
  #natl.WHO.shp.path <- 'E:/rshiny_history/SurveyPrevRShiny_0511/raw_data/Detailed_Boundary_ADM0/GLOBAL_ADM0.shp'
  natl.WHO.shp <- suppressWarnings(sf::st_read(natl.WHO.shp.path))
  natl.WHO.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]
  
  natl.WHO.shp <- sf::st_as_sf(natl.WHO.shp)
  natl.WHO.shp <- sf::st_transform(natl.WHO.shp,4326)
  
  #######################
  ### process Admin-1
  #######################
  
  #adm1.WHO.shp.path <- 'E:/rshiny_history/SurveyPrevRShiny_0511/raw_data/Detailed_Boundary_ADM1/GLOBAL_ADM1.shp'
  adm1.WHO.shp <- suppressWarnings(sf::st_read(adm1.WHO.shp.path))
  adm1.WHO.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]
  
  adm1.WHO.shp <- sf::st_as_sf(adm1.WHO.shp)
  adm1.WHO.shp <- sf::st_transform(adm1.WHO.shp,4326)
  
  #######################
  ### process Admin-2
  #######################
  
  #adm2.WHO.shp.path <- 'E:/rshiny_history/SurveyPrevRShiny_0511/raw_data/Detailed_Boundary_ADM2/GLOBAL_ADM2.shp'
  adm2.WHO.shp <- suppressWarnings(sf::st_read(adm2.WHO.shp.path))
  adm2.WHO.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]
  
  adm2.WHO.shp <- sf::st_as_sf(adm2.WHO.shp)
  adm2.WHO.shp <- sf::st_transform(adm2.WHO.shp,4326)
  
  #tmp.adm2.ctry <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE=='BFA',]
  #tmp.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE=='BFA',]
  
  
  country.fullname <- 'Burkina Faso'
  
  prepare_WHO_country_shp <- function(country.fullname){
    
    return.shp.list <- list()
    
    ### initialize
    country.ISO3 <-  DHS.country.meta[DHS.country.meta$CountryName==country.fullname,'ISO3_CountryCode']
    country.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE==country.ISO3,]
    
    
    ### national shapefile
    natl.GUID <- unique(country.adm2.link$GUID.LEVEL.0)
    natl.GUID <-  gsub("^\\{|\\}$", "", natl.GUID)
    country.natl.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(natl.WHO.shp$GUID) %in% natl.GUID,]
    
    if(dim(country.natl.shp)[1]>0){
      
      adm0.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','GUID.LEVEL.0')])
      country.natl.shp$NAME_0 <- adm0.name.ref.list$ADM0_VIZ_NAME[1]
      
      return.shp.list[['National']]<-country.natl.shp
      
    }else{return(NULL)}
    
    
    
    ### Admin-1 shapefile
    adm1.GUID <- unique(country.adm2.link$GUID.LEVEL.1..)
    adm1.GUID <-  gsub("^\\{|\\}$", "", adm1.GUID)
    
    country.adm1.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(adm1.WHO.shp$GUID) %in% adm1.GUID,]
    
    adm1.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME','GUID.LEVEL.1..')])
    adm1.name.ref.list$GUID.LEVEL.1.. <-  gsub("^\\{|\\}$", "", adm1.name.ref.list$GUID.LEVEL.1..)
    
    adm1.match.order <- match(toupper(country.adm1.shp$GUID),adm1.name.ref.list$GUID.LEVEL.1..)
    
    
    if(dim(country.adm1.shp)[1]>0){
      
      country.adm1.shp$NAME_0 <- adm1.name.ref.list$ADM0_VIZ_NAME[adm1.match.order]
      country.adm1.shp$NAME_1 <- adm1.name.ref.list$ADM1_VIZ_NAME[adm1.match.order]
      
      ### make valid for shapefile
      if(!all(sf::st_is_valid(country.adm1.shp))){
        
        wrong_row <- which(sf::st_is_valid(country.adm1.shp)==F)
        country.adm1.shp[wrong_row,] <- sf::st_make_valid(country.adm1.shp[wrong_row,])
        country.adm1.shp <- sf::st_as_sf(tmp.adm)
        country.adm1.shp <- sf::st_transform(country.adm1.shp,4326)
        
        message('Fixed invalid shapefile.')
        
      }
      
      return.shp.list[['Admin-1']]<-country.adm1.shp
      
    }else{return(return.shp.list)}
    
    
    ### Admin-2 shapefile
    adm2.GUID <- unique(country.adm2.link$GLOBAL.UNIQUE.IDENTIFIER..)
    adm2.GUID <-  gsub("^\\{|\\}$", "", adm2.GUID)
    
    country.adm2.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(adm2.WHO.shp$GUID) %in% adm2.GUID,]
    
    adm2.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME',
                                                      'ADM2_VIZ_NAME','GLOBAL.UNIQUE.IDENTIFIER..')])
    
    adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER.. <-  gsub("^\\{|\\}$", "", adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)
    
    adm2.match.order <- match(toupper(country.adm2.shp$GUID),adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)
    
    
    if(dim(country.adm2.shp)[1]>0){
      
      country.adm2.shp$NAME_0 <- adm2.name.ref.list$ADM0_VIZ_NAME[adm2.match.order]
      country.adm2.shp$NAME_1 <- adm2.name.ref.list$ADM1_VIZ_NAME[adm2.match.order]
      country.adm2.shp$NAME_2 <- adm2.name.ref.list$ADM2_VIZ_NAME[adm2.match.order]
      
      ### make valid for shapefile
      if(!all(sf::st_is_valid(country.adm2.shp))){
        
        wrong_row <- which(sf::st_is_valid(country.adm2.shp)==F)
        country.adm2.shp[wrong_row,] <- sf::st_make_valid(country.adm2.shp[wrong_row,])
        country.adm2.shp <- sf::st_as_sf(tmp.adm)
        country.adm2.shp <- sf::st_transform(country.adm2.shp,4326)
        
        message('Fixed invalid shapefile.')
        
      }
      
      ### fix wrong shapefile for congo, overlapping
      if(country.ISO3 =='COD'){
        
        country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry <-
          sf::st_difference(country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry ,
                            country.adm2.shp[country.adm2.shp$ADM2_NAME=='BILI2',]$geometry)
      }
      
      return.shp.list[['Admin-2']]<-country.adm2.shp
      
      
    }else{return(return.shp.list)}
    
    
    
    
    ### return shapefile list
    return(return.shp.list)
    
  }
  
  tmp.rwa.shp.list <- prepare_WHO_country_shp('Rwanda')
  
  ##################################
  ### save WHO country shapefiles
  ##################################
  
  
  for(WHO.country in WHO.app.countries){
    
    message(WHO.country)
    
    WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
    message(WHO.country.ISO3)
    
    ### make directory
    dir.create(file.path('data/WHO_shp', WHO.country.ISO3), showWarnings = FALSE)
    
    if(length(list.files(paste0('data/WHO_shp/', WHO.country.ISO3)))>0){
      message(paste0(country,' already downloaded, skip.'))
      next
    }
    
    ### prepare shapefile
    country.shp.list <- prepare_WHO_country_shp(country.fullname=WHO.country)
    
    
    ### save shapefile
    saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))
    
    
    
  }
  
  
  ### make valid
  for(WHO.country in WHO.app.countries){
    
    # WHO.country = 'Democratic Republic of the Congo'
    message(WHO.country)
    
    WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
    message(WHO.country.ISO3)
    
    country.shp.list<- readRDS(file=paste0('data/WHO_shp/',WHO.country.ISO3,
                                           '/',WHO.country.ISO3,'_shp.rds'))
    
    for (tmp.name in names(country.shp.list)){
      
      tmp.adm <- country.shp.list[[tmp.name]]
      
      if(!all(sf::st_is_valid(tmp.adm))){
        
        tmp.adm <- sf::st_make_valid(tmp.adm)
        tmp.adm <- sf::st_as_sf(tmp.adm)
        tmp.adm <- sf::st_transform(tmp.adm,4326)
        
        country.shp.list[[tmp.name]] <- tmp.adm
        
        message('Country: ',WHO.country, ' ',tmp.name,' shapefile not valid.')
        
        saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))
        
        message('Fixed.')
        
      }
      
    }
    
  }
  
  
  ### fix wrong shapefile for congo
  WHO.country <- 'Democratic Republic of the Congo'
  WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
  message(WHO.country.ISO3)
  
  country.shp.list<- readRDS(file=paste0('data/WHO_shp/',WHO.country.ISO3,
                                         '/',WHO.country.ISO3,'_shp.rds'))
  
  poly.adm2 <- country.shp.list[['Admin-2']]
  
  poly.adm2[poly.adm2$ADM2_NAME=='BOSOBOLO',]$geometry <-
    sf::st_difference(poly.adm2[poly.adm2$ADM2_NAME=='BOSOBOLO',]$geometry ,
                      poly.adm2[poly.adm2$ADM2_NAME=='BILI2',]$geometry)
  
  
  country.shp.list[['Admin-2']] <- poly.adm2
  saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))
  
  
}




###############################################################
###  create DHS chapter Info
###############################################################

if(FALSE){
  
  dhs_chapters <- data.frame(
    Chapter = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
    Title = c("Housing Characteristics And Household Population",
              "Characteristics Of Respondents",
              "Marriage And Sexual Activity",
              "Fertility",
              "Fertility Preferences",
              "Family Planning",
              "Infant And Child Mortality",
              "Maternal Health",
              "Child Health",
              "Nutrition Of Children And Adults",
              "Malaria",
              "HIV/AIDS-Related Knowledge, Attitudes, And Behaviour",
              "HIV Prevalence",
              "Women's Empowerment",
              "Adult And Maternal Mortality",
              "Domestic Violence",
              "Female Genital Cutting",
              "Fistula"),
    Acronym = c("PH", "RC", "MS", "FE", "FF", "FP", "CM", "RH", "CH", "NT", "ML", "HK", "HV", "WE", "AM", "DV", "FG", "FS")
  )
  
}


###############################################################
###  create ref tab for newly added indicators
###############################################################


if(FALSE){
  
  library(surveyPrevGithub)
  library(dplyr)
  data(match_all_result)
  #match_all_result <- surveyPrevGithub::match_all_result
  
  #ID <- match_all_result$indicator_ID_DHS
  #Description <- match_all_result$DHS_label
  #Full_definition <- match_all_result$DHS_definition
  #Chap_abbrev <-  toupper(substr(match_all_result$indicator_ID_Github_raw, start = 1, stop = 2))
  match_all_result <- match_all_result %>% mutate(ID_first_two_letters = substr(indicator_ID_DHS, start = 1, stop = 2))
  
  match_all_result <- match_all_result %>%
    mutate(indicator_chapter = case_when( !is.na(indicator_chapter)~indicator_chapter,
                                          indicator_ID_DHS == 'HA_KAID_W_HRD' ~ 'Chap13_HK',
                                          indicator_ID_DHS == 'HA_STIS_W_STI' ~ 'Chap13_HK',
                                          indicator_ID_DHS == 'AN_NUTS_W_OVW' ~ 'Chap11_NT',
                                          ID_first_two_letters=='HC'~ 'Chap02_PH',
                                          ID_first_two_letters=='DV'~ 'Chap17_DV',
                                          ID_first_two_letters=='MA'~ 'Chap04_MS',
                                          TRUE~indicator_chapter)
    )
  
  Chap_abbrev <- sapply(match_all_result$indicator_chapter, function(x) strsplit(x, "_")[[1]][2], USE.NAMES=FALSE)
  
  
  
  
  ID <- match_all_result$indicator_ID_DHS
  
  ref_tab_new <- data.frame(ID=ID,
                            #Description= Description,
                            #Full_definition=Full_definition,
                            Chap_abbrev= Chap_abbrev,
                            recode = match_all_result$batch_recode_group
  )
  
  #ref_tab_new$Chap_abbrev <- ifelse(is.na(ref_tab_new$Chap_abbrev), substr(ref_tab_new$ID, 1, 2), ref_tab_new$Chap_abbrev)
  
  
  ref_tab_new <- merge(ref_tab_new,dhs_chapters,by.x='Chap_abbrev',by.y='Acronym',all.x=T)
  ref_tab_new$Topic <- ref_tab_new$Title
  
  ref_tab_new$IR <- grepl('IR', ref_tab_new$recode)
  ref_tab_new$PR <- grepl('PR', ref_tab_new$recode)
  ref_tab_new$KR <- grepl('KR', ref_tab_new$recode)
  ref_tab_new$BR <- grepl('BR', ref_tab_new$recode)
  ref_tab_new$HR <- grepl('HR', ref_tab_new$recode)
  ref_tab_new$MR <- grepl('MR', ref_tab_new$recode)
  ref_tab_new$AR <- grepl('AR', ref_tab_new$recode)
  ref_tab_new$CR <- grepl('CR', ref_tab_new$recode)
  
  
  tmp_dict <- SurveyPrevRShiny::DHS_ind_dictionary[,c('DHS Standard Indicator ID','Full Definition','Label')]
  tmp_dict$ID <- tmp_dict$`DHS Standard Indicator ID`
  tmp_dict$Full_definition <- tmp_dict$`Full Definition`
  tmp_dict$Description <- tmp_dict$Label
  
  tmp_dict <- tmp_dict[!duplicated(tmp_dict[,c('ID')]),]
  
  
  ref_tab_new <- merge(ref_tab_new,tmp_dict[,c('ID','Full_definition','Description')],by='ID',all.x=T)
  
  ref_tab_new <- ref_tab_new[,c('ID', "Description","Full_definition","Topic" , "Chap_abbrev",
                                "IR", "PR" ,"KR", "BR", "HR","MR", "AR", "CR")]
  
  ref_tab_new[ref_tab_new$ID=='CM_ECMR_C_NNF',]$Description <- 'Probability of dying in the first month of life in the five years preceding the survey'
  ref_tab_new[ref_tab_new$ID=='CM_ECMR_C_NNF',]$Full_definition <-'Probability of dying in the first month of life in the five or ten years preceding the survey, per 1,000 live births. Estimates are given for ten year periods for all characteristics, but for five year periods only for the national total, by residence, and by sex.'
  
  #save(ref_tab_new,file='data/indicator_list_new.rda')
  
  #recode_list <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  #recode_list[which(wide_format[7,recode_list]==T)]
  #recode_list[which(full_ind_des[full_ind_des$ID=='HA_HIVP_B_HIV',recode_list]==T)]
}


###############################################################
###  create ref tab for initial 22 indicators
###############################################################

if(FALSE){
  
  IR_Individual <- c( "RH_ANCN_W_N4P",  "AN_ANEM_W_ANY",
                      "FP_NADA_W_UNT", "FP_CUSA_W_MOD", "AN_NUTS_W_THN","HA_HIVP_B_HIV")
  PR_Household_Member <- c("CN_ANMC_C_ANY", "CN_NUTS_C_WH2", "CN_NUTS_C_HA2",
                           "WS_TLET_H_IMP", "WS_TLET_P_BAS",
                           "WS_SRCE_P_BAS")
  KR_Children <- c("CH_DIAT_C_ORT", "CH_VACC_C_DP3", "CH_VACC_C_DP1",
                   "CH_VACC_C_BAS", "CH_VACC_C_NON", "CN_BRFS_C_EXB", "CH_VACC_C_MSL"
  )
  BRdata_Birth <- c("RH_DELA_C_SKP", "CM_ECMR_C_NNR")
  HRdata_Household <- c("ML_NETP_H_IT2")
  
  MR_men <- c("HA_HIVP_B_HIV")
  AR_HIV<- c("HA_HIVP_B_HIV")
  CR_couple<- NA
  
  
  # Combine all indicators into a single vector and create a data frame
  all_indicators <- unique(c(IR_Individual, PR_Household_Member, KR_Children, BRdata_Birth, HRdata_Household,MR_men))
  wide_format <- data.frame(ID = all_indicators, stringsAsFactors = FALSE)
  
  # Create columns for each data type and check if the indicator belongs to that type
  wide_format$IR <- wide_format$ID %in% IR_Individual
  wide_format$PR <- wide_format$ID %in% PR_Household_Member
  wide_format$KR <- wide_format$ID %in% KR_Children
  wide_format$BR <- wide_format$ID %in% BRdata_Birth
  wide_format$HR <- wide_format$ID %in% HRdata_Household
  wide_format$MR <- wide_format$ID %in% MR_men
  wide_format$AR <- wide_format$ID %in% AR_HIV
  wide_format$CR <- wide_format$ID %in% CR_couple
  
  # merge back with the information data frame
  surveyPrev_ind_list <-  surveyPrev::surveyPrevIndicators
  #full_ind_des <- merge(surveyPrev_ind_list,wide_format,by='ID',all.x=T)
  full_ind_des[full_ind_des$ID=='FP_CUSA_W_MOD',]$Description <-  "Modern contraceptive prevalence rate (all women currently using any modern method of contraception)"
  
  #save(full_ind_des,file='indicator_list.rda')
  
  #recode_list <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  #recode_list[which(wide_format[7,recode_list]==T)]
  #recode_list[which(full_ind_des[full_ind_des$ID=='HA_HIVP_B_HIV',recode_list]==T)]
}


if(FALSE){
  
  ref_tab_22 <- SurveyPrevRShiny::ref_tab_22
  
  #ref_tab_22$Full_definition <- ref_tab_22$Description
  ref_tab_22$Full_definition <- NULL
  
  
  ref_tab_22$Chap_abbrev <- c(rep('NT',times=2),
                              rep('CH',times=6),
                              rep('CM',times=1),
                              rep('NT',times=4),
                              rep('FP',times=2),
                              rep('HV',times=1),
                              rep('ML',times=1),
                              rep('RH',times=2),
                              rep('PH',times=3))
  
  ref_tab_22$Topic <- c(rep('Nutrition Of Children And Adults',times=2),
                        rep('Child Health',times=6),
                        rep('Infant And Child Mortality',times=1),
                        rep('Nutrition Of Children And Adults',times=4),
                        rep('Family Planning',times=2),
                        rep('HIV Prevalence',times=1),
                        rep('Malaria',times=1),
                        rep('Maternal Health',times=2),
                        rep('Housing Characteristics And Household Population',times=3))
  
  
  tmp_dict <- SurveyPrevRShiny::DHS_ind_dictionary[,c('DHS Standard Indicator ID','Full Definition')]
  tmp_dict$ID <- tmp_dict$`DHS Standard Indicator ID`
  tmp_dict$Full_definition <- tmp_dict$`Full Definition`
  tmp_dict <- tmp_dict[!duplicated(tmp_dict[,c('ID')]),]
  
  ref_tab_22 <- merge(ref_tab_22,tmp_dict[,c('ID','Full_definition')],by='ID')
  
  ref_tab_22 <- ref_tab_22[,c('ID', "Description","Full_definition","Topic" , "Chap_abbrev",
                              "IR", "PR" ,"KR", "BR", "HR","MR", "AR", "CR")]
  
  save(ref_tab_22,file='data/indicator_list_22.rda')
  
  #recode_list <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  #recode_list[which(wide_format[7,recode_list]==T)]
  #recode_list[which(full_ind_des[full_ind_des$ID=='HA_HIVP_B_HIV',recode_list]==T)]
}


###############################################################
###  merge new and old indicators
###############################################################

if(FALSE){
  library(surveyPrevGithub)
}


if(FALSE){
  
  ref_tab_new_no_dup <- ref_tab_new[!ref_tab_new$ID %in% ref_tab_22$ID,]
  ref_tab_all <- rbind(ref_tab_22,ref_tab_new_no_dup)
  
  
  ref_tab_all <- merge(ref_tab_all,dhs_chapters,
                       by.x='Chap_abbrev',
                       by.y='Acronym',
                       all.x=T)
  
  #ref_tab_all <- ref_tab_all[ref_tab_all$Chap_abbrev!='DV',]
  
  
  ref_tab_all$Topic <- paste0('Chapter ',formatC(ref_tab_all$Chapter, width = 2, format = "d", flag = "0"),
                              ' - ',ref_tab_all$Title)
  
  save(ref_tab_all,file='data/indicator_list_all.rda')
  
  #write.csv(ref_tab_all[,c(1:5)],row.names = F,file='indicator_list_0916.csv')
}



#######################################################################
###  prepare DHS estimates for all surveys for supported indicators
#######################################################################

if(FALSE){
  dhs_survey_list <- rdhs::dhs_surveys()
  dhs_survey_list <- dhs_survey_list[dhs_survey_list$SurveyYear>2000 & dhs_survey_list$SurveyType== 'DHS',]
  
  res_ind_supported <- data.frame()
  
  for (i in 1:dim(dhs_survey_list)[1]){
    print(i)
    tmp_cty_code <- dhs_survey_list$DHS_CountryCode[i]
    tmp_svy_year <- dhs_survey_list$SurveyYear[i]
    print(paste0(dhs_survey_list$DHS_CountryCode[i],dhs_survey_list$SurveyYear[i]))
    
    
    cty_svy_res_file <- paste0('E:/Dropbox/YunhanJon/DHS-indicators/Step_3_Data/all_survey_est/',
                               tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda')
    
    if(!file.exists(cty_svy_res_file)){
      
      
      tmp_call <- paste0("https://api.dhsprogram.com/rest/dhs/data?countryIds=",tmp_cty_code,"&surveyYear=",tmp_svy_year,"&perpage=10000&f=csv")
      tmp_res <- read.csv(tmp_call)
      
      
      print('done')
      save(tmp_res,file=paste0('E:/Dropbox/YunhanJon/DHS-indicators/Step_3_Data/all_survey_est/',
                               tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda'))
      
    }else{
      
      load(cty_svy_res_file)
    }
    
    tmp_res <- tmp_res[tmp_res$IndicatorId %in% ref_tab_all$ID,  ]
    res_ind_supported <- rbind(res_ind_supported,tmp_res)
    
  }
  
  
  DHS_api_est <- res_ind_supported[,c('IndicatorId','Indicator','Value','DHS_CountryCode',
                                      'CountryName','SurveyYear','ByVariableLabel')]
  
  colnames(DHS_api_est) <- c('DHS Standard ID','Definition','Estimate','Country Code','Country','Survey Year','By Variable Label')
  DHS_api_est <- DHS_api_est[,c('Country','Country Code','Survey Year','DHS Standard ID','Definition','Estimate','By Variable Label')]
  #save(DHS_api_est,file='data/DHS_api_est.rda')
  
  
}

