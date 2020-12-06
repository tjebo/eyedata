library(dplyr)
library(eye)

## amd2 dataset
amd2_raw <- readr::read_delim("./data-raw/fasler/Moorfields_AMD_Database_1.csv", delim = ";")
amd2_raw$Id <- paste0("id_", as.integer(as.factor(amd_raw$Id)))
amd2 <-
  amd_raw2 %>%
  rename(patID = Id, eye = Eye, va = VA_ETDRS_Letters,
         inj_no = InjectionNumber, time = FollowupDays,
         sex = Gender, age0 = BaselineAge) %>%
  mutate(eye = recodeye(eye),
         sex = if_else(sex == 0, "m", "f")) %>%
  select(patID, sex, age0, everything())

usethis::use_data(amd2, overwrite = TRUE)

## AMD data set (12year survival)
amd_raw <- read.csv("./data-raw/fu/MEH_AMD_survivaloutcomes_database.csv")

amd_raw$anon_id <- as.integer(as.factor(amd_raw$anon_id))

amd <-
  amd_raw %>%
  mutate(loaded = if_else(loaded == "loaded", 1, 0),
         pre2013 = if_else(date_inj1 == "Pre-2013", 1, 0),
         regimen = if_else(regimen == "Ranabizumab only", "ranibizumab", "aflibercept"),
         injgiven = !is.na(injgiven),
         age_group = factor(age_group, levels = c( "50-59", "60-69", "70-79",">80")),
         across(where(function(x) all(unique(x) %in% 0:1)), as.logical)) %>%
  select(patID = anon_id, sex = gender, age = age_group,
         avdays_induc = mean_inj_interval,
         everything(), -starts_with("X"), -contains("inj1"), -injnum) %>%
  arrange(patID, time) %>%
  mutate(patID = as.character(paste0("id_", patID)))

## simplifying ethnicity codes
lu_eth_amd <- c("asian", "caucasian", "unknown_other", "afrocarribean", "mixed" )
names(lu_eth_amd) <- unique(amd$ethnicity)
amd$ethnicity <- lu_eth_amd[amd$ethnicity]

amd <- tidyr::as_tibble(amd)
usethis::use_data(amd, overwrite = TRUE)


## AMD OCT data
amdoctraw <- read.csv("./data-raw/moraes/AMD_baseline.csv")

# ID 675 had treatment both eyes as first eye. - considering as "first treated eye"
# setdiff(1:2967,amdoctraw$ID) - ID 2773 is missing
amdoct <-
  amdoctraw %>%
  select(patID = ID, sex = Gender, ageStrat = Age_grouped,
         ethnicity = Ethnicity_grouped, eye = Eye, first_eye = FirstTreatedEye,
         va = VA_ETDRS, inj = InjectionGiven, time = DaysSinceBaseline,
         everything(), -InjectionNumber, -First_or_Second_Treated_Eye,
         -oct_shape, -segmentation_voxel_size_um) %>%
  mutate(patID = as.integer(as.factor(patID)),
         va = as.integer(va),
         ageStrat = factor(ageStrat, levels = c("50-59", "60-69", "70-79",">80" )),
         VAUnder1Letter = clean_va(VAUnder1Letter),
         va = if_else(!is.na(VAUnder1Letter), VAUnder1Letter, as.character(va)),
         first_eye = if_else(first_eye == "Both", eye, first_eye),
         first_eye = eye == first_eye,
         eye = recodeye(eye),
         sex = if_else(sex == "Male", "m", "f")) %>%
  select(-VAUnder1Letter) %>%
  arrange(patID)

## replacing implausible ETDRS values with NA and simplifying ethnicity codes
amdoct$va[as.integer(amdoct$va)>100] <- NA
lu_eth_amd <- c("white", "asian", "other_unknown",  "black")
names(lu_eth_amd) <- unique(amdoct$ethnicity)
amdoct$ethnicity <- lu_eth_amd[amdoct$ethnicity]
amdoct$patID <- paste("id", amdoct$patID, sep = "_")
amdoct <- as_tibble(amdoct)

amdoct <- tidyr::as_tibble(amdoct)
usethis::use_data(amdoct, overwrite = TRUE)


### DME data
dme_raw <- read.csv("./data-raw/kern/200319_DMO_report1_anonymised.csv")
## simplify patient code
dme_raw$anon_id <- as.integer(as.factor(dme_raw$anon_id))
dme_raw[["inj_num"]] <- NULL
## replacing implausible ETDRS values with NA
dme_raw$va[dme_raw$va>100] <- NA
lu_eth_dme <- c("asian", "unknown", "other",  "white", "black", "mixed")
names(lu_eth_dme) <- unique(dme_raw$ethnicity)

dme <-
  dme_raw %>%
  select(patID = anon_id, sex = gender, ageStrat = baseline_age, ethnicity,
         everything(), -X, -baseline_va) %>%
  rename(inj = inj_given, time = follow_up_days) %>%
  mutate(inj = inj == "y",
         ageStrat = factor(ageStrat, levels =
                             c("[20,30]", "(30,40]", "(40,50]", "(50,60]",
                               "(60,70]", "(70,80]", "(80,100]")))

dme <- dme %>%
  arrange(patID, eye, time) %>%
  mutate(patID = paste("id", patID, sep = "_"))

dme$ethnicity <- lu_eth_dme[dme$ethnicity]
dme <- tidyr::as_tibble(dme)

usethis::use_data(dme, overwrite = TRUE)

## AMD NV 10 year data
amd3_raw <- read.csv("./data-raw/arpa/Moorfields_AMD_Database_10_years.csv")
amd3_raw$anon_id <- paste0("id_", as.integer(as.factor(amd3_raw$anon_id)))

amd3<-
  amd3_raw %>%
  select(patID = anon_id, sex = gender, time = ttoinj_d,
         everything(), stable10y = ten_y_stable, injgiven = inj_given,
         -va_inj1, -va_lastvisit, -crt_inj1,-crt_lastvisit,-X, -inj_num) %>%
  mutate(sex = if_else(sex == "Male", "m", "f"),
         injgiven = !is.na(injgiven),
         ltfu_outcome = if_else(ltfu_outcome == "", NA_character_, ltfu_outcome),
         across(where(function(x) all(unique(x) %in% 0:1)), as.logical))

names(amd3) <- gsub("_mo","",names(amd3))
## replacing implausible ETDRS values with NA and simplifying ethnicity codes

lu_eth_amd3 <- c("white", "unknown" ,"asian",  "mixed", "other")
names(lu_eth_amd3) <- unique(amd3$ethnicity)
amd3$ethnicity <- lu_eth_amd3[amd3$ethnicity]

amd3 <- tidyr::as_tibble(amd3)
usethis::use_data(amd3, overwrite = TRUE)
