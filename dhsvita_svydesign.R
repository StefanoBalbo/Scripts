rm(list=ls())
gc() 

# # WD AND LIBRARIES # # # # # # # # # # # # # # # # # # # # # # # # # # 
setwd("C:\\Users\\stefa\\/"); getwd()

library(data.table)
library(haven)
library(labelled)
library(here)
library(foreign)
library(lme4)
library(survey)
library(sf)
library(sp)
library(stats)
library(tidyverse)
library(tidyr)
library(tmap)
library(raster)
library(dplyr)
library(ggplot2)
library(mapview)

# Congo # Niger # Liberia # Togo #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# CONGO #
{
  #congo_IR <- read_dta("data_files/Congo/CDIR61FL.DTA") # Individual Women's Data - Individual Recode (IR)
  #congo_BR <- read_dta("data_files/Congo/CDBR61FL.DTA") # Births' data - Birth's Recode (BR)
}

# HH %% # 

congo_HR <- read_dta("VAS Coverage/data_files/Congo/CDHR61FL.DTA") # Household Data - Household Recode (HR)
congo_HR$country = "DRC"

congo <- congo_HR %>%
  group_by(hhid) %>%
  summarise(
            country = country,
            country_code = hv000,
            sample_year = hv007,
            sample_month = hv006,
            weight = hv005,
           wt = (hv005/1000000),
            psu = hv021,
            sample_stratum = hv022,
            sample_domain = hv023,
            cluster = hv001,
            household = hv002,
            wealth = hv270,
            # geo
            region_id = hv024,
            region = case_when(hv024 == 1 ~ "Region 1", 
                               hv024 == 2 ~ "Region 2",
                               hv024 == 3 ~ "Region 3", 
                               hv024 == 4 ~ "Region 4",
                               hv024 == 5 ~ "Region 5",
                               hv024 == 6 ~ "Region 6",
                               hv024 == 7 ~ "Region 7",
                               hv024 == 8 ~ "Region 8",
                               hv024 == 9 ~ "Region 9",
                               hv024 == 10 ~ "Region 10",
                               hv024 == 11 ~ "Region 11"),
            residence_id = hv025,
            residence = ifelse(hv025 == 1, "Urban", "Rural")
  ) 

congo$residence_binary <- ifelse(congo$residence_id == 1, 1, 0) # Convert to binary: 1 for Urban, 0 for Rural

congo_design <- svydesign(id = ~psu, weights = ~wt, data = congo, nest = TRUE)

urban_households_proportion <- plyr::ddply(congo, ~region_id , summarise , mean = weighted.mean(residence_binary, wt)) 
rural_households_proportion = 1 - urban_households_proportion
rural_households_proportion$region_id = 1:11

proportions_urban <- urban_households_proportion %>% 
  dplyr::select(region_id, mean) %>% 
  rename(residence_prop = mean) %>% 
  mutate(residence = "Urban")

proportions_rural <- rural_households_proportion %>% 
  dplyr::select(region_id, mean) %>% 
  rename(residence_prop = mean) %>% 
  mutate(residence = "Rural")

proportions <- bind_rows(proportions_urban, proportions_rural) %>%
  arrange(region_id, residence)

congo <- congo %>%
  left_join(proportions, by = c("region_id", "residence")) %>%
  mutate(
    residence_perc = residence_prop * 100
  )

proportions$country = "DRC"

table(congo$residence_perc)
table(congo$residence_prop)

congo = as.data.frame(congo)
proportions = as.data.frame(proportions)

proportions = rename(proportions, household_prop = residence_prop)
proportions

path = 'C:\\Users\\stefa\\Documents/'
openxlsx::write.xlsx(congo,  paste(path,'congo_hh.xlsx', sep = ''))
write.csv(proportions, paste(path,'congo_hhproportions.csv', sep = ''))
openxlsx::write.xlsx(proportions,  paste(path,'congo_hhproportions.xlsx', sep = ''))

rm(list=ls())

# # #

# CH %% # 

congo_KR <- read_dta("VAS Coverage/data_files/Congo/CDKR61FL.DTA") # Children's Data - Children's Recode (KR)
congo_KR$country = "DRC"

congo_child <- congo_KR %>%
  group_by(caseid) %>%
  summarise(midx,
            country = country,
            country_code = v000,
            sample_month = v006,
            sample_year = v007,
            cluster = v001,
            household = v002,
            area = v004,
            weight = v005,
            wt = (v005/1000000), # Number of units in the population that a sampled unit represents
            psu = v021,
            sample_stratum = v022,
            sample_domain = v023,
            wealth = v190,
            # geo
            region_id = v024,
            residence_id = v025,
            region = case_when(v024 == 1 ~ "Region 1", 
                               v024 == 2 ~ "Region 2",
                               v024 == 3 ~ "Region 3", 
                               v024 == 4 ~ "Region 4",
                               v024 == 5 ~ "Region 5",
                               v024 == 6 ~ "Region 6",
                               v024 == 7 ~ "Region 7",
                               v024 == 8 ~ "Region 8",
                               v024 == 9 ~ "Region 9",
                               v024 == 10 ~ "Region 10",
                               v024 == 11 ~ "Region 11"),
            residence = ifelse(v025 == 1, "Urban", "Rural"),
            distance_facility = v467d, # 0 No problem 1 Big problem 2 Not a big problem
            # child
            child_age = hw1, # Children age in months
    # Vit A variables
    child_vitA_recent = h33, # Child was taken to a medical facility for treatment of the fever and/or cough and received vitamin A (most recent)
    child_vitA_last6m = h34, # Received or not a vitamin A dose in form of an ampoule, a capsule or syrup in last 6 months
    birth_vitA_a2m = m54, # Received Vitamin A dose in first 2 months after delivery
    # DPT (Diphteria, pertussis and tetanus vacccination) variables
    DPT1 = h3,
    DPT2 = h5,
    DPT3 = h7,
    # Diarrhea
    diarrhea = h11, # 0 no, 1 last 24hs, 2 last 2 weeks, 8/9 doesnt know
    # ANCV (Antenatal care visits)
    months_ANCV = m13, # Months pregnant at first antenatal visit 
    amount_ANCV = m14, # Antenatal visits during pregnancy
    )

names(congo_child)
congo_child_2 = subset(congo_child, child_age >= 6)
summary(congo_child_2$child_age)
congo_child = congo_child_2; rm(congo_child_2)

congo_child$child_age_dummy <- ifelse(congo_child$child_age >= 24, 1, 0)

congo_child$residence_binary <- ifelse(congo_child$residence_id == 1, 1, 0)

children_older_24 <- filter(congo_child, child_age_dummy == 1)
children_younger_24 <- filter(congo_child, child_age_dummy == 0)

table(children_older_24$diarrhea)
table(children_younger_24$diarrhea) # 0 no, 1 last 24hs, 2 last 2 weeks, 8/9 doesnt know

urban_older_24 <- plyr::ddply(children_older_24, ~region_id, summarise, mean = weighted.mean(residence_binary, wt))
rural_older_24 <- transform(urban_older_24, mean = 1 - mean)

urban_younger_24 <- plyr::ddply(children_younger_24, ~region_id, summarise, mean = weighted.mean(residence_binary, wt))
rural_younger_24 <- transform(urban_younger_24, mean = 1 - mean)

urban_older_24$age_group <- "Older 24 months"
urban_younger_24$age_group <- "Younger 24 months"
rural_older_24$age_group <- "Older 24 months"
rural_younger_24$age_group <- "Younger 24 months"

urban_children_proportion <- bind_rows(urban_younger_24, urban_older_24)
rural_children_proportion <- bind_rows(rural_younger_24, rural_older_24)

proportions_urban <- urban_children_proportion %>%
  rename(children_prop = mean) %>%
  mutate(children_perc = children_prop * 100,
         residence = "Urban")

proportions_rural <- rural_children_proportion %>%
  rename(children_prop = mean) %>%
  mutate(children_perc = children_prop * 100,
         residence = "Rural")

proportions <- bind_rows(proportions_urban, proportions_rural) %>%
  arrange(region_id, residence, age_group)

children_count <- congo_child %>%
  mutate(residence = ifelse(residence_binary == 1, "Urban", "Rural")) %>%
  group_by(region_id, residence) %>%
  dplyr::summarize(total_in_group = sum(wt, na.rm = TRUE), .groups = 'drop')



proportions <- proportions %>%
  mutate(
    child_age_dummy = ifelse(age_group == "Older 24 months", 1, 0),
    residence_binary = ifelse(residence == "Urban", 1, 0)
  )

total_weighted_count <- congo_child %>%
  group_by(child_age_dummy, residence_binary) %>%
  summarize(total_wt = sum(wt, na.rm = TRUE), .groups = 'drop')

children_count <- congo_child %>%
  mutate(residence = ifelse(residence_binary == 1, "Urban", "Rural")) %>%
  group_by(region_id, residence) %>%
  summarize(total_in_group = sum(wt, na.rm = TRUE), .groups = 'drop')

proportions <- proportions %>%
  left_join(children_count, by = c("region_id", "residence")) %>%
  left_join(total_weighted_count, by = c("child_age_dummy", "residence_binary"))

proportions <- proportions %>%
  mutate(children_across_regions = (children_prop * total_in_group) / total_wt)

total_children_across <- sum(proportions$children_across_regions, na.rm = TRUE)
proportions <- proportions %>%
  mutate(children_across_regions = (children_across_regions / total_children_across) * 100)

proportions$country = "DRC"
print(proportions)
proportions = as.data.frame(proportions)

sum(proportions$children_across_regions, na.rm = TRUE)

path = 'C:\\Users\\stefa\\Documents\\Code\\'
openxlsx::write.xlsx(congo_child,  paste(path,'congo_ch.xlsx', sep = ''))
write.csv(proportions, paste(path,'congo_chproportions.csv', sep = ''))
openxlsx::write.xlsx(proportions,  paste(path,'congo_chproportions.xlsx', sep = ''))

rm(list=setdiff(ls(), "congo_child"))

# # # 

# VAS # 
table(congo_child$child_vitA_last6m)
summary(congo_child$child_vitA_last6m)
congo_child <- congo_child %>%
  mutate(child_vitA_last6m = case_when(child_vitA_last6m == 8 ~ 2, 
                                      is.na(child_vitA_last6m) ~ 3, 
                                      TRUE ~ child_vitA_last6m)) 
table(congo_child$child_vitA_last6m) # 0 = No, 1 = Yes, 2 = Doesnt Know, 3 = No Data
summary(congo_child$child_vitA_last6m)

# DPT Vaccination #
# 0 = No, 1 = Vaccination date on card, 2 = Reported by mother, 3 = Vaccination marked on card, 8 = Doesnt know
table(congo_child$DPT1) 
summary(congo_child$DPT1)
table(congo_child$DPT2) 
summary(congo_child$DPT2)
table(congo_child$DPT3)
summary(congo_child$DPT3)

{
congo_child <- congo_child %>%
  mutate(DPT1 = case_when(DPT1 == 2 ~ 1,
                          DPT1 == 3 ~ 1,
                          TRUE ~ DPT1)) 
congo_child <- congo_child %>%
  mutate(DPT1 = case_when(DPT1 == 8 ~ 2, 
                          is.na(DPT1) ~ 3,
                          TRUE ~ DPT1)) 
congo_child <- congo_child %>%
  mutate(DPT2 = case_when(DPT2 == 2 ~ 1,
                          DPT2 == 3 ~ 1,
                          TRUE ~ DPT2)) 
congo_child <- congo_child %>%
  mutate(DPT2 = case_when(DPT2 == 8 ~ 2, 
                          is.na(DPT2) ~ 3,
                          TRUE ~ DPT2)) 
congo_child <- congo_child %>%
  mutate(DPT3 = case_when(DPT3 == 2 ~ 1,
                          DPT3 == 3 ~ 1,
                          TRUE ~ DPT3)) 
congo_child <- congo_child %>%
  mutate(DPT3 = case_when(DPT3 == 8 ~ 2, 
                          is.na(DPT3) ~ 3,
                          TRUE ~ DPT3))
}

# 0 = No, 1 = Yes, 2 = Doesnt Know, 3 = No Data
table(congo_child$DPT1) 
summary(congo_child$DPT1)
table(congo_child$DPT2) 
summary(congo_child$DPT2)
table(congo_child$DPT3)
summary(congo_child$DPT3)


# ANCV (Antenatal care visits)
table(congo_child$amount_ANCV)
congo_child$ANCV_min1 = ifelse(congo_child$amount_ANCV >= 1, 1, 0) # At least 1 antenatal visit
congo_child$ANCV_min4 = ifelse(congo_child$amount_ANCV >= 4, 1, 0) # At least 4 antenatal visits 
table(congo_child$ANCV_min1)
table(congo_child$ANCV_min4)
# 0 = No, 1 = Yes


# VAS and DPT coverages #
vas_coverage <- congo_child %>%
  filter(child_vitA_last6m %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(vas_coverage = weighted.mean(child_vitA_last6m == 1, wt, na.rm = TRUE)) %>%
  ungroup()

dpt_coverage <- congo_child %>%
  filter(DPT1 %in% c(0, 1), DPT2 %in% c(0, 1), DPT3 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(
    dpt1_coverage = weighted.mean(DPT1 == 1, wt, na.rm = TRUE),
    dpt2_coverage = weighted.mean(DPT2 == 1, wt, na.rm = TRUE),
    dpt3_coverage = weighted.mean(DPT3 == 1, wt, na.rm = TRUE)
  ) %>%
  ungroup()

coverage <- left_join(vas_coverage, dpt_coverage, by = c("region_id", "residence", "child_age_dummy"))

# ANCV coverage # 
ANCV_1 <- congo_child %>%
  filter(ANCV_min1 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(ANCV_min1 = weighted.mean(ANCV_min1 == 1, wt, na.rm = TRUE)) %>%
  ungroup()

ANCV_4 <- congo_child %>%
  filter(ANCV_min4 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(ANCV_min4 = weighted.mean(ANCV_min4 == 1, wt, na.rm = TRUE)) %>%
  ungroup()

ANCV <- left_join(ANCV_1, ANCV_4, by = c("region_id", "residence", "child_age_dummy"))

coverage2 <- left_join(coverage, ANCV, by = c("region_id", "residence", "child_age_dummy"))

coverage2 <- coverage2 %>% 
  distinct(region_id, residence, child_age_dummy, .keep_all = TRUE)

coverage2 <- coverage2 %>% 
  drop_na(child_age_dummy)


table(congo_child$diarrhea)
congo_child <- congo_child %>%
  mutate(diarrhea = case_when(diarrhea == 2 ~ 1,
                              diarrhea == 8 ~ 0,
                          TRUE ~ diarrhea)) 
table(congo_child$diarrhea) # == 1 YES, == 0 NO
names(congo_child)

weighted_proportion <- congo_child %>%
  summarise(weighted_prop = sum(wt * diarrhea, na.rm = TRUE) / sum(wt, na.rm = TRUE))
weighted_proportion_by_region <- congo_child %>%
  group_by(region_id) %>%
  summarise(weighted_prop = sum(wt * diarrhea, na.rm = TRUE) / sum(wt, na.rm = TRUE))
weighted_proportion_by_region

summary(congo_child$child_age)
congo_child$child_age_breaks = case_when(congo_child$child_age <= 11 ~ "6-11",
                                congo_child$child_age >= 12 & congo_child$child_age <= 23 ~ "12-23",
                                 congo_child$child_age >= 24 & congo_child$child_age <= 35 ~ "24-35",
                                   congo_child$child_age >= 36 & congo_child$child_age <= 47 ~ "36-47",
                                     congo_child$child_age >= 48 & congo_child$child_age <= 59 ~ "48-59",
                                        TRUE ~ NA_character_)
table(congo_child$child_age_breaks)

diarrhea_by_age_weighted <- congo_child %>%
  group_by(child_age_breaks) %>%
  summarise(
    total_weighted_children = sum(wt, na.rm = TRUE),
    weighted_children_with_diarrhea = sum(wt[diarrhea == 1], na.rm = TRUE),
    percent_with_diarrhea = (weighted_children_with_diarrhea / total_weighted_children) * 100
  )

diarrhea_table_weighted <- diarrhea_by_age_weighted %>%
  arrange(factor(child_age_breaks, levels = c("6-11", "12-23", "24-35", "36-47", "48-59")))
diarrhea_table_weighted

proportions_hh = read.csv("VAS Coverage/results/congo_hhproportions.csv")
proportions_ch = read.csv("VAS Coverage/results/congo_chproportions.csv")
proportions = left_join(proportions_hh, proportions_ch, by = c("region_id", "residence", "country"))

proportions <- proportions %>% 
  distinct(region_id, residence, age_group, .keep_all = TRUE)

proportions <- proportions %>%
  mutate(child_age_dummy = ifelse(age_group == "Older 24 months", 1, 0))

final_data <- left_join(proportions, coverage2, by = c("region_id", "residence", "child_age_dummy"))
final_data = final_data[, -c(1, 6)]; final_data

rm(list=setdiff(ls(), c("congo_child", "final_data")))


# GIS # AVERAGE DISTANCE TO FACILITIES # # # # # # # # # # # # # # # # # # # # # # # # # # #
names(congo_child)
names(final_data)
congo_hh_GIS = st_read("VAS Coverage/data_files/Congo/GPS/CDGE61FL_DHS_hous/CDGE61FL.shp") # Household-GPS points
congo_fa_GIS = st_read("VAS Coverage/data_files/Congo/GPS/CDGE71FLSR_SPA_faci/CDGE71FLSR.shp") # Facility-GPS points
#congo_fa_GIS = st_read("data_files/Congo/GPS/CongoHealthFacilities_SPA_faci/osm_rd_congo_health.geojson") # Facility-GPS points
congo = openxlsx::read.xlsx("VAS Coverage/results/congo_hh.xlsx")

mapview(congo_fa_GIS, color = "red") + mapview(congo_hh_GIS, color = "blue")
max(congo_hh_GIS$DHSCLUST) == max(congo_child$cluster)

congo_hh_GIS = rename(congo_hh_GIS, cluster = DHSCLUST)

congo_hh = left_join(congo, congo_hh_GIS, by = "cluster"); names(congo_hh)
max(congo_hh$cluster)

distance_nearest_facility_mts = st_distance(congo_hh_GIS, congo_fa_GIS)
distance_nearest_facility_mts = as.data.frame(distance_nearest_facility_mts)
distance_nearest_facility_mts = apply(distance_nearest_facility_mts, 1, FUN = min)

# table(congo_fa_GIS$amenity)
# congo_fa_GIS = congo_fa_GIS[congo_fa_GIS$amenity %in% c("hospital", "clinic", "clinic;hospital", "doctors"), ]; table(congo_fa_GIS$amenity)

congo_fa_GIS$FacilityType <- ifelse(congo_fa_GIS$SPATYPEC %in% c(1, 4, 5), 'Other', 'Hospital')
congo_fa_GIS$Ownership <- ifelse(congo_fa_GIS$SPAMANGC == 1, 'Public', 'Private')

# congo_fa_GIS$FacilityType <- ifelse(congo_fa_GIS$amenity %in% 'clinic', 'Other', 'Hospital'); table(congo_fa_GIS$FacilityType)
# congo_fa_GIS$Ownership <- "Public"; table(congo_fa_GIS$Ownership)

congo_fa_public = congo_fa_GIS %>%
  filter(Ownership == "Public"); table(congo_fa_GIS$Ownership)

distance_nearest_pubfacility_mts = st_distance(congo_hh_GIS, congo_fa_public)
distance_nearest_pubfacility_mts = as.data.frame(distance_nearest_pubfacility_mts)
distance_nearest_pubfacility_mts = apply(distance_nearest_pubfacility_mts, 1, FUN = min)

congo_hh_GIS$distance_nearest_facility_mts = distance_nearest_facility_mts
congo_hh_GIS$distance_nearest_facility_km = distance_nearest_facility_mts / 1000

congo_hh_GIS$distance_nearest_pubfacility_mts = distance_nearest_pubfacility_mts
congo_hh_GIS$distance_nearest_pubfacility_km =distance_nearest_pubfacility_mts / 1000

summary(congo_hh_GIS$distance_nearest_facility_km)
summary(congo_hh_GIS$distance_nearest_pubfacility_km)

congo_hh <- left_join(congo_hh, congo_hh_GIS[c("cluster", "distance_nearest_facility_km", "distance_nearest_pubfacility_km")], by = "cluster")
congo_ch <- left_join(congo_child, congo_hh_GIS[c("cluster", "distance_nearest_facility_km", "distance_nearest_pubfacility_km")], by = "cluster")
summary(congo_hh$distance_nearest_facility_km)
summary(congo_ch$distance_nearest_facility_km)
summary(congo_hh$distance_nearest_pubfacility_km)
summary(congo_ch$distance_nearest_pubfacility_km)


# DISTANCE GROUPS DUMMY VARIABLE # # # # # # # # # # # # # # # # # # # # # # # # # # #
congo_hh$distance_group = cut(congo_hh$distance_nearest_facility_km, 
                              breaks = quantile(congo_hh$distance_nearest_facility_km, probs = 0:10/10, na.rm = TRUE), 
                              include.lowest = TRUE, labels = FALSE)
table(congo_hh$distance_group)
summary(congo_hh$distance_nearest_facility_km)

library(dplyr)
library(Hmisc)
congo_hh_aggregated <- congo_hh %>%
  group_by(region_id, residence) %>%
  summarise(
    min_distance_km = weighted.mean(distance_nearest_facility_km, wt, na.rm = TRUE, trim = 0),
    mean_distance_km  = weighted.mean(distance_nearest_facility_km, wt, na.rm = TRUE),
    median_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.5, na.rm = TRUE),
    q1_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.25, na.rm = TRUE),
    q3_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.75, na.rm = TRUE),
    max_distance_km = max(distance_nearest_facility_km, na.rm = TRUE),
    Percentile_5 = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.05, na.rm = TRUE),
    Percentile_95 = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.95, na.rm = TRUE),
    distance_public_km = wtd.quantile(distance_nearest_pubfacility_km, weights = wt, probs = 0.5, na.rm = TRUE),
    distance_group = as.integer(names(sort(table(distance_group), decreasing = TRUE)[1]))
  ) %>%
  ungroup()

final_data = as.data.frame(final_data)
final_data$region_id = as.character(final_data$region_id)
congo_hh_aggregated = as.data.frame(congo_hh_aggregated)
congo_hh_aggregated$region_id = as.character(congo_hh_aggregated$region_id)

final_data <- left_join(final_data, congo_hh_aggregated, by = c("region_id", "residence"))

wealth_aggregated <- congo_hh %>%
  group_by(region_id, residence) %>%
  summarise(
    Total_Weight = sum(wt),  # Sum of weights for normalization
    Weighted_Poorer = sum(wt * (wealth == 1)),
    Weighted_Richest = sum(wt * (wealth == 5)),
    Poorer = Weighted_Poorer / Total_Weight,  # Proportion of Poorer
    Richest = Weighted_Richest / Total_Weight,  # Proportion of Richest
    .groups = 'drop'
  )
wealth_aggregated$region_id <- as.character(wealth_aggregated$region_id)  # Ensure region_id is character if necessary
wealth_aggregated = wealth_aggregated[, -c(3, 4, 5)]; wealth_aggregated

final_data <- left_join(final_data, wealth_aggregated, by = c("region_id", "residence")); final_data

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/VAS Coverage/results/'
openxlsx::write.xlsx(final_data,  paste(path,'congo_coverage.xlsx', sep = ''))

final_data_older_24 <- filter(final_data, child_age_dummy == 1)
final_data_younger_24 <- filter(final_data, child_age_dummy == 0)

summary(final_data_older_24$vas_coverage)
summary(final_data_younger_24$vas_coverage)


# FINAL FILTERED CHILDREN DATASET # # # # # # # # # # # # # # # # # # # # # # # # # # #
final_data$residence_binary <- ifelse(final_data$residence == "Urban", 1, 0)
congo_ch$residence_binary <- as.numeric(congo_ch$residence_binary)
congo_ch$region_id <- as.numeric(congo_ch$region_id)
final_data$residence_binary <- as.numeric(as.character(final_data$residence_binary))
final_data$region_id <- as.numeric(as.character(final_data$region_id))

summary(final_data$vas_coverage)
hist(final_data$vas_coverage)

quantile_breaks <- quantile(final_data$vas_coverage, probs = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)
final_data$vas_coverage_group <- cut(final_data$vas_coverage,
                                     breaks = quantile_breaks,
                                     include.lowest = TRUE,
                                     labels = FALSE,
                                     right = TRUE)

table(final_data$vas_coverage_group, useNA = "ifany")
final_data

names(final_data)
names(congo_ch)
nrow(congo_ch); nrow(final_data)

keys <- c("region_id", "residence", "country", "child_age_dummy")
congo_ch_2 <- merge(congo_ch, final_data, by = keys, all.x = TRUE); names(congo_ch_2)

table(congo_ch_2$region_id)
table(congo_ch_2$distance_group) # 1 == less coverage, to 10 == more coverage, quantile(1:10)
summary(congo_ch_2$distance_nearest_facility_km)
table(congo_ch_2$wealth)
table(congo_ch_2$distance_facility)
table(congo_ch_2$vas_coverage_group) # 1 == -50% coverage, to 5 == +90% coverage, quantile(1:6)

path = 'C:\\Users\\stefa\\Documents\\Code\\'
write.csv(congo_ch_2, paste(path,'congo_ch_final.csv', sep = ''))
st_write(congo_ch_2, paste(path,'congo_ch_final.gpkg', sep = ''), append = FALSE)

# congo_ch_2 = rename(congo_ch_2, residence_binary = residence_binary.x,
#                     ANCV_min1 = ANCV_min1.x,
#                     ANCV_min4 = ANCV_min4.x)
# congo_ch_2 = congo_ch_2[, -c(46, 47, 60)]
# congo_ch_2 = st_drop_geometry(congo_ch_2)
# congo_ch_2 = congo_ch_2[, -36]
# write_dta(congo_ch_2, "congo_ch_final.dta")



# # # DISTANCE GROUPS BY REGION # # # # # # # # # # # # # # # # # # # # # # # # # # #
# summarized_data <- congo_child_distance %>%
#   group_by(region_id, distance_group, vas_coverage_group) %>%
#   summarise(weighted_children_perc = sum(children_perc * wt, na.rm = TRUE), .groups = 'drop')
# 
# total_weights <- summarized_data %>%
#   group_by(region_id) %>%
#   summarise(total_weight = sum(weighted_children_perc, na.rm = TRUE), .groups = 'drop')
# 
# summarized_data <- left_join(summarized_data, total_weights, by = "region_id")
# 
# summarized_data <- summarized_data %>%
#   mutate(weighted_children_perc = (weighted_children_perc / total_weight) * 100)
# 
# wide_data <- pivot_wider(summarized_data,
#                          names_from = region_id, 
#                          values_from = weighted_children_perc,
#                          names_prefix = "Region_",
#                          values_fill = list(weighted_children_perc = 0))
# 
# wide_data[is.na(wide_data)] <- 0
# wide_data <- wide_data %>%
#   mutate(across(starts_with("Region_"), ~./sum(.) * 100))
# print(wide_data, n = 22)
# 
# path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
# openxlsx::write.xlsx(wide_data,  paste(path,'congo_distance_groups_byregion.xlsx', sep = ''))
# 

 rm(list=setdiff(ls(), c("congo_ch_2", "congo_fa_GIS", "congo_hh_GIS")))


# # # ESTIMATE DE IMPACT ON VAS COVERAGE # # #
# Analyze the impact of distance in VAS coverage
congo = congo_ch_2; names(congo)

congo = rename(congo, vas = child_vitA_last6m,
               residence_binary = residence_binary.x
               ); names(congo)

congo$vas = case_when(congo$vas == 2 ~ 0, 
                      congo$vas == 3 ~ 0,
                      TRUE ~ congo$vas)
congo$vas = as.integer(congo$vas)
class(congo); class(congo$vas)
table(congo$vas)

table(congo$wealth)
congo$wealth_factor <- factor(congo$wealth,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# BINOMIAL MODEL DISTANCE & WEALTH #
form1 = vas ~ median_distance_km + wealth_factor + median_distance_km:wealth_factor
form1 = as.formula(form1)

probit1 = glm(form1, family = binomial(link = "probit"), data = congo, weights = wt, x = TRUE)
summary(probit1)
summary(congo$vas - probit1$fitted.values) 
summary(congo$median_distance_km)

coefficients1 = probit1$coefficients; coefficients1
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect1 = erer::maBina(probit1, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect1)


# BINOMIAL MODEL DISTANCE #
form2 = vas ~ median_distance_km
form2 = as.formula(form2)

probit2 = glm(form2, family = binomial(link = "probit"), data = congo, weights = wt, x = TRUE)
summary(probit2)
summary(congo$vas - probit2$fitted.values) 
summary(congo$median_distance_km)

coefficients2 = probit2$coefficients; coefficients2 
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect2 = erer::maBina(probit2, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect2)


# BINOMIAL ln(MODEL DISTANCE) & ln(WEALTH) #
form3 = vas ~ log(median_distance_km) + log(wealth)
form3 = as.formula(form3)

probit3 = glm(form3, family = binomial(link = "probit"), data = congo, weights = wt, x = TRUE)
summary(probit3)
summary(congo$vas - probit3$fitted.values) 
summary(congo$median_distance_km)

coefficients3 = probit3$coefficients; coefficients3
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect3 = erer::maBina(probit3, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect3)


# Save results
setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/VAS Coverage/results/"); getwd()
dir.create("congo_probit")

coefficients1 = as.table(coefficients1)
coefficients2 = as.table(coefficients2)
coefficients3 = as.table(coefficients3)
coefficients1 = as.data.frame(coefficients1)
coefficients2 = as.data.frame(coefficients2)
coefficients3 = as.data.frame(coefficients3)
coefficients1 = rename(coefficients1, Variable = Var1, Coefficient = Freq); coefficients1 
coefficients2 = rename(coefficients2, Variable = Var1, Coefficient = Freq); coefficients2
coefficients3 = rename(coefficients3, Variable = Var1, Coefficient = Freq); coefficients3

openxlsx::write.xlsx(coefficients1, 'congo_probit/congo_coefdistancewealth.xlsx')
openxlsx::write.xlsx(coefficients2, 'congo_probit/congo_coefdistance.xlsx')
openxlsx::write.xlsx(coefficients3, 'congo_probit/congo_coeflog.xlsx')

ma_effect1 <- as.data.frame(ma_effect1$out,
                            rownames_to_column = c("(Intercept)", "median_distance_km",
                                          "wealth_factorPoorer", "wealth_factorMiddle",
                                          "wealth_factorRicher", "wealth_factorRichest",
                                          "median_distance_km:wealth_factorPoorer",
                                          "median_distance_km:wealth_factorMiddle",
                                          "median_distance_km:wealth_factorRicher",
                                          "median_distance_km:wealth_factorRichest"))
ma_effect1 = zoo::fortify.zoo(ma_effect1)
ma_effect1$Index <- rownames(ma_effect1); ma_effect1


ma_effect2 <- as.data.frame(ma_effect2$out,
                            row.names = c("(Intercept)", "median_distance_km"))
ma_effect2 = zoo::fortify.zoo(ma_effect2)
ma_effect2$Index <- rownames(ma_effect2); ma_effect2


ma_effect3 <- as.data.frame(ma_effect3$out,
                            row.names = c("(Intercept)", "log(median_distance_km)", "log(wealth)"))
ma_effect3 = zoo::fortify.zoo(ma_effect3)
ma_effect3$Index <- rownames(ma_effect3); ma_effect3


openxlsx::write.xlsx(ma_effect1, 'congo_probit/congo_medistancewealth.xlsx')
openxlsx::write.xlsx(ma_effect2, 'congo_probit/congo_medistance.xlsx')
openxlsx::write.xlsx(ma_effect3, 'congo_probit/congo_melog.xlsx')

rm(list=setdiff(ls(), "congo"))

# # Model with random intercepts for clusters
# form1 = vas ~ median_distance_km + wealth_factor + (1 | cluster)
# probit1 = glmer(form1, family = binomial(link = "probit"), data = congo, weights = wt); summary(probit1)
# 
# # Model with interaction term and random intercepts
# form2 = vas ~ median_distance_km * wealth_factor + (1 | cluster)
# probit2 = glmer(form2, family = binomial(link = "probit"), data = congo, weights = wt); summary(probit2)
# 
# anova(probit1, probit2)


# # # SPATIAL REGRESSION # # #
# congo_sf = congo %>%
#  dplyr::select( "country", "country_code", "region_id", "region", "geometry", "residence_binary", "residence",
#           "children_perc", "caseid", "midx", "cluster", "household", "weight", "wt", "psu", "sample_stratum",
#           "children_prop", "wealth", "wealth_factor",
#           "distance_facility", "distance_group", "distance_nearest_facility_km",
#           "min_distance_km", "mean_distance_km", "median_distance_km", "max_distance_km",
#           "child_age","child_is_under_59", "child_vitA_recent", "vas", "vas_coverage", "vas_coverage_group",
#           "DPT1", "DPT2", "DPT3", "months_ANCV", "amount_ANCV", "ANCV_min1", "ANCV_min4")
# names(congo_sf)
# summary(congo_sf$geometry)
# congo_sf <- st_as_sf(congo_sf); class(congo_sf)
# mapview(congo_sf)
#
# congo_sf <- congo_sf %>%
#   dplyr::filter(!is.na(median_distance_km), !is.na(wealth_factor), !is.na(vas_coverage)) %>%
#   dplyr::mutate(coord_na = is.na(st_coordinates(.)[,1])) %>%
#   dplyr::filter(!coord_na) %>%
#   dplyr::select(-coord_na)
#
# form = vas_coverage ~ median_distance_km + wealth_factor
# ols = lm(form, congo_sf)
# summary(ols)
#
# coords <- st_coordinates(congo_sf)
# sum(is.na(coords))
# coords <- coords[complete.cases(coords), ]
#
# set.seed(123)
# coords_jittered <- jitter(coords, amount = 0.0001)
# knn <- spdep::knearneigh(coords, k = 4)
# nb <- spdep::knn2nb(knn, row.names = NULL)
# lw <- spdep::nb2listw(nb, style = "B", zero.policy = TRUE)
#
# sar_model <- spatialreg::lagsarlm(vas_coverage ~ median_distance_km + wealth_factor, data = congo_sf, listw = lw, zero.policy = TRUE, na.action = na.omit)
#

# rm(list=setdiff(ls(), c("congo", "ols")))
#
# # # # DECISION TREE # # #
table(congo$region)
congo$region_name = case_when(congo$region_id == 1 ~ "Kinshasa",
                              congo$region_id == 2 ~ "Bandundu",
                              congo$region_id == 3 ~ "Bas_Congo",
                              congo$region_id == 4 ~ "Equateur",
                              congo$region_id == 5 ~ "Kasai_Occidental",
                              congo$region_id == 6 ~ "Kasai_Oriental",
                              congo$region_id == 7 ~ "Katanga",
                              congo$region_id == 8 ~ "Maniema",
                              congo$region_id == 9 ~ "Nord_Kivu",
                              congo$region_id == 10 ~ "Orientale",
                              congo$region_id == 11 ~ "Sud_Kivu")
table(congo$region_name)
names(congo)

# Probit by regions
summary(congo$vas_coverage) # children's VAS coverage in %
table(congo$vas) # dummy 0 == NO Vit A delivered, 1 == YES Vit A delivered
table(congo$region_id)
table(congo$region_name)
table(congo$residence) 
table(congo$residence_binary) # Rural == 0, Urban == 1
summary(congo$median_distance_km) # median distance housholds to nearest medical facility
table(congo$wealth_factor) # children's household wealth
table(congo$wealth)

congo$region_id <- as.factor(congo$region_id)
congo$residence_rural = ifelse(congo$residence_binary == 0, 1, 0)

form = vas ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id
form = as.formula(form); form

probit_reg = glm(form, family = binomial(link = "probit"), data = congo, weights = wt, x = TRUE)

summary(probit_reg)
confint_table = confint(probit_reg, level = 0.95); confint_table

coefficients_table = summary(probit_reg)$coefficients; coefficients_table
coefficients_df = as.data.frame(coefficients_table); coefficients_df
coefficients_df$Variable = rownames(coefficients_df); coefficients_df

colnames(coefficients_df) = c("Estimate", "Std. Error", "z value", "P-value", "Variable"); names(coefficients_df)
coefficients_df = coefficients_df[c("Variable", "Estimate", "Std. Error", "z value", "P-value")]; names(coefficients_df)

openxlsx::write.xlsx(coefficients_df, "congo_probit/regprob_coefficients_table.xlsx")
openxlsx::write.xlsx(as.data.frame(confint_table), file = "congo_probit/regprob_confint_table.xlsx", sheetName = "Confidence Intervals", rowNames = TRUE)


# Beta Regression by regions
library(betareg)
congo$vas_coverage_scaled = congo$vas_coverage / 100
beta_reg = betareg(vas_coverage_scaled ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id, data = congo)
summary(beta_reg)

confint(beta_reg, level = 0.95)

# Transformed OLS by regions
congo$vas_coverage_logit = log(congo$vas_coverage_scaled / (1 - congo$vas_coverage_scaled))
ols_reg = lm(vas_coverage_logit ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id, data = congo)
summary(ols_reg)

# compare
AIC(probit_reg, beta_reg, ols_reg)


setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/VAS Coverage/"); getwd()
congo_limits = st_read("data_files/Congo/GPS/congo_limits/congo_limits.shp"); names(congo_limits)

# Random Forest
library(caret)
library(raster)
library(sf) 
library(doParallel)
library(foreach)
names(congo)
summary(congo$geometry)
names(congo_limits)
summary(congo_limits$geometry)

table(congo$region_id)
table(congo$region_name)
table(congo_limits$NAME_1)

congo_limits <- congo_limits %>%
  mutate(NAME_1 = case_when(
    NAME_1 %in% c("Mai-Ndombe", "Kwilu", "Kwilu", "Kwango") ~ "Bandundu",
    NAME_1 %in% c("Kongo-Central", "Kwango") ~ "Bas_Congo",
    NAME_1 %in% c("Équateur", "Equateur", "Tshuapa", "Mongala", "Nord-Ubangi", "Sud-Ubangi") ~ "Equateur",
    NAME_1 %in% c("Kasai", "Kasaï", "Kasaï-Central") ~ "Kasai_Occidental",
    NAME_1 %in% c("Kasaï-Oriental", "Kasai Oriental", "Lomami", "Sankuru") ~ "Kasai_Oriental",
    NAME_1 %in% c("Lualaba", "Haut-Lomami", "Tanganyika", "Haut-Katanga") ~ "Katanga",
    NAME_1 %in% c("Bas-Uele", "Tshopo", "Ituri", "Haut-Uele") ~ "Orientale",
    NAME_1 %in% c("Nord-Kivu") ~ "Nord_Kivu",
    NAME_1 %in% c("Sud-Kivu") ~ "Sud_Kivu",
    TRUE ~ NAME_1
  ))

table(congo$region_name)
table(congo_limits$NAME_1)

congo_limits <- congo_limits %>%
  mutate(region_id = case_when(
    NAME_1 == "Kinshasa"         ~ 1,
    NAME_1 == "Bandundu"         ~ 2,
    NAME_1 == "Bas_Congo"        ~ 3,
    NAME_1 == "Equateur"         ~ 4,
    NAME_1 == "Kasai_Occidental" ~ 5,
    NAME_1 == "Kasai_Oriental"   ~ 6,
    NAME_1 == "Katanga"          ~ 7,
    NAME_1 == "Maniema"          ~ 8,
    NAME_1 == "Nord_Kivu"        ~ 9,
    NAME_1 == "Orientale"        ~ 10,
    NAME_1 == "Sud_Kivu"         ~ 11
  ))
table(congo_limits$region_id)

summary(congo$vas_coverage)
congo$vas_coverage = replace(congo$vas_coverage, is.na(congo$vas_coverage), 0); summary(congo$vas_coverage)
table(congo$child_age_dummy) # if child is above 24 months old == 1, 0 if under
summary(congo$median_distance_km) # median distance to nearest facility
table(congo$wealth_factor) # children living in household's wealth
table(congo$residence_rural) # == 1 if children lives in rural, == 0 if urban

set.seed(123)
form = vas_coverage ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id
form = as.formula(form)

vars <- c("vas_coverage", "child_age_dummy", "median_distance_km", "wealth_factor", "residence_rural", "region_id")
sapply(congo[vars], function(x) sum(is.na(x))) # Check for missing values in these variables

impute_value <- function(x) {
  if(is.numeric(x)) {
    return(ifelse(is.na(x), median(x, na.rm = TRUE), x))
  } else {
    mode <- names(which.max(table(x)))
    return(ifelse(is.na(x), mode, x))
  }
}

congo[vars] <- lapply(congo[vars], impute_value)
sapply(congo[vars], function(x) sum(is.na(x)))

split <- createDataPartition(congo$vas_coverage, p = 0.75, list = FALSE)[,1]
train_set <- congo[split, ]
test_set <- congo[-split, ]

sapply(train_set[vars], function(x) sum(is.na(x)))

numCores <- parallel::detectCores()
registerDoParallel(cores = numCores)
train_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

{
model <- train(form, 
               data = train_set, 
               method = "rf",
               trControl = train_control)
print(model)
stopImplicitCluster()
}

predictions <- predict(model, newdata = test_set)
summary(predictions)
test_set$predicted_vas_coverage <- predictions
region_coverage <- aggregate(predicted_vas_coverage ~ region_id, data = test_set, mean)

unique(test_set$region_id)
unique(congo_limits$region_id)

congo_limits$region_id <- as.character(congo_limits$region_id)
region_coverage$region_id <- as.character(region_coverage$region_id)

congo_limits <- merge(congo_limits, region_coverage, by = "region_id", all.x = TRUE)
str(congo_limits)

summary(congo_limits$predicted_vas_coverage)
congo_limits$predicted_vas_coverage[is.na(congo_limits$predicted_vas_coverage)] <- 0

library(viridis)
summary(congo_limits$predicted_vas_coverage)
breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 1)
color_count <- length(breaks) - 1
colors <- viridis::viridis(color_count)

coverage_map <- mapview(congo_limits, zcol = "predicted_vas_coverage",
                        col.regions = colors,
                        at = breaks, alpha.regions = 1)
print(coverage_map)

library(ggplot2)
library(sf)
gg_map <- ggplot() +
  geom_sf(data = congo_limits, aes(fill = predicted_vas_coverage), colour = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Predicted VAS Coverage %",
    breaks = breaks[-length(breaks)],
    labels = scales::percent(breaks[-length(breaks)]),
    limits = c(0, 1),
    guide = guide_legend(title.position = "top")
  ) +
  labs(
    title = "Random Forest Model: Predicted VAS Coverage",
    subtitle = "Vas_Coverage = Child_Age + Distance_NearFa + HH_Wealth_Factor + Residence_Rural + Region_Incidence",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(gg_map)
ggsave("Maps/Predicted_VAS_Coverage_Congo.png", gg_map, width = 10, height = 8, dpi = 300)


# congo_limits_utm <- st_transform(congo_limits, crs = 32632)
# library(raster)
# cell_size <- sqrt(250000)
# raster_template <- raster(extent(congo_limits_utm), res = c(cell_size, cell_size))
# crs(raster_template) <- crs(congo_limits_utm)
# rasterized_data <- rasterize(congo_limits_utm, raster_template, field = "predicted_vas_coverage", fun = mean, background = NA)
# 
# library(raster)
# writeRaster(rasterized_data, filename = "high_res_vas_coverage.tif", format = "GTiff")
# 
# library(rasterVis)
# levelplot(rasterized_data, col.regions = viridis::viridis(100), margin = FALSE)
# 
# summary(congo_limits)
# 
rm(list=setdiff(ls(), c("congo", "congo_limits")))



# # # # # # # # # Maps # # # # # # # # # 

table(congo$cluster) # clusters that group children/households across survey
summary(congo$vas_coverage) # Vit A Sup Coverage in %
names(congo) # children/households points dataset
names(congo_limits) # country regions layer dataset

library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(Hmisc)

# Histogram 

hist(congo$vas_coverage)
hist_vas = ggplot(congo, aes(x=vas_coverage)) +
  geom_histogram(bins=30, fill=rgb(1, 0, 0, 0.5), color="black") +
  scale_x_continuous(breaks=seq(0, 1, by=0.1), limits=c(0, 1)) +
  labs(x = "VAS Coverage (%)", 
       y = "Number of Children (6-59 months)", 
       title = "DRC Children VAS Coverage Distribution",
       caption = "Data source: DHS Survey Datasets") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(plot.caption = element_text(hjust=0, face="italic"))  # Align and style the caption text
print(hist_vas)
ggsave("results/histogram_vascov_congo.png", hist_vas, width = 10, height = 8, dpi = 300)


# Vas_Coverage by clusters
cluster_summary <- congo %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary)

cluster_summary$distance[is.na(cluster_summary$distance)] = 10.910
cluster_summary$distance_pub[is.na(cluster_summary$distance_pub)] = 17.0255
summary(cluster_summary)

congo_clusters <- congo %>%
  distinct(cluster, .keep_all = TRUE) %>%
  st_as_sf()  

cluster_summary <- cluster_summary %>%
  left_join(congo_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary <- st_as_sf(cluster_summary, sf_column_name = "geometry"); str(cluster_summary)

congo_limits <- st_as_sf(congo_limits)

summary(cluster_summary$vas_coverage)

library(RColorBrewer)
breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1) 
labels <- scales::percent(breaks)

gg_map_vas <- ggplot() +
  geom_sf(data = congo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "DRC Average VAS Coverage by Cluster | Children 6-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas)
ggsave("Maps/Cluster_VAS_Coverage_Congo.png", gg_map_vas, width = 10, height = 8, dpi = 300)


# Vas_Coverage by clusters for children under and above 24 months old
table(congo$child_age_dummy) # comes from ifelse(congo_child$child_age >= 24, 1, 0)

# under 24 months
cluster_summary_under24 <- congo %>%
  filter(child_age_dummy == 0, vas_coverage > 0) %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_under24)

cluster_summary_under24$distance[is.na(cluster_summary_under24$distance)] = 10.881
cluster_summary_under24$distance_pub[is.na(cluster_summary_under24$distance_pub)] = 16.781
summary(cluster_summary_under24)

cluster_summary_under24 <- cluster_summary_under24 %>%
  left_join(congo_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary_under24 <- st_as_sf(cluster_summary_under24, sf_column_name = "geometry")


# above 24 months
cluster_summary_above24 <- congo %>%
  filter(child_age_dummy == 1, vas_coverage > 0) %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_above24)

cluster_summary_above24$distance[is.na(cluster_summary_above24$distance)] = 11.105
cluster_summary_above24$distance_pub[is.na(cluster_summary_above24$distance_pub)] = 17.101
summary(cluster_summary_above24)

cluster_summary_above24 <- cluster_summary_above24 %>%
  left_join(congo_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary_above24 <- st_as_sf(cluster_summary_above24, sf_column_name = "geometry")


gg_map_vas_under24 <- ggplot() +
  geom_sf(data = congo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_under24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "DRC Average VAS Coverage by Cluster | Children 6-24 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_under24)
ggsave("Maps/VAS_Coverage_Under24_Congo.png", gg_map_vas_under24, width = 10, height = 8, dpi = 300)


summary(cluster_summary_above24$vas_coverage)
gg_map_vas_above24 <- ggplot() +
  geom_sf(data = congo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_above24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "DRC Average VAS Coverage by Cluster | Children 24-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_above24)
ggsave("Maps/VAS_Coverage_Above24_Congo.png", gg_map_vas_above24, width = 10, height = 8, dpi = 300)

# Histogram
hist_vas_sep = ggplot() +
  geom_histogram(data = cluster_summary_under24, aes(x = vas_coverage, fill = "6-24 months"), bins = 30, alpha = 0.5) +
  geom_histogram(data = cluster_summary_above24, aes(x = vas_coverage, fill = "24-59 months"), bins = 30, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  scale_fill_manual(values = c("6-24 months" = rgb(1, 0, 0, 0.5), "24-59 months" = rgb(0, 0, 1, 0.5)),
                    name = "Age Group", 
                    labels = c("6-24 months", "24-59 months")) +
  labs(x = "VAS Coverage (%)", 
       y = "Children",
       title = "VAS Coverage Distribution by Age Group",
       subtitle = "Red: 6-24 months, Blue: 24-59 months") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Age Group",
                             override.aes = list(colour = c("red", "blue"), size = 4))) +
  theme(legend.position = "topright") 
print(hist_vas_sep)
ggsave("results/histogram_vassep_congo.png", hist_vas_sep, width = 10, height = 8, dpi = 300)


# For Nearest Facility by Clusters with adjusted scale
gg_map_distance <- ggplot() +
  geom_sf(data = congo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = distance, color = distance, size = total_weight), shape = 21, alpha = 0.8) +
  scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                        values = rescale(c(0, 60, 125), to = c(0, 1)),
                        name = "Distance to nearest facility (km)",
                        limits = c(0, 125),
                        breaks = c(0, 30, 60, 90, 125),
                        labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
  scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                       values = rescale(c(0, 60, 125), to = c(0, 1))) +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "DRC Distance to Nearest Health Facility by Cluster",
    subtitle = "Colored by distance in kilometers",
    caption = "Source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
print(gg_map_distance)
ggsave("Maps/Distance_to_Facility_Congo.png", gg_map_distance, width = 12, height = 10, dpi = 300)

# For Nearest Public Facility by Clusters
# gg_map_distance_pub <- ggplot() +
#   geom_sf(data = congo_limits, fill = NA, color = "gray", size = 0.2) +
#   geom_sf(data = cluster_summary, aes(fill = distance_pub, color = distance_pub, size = total_weight), shape = 21, alpha = 0.8) +
#   scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
#                         values = rescale(c(0, 60, 125), to = c(0, 1)),
#                         name = "Distance to public facility (km)",
#                         limits = c(0, 125),
#                         breaks = c(0, 30, 60, 90, 125),
#                         labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
#   scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
#                        values = rescale(c(0, 60, 125), to = c(0, 1))) +
#   scale_size(range = c(3, 15), name = "Total Children Weight") +
#   labs(
#     title = "Distance to Nearest Public Health Facility by Cluster",
#     subtitle = "Colored by Distance in kilometers",
#     caption = "Source: DHS Survey Datasets"
#   ) +
#   theme_minimal() +
#   guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
# gg_map_distance_pub
# ggsave("Maps/Distance_to_Public_Facility_Congo.png", gg_map_distance_pub, width = 12, height = 10, dpi = 300)

# Facility locations and type
congo_fa_GIS = st_read("data_files/Congo/GPS/CDGE71FLSR_SPA_faci/CDGE71FLSR.shp") # Facility-GPS points
names(congo_fa_GIS)
summary(congo_fa_GIS$geometry)
table(congo_fa_GIS$SPATYPEN) # type of facility (hospital or others)
table(congo_fa_GIS$SPATYPEC) # dummy for type of facility (should group Hospitals vs Other)
table(congo_fa_GIS$SPAMANGN) # ownership facility (public or private)
table(congo_fa_GIS$SPAMANGC) # dummy for ownership facility (should group Public vs Private)

congo_fa_GIS$FacilityType <- ifelse(congo_fa_GIS$SPATYPEC %in% c(1, 4, 5), 'Other', 'Hospital')
congo_fa_GIS$Ownership <- ifelse(congo_fa_GIS$SPAMANGC == 1, 'Public', 'Private')

table(congo_fa_GIS$FacilityType)
table(congo_fa_GIS$Ownership)

gg_facility_map <- ggplot() +
  geom_sf(data = congo_limits, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = congo_fa_GIS, aes(color = Ownership, shape = FacilityType, geometry = geometry), size = 2) +
  scale_color_manual(values = c("Public" = "purple", "Private" = "#FF7F50")) +
  scale_shape_manual(values = c("Hospital" = 19, "Other" = 15)) +
  labs(title = "Health Facilities",
       subtitle = "Facility Type and Ownership",
       caption = "Source: GADM | DHS Survey Datasets",
       color = "Ownership",
       shape = "Facility Type") +
  theme_minimal()
print(gg_facility_map)
ggsave("Maps/Facility_classification_Congo.png", gg_facility_map, width = 12, height = 10, dpi = 300)

# Public Facility locations
# table(congo_fa_GIS$Ownership)
# congo_fa_public = congo_fa_GIS %>%
#   filter(Ownership == "Public"); table(congo_fa_public$Ownership)
# 
# public_facility_map <- ggplot() +
#   geom_sf(data = congo_limits, fill = "lightgray", color = "black", size = 0.2) +
#   geom_sf(data = congo_fa_public, aes(color = Ownership, shape = FacilityType, geometry = geometry), size = 2) +
#   scale_color_manual(values = c("Public" = "purple")) +
#   scale_shape_manual(values = c("Hospital" = 19, "Other" = 15)) +
#   labs(title = "Health Public Facilities",
#        subtitle = "By Facility Type",
#        caption = "Source: DHS Survey Datasets",
#        color = "Ownership",
#        shape = "Facility Type") +
#   theme_minimal()
# print(public_facility_map)
# ggsave("Maps/PubFacility_classification_Congo.png", public_facility_map, width = 12, height = 10, dpi = 300)

# Amount of facilities per region
summary(congo_fa_GIS$geometry) # facility coords
table(congo_fa_GIS$ADM1NAME) # regions/provinces
table(congo_fa_GIS$Ownership) # public/private
table(congo_fa_GIS$FacilityType) #hospital/other

fa_per_reg <- congo_fa_GIS %>%
  group_by(ADM1NAME) %>%
  summarise(
    Private_Facilities = sum(Ownership == "Private"),
    Public_Facilities = sum(Ownership == "Public"),
    Hospitals = sum(FacilityType == "Hospital"),
    Other_Facilities = sum(FacilityType == "Other"),
    Total_Facilities = n()
  ) %>%
  ungroup()

fa_per_reg_total <- fa_per_reg %>%
  bind_rows(summarise(fa_per_reg, across(where(is.numeric), sum), .groups = "drop")) %>%
  mutate(ADM1NAME = replace_na(ADM1NAME, "Total"))
fa_per_reg_total = st_drop_geometry(fa_per_reg_total); fa_per_reg_total

openxlsx::write.xlsx(fa_per_reg_total, "results/Congo_Health_Facilities_Per_Region.xlsx", rowNames = FALSE)

              
# Road lengths (KM), population and transport costs
fuel_price = 1.1 # pump price for diesel fuel (US$ per liter) # we assume == U$D 1.10
fuel_cost_per_km <- (fuel_price / 12) # As a general rule, a gasoline car consumes one liter per 12 kilometers, which translates into a consumption of 4 to 12 liters of fuel per 100 kilometers.

names(congo)
summary(congo$geometry)
summary(congo_limits$geometry)

congo_limits = st_read("data_files/Congo/GPS/congo_limits/congo_limits.shp"); names(congo_limits)
congo_roads = st_read("data_files/Congo/GPS/congo_roads/congo_roads.shp"); names(congo_roads) # Congo Roads DATA #
table(congo_roads$NTLCLASS)
congo_roads2 = subset(congo_roads, NTLCLASS == "DRC", drop = FALSE)
congo_roads = congo_roads2; rm(congo_roads2)
mapview(congo_limits, color = "red") + mapview(congo_roads, color = "green")
summary(congo_roads$LENGTH_KM) # I should calculate the transport costs for Congo

congo_popul = openxlsx::read.xlsx("data_files/Congo/DRC_population and density.xlsx")
colnames(congo_popul) <- c("Province", "Pop_density_per_km2_2019", "Area_km2", "Population_2019", "Old_region"); names(congo_popul)

table(congo$region_name)
table(congo_limits$NAME_1)
table(congo_popul$Old_region) # Old regions match with raster data
table(congo_popul$Province) # New regions

congo_roads <- st_transform(congo_roads, st_crs(congo_limits))
roads_by_province <- st_join(congo_roads, congo_limits, join = st_intersects)
table(roads_by_province$NAME_1)

province_road_lengths <- roads_by_province %>%
  group_by(NAME_1) %>%
  dplyr::summarize(total_road_length_km = sum(LENGTH_KM, na.rm = TRUE)); summary(province_road_lengths$total_road_length_km)

table(province_road_lengths$NAME_1)
table(congo_popul$Province)

province_data <- left_join(province_road_lengths, congo_popul, by = c("NAME_1" = "Province")); province_data

province_data <- province_data %>%
  mutate(per_capita_transport_cost = (total_road_length_km * fuel_cost_per_km) / Population_2019); province_data

summary(province_data$total_road_length_km)
table(province_data$NAME_1)
table(congo_limits$NAME_1)

province_data_no_geom <- st_drop_geometry(province_data)

merged_data <- left_join(congo_limits, province_data_no_geom, by = "NAME_1")

merged_data = rename(merged_data, Province = NAME_1); names(merged_data)
merged_data$total_road_length_km <- round(merged_data$total_road_length_km, 0)
summary(merged_data$total_road_length_km)

colors <- c("darkgreen", "#1B9E77", "orange", "red", "darkred")
values <- rescale(c(0, 0.5, 1))
gg_map_costs = ggplot(data = merged_data) +
  geom_sf(aes(fill = per_capita_transport_cost), color = "white") +
  geom_sf_text(aes(label = paste(Province, '\nPopulation:', Population_2019, '\nRoad Km:', round(total_road_length_km))), 
               size = 3, check_overlap = TRUE) +
  scale_fill_gradientn(colors = colors, 
                       values = values, 
                       name = "Per Capita\nTransport Cost", 
                       labels = scales::comma, 
                       limits = range(merged_data$per_capita_transport_cost, na.rm = TRUE)) +
  labs(title = "Per Capita Transport Costs",
       subtitle = "By province in the Democratic Republic of the Congo",
       caption = "Source: GADM | Global Roads Open Access Data Set (NASA)") +
  theme_minimal()
print(gg_map_costs)
ggsave("Maps/transport_costs_Congo.png", gg_map_costs, width = 12, height = 10, dpi = 300)

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/VAS Cost Optimization/'
st_write(congo, paste(path,'congo_child_final.gpkg', sep = ''), layer = "congo_with_provinces", driver = "GPKG")

rm(list=setdiff(ls(), c("congo")))
#congo = st_read("congo_child_final.gpkg")

names(congo)
table(congo$region_id) # DHS Region Index
table(congo$region)
table(congo$region_name)
summary(congo$cluster)
summary(congo$geometry)

congo2 = st_as_sf(congo)
mapview::mapview(congo2)

setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/VAS Cost Optimization/DRC GIS/"); getwd()
congo_provinces = st_read("congo_provinces.shp")
table(congo_provinces$NAME_1)
summary(congo_provinces$geometry)
mapview::mapview(congo_provinces)

congo_with_provinces <- st_join(congo2, congo_provinces, join = st_within)

head(congo_with_provinces)

table(congo_with_provinces$NAME_1)

congo_with_provinces <- congo_with_provinces %>%
  rename(province_name = NAME_1)

region_to_province <- openxlsx::read.xlsx("region_to_province.xlsx")

table(congo_with_provinces$province_name)
table(region_to_province$Province)

congo_with_provinces <- congo_with_provinces %>%
  left_join(region_to_province, by = c("province_name" = "Province"))

names(congo_with_provinces)
congo2 = congo_with_provinces[,-c(70, 71, 72, 74, 75, 76, 77, 78, 79, 80, 82, 83)]
names(congo2)

congo_without_geometry <- congo2 %>%
  st_set_geometry(NULL)
openxlsx::write.xlsx(congo_without_geometry, "congo_with_provinces.xlsx")
st_write(congo2, "congo_with_provinces.gpkg", layer = "congo_with_provinces", driver = "GPKG", append=FALSE)

congo = congo_without_geometry
rm(list=setdiff(ls(), c("congo")))

names(congo)
table(congo$Province_id)
table(congo$child_age_dummy)
summary(congo$median_distance_km)


congo_provinces <- congo %>%
  group_by(Province_id, province_name) %>%
  summarise(
    country = first(country),  
    region_id = first(region_id),  
    region_name = first(region_name), 
    under_24_prop = sum(ifelse(child_age_dummy == 0, wt, 0)) / sum(wt),
    above_24_prop = sum(ifelse(child_age_dummy == 1, wt, 0)) / sum(wt),
    
    near_facility_under_24 = sum(ifelse(child_age_dummy == 0 & distance_nearest_facility_km <= 20, wt, 0)) / sum(ifelse(child_age_dummy == 0, wt, 0)),
    near_facility_above_24 = sum(ifelse(child_age_dummy == 1 & distance_nearest_facility_km <= 20, wt, 0)) / sum(ifelse(child_age_dummy == 1, wt, 0)),
    
    away_facility_under_24 = sum(ifelse(child_age_dummy == 0 & distance_nearest_facility_km > 20, wt, 0)) / sum(ifelse(child_age_dummy == 0, wt, 0)),
    away_facility_above_24 = sum(ifelse(child_age_dummy == 1 & distance_nearest_facility_km > 20, wt, 0)) / sum(ifelse(child_age_dummy == 1, wt, 0)),
    
    average_distance_near_under_24 = weighted.mean(distance_nearest_facility_km[child_age_dummy == 0 & distance_nearest_facility_km <= 20], 
                                                   wt[child_age_dummy == 0 & distance_nearest_facility_km <= 20], na.rm = TRUE),
    average_distance_near_above_24 = weighted.mean(distance_nearest_facility_km[child_age_dummy == 1 & distance_nearest_facility_km <= 20], 
                                                   wt[child_age_dummy == 1 & distance_nearest_facility_km <= 20], na.rm = TRUE),
    
    average_distance_away_under_24 = weighted.mean(distance_nearest_facility_km[child_age_dummy == 0 & distance_nearest_facility_km > 20], 
                                                   wt[child_age_dummy == 0 & distance_nearest_facility_km > 20], na.rm = TRUE),
    average_distance_away_above_24 = weighted.mean(distance_nearest_facility_km[child_age_dummy == 1 & distance_nearest_facility_km > 20], 
                                                   wt[child_age_dummy == 1 & distance_nearest_facility_km > 20], na.rm = TRUE),
    
    urban_proportion_under_24 = sum(ifelse(child_age_dummy == 0 & residence_binary == 1, wt, 0)) / sum(ifelse(child_age_dummy == 0, wt, 0)),
    urban_proportion_above_24 = sum(ifelse(child_age_dummy == 1 & residence_binary == 1, wt, 0)) / sum(ifelse(child_age_dummy == 1, wt, 0)),
    
    vas_coverage_under_24 = sum(ifelse(child_age_dummy == 0 & vas == 1, wt, 0)) / sum(ifelse(child_age_dummy == 0, wt, 0)),
    vas_coverage_above_24 = sum(ifelse(child_age_dummy == 1 & vas == 1, wt, 0)) / sum(ifelse(child_age_dummy == 1, wt, 0))
  )
drc_total <- congo %>%
  summarise(
    Province_id = 27,
    province_name = "DRC Total",
    country = first(country),  
    region_id = 27, 
    region_name = "DRC Total",
    under_24_prop = sum(ifelse(child_age_dummy == 0, wt, 0)) / sum(wt),
    above_24_prop = sum(ifelse(child_age_dummy == 1, wt, 0)) / sum(wt),
    
    near_facility_under_24 = sum(ifelse(child_age_dummy == 0 & distance_nearest_facility_km <= 20, wt, 0)) / sum(ifelse(child_age_dummy == 0, wt, 0)),
    near_facility_above_24 = sum(ifelse(child_age_dummy == 1 & distance_nearest_facility_km <= 20, wt, 0)) / sum(ifelse(child_age_dummy == 1, wt, 0)),
    
    away_facility_under_24 = sum(ifelse(child_age_dummy == 0 & distance_nearest_facility_km > 20, wt, 0)) / sum(ifelse(child_age_dummy == 0, wt, 0)),
    away_facility_above_24 = sum(ifelse(child_age_dummy == 1 & distance_nearest_facility_km > 20, wt, 0)) / sum(ifelse(child_age_dummy == 1, wt, 0)),
    
    average_distance_near_under_24 = weighted.mean(distance_nearest_facility_km[child_age_dummy == 0 & distance_nearest_facility_km <= 20], 
                                                   wt[child_age_dummy == 0 & distance_nearest_facility_km <= 20], na.rm = TRUE),
    average_distance_near_above_24 = weighted.mean(distance_nearest_facility_km[child_age_dummy == 1 & distance_nearest_facility_km <= 20], 
                                                   wt[child_age_dummy == 1 & distance_nearest_facility_km <= 20], na.rm = TRUE),
    
    average_distance_away_under_24 = weighted.mean(distance_nearest_facility_km[child_age_dummy == 0 & distance_nearest_facility_km > 20], 
                                                   wt[child_age_dummy == 0 & distance_nearest_facility_km > 20], na.rm = TRUE),
    average_distance_away_above_24 = weighted.mean(distance_nearest_facility_km[child_age_dummy == 1 & distance_nearest_facility_km > 20], 
                                                   wt[child_age_dummy == 1 & distance_nearest_facility_km > 20], na.rm = TRUE),
    
    urban_proportion_under_24 = sum(ifelse(child_age_dummy == 0 & residence_binary == 1, wt, 0)) / sum(ifelse(child_age_dummy == 0, wt, 0)),
    urban_proportion_above_24 = sum(ifelse(child_age_dummy == 1 & residence_binary == 1, wt, 0)) / sum(ifelse(child_age_dummy == 1, wt, 0)),
    
    vas_coverage_under_24 = sum(ifelse(child_age_dummy == 0 & vas == 1, wt, 0)) / sum(ifelse(child_age_dummy == 0, wt, 0)),
    vas_coverage_above_24 = sum(ifelse(child_age_dummy == 1 & vas == 1, wt, 0)) / sum(ifelse(child_age_dummy == 1, wt, 0))
  )

congo_provinces <- congo_provinces %>%
  filter(!is.na(Province_id)) %>%  
  bind_rows(drc_total) %>%  
  arrange(Province_id)  
names(congo_provinces)

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/VAS Cost Optimization/'
openxlsx::write.xlsx(congo_provinces, paste(path,'congo_province_summary.xlsx', sep = ''))


congo_summary <- congo %>%
  group_by(Province_id, child_age_dummy, distance_group = ifelse(distance_nearest_facility_km <= 20, "near", "away")) %>%
  summarise(
    vas_coverage = sum(ifelse(vas == 1, wt, 0)) / sum(wt),
    .groups = 'drop'
  ) %>%
  mutate(
    age_group = ifelse(child_age_dummy == 0, "under_24m", "above_24m")
  ) %>%
  dplyr::select(Province_id, age_group, distance_group, vas_coverage) %>%
  arrange(Province_id, age_group, distance_group)
print(congo_summary)

congo_total <- congo %>%
  group_by(child_age_dummy, distance_group = ifelse(distance_nearest_facility_km <= 20, "near", "away")) %>%
  summarise(
    vas_coverage = sum(ifelse(vas == 1, wt, 0)) / sum(wt),
    .groups = 'drop'
  ) %>%
  mutate(
    age_group = ifelse(child_age_dummy == 0, "under_24m", "above_24m"),
    Province_id = 27 
  ) %>%
  dplyr::select(Province_id, age_group, distance_group, vas_coverage) %>%
  arrange(age_group, distance_group)

congo_summary_final <- congo_summary %>%
  bind_rows(congo_total) %>%
  arrange(Province_id, age_group, distance_group)
print(congo_summary_final)

path <- 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/VAS Cost Optimization/'
openxlsx::write.xlsx(congo_summary_final, paste0(path, 'congo_summary_24sep.xlsx'))





  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #





# TOGO #
{
  # togo_IR <- read_dta("data_files/Togo/TGIR61FL.DTA") # Individual Women's Data - Individual Recode (IR)
  # togo_BR <- read_dta("data_files/Togo/TGBR61FL.DTA") # Births' data - Birth's Recode (BR)
}

# HH %% # 

togo_HR <- read_dta("data_files/Togo/TGHR61FL.DTA") # Household Data - Household Recode (HR)
togo_HR$country = "Togo"

togo <- togo_HR %>%
  group_by(hhid) %>%
  summarise(
    country = country,
    country_code = hv000,
    sample_year = hv007,
    sample_month = hv006,
    weight = hv005,
    wt = (hv005/1000000),
    psu = hv021,
    sample_stratum = hv022,
    sample_domain = hv023,
    cluster = hv001,
    household = hv002,
    wealth = hv270,
    # geo
    region_id = hv024,
    region = case_when(hv024 == 1 ~ "Region 1", 
                       hv024 == 2 ~ "Region 2",
                       hv024 == 3 ~ "Region 3", 
                       hv024 == 4 ~ "Region 4",
                       hv024 == 5 ~ "Region 5",
                       hv024 == 6 ~ "Region 6"),
    residence_id = hv025,
    residence = ifelse(hv025 == 1, "Urban", "Rural")
  ) 

togo$residence_binary <- ifelse(togo$residence_id == 1, 1, 0) # Convert to binary: 1 for Urban, 0 for Rural

togo_design <- svydesign(id = ~psu, weights = ~wt, data = togo, nest = TRUE)

urban_households_proportion <- plyr::ddply(togo, ~region_id , summarise , mean = weighted.mean(residence_binary, wt)) 
rural_households_proportion = 1 - urban_households_proportion
rural_households_proportion$region_id = 1:6

proportions_urban <- urban_households_proportion %>% 
  dplyr::select(region_id, mean) %>% 
  rename(residence_prop = mean) %>% 
  mutate(residence = "Urban")

proportions_rural <- rural_households_proportion %>% 
  dplyr::select(region_id, mean) %>% 
  rename(residence_prop = mean) %>% 
  mutate(residence = "Rural")

proportions <- bind_rows(proportions_urban, proportions_rural) %>%
  arrange(region_id, residence)

togo <- togo %>%
  left_join(proportions, by = c("region_id", "residence")) %>%
  mutate(
    residence_perc = residence_prop * 100
  )

proportions$country = "Togo"

table(togo$residence_perc)
table(togo$residence_prop)

togo = as.data.frame(togo)
proportions = as.data.frame(proportions)

proportions = rename(proportions, household_prop = residence_prop)
proportions

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(togo,  paste(path,'togo_hh.xlsx', sep = ''))
write.csv(proportions, paste(path,'togo_hhproportions.csv', sep = ''))
openxlsx::write.xlsx(proportions,  paste(path,'togo_hhproportions.xlsx', sep = ''))

rm(list=ls())

# # #

# CH %% # 

togo_KR <- read_dta("data_files/Togo/TGKR61FL.DTA") # Children's Data - Children's Recode (KR)
togo_KR$country = "Togo"

togo_child <- togo_KR %>%
  group_by(caseid) %>%
  summarise(midx,
            country = country,
            country_code = v000,
            sample_month = v006,
            sample_year = v007,
            cluster = v001,
            household = v002,
            area = v004,
            weight = v005,
            wt = (v005/1000000), # Number of units in the population that a sampled unit represents
            psu = v021,
            sample_stratum = v022,
            sample_domain = v023,
            wealth = v190,
            # geo
            region_id = v024,
            residence_id = v025,
            region = case_when(v024 == 1 ~ "Region 1", 
                               v024 == 2 ~ "Region 2",
                               v024 == 3 ~ "Region 3", 
                               v024 == 4 ~ "Region 4",
                               v024 == 5 ~ "Region 5",
                               v024 == 6 ~ "Region 6"),
            residence = ifelse(v025 == 1, "Urban", "Rural"),
            distance_facility = v467d, # 0 No problem 1 Big problem 2 Not a big problem
            # child
            child_age = hw1, # Children age in months
            # Vit A variables
            child_vitA_recent = h33, # Child was taken to a medical facility for treatment of the fever and/or cough and received vitamin A (most recent)
            child_vitA_last6m = h34, # Received or not a vitamin A dose in form of an ampoule, a capsule or syrup in last 6 months
            birth_vitA_a2m = m54, # Received Vitamin A dose in first 2 months after delivery
            # DPT (Diphteria, pertussis and tetanus vacccination) variables
            DPT1 = h3,
            DPT2 = h5,
            DPT3 = h7,
            # ANCV (Antenatal care visits)
            months_ANCV = m13, # Months pregnant at first antenatal visit 
            amount_ANCV = m14, # Antenatal visits during pregnancy
  )

names(togo_child)
togo_child_2 = subset(togo_child, child_age >= 6)
summary(togo_child_2$child_age)
togo_child = togo_child_2; rm(togo_child_2)

togo_child$residence_binary <- ifelse(togo_child$residence_id == 1, 1, 0)

togo_child$child_age_dummy <- ifelse(togo_child$child_age >= 24, 1, 0)

children_older_24 <- filter(togo_child, child_age_dummy == 1)
children_younger_24 <- filter(togo_child, child_age_dummy == 0)

urban_older_24 <- plyr::ddply(children_older_24, ~region_id, summarise, mean = weighted.mean(residence_binary, wt))
rural_older_24 <- transform(urban_older_24, mean = 1 - mean)

urban_younger_24 <- plyr::ddply(children_younger_24, ~region_id, summarise, mean = weighted.mean(residence_binary, wt))
rural_younger_24 <- transform(urban_younger_24, mean = 1 - mean)

urban_older_24$age_group <- "Older 24 months"
urban_younger_24$age_group <- "Younger 24 months"
rural_older_24$age_group <- "Older 24 months"
rural_younger_24$age_group <- "Younger 24 months"

urban_children_proportion <- bind_rows(urban_younger_24, urban_older_24)
rural_children_proportion <- bind_rows(rural_younger_24, rural_older_24)

proportions_urban <- urban_children_proportion %>%
  rename(children_prop = mean) %>%
  mutate(children_perc = children_prop * 100,
         residence = "Urban")

proportions_rural <- rural_children_proportion %>%
  rename(children_prop = mean) %>%
  mutate(children_perc = children_prop * 100,
         residence = "Rural")

proportions <- bind_rows(proportions_urban, proportions_rural) %>%
  arrange(region_id, residence, age_group)

children_count <- togo_child %>%
  mutate(residence = ifelse(residence_binary == 1, "Urban", "Rural")) %>%
  group_by(region_id, residence) %>%
  dplyr::summarize(total_in_group = sum(wt, na.rm = TRUE), .groups = 'drop')

proportions <- proportions %>%
  mutate(
    child_age_dummy = ifelse(age_group == "Older 24 months", 1, 0),
    residence_binary = ifelse(residence == "Urban", 1, 0)
  )

total_weighted_count <- togo_child %>%
  group_by(child_age_dummy, residence_binary) %>%
  dplyr::summarize(total_wt = sum(wt, na.rm = TRUE), .groups = 'drop')

children_count <- togo_child %>%
  mutate(residence = ifelse(residence_binary == 1, "Urban", "Rural")) %>%
  group_by(region_id, residence) %>%
  dplyr::summarize(total_in_group = sum(wt, na.rm = TRUE), .groups = 'drop')

proportions <- proportions %>%
  left_join(children_count, by = c("region_id", "residence")) %>%
  left_join(total_weighted_count, by = c("child_age_dummy", "residence_binary"))

proportions <- proportions %>%
  mutate(children_across_regions = (children_prop * total_in_group) / total_wt)

total_children_across <- sum(proportions$children_across_regions, na.rm = TRUE)
proportions <- proportions %>%
  mutate(children_across_regions = (children_across_regions / total_children_across) * 100)

proportions$country = "Togo"
print(proportions)
proportions = as.data.frame(proportions)

sum(proportions$children_across_regions, na.rm = TRUE)

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(togo_child,  paste(path,'togo_ch.xlsx', sep = ''))
write.csv(proportions, paste(path,'togo_chproportions.csv', sep = ''))
openxlsx::write.xlsx(proportions,  paste(path,'togo_chproportions.xlsx', sep = ''))

rm(list=setdiff(ls(), "togo_child"))

# # # 

# VAS # 
table(togo_child$child_vitA_last6m)
summary(togo_child$child_vitA_last6m)
togo_child <- togo_child %>%
  mutate(child_vitA_last6m = case_when(child_vitA_last6m == 8 ~ 2, 
                                       is.na(child_vitA_last6m) ~ 3, 
                                       TRUE ~ child_vitA_last6m)) 
table(togo_child$child_vitA_last6m) # 0 = No, 1 = Yes, 2 = Doesnt Know, 3 = No Data
summary(togo_child$child_vitA_last6m)

# DPT Vaccination #
# 0 = No, 1 = Vaccination date on card, 2 = Reported by mother, 3 = Vaccination marked on card, 8 = Doesnt know
table(togo_child$DPT1) 
summary(togo_child$DPT1)
table(togo_child$DPT2) 
summary(togo_child$DPT2)
table(togo_child$DPT3)
summary(togo_child$DPT3)

{
  togo_child <- togo_child %>%
    mutate(DPT1 = case_when(DPT1 == 2 ~ 1,
                            DPT1 == 3 ~ 1,
                            TRUE ~ DPT1)) 
  togo_child <- togo_child %>%
    mutate(DPT1 = case_when(DPT1 == 8 ~ 2, 
                            is.na(DPT1) ~ 3,
                            TRUE ~ DPT1)) 
  togo_child <- togo_child %>%
    mutate(DPT2 = case_when(DPT2 == 2 ~ 1,
                            DPT2 == 3 ~ 1,
                            TRUE ~ DPT2)) 
  togo_child <- togo_child %>%
    mutate(DPT2 = case_when(DPT2 == 8 ~ 2, 
                            is.na(DPT2) ~ 3,
                            TRUE ~ DPT2)) 
  togo_child <- togo_child %>%
    mutate(DPT3 = case_when(DPT3 == 2 ~ 1,
                            DPT3 == 3 ~ 1,
                            TRUE ~ DPT3)) 
  togo_child <- togo_child %>%
    mutate(DPT3 = case_when(DPT3 == 8 ~ 2, 
                            is.na(DPT3) ~ 3,
                            TRUE ~ DPT3))
}

# 0 = No, 1 = Yes, 2 = Doesnt Know, 3 = No Data
table(togo_child$DPT1) 
summary(togo_child$DPT1)
table(togo_child$DPT2) 
summary(togo_child$DPT2)
table(togo_child$DPT3)
summary(togo_child$DPT3)


# ANCV (Antenatal care visits)
table(togo_child$amount_ANCV)
togo_child$ANCV_min1 = ifelse(togo_child$amount_ANCV >= 1, 1, 0) # At least 1 antenatal visit
togo_child$ANCV_min4 = ifelse(togo_child$amount_ANCV >= 4, 1, 0) # At least 4 antenatal visits 
table(togo_child$ANCV_min1)
table(togo_child$ANCV_min4)
# 0 = No, 1 = Yes


# VAS and DPT coverages #
vas_coverage <- togo_child %>%
  filter(child_vitA_last6m %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(vas_coverage = weighted.mean(child_vitA_last6m == 1, wt, na.rm = TRUE)) %>%
  ungroup()

dpt_coverage <- togo_child %>%
  filter(DPT1 %in% c(0, 1), DPT2 %in% c(0, 1), DPT3 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(
    dpt1_coverage = weighted.mean(DPT1 == 1, wt, na.rm = TRUE),
    dpt2_coverage = weighted.mean(DPT2 == 1, wt, na.rm = TRUE),
    dpt3_coverage = weighted.mean(DPT3 == 1, wt, na.rm = TRUE)
  ) %>%
  ungroup()

coverage <- left_join(vas_coverage, dpt_coverage, by = c("region_id", "residence", "child_age_dummy"))

# ANCV coverage # 
ANCV_1 <- togo_child %>%
  filter(ANCV_min1 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(ANCV_min1 = weighted.mean(ANCV_min1 == 1, wt, na.rm = TRUE)) %>%
  ungroup()

ANCV_4 <- togo_child %>%
  filter(ANCV_min4 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(ANCV_min4 = weighted.mean(ANCV_min4 == 1, wt, na.rm = TRUE)) %>%
  ungroup()

ANCV <- left_join(ANCV_1, ANCV_4, by = c("region_id", "residence", "child_age_dummy"))

coverage2 <- left_join(coverage, ANCV, by = c("region_id", "residence", "child_age_dummy"))

coverage2 <- coverage2 %>% 
  distinct(region_id, residence, child_age_dummy, .keep_all = TRUE)

coverage2 <- coverage2 %>% 
  drop_na(child_age_dummy)

proportions_hh = read.csv("results/togo_hhproportions.csv")
proportions_ch = read.csv("results/togo_chproportions.csv")
proportions = left_join(proportions_hh, proportions_ch, by = c("region_id", "residence", "country"))

proportions <- proportions %>% 
  distinct(region_id, residence, age_group, .keep_all = TRUE)

proportions <- proportions %>%
  mutate(child_age_dummy = ifelse(age_group == "Older 24 months", 1, 0))

final_data <- left_join(proportions, coverage2, by = c("region_id", "residence", "child_age_dummy"))
final_data = final_data[, -c(1, 6)]; final_data

rm(list=setdiff(ls(), c("togo_child", "final_data")))


# GIS # AVERAGE DISTANCE TO FACILITIES # # # # # # # # # # # # # # # # # # # # # # # # # # #
names(togo_child)
names(final_data)
togo_hh_GIS = st_read("data_files/Togo/GPS/TGGE62FL_DHS_hous/TGGE62FL.shp") # Household-GPS points
togo_fa_GIS = st_read("data_files/Togo/GPS/TogoHealthFacilities_SPA_faci/hotosm_tgo_health_facilities_points_shp.shp") # Facility-GPS points
togo = openxlsx::read.xlsx("results/togo_hh.xlsx")

st_crs(togo_fa_GIS) <- 4326
mapview(togo_fa_GIS, color = "red") + mapview(togo_hh_GIS, color = "blue")
max(togo_hh_GIS$DHSCLUST) == max(togo_child$cluster)

togo_hh_GIS = rename(togo_hh_GIS, cluster = DHSCLUST)

togo_hh = left_join(togo, togo_hh_GIS, by = "cluster"); names(togo_hh)
max(togo_hh$cluster)

distance_nearest_facility_mts = st_distance(togo_hh_GIS, togo_fa_GIS)
distance_nearest_facility_mts = as.data.frame(distance_nearest_facility_mts)
distance_nearest_facility_mts = apply(distance_nearest_facility_mts, 1, FUN = min)

# togo_fa_GIS2 = st_drop_geometry(togo_fa_GIS)
# path = 'C:\\Users\\stefa\\Documents\\Code\\ / /'
# openxlsx::write.xlsx(togo_fa_GIS2,  paste(path,'togo_fa_GIS.xlsx', sep = ''))
# rm(togo_fa_GIS2)

table(togo_fa_GIS$amenity)
togo_fa_GIS = togo_fa_GIS[togo_fa_GIS$amenity %in% c("hospital", "clinic"), ]; table(togo_fa_GIS$amenity)

togo_fa_GIS$FacilityType <- ifelse(togo_fa_GIS$amenity %in% 'clinic', 'Other', 'Hospital'); table(togo_fa_GIS$FacilityType)
togo_fa_GIS$Ownership <- "Public"; table(togo_fa_GIS$Ownership)

togo_fa_public = togo_fa_GIS %>%
  filter(Ownership == "Public"); table(togo_fa_public$Ownership)

distance_nearest_pubfacility_mts = st_distance(togo_hh_GIS, togo_fa_public)
distance_nearest_pubfacility_mts = as.data.frame(distance_nearest_pubfacility_mts)
distance_nearest_pubfacility_mts = apply(distance_nearest_pubfacility_mts, 1, FUN = min)

togo_hh_GIS$distance_nearest_facility_mts = distance_nearest_facility_mts
togo_hh_GIS$distance_nearest_facility_km = distance_nearest_facility_mts / 1000

togo_hh_GIS$distance_nearest_pubfacility_mts = distance_nearest_pubfacility_mts
togo_hh_GIS$distance_nearest_pubfacility_km = distance_nearest_pubfacility_mts / 1000

summary(togo_hh_GIS$distance_nearest_facility_km)
summary(togo_hh_GIS$distance_nearest_pubfacility_km)

togo_hh <- left_join(togo_hh, togo_hh_GIS[c("cluster", "distance_nearest_facility_km", "distance_nearest_pubfacility_km")], by = "cluster")
togo_ch <- left_join(togo_child, togo_hh_GIS[c("cluster", "distance_nearest_facility_km", "distance_nearest_pubfacility_km")], by = "cluster")
summary(togo_hh$distance_nearest_facility_km)
summary(togo_ch$distance_nearest_facility_km)
summary(togo_hh$distance_nearest_pubfacility_km)
summary(togo_ch$distance_nearest_pubfacility_km)


# DISTANCE GROUPS DUMMY VARIABLE # # # # # # # # # # # # # # # # # # # # # # # # # # #
togo_hh$distance_group = cut(togo_hh$distance_nearest_facility_km, 
                              breaks = quantile(togo_hh$distance_nearest_facility_km, probs = 0:10/10, na.rm = TRUE), 
                              include.lowest = TRUE, labels = FALSE)
table(togo_hh$distance_group)
summary(togo_hh$distance_nearest_facility_km)

library(dplyr)
library(Hmisc)
togo_hh_aggregated <- togo_hh %>%
  group_by(region_id, residence) %>%
  summarise(
    min_distance_km = weighted.mean(distance_nearest_facility_km, wt, na.rm = TRUE, trim = 0),
    mean_distance_km  = weighted.mean(distance_nearest_facility_km, wt, na.rm = TRUE),
    median_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.5, na.rm = TRUE),
    q1_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.25, na.rm = TRUE),
    q3_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.75, na.rm = TRUE),
    max_distance_km = max(distance_nearest_facility_km, na.rm = TRUE),
    Percentile_5 = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.05, na.rm = TRUE),
    Percentile_95 = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.95, na.rm = TRUE),
    distance_public_km = wtd.quantile(distance_nearest_pubfacility_km, weights = wt, probs = 0.5, na.rm = TRUE),
    distance_group = as.integer(names(sort(table(distance_group), decreasing = TRUE)[1]))
  ) %>%
  ungroup()

final_data = as.data.frame(final_data)
final_data$region_id = as.character(final_data$region_id)
togo_hh_aggregated = as.data.frame(togo_hh_aggregated)
togo_hh_aggregated$region_id = as.character(togo_hh_aggregated$region_id)

final_data <- left_join(final_data, togo_hh_aggregated, by = c("region_id", "residence"))

wealth_aggregated <- togo_hh %>%
  group_by(region_id, residence) %>%
  summarise(
    Total_Weight = sum(wt),  # Sum of weights for normalization
    Weighted_Poorer = sum(wt * (wealth == 1)),
    Weighted_Richest = sum(wt * (wealth == 5)),
    Poorer = Weighted_Poorer / Total_Weight,  # Proportion of Poorer
    Richest = Weighted_Richest / Total_Weight,  # Proportion of Richest
    .groups = 'drop'
  )
wealth_aggregated$region_id <- as.character(wealth_aggregated$region_id)  # Ensure region_id is character if necessary
wealth_aggregated = wealth_aggregated[, -c(3, 4, 5)]; wealth_aggregated

final_data <- left_join(final_data, wealth_aggregated, by = c("region_id", "residence")); final_data

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(final_data,  paste(path,'togo_coverage.xlsx', sep = ''))


# FINAL FILTERED CHILDREN DATASET # # # # # # # # # # # # # # # # # # # # # # # # # # #
final_data$residence_binary <- ifelse(final_data$residence == "Urban", 1, 0)
togo_ch$residence_binary <- as.numeric(togo_ch$residence_binary)
togo_ch$region_id <- as.numeric(togo_ch$region_id)
final_data$residence_binary <- as.numeric(as.character(final_data$residence_binary))
final_data$region_id <- as.numeric(as.character(final_data$region_id))

summary(final_data$vas_coverage)
hist(final_data$vas_coverage)

quantile_breaks <- quantile(final_data$vas_coverage, probs = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)
final_data$vas_coverage_group <- cut(final_data$vas_coverage,
                                     breaks = quantile_breaks,
                                     include.lowest = TRUE,
                                     labels = FALSE,
                                     right = TRUE)

table(final_data$vas_coverage_group, useNA = "ifany")
final_data

names(final_data)
names(togo_ch)
nrow(togo_ch); nrow(final_data)

keys <- c("region_id", "residence", "country", "child_age_dummy")
togo_ch_2 <- merge(togo_ch, final_data, by = keys, all.x = TRUE); names(togo_ch_2)

table(togo_ch_2$region_id)
table(togo_ch_2$distance_group) # 1 == less coverage, to 10 == more coverage, quantile(1:10)
summary(togo_ch_2$distance_nearest_facility_km)
table(togo_ch_2$wealth)
table(togo_ch_2$distance_facility)
table(togo_ch_2$vas_coverage_group) # 1 == -50% coverage, to 5 == +90% coverage, quantile(1:6)

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
write.csv(togo_ch_2, paste(path,'togo_ch_final.csv', sep = ''))
st_write(togo_ch_2, paste(path,'togo_ch_final.gpkg', sep = ''), append = FALSE)

# # # DISTANCE GROUPS BY REGION # # # # # # # # # # # # # # # # # # # # # # # # # # #
# summarized_data <- togo_child_distance %>%
#   group_by(region_id, distance_group, vas_coverage_group) %>%
#   summarise(weighted_children_perc = sum(children_perc * wt, na.rm = TRUE), .groups = 'drop')
# 
# total_weights <- summarized_data %>%
#   group_by(region_id) %>%
#   summarise(total_weight = sum(weighted_children_perc, na.rm = TRUE), .groups = 'drop')
# 
# summarized_data <- left_join(summarized_data, total_weights, by = "region_id")
# 
# summarized_data <- summarized_data %>%
#   mutate(weighted_children_perc = (weighted_children_perc / total_weight) * 100)
# 
# wide_data <- pivot_wider(summarized_data,
#                          names_from = region_id, 
#                          values_from = weighted_children_perc,
#                          names_prefix = "Region_",
#                          values_fill = list(weighted_children_perc = 0))
# 
# wide_data[is.na(wide_data)] <- 0
# wide_data <- wide_data %>%
#   mutate(across(starts_with("Region_"), ~./sum(.) * 100))
# print(wide_data, n = 22)
# 
# path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
# openxlsx::write.xlsx(wide_data,  paste(path,'togo_distance_groups_byregion.xlsx', sep = ''))
# 

rm(list=setdiff(ls(), c("togo_ch_2", "togo_fa_GIS", "togo_hh_GIS")))


# # # ESTIMATE DE IMPACT ON VAS COVERAGE # # #
# Analyze the impact of distance in VAS coverage
togo = togo_ch_2; names(togo)

togo = rename(togo, vas = child_vitA_last6m,
               residence_binary = residence_binary.x
); names(togo)

togo$vas = case_when(togo$vas == 2 ~ 0, 
                      togo$vas == 3 ~ 0,
                      TRUE ~ togo$vas)
togo$vas = as.integer(togo$vas)
class(togo); class(togo$vas)
table(togo$vas)

table(togo$wealth)
togo$wealth_factor <- factor(togo$wealth,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# BINOMIAL MODEL DISTANCE & WEALTH #
form1 = vas ~ median_distance_km + wealth_factor + median_distance_km:wealth_factor
form1 = as.formula(form1)

probit1 = glm(form1, family = binomial(link = "probit"), data = togo, weights = wt, x = TRUE)
summary(probit1)
summary(togo$vas - probit1$fitted.values) 
summary(togo$median_distance_km)

coefficients1 = probit1$coefficients; coefficients1
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect1 = erer::maBina(probit1, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect1)


# BINOMIAL MODEL DISTANCE #
form2 = vas ~ median_distance_km
form2 = as.formula(form2)

probit2 = glm(form2, family = binomial(link = "probit"), data = togo, weights = wt, x = TRUE)
summary(probit2)
summary(togo$vas - probit2$fitted.values) 
summary(togo$median_distance_km)

coefficients2 = probit2$coefficients; coefficients2 
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect2 = erer::maBina(probit2, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect2)


# BINOMIAL ln(MODEL DISTANCE) & ln(WEALTH) #
form3 = vas ~ log(median_distance_km) + log(wealth)
form3 = as.formula(form3)

probit3 = glm(form3, family = binomial(link = "probit"), data = togo, weights = wt, x = TRUE)
summary(probit3)
summary(togo$vas - probit3$fitted.values) 
summary(togo$median_distance_km)

coefficients3 = probit3$coefficients; coefficients3
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect3 = erer::maBina(probit3, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect3)


# Save results
setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/"); getwd()
dir.create("togo_probit")

coefficients1 = as.table(coefficients1)
coefficients2 = as.table(coefficients2)
coefficients3 = as.table(coefficients3)
coefficients1 = as.data.frame(coefficients1)
coefficients2 = as.data.frame(coefficients2)
coefficients3 = as.data.frame(coefficients3)
coefficients1 = rename(coefficients1, Variable = Var1, Coefficient = Freq); coefficients1 
coefficients2 = rename(coefficients2, Variable = Var1, Coefficient = Freq); coefficients2
coefficients3 = rename(coefficients3, Variable = Var1, Coefficient = Freq); coefficients3

openxlsx::write.xlsx(coefficients1, 'togo_probit/togo_coefdistancewealth.xlsx')
openxlsx::write.xlsx(coefficients2, 'togo_probit/togo_coefdistance.xlsx')
openxlsx::write.xlsx(coefficients3, 'togo_probit/togo_coeflog.xlsx')

ma_effect1 <- as.data.frame(ma_effect1$out,
                            rownames_to_column = c("(Intercept)", "median_distance_km",
                                                   "wealth_factorPoorer", "wealth_factorMiddle",
                                                   "wealth_factorRicher", "wealth_factorRichest",
                                                   "median_distance_km:wealth_factorPoorer",
                                                   "median_distance_km:wealth_factorMiddle",
                                                   "median_distance_km:wealth_factorRicher",
                                                   "median_distance_km:wealth_factorRichest"))
ma_effect1 = zoo::fortify.zoo(ma_effect1)
ma_effect1$Index <- rownames(ma_effect1); ma_effect1


ma_effect2 <- as.data.frame(ma_effect2$out,
                            row.names = c("(Intercept)", "median_distance_km"))
ma_effect2 = zoo::fortify.zoo(ma_effect2)
ma_effect2$Index <- rownames(ma_effect2); ma_effect2


ma_effect3 <- as.data.frame(ma_effect3$out,
                            row.names = c("(Intercept)", "log(median_distance_km)", "log(wealth)"))
ma_effect3 = zoo::fortify.zoo(ma_effect3)
ma_effect3$Index <- rownames(ma_effect3); ma_effect3


openxlsx::write.xlsx(ma_effect1, 'togo_probit/togo_medistancewealth.xlsx')
openxlsx::write.xlsx(ma_effect2, 'togo_probit/togo_medistance.xlsx')
openxlsx::write.xlsx(ma_effect3, 'togo_probit/togo_melog.xlsx')



rm(list=setdiff(ls(), "togo"))

# # Model with random intercepts for clusters
# form1 = vas ~ median_distance_km + wealth_factor + (1 | cluster)
# probit1 = glmer(form1, family = binomial(link = "probit"), data = togo, weights = wt); summary(probit1)
# 
# # Model with interaction term and random intercepts
# form2 = vas ~ median_distance_km * wealth_factor + (1 | cluster)
# probit2 = glmer(form2, family = binomial(link = "probit"), data = togo, weights = wt); summary(probit2)
# 
# anova(probit1, probit2)


# # # SPATIAL REGRESSION # # #
# togo_sf = togo %>%
#  dplyr::select( "country", "country_code", "region_id", "region", "geometry", "residence_binary", "residence",
#           "children_perc", "caseid", "midx", "cluster", "household", "weight", "wt", "psu", "sample_stratum",
#           "children_prop", "wealth", "wealth_factor",
#           "distance_facility", "distance_group", "distance_nearest_facility_km",
#           "min_distance_km", "mean_distance_km", "median_distance_km", "max_distance_km",
#           "child_age","child_is_under_59", "child_vitA_recent", "vas", "vas_coverage", "vas_coverage_group",
#           "DPT1", "DPT2", "DPT3", "months_ANCV", "amount_ANCV", "ANCV_min1", "ANCV_min4")
# names(togo_sf)
# summary(togo_sf$geometry)
# togo_sf <- st_as_sf(togo_sf); class(togo_sf)
# mapview(togo_sf)
#
# togo_sf <- togo_sf %>%
#   dplyr::filter(!is.na(median_distance_km), !is.na(wealth_factor), !is.na(vas_coverage)) %>%
#   dplyr::mutate(coord_na = is.na(st_coordinates(.)[,1])) %>%
#   dplyr::filter(!coord_na) %>%
#   dplyr::select(-coord_na)
#
# form = vas_coverage ~ median_distance_km + wealth_factor
# ols = lm(form, togo_sf)
# summary(ols)
#
# coords <- st_coordinates(togo_sf)
# sum(is.na(coords))
# coords <- coords[complete.cases(coords), ]
#
# set.seed(123)
# coords_jittered <- jitter(coords, amount = 0.0001)
# knn <- spdep::knearneigh(coords, k = 4)
# nb <- spdep::knn2nb(knn, row.names = NULL)
# lw <- spdep::nb2listw(nb, style = "B", zero.policy = TRUE)
#
# sar_model <- spatialreg::lagsarlm(vas_coverage ~ median_distance_km + wealth_factor, data = togo_sf, listw = lw, zero.policy = TRUE, na.action = na.omit)
#

# rm(list=setdiff(ls(), c("togo", "ols")))
#
# # # # DECISION TREE # # #
table(togo$region)
togo$region_name = case_when(togo$region_id == 1 ~ "Grande Agglomeration de Lome",
                              togo$region_id == 2 ~ "Maritime",
                              togo$region_id == 3 ~ "Plateaux",
                              togo$region_id == 4 ~ "Centre",
                              togo$region_id == 5 ~ "Kara",
                              togo$region_id == 6 ~ "Savanes")
table(togo$region_name)
names(togo)

# Probit by regions
summary(togo$vas_coverage) # children's VAS coverage in %
table(togo$vas) # dummy 0 == NO Vit A delivered, 1 == YES Vit A delivered
table(togo$region_id)
table(togo$region_name)
table(togo$residence) 
table(togo$residence_binary) # Rural == 0, Urban == 1
summary(togo$median_distance_km) # median distance housholds to nearest medical facility
table(togo$wealth_factor) # children's household wealth
table(togo$wealth)

togo$region_id <- as.factor(togo$region_id)
togo$residence_rural = ifelse(togo$residence_binary == 0, 1, 0)

form = vas ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id
form = as.formula(form); form

probit_reg = glm(form, family = binomial(link = "probit"), data = togo, weights = wt, x = TRUE)

summary(probit_reg)
confint(probit_reg, level = 0.95)


# Beta Regression by regions
library(betareg)
togo$vas_coverage_scaled = togo$vas_coverage / 100
beta_reg = betareg(vas_coverage_scaled ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id, data = togo)
summary(beta_reg)

confint(beta_reg, level = 0.95)

# Transformed OLS by regions
togo$vas_coverage_logit = log(togo$vas_coverage_scaled / (1 - togo$vas_coverage_scaled))
ols_reg = lm(vas_coverage_logit ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id, data = togo)
summary(ols_reg)

# compare
AIC(probit_reg, beta_reg, ols_reg)


setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/"); getwd()
togo_limits = st_read("data_files/togo/GPS/togo_limits/togo_limits.shp"); names(togo_limits)

# Random Forest
library(caret)
library(raster)
library(sf) 
library(doParallel)
library(foreach)
names(togo)
summary(togo$geometry)
names(togo_limits)
summary(togo_limits$geometry)

table(togo$region_id)
table(togo$region_name)
table(togo_limits$NAME_1)

togo$region_name = case_when(togo$region_name == "Grande Agglomeration de Lome" ~ "Maritime",
                             TRUE ~ togo$region_name)

table(togo$region_name)
table(togo_limits$NAME_1)

togo_limits <- togo_limits %>%
  mutate(region_id = case_when(
    NAME_1 == "Maritime"  ~ 2,
    NAME_1 == "Plateaux"  ~ 3,
    NAME_1 == "Centre"    ~ 4,
    NAME_1 == "Kara"      ~ 5,
    NAME_1 == "Savanes"   ~ 6))
table(togo_limits$region_id)

summary(togo$vas_coverage)
togo$vas_coverage = replace(togo$vas_coverage, is.na(togo$vas_coverage), 0); summary(togo$vas_coverage)
table(togo$child_age_dummy) # if child is above 24 months old == 1, 0 if under
summary(togo$median_distance_km) # median distance to nearest facility
table(togo$wealth_factor) # children living in household's wealth
table(togo$residence_rural) # == 1 if children lives in rural, == 0 if urban

set.seed(123)
form = vas_coverage ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id
form = as.formula(form)

vars <- c("vas_coverage", "child_age_dummy", "median_distance_km", "wealth_factor", "residence_rural", "region_id")
sapply(togo[vars], function(x) sum(is.na(x))) # Check for missing values in these variables

impute_value <- function(x) {
  if(is.numeric(x)) {
    return(ifelse(is.na(x), median(x, na.rm = TRUE), x))
  } else {
    mode <- names(which.max(table(x)))
    return(ifelse(is.na(x), mode, x))
  }
}

togo[vars] <- lapply(togo[vars], impute_value)
sapply(togo[vars], function(x) sum(is.na(x)))

split <- createDataPartition(togo$vas_coverage, p = 0.75, list = FALSE)[,1]
train_set <- togo[split, ]
test_set <- togo[-split, ]

sapply(train_set[vars], function(x) sum(is.na(x)))

numCores <- parallel::detectCores()
registerDoParallel(cores = numCores)
train_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

{
model <- train(form, 
               data = train_set, 
               method = "rf",
               trControl = train_control)
print(model)
stopImplicitCluster()
}

predictions <- predict(model, newdata = test_set)
summary(predictions)
test_set$predicted_vas_coverage <- predictions
region_coverage <- aggregate(predicted_vas_coverage ~ region_id, data = test_set, mean)

unique(test_set$region_id)
unique(togo_limits$region_id)

togo_limits$region_id <- as.character(togo_limits$region_id)
region_coverage$region_id <- as.character(region_coverage$region_id)

togo_limits <- merge(togo_limits, region_coverage, by = "region_id", all.x = TRUE)
str(togo_limits)

summary(togo_limits$predicted_vas_coverage)
togo_limits$predicted_vas_coverage[is.na(togo_limits$predicted_vas_coverage)] <- 0

library(viridis)
summary(togo_limits$predicted_vas_coverage)
breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 1)
color_count <- length(breaks) - 1
colors <- viridis::viridis(color_count)

coverage_map <- mapview(togo_limits, zcol = "predicted_vas_coverage",
                        col.regions = colors,
                        at = breaks, alpha.regions = 1)
print(coverage_map)

library(ggplot2)
library(sf)
gg_map <- ggplot() +
  geom_sf(data = togo_limits, aes(fill = predicted_vas_coverage), colour = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Predicted VAS Coverage %",
    breaks = breaks[-length(breaks)],
    labels = scales::percent(breaks[-length(breaks)]),
    limits = c(0, 1),
    guide = guide_legend(title.position = "top")
  ) +
  labs(
    title = "Random Forest Model: Predicted VAS Coverage",
    subtitle = "Vas_Coverage = Child_Age + Distance_NearFa + HH_Wealth_Factor + Residence_Rural + Region_Incidence",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(gg_map)
ggsave("Maps/Predicted_VAS_Coverage_togo.png", gg_map, width = 10, height = 8, dpi = 300)


# togo_limits_utm <- st_transform(togo_limits, crs = 32632)
# library(raster)
# cell_size <- sqrt(250000)
# raster_template <- raster(extent(togo_limits_utm), res = c(cell_size, cell_size))
# crs(raster_template) <- crs(togo_limits_utm)
# rasterized_data <- rasterize(togo_limits_utm, raster_template, field = "predicted_vas_coverage", fun = mean, background = NA)
# 
# library(raster)
# writeRaster(rasterized_data, filename = "high_res_vas_coverage.tif", format = "GTiff")
# 
# library(rasterVis)
# levelplot(rasterized_data, col.regions = viridis::viridis(100), margin = FALSE)
# 
# summary(togo_limits)
# 
rm(list=setdiff(ls(), c("togo", "togo_limits")))



# # # # # # # # # Maps # # # # # # # # # 

table(togo$cluster) # clusters that group children/households across survey
summary(togo$vas_coverage) # Vit A Sup Coverage in %
names(togo) # children/households points dataset
names(togo_limits) # country regions layer dataset

library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(Hmisc)

# Histogram 

hist(togo$vas_coverage)
hist_vas = ggplot(togo, aes(x=vas_coverage)) +
  geom_histogram(bins=30, fill=rgb(1, 0, 0, 0.5), color="black") +
  scale_x_continuous(breaks=seq(0, 1, by=0.1), limits=c(0, 1)) +
  labs(x = "VAS Coverage (%)", 
       y = "Number of Children (6-59 months)", 
       title = "Togo Children VAS Coverage Distribution",
       caption = "Data source: DHS Survey Datasets") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(plot.caption = element_text(hjust=0, face="italic"))  # Align and style the caption text
print(hist_vas)
ggsave("results/histogram_vascov_togo.png", hist_vas, width = 10, height = 8, dpi = 300)


cluster_summary <- togo %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary)

cluster_summary$distance[is.na(cluster_summary$distance)] = 4.59355
cluster_summary$distance_pub[is.na(cluster_summary$distance_pub)] = 5.36856
summary(cluster_summary)

togo_clusters <- togo %>%
  distinct(cluster, .keep_all = TRUE) %>%
  st_as_sf()  

cluster_summary <- cluster_summary %>%
  left_join(togo_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary <- st_as_sf(cluster_summary, sf_column_name = "geometry"); str(cluster_summary)

togo_limits <- st_as_sf(togo_limits)

summary(cluster_summary$vas_coverage)
# Vas_Coverage by clusters
library(RColorBrewer)
breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1) 
labels <- scales::percent(breaks)

gg_map_vas <- ggplot() +
  geom_sf(data = togo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Togo Average VAS Coverage by Cluster | Children 6-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas)
ggsave("Maps/Cluster_VAS_Coverage_togo.png", gg_map_vas, width = 10, height = 8, dpi = 300)


# Vas_Coverage by clusters for children under and above 24 months old
table(togo$child_age_dummy) # comes from ifelse(togo_child$child_age >= 24, 1, 0)

# under 24 months
cluster_summary_under24 <- togo %>%
  filter(child_age_dummy == 0, vas_coverage > 0) %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_under24)

cluster_summary_under24$distance[is.na(cluster_summary_under24$distance)] = 4.21103
cluster_summary_under24$distance_pub[is.na(cluster_summary_under24$distance_pub)] = 4.89878
summary(cluster_summary_under24)

cluster_summary_under24 <- cluster_summary_under24 %>%
  left_join(togo_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary_under24 <- st_as_sf(cluster_summary_under24, sf_column_name = "geometry")


# above 24 months
cluster_summary_above24 <- togo %>%
  filter(child_age_dummy == 1, vas_coverage > 0) %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_above24)

cluster_summary_above24$distance[is.na(cluster_summary_above24$distance)] = 4.88051
cluster_summary_above24$distance_pub[is.na(cluster_summary_above24$distance_pub)] = 5.88026
summary(cluster_summary_above24)

cluster_summary_above24 <- cluster_summary_above24 %>%
  left_join(togo_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary_above24 <- st_as_sf(cluster_summary_above24, sf_column_name = "geometry")


gg_map_vas_under24 <- ggplot() +
  geom_sf(data = togo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_under24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Togo Average VAS Coverage by Cluster | Children 6-24 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_under24)
ggsave("Maps/VAS_Coverage_Under24_togo.png", gg_map_vas_under24, width = 10, height = 8, dpi = 300)


summary(cluster_summary_above24$vas_coverage)
gg_map_vas_above24 <- ggplot() +
  geom_sf(data = togo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_above24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Togo Average VAS Coverage by Cluster | Children 24-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_above24)
ggsave("Maps/VAS_Coverage_Above24_togo.png", gg_map_vas_above24, width = 10, height = 8, dpi = 300)

# Histogram
hist_vas_sep = ggplot() +
  geom_histogram(data = cluster_summary_under24, aes(x = vas_coverage, fill = "6-24 months"), bins = 30, alpha = 0.5) +
  geom_histogram(data = cluster_summary_above24, aes(x = vas_coverage, fill = "24-59 months"), bins = 30, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  scale_fill_manual(values = c("6-24 months" = rgb(1, 0, 0, 0.5), "24-59 months" = rgb(0, 0, 1, 0.5)),
                    name = "Age Group", 
                    labels = c("6-24 months", "24-59 months")) +
  labs(x = "VAS Coverage (%)", 
       y = "Children",
       title = "VAS Coverage Distribution by Age Group",
       subtitle = "Red: 6-24 months, Blue: 24-59 months") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Age Group",
                             override.aes = list(colour = c("red", "blue"), size = 4))) +
  theme(legend.position = "topright") 
print(hist_vas_sep)
ggsave("results/histogram_vassep_togo.png", hist_vas_sep, width = 10, height = 8, dpi = 300)


# For Nearest Facility by Clusters with adjusted scale
gg_map_distance <- ggplot() +
  geom_sf(data = togo_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = distance, color = distance, size = total_weight), shape = 21, alpha = 0.8) +
  scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                        values = rescale(c(0, 60, 125), to = c(0, 1)),
                        name = "Distance to nearest facility (km)",
                        limits = c(0, 125),
                        breaks = c(0, 30, 60, 90, 125),
                        labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
  scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                       values = rescale(c(0, 60, 125), to = c(0, 1))) +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Togo Distance to Nearest Health Facility by Cluster",
    subtitle = "Colored by distance in kilometers",
    caption = "Source: GADM | OpenStreetMap (OSM)"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
print(gg_map_distance)
ggsave("Maps/Distance_to_Facility_togo.png", gg_map_distance, width = 12, height = 10, dpi = 300)

# For Nearest Public Facility by Clusters
# gg_map_distance_pub <- ggplot() +
#   geom_sf(data = togo_limits, fill = NA, color = "gray", size = 0.2) +
#   geom_sf(data = cluster_summary, aes(fill = distance_pub, color = distance_pub, size = total_weight), shape = 21, alpha = 0.8) +
#   scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
#                         values = rescale(c(0, 60, 125), to = c(0, 1)),
#                         name = "Distance to public facility (km)",
#                         limits = c(0, 125),
#                         breaks = c(0, 30, 60, 90, 125),
#                         labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
#   scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
#                        values = rescale(c(0, 60, 125), to = c(0, 1))) +
#   scale_size(range = c(3, 15), name = "Total Children Weight") +
#   labs(
#     title = "Distance to Nearest Public Health Facility by Cluster",
#     subtitle = "Colored by Distance in kilometers",
#     caption = "Source: DHS Survey Datasets"
#   ) +
#   theme_minimal() +
#   guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
# gg_map_distance_pub
# ggsave("Maps/Distance_to_Public_Facility_togo.png", gg_map_distance_pub, width = 12, height = 10, dpi = 300)

# Facility locations and type
togo_fa_GIS = st_read("data_files/Togo/GPS/TogoHealthFacilities_SPA_faci/hotosm_tgo_health_facilities_points_shp.shp") # Facility-GPS points
names(togo_fa_GIS)
summary(togo_fa_GIS$geometry)
table(togo_fa_GIS$amenity)

togo_fa_GIS = togo_fa_GIS[togo_fa_GIS$amenity %in% c("hospital", "clinic"), ]; table(togo_fa_GIS$amenity)

togo_fa_GIS$FacilityType = togo_fa_GIS$amenity
togo_fa_GIS$Ownership <- "Public"

table(togo_fa_GIS$FacilityType)
table(togo_fa_GIS$Ownership)

st_crs(togo_fa_GIS) <- 4326

togo_fa_GIS <- togo_fa_GIS[!is.na(st_geometry(togo_fa_GIS)), ]

adjusted_theme <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
gg_facility_map <- ggplot() +
  geom_sf(data = togo_limits, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = togo_fa_GIS, aes(color = Ownership, shape = FacilityType, geometry = geometry), size = 2) +
  scale_color_manual(values = c("Public" = "purple")) +
  scale_shape_manual(values = c("hospital" = 19, "clinic" = 15)) +
  labs(title = "Health Facilities in Togo",
       subtitle = "Facility Type and Ownership",
       caption = "Source: OpenStreetMap (OSM)",
       color = "Ownership",
       shape = "Facility Type") +
  adjusted_theme
print(gg_facility_map)
ggsave("Maps/Facility_classification_togo.png", gg_facility_map, width = 12, height = 10, dpi = 300)


# Amount of facilities per region
summary(togo_fa_GIS$geometry) # facility coords
table(togo_limits$NAME_1) # regions/provinces
summary(togo_limits$geometry)
table(togo_fa_GIS$Ownership) # public/private
table(togo_fa_GIS$FacilityType) #hospital/other

togo_fa_GIS <- st_as_sf(togo_fa_GIS)
togo_limits <- st_as_sf(togo_limits)
facilities_with_regions <- st_join(togo_fa_GIS, togo_limits, join = st_within)

fa_per_reg_total <- facilities_with_regions %>%
  group_by(NAME_1, FacilityType) %>%
  summarise(Total_Facilities = n(), .groups = 'drop') %>%
  spread(key = FacilityType, value = Total_Facilities, fill = 0)

fa_per_reg_total <- fa_per_reg_total %>%
  group_by(NAME_1) %>%
  summarise(
    clinic = sum(clinic, na.rm = TRUE),
    hospital = sum(hospital, na.rm = TRUE)
  )
fa_per_reg_total <- fa_per_reg_total %>%
  rename(Region = NAME_1)

total_row <- fa_per_reg_total %>%
  summarise(
    Region = "Total",
    clinic = sum(clinic, na.rm = TRUE),
    hospital = sum(hospital, na.rm = TRUE)
  )
fa_per_reg_total <- bind_rows(fa_per_reg_total, total_row); fa_per_reg_total
fa_per_reg_total = st_drop_geometry(fa_per_reg_total)

openxlsx::write.xlsx(fa_per_reg_total, "results/togo_Health_Facilities_Per_Region.xlsx", rowNames = FALSE)


# Road lengths (KM), population and transport costs
fuel_price = 1.1 # pump price for diesel fuel (US$ per liter) # we assume == U$D 1.10
fuel_cost_per_km <- (fuel_price / 12) # As a general rule, a gasoline car consumes one liter per 12 kilometers, which translates into a consumption of 4 to 12 liters of fuel per 100 kilometers.

names(togo)
summary(togo$geometry)
summary(togo_limits$geometry)

togo_limits = st_read("data_files/togo/GPS/togo_limits/togo_limits.shp"); names(togo_limits)
togo_roads = st_read("data_files/togo/GPS/togo_roads/togo_roads.shp"); names(togo_roads) # togo Roads DATA #
table(togo_roads$NTLCLASS)
togo_roads2 = subset(togo_roads, NTLCLASS == "Togo", drop = FALSE)
togo_roads = togo_roads2; rm(togo_roads2)
mapview(togo_limits, color = "red") + mapview(togo_roads, color = "green")
summary(togo_roads$LENGTH_KM) # I should calculate the transport costs for togo

togo_popul = openxlsx::read.xlsx("data_files/Togo/togo_population and density.xlsx")
names(togo_popul)

table(togo$region_name)
table(togo_limits$NAME_1)
table(togo_popul$Region)

togo_roads <- st_transform(togo_roads, st_crs(togo_limits))
roads_by_province <- st_join(togo_roads, togo_limits, join = st_intersects)
table(roads_by_province$NAME_1)

province_road_lengths <- roads_by_province %>%
  group_by(NAME_1) %>%
  dplyr::summarize(total_road_length_km = sum(LENGTH_KM, na.rm = TRUE)); summary(province_road_lengths$total_road_length_km)

table(province_road_lengths$NAME_1)
table(togo_popul$Region)

province_data <- left_join(province_road_lengths, togo_popul, by = c("NAME_1" = "Region")); province_data

province_data <- province_data %>%
  mutate(per_capita_transport_cost = (total_road_length_km * fuel_cost_per_km) / Population_2023); province_data

summary(province_data$total_road_length_km)
table(province_data$NAME_1)
table(togo_limits$NAME_1)

province_data_no_geom <- st_drop_geometry(province_data)

merged_data <- left_join(togo_limits, province_data_no_geom, by = "NAME_1")

merged_data = rename(merged_data, Region = NAME_1); names(merged_data)
merged_data$total_road_length_km <- round(merged_data$total_road_length_km, 0)
summary(merged_data$total_road_length_km)

colors <- c("darkgreen", "#1B9E77", "orange", "red", "darkred")
values <- rescale(c(0, 0.5, 1))
gg_map_costs = ggplot(data = merged_data) +
  geom_sf(aes(fill = per_capita_transport_cost), color = "white") +
  geom_sf_text(aes(label = paste(Region, '\nPopulation:', Population_2023, '\nRoad Km:', round(total_road_length_km))), 
               size = 3, check_overlap = TRUE) +
  scale_fill_gradientn(colors = colors, 
                       values = values, 
                       name = "Per Capita\nTransport Cost", 
                       labels = scales::comma, 
                       limits = range(merged_data$per_capita_transport_cost, na.rm = TRUE)) +
  labs(title = "Per Capita Transport Costs",
       subtitle = "By province in Togo",
       caption = "Source: GADM | Global Roads Open Access Data Set (NASA)") +
  theme_minimal()
print(gg_map_costs)
ggsave("Maps/transport_costs_togo.png", gg_map_costs, width = 12, height = 10, dpi = 300)

# merged_data_no_geom = st_drop_geometry(merged_data)
# head(merged_data_no_geom)
# 
# summary_table <- merged_data_no_geom %>%
#   group_by(Region) %>%
#   summarise(
#     Total_Road_Length_KM = sum(total_road_length_km, na.rm = TRUE),
#     Population_2023 = sum(Population_2023, na.rm = TRUE),
#     Per_Capita_Transport_Cost = mean(per_capita_transport_cost, na.rm = TRUE)
#   ); print(summary_table)
# openxlsx::write.xlsx(summary_table, "summary_table.xlsx")

rm(list=ls())


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# NIGER #
{
  #niger_IR <- read_dta("data_files/niger/CDIR61FL.DTA") # Individual Women's Data - Individual Recode (IR)
  #niger_BR <- read_dta("data_files/niger/CDBR61FL.DTA") # Births' data - Birth's Recode (BR)
}

# HH %% # 

niger_HR <- read_dta("data_files/Niger/NIHR61FL.DTA") # Household Data - Household Recode (HR)
niger_HR$country = "Niger"

niger <- niger_HR %>%
  group_by(hhid) %>%
  summarise(
    country = country,
    country_code = hv000,
    sample_year = hv007,
    sample_month = hv006,
    weight = hv005,
    wt = (hv005/1000000),
    psu = hv021,
    sample_stratum = hv022,
    sample_domain = hv023,
    cluster = hv001,
    household = hv002,
    wealth = hv270,
    # geo
    region_id = hv024,
    region = case_when(hv024 == 1 ~ "Region 1", 
                       hv024 == 2 ~ "Region 2",
                       hv024 == 3 ~ "Region 3", 
                       hv024 == 4 ~ "Region 4",
                       hv024 == 5 ~ "Region 5",
                       hv024 == 6 ~ "Region 6",
                       hv024 == 7 ~ "Region 7",
                       hv024 == 8 ~ "Region 8"),
    residence_id = hv025,
    residence = ifelse(hv025 == 1, "Urban", "Rural")
  ) 

niger$residence_binary <- ifelse(niger$residence_id == 1, 1, 0) # Convert to binary: 1 for Urban, 0 for Rural

niger_design <- svydesign(id = ~psu, weights = ~wt, data = niger, nest = TRUE)

urban_households_proportion <- plyr::ddply(niger, ~region_id , summarise , mean = weighted.mean(residence_binary, wt)) 
rural_households_proportion = 1 - urban_households_proportion
rural_households_proportion$region_id = 1:8

proportions_urban <- urban_households_proportion %>% 
  dplyr::select(region_id, mean) %>% 
  rename(residence_prop = mean) %>% 
  mutate(residence = "Urban")

proportions_rural <- rural_households_proportion %>% 
  dplyr::select(region_id, mean) %>% 
  rename(residence_prop = mean) %>% 
  mutate(residence = "Rural")

proportions <- bind_rows(proportions_urban, proportions_rural) %>%
  arrange(region_id, residence)

niger <- niger %>%
  left_join(proportions, by = c("region_id", "residence")) %>%
  mutate(
    residence_perc = residence_prop * 100
  )

proportions$country = "Niger"

table(niger$residence_perc)
table(niger$residence_prop)

niger = as.data.frame(niger)
proportions = as.data.frame(proportions)

proportions = rename(proportions, household_prop = residence_prop)
proportions

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(niger,  paste(path,'niger_hh.xlsx', sep = ''))
write.csv(proportions, paste(path,'niger_hhproportions.csv', sep = ''))
openxlsx::write.xlsx(proportions,  paste(path,'niger_hhproportions.xlsx', sep = ''))

rm(list=ls())

# # #

# CH %% # 

niger_KR <- read_dta("data_files/Niger/NIKR61FL.DTA") # Children's Data - Children's Recode (KR)
niger_KR$country = "Niger"

niger_child <- niger_KR %>%
  group_by(caseid) %>%
  summarise(midx,
            country = country,
            country_code = v000,
            sample_month = v006,
            sample_year = v007,
            cluster = v001,
            household = v002,
            area = v004,
            weight = v005,
            wt = (v005/1000000), # Number of units in the population that a sampled unit represents
            psu = v021,
            sample_stratum = v022,
            sample_domain = v023,
            wealth = v190,
            # geo
            region_id = v024,
            residence_id = v025,
            region = case_when(v024 == 1 ~ "Region 1", 
                               v024 == 2 ~ "Region 2",
                               v024 == 3 ~ "Region 3", 
                               v024 == 4 ~ "Region 4",
                               v024 == 5 ~ "Region 5",
                               v024 == 6 ~ "Region 6",
                               v024 == 7 ~ "Region 7",
                               v024 == 8 ~ "Region 8"),
            residence = ifelse(v025 == 1, "Urban", "Rural"),
            distance_facility = v467d, # 0 No problem 1 Big problem 2 Not a big problem
            # child
            child_age = hw1, # Children age in months
            # Vit A variables
            child_vitA_recent = h33, # Child was taken to a medical facility for treatment of the fever and/or cough and received vitamin A (most recent)
            child_vitA_last6m = h34, # Received or not a vitamin A dose in form of an ampoule, a capsule or syrup in last 6 months
            birth_vitA_a2m = m54, # Received Vitamin A dose in first 2 months after delivery
            # DPT (Diphteria, pertussis and tetanus vacccination) variables
            DPT1 = h3,
            DPT2 = h5,
            DPT3 = h7,
            # ANCV (Antenatal care visits)
            months_ANCV = m13, # Months pregnant at first antenatal visit 
            amount_ANCV = m14, # Antenatal visits during pregnancy
  )

names(niger_child)
niger_child_2 = subset(niger_child, child_age >= 6)
summary(niger_child_2$child_age)
niger_child = niger_child_2; rm(niger_child_2)

niger_child$residence_binary <- ifelse(niger_child$residence_id == 1, 1, 0)

niger_child$child_age_dummy <- ifelse(niger_child$child_age >= 24, 1, 0)

children_older_24 <- filter(niger_child, child_age_dummy == 1)
children_younger_24 <- filter(niger_child, child_age_dummy == 0)

urban_older_24 <- plyr::ddply(children_older_24, ~region_id, summarise, mean = weighted.mean(residence_binary, wt))
rural_older_24 <- transform(urban_older_24, mean = 1 - mean)

urban_younger_24 <- plyr::ddply(children_younger_24, ~region_id, summarise, mean = weighted.mean(residence_binary, wt))
rural_younger_24 <- transform(urban_younger_24, mean = 1 - mean)

urban_older_24$age_group <- "Older 24 months"
urban_younger_24$age_group <- "Younger 24 months"
rural_older_24$age_group <- "Older 24 months"
rural_younger_24$age_group <- "Younger 24 months"

urban_children_proportion <- bind_rows(urban_younger_24, urban_older_24)
rural_children_proportion <- bind_rows(rural_younger_24, rural_older_24)

proportions_urban <- urban_children_proportion %>%
  rename(children_prop = mean) %>%
  mutate(children_perc = children_prop * 100,
         residence = "Urban")

proportions_rural <- rural_children_proportion %>%
  rename(children_prop = mean) %>%
  mutate(children_perc = children_prop * 100,
         residence = "Rural")

proportions <- bind_rows(proportions_urban, proportions_rural) %>%
  arrange(region_id, residence, age_group)

children_count <- niger_child %>%
  mutate(residence = ifelse(residence_binary == 1, "Urban", "Rural")) %>%
  group_by(region_id, residence) %>%
  dplyr::summarize(total_in_group = sum(wt, na.rm = TRUE), .groups = 'drop')

proportions <- proportions %>%
  mutate(
    child_age_dummy = ifelse(age_group == "Older 24 months", 1, 0),
    residence_binary = ifelse(residence == "Urban", 1, 0)
  )

total_weighted_count <- niger_child %>%
  group_by(child_age_dummy, residence_binary) %>%
  dplyr::summarize(total_wt = sum(wt, na.rm = TRUE), .groups = 'drop')

children_count <- niger_child %>%
  mutate(residence = ifelse(residence_binary == 1, "Urban", "Rural")) %>%
  group_by(region_id, residence) %>%
  dplyr::summarize(total_in_group = sum(wt, na.rm = TRUE), .groups = 'drop')

proportions <- proportions %>%
  left_join(children_count, by = c("region_id", "residence")) %>%
  left_join(total_weighted_count, by = c("child_age_dummy", "residence_binary"))

proportions <- proportions %>%
  mutate(children_across_regions = (children_prop * total_in_group) / total_wt)

total_children_across <- sum(proportions$children_across_regions, na.rm = TRUE)
proportions <- proportions %>%
  mutate(children_across_regions = (children_across_regions / total_children_across) * 100)

proportions$country = "Niger"
print(proportions)
proportions = as.data.frame(proportions)

sum(proportions$children_across_regions, na.rm = TRUE)

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(niger_child,  paste(path,'niger_ch.xlsx', sep = ''))
write.csv(proportions, paste(path,'niger_chproportions.csv', sep = ''))
openxlsx::write.xlsx(proportions,  paste(path,'niger_chproportions.xlsx', sep = ''))

rm(list=setdiff(ls(), "niger_child"))

# # # 

# VAS # 
table(niger_child$child_vitA_last6m)
summary(niger_child$child_vitA_last6m)
niger_child <- niger_child %>%
  mutate(child_vitA_last6m = case_when(child_vitA_last6m == 8 ~ 2, 
                                       child_vitA_last6m == 9 ~ 2, 
                                       is.na(child_vitA_last6m) ~ 3, 
                                       TRUE ~ child_vitA_last6m)) 
table(niger_child$child_vitA_last6m) # 0 = No, 1 = Yes, 2 = Doesnt Know, 3 = No Data
summary(niger_child$child_vitA_last6m)

# DPT Vaccination #
# 0 = No, 1 = Vaccination date on card, 2 = Reported by mother, 3 = Vaccination marked on card, 8 = Doesnt know
table(niger_child$DPT1) 
summary(niger_child$DPT1)
table(niger_child$DPT2) 
summary(niger_child$DPT2)
table(niger_child$DPT3)
summary(niger_child$DPT3)

{
  niger_child <- niger_child %>%
    mutate(DPT1 = case_when(DPT1 == 2 ~ 1,
                            DPT1 == 3 ~ 1,
                            TRUE ~ DPT1)) 
  niger_child <- niger_child %>%
    mutate(DPT1 = case_when(DPT1 == 8 ~ 2, 
                            DPT1 == 9 ~ 2,
                            is.na(DPT1) ~ 3,
                            TRUE ~ DPT1)) 
  niger_child <- niger_child %>%
    mutate(DPT2 = case_when(DPT2 == 2 ~ 1,
                            DPT2 == 3 ~ 1,
                            TRUE ~ DPT2)) 
  niger_child <- niger_child %>%
    mutate(DPT2 = case_when(DPT2 == 8 ~ 2, 
                            DPT2 == 9 ~ 2,
                            is.na(DPT2) ~ 3,
                            TRUE ~ DPT2)) 
  niger_child <- niger_child %>%
    mutate(DPT3 = case_when(DPT3 == 2 ~ 1,
                            DPT3 == 3 ~ 1,
                            TRUE ~ DPT3)) 
  niger_child <- niger_child %>%
    mutate(DPT3 = case_when(DPT3 == 8 ~ 2,
                            DPT3 == 9 ~ 2,
                            is.na(DPT3) ~ 3,
                            TRUE ~ DPT3))
}

# 0 = No, 1 = Yes, 2 = Doesnt Know, 3 = No Data
table(niger_child$DPT1) 
summary(niger_child$DPT1)
table(niger_child$DPT2) 
summary(niger_child$DPT2)
table(niger_child$DPT3)
summary(niger_child$DPT3)


# ANCV (Antenatal care visits)
table(niger_child$amount_ANCV)
niger_child$ANCV_min1 = ifelse(niger_child$amount_ANCV >= 1, 1, 0) # At least 1 antenatal visit
niger_child$ANCV_min4 = ifelse(niger_child$amount_ANCV >= 4, 1, 0) # At least 4 antenatal visits 
table(niger_child$ANCV_min1)
table(niger_child$ANCV_min4)
# 0 = No, 1 = Yes


# VAS and DPT coverages #
vas_coverage <- niger_child %>%
  filter(child_vitA_last6m %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(vas_coverage = weighted.mean(child_vitA_last6m == 1, wt, na.rm = TRUE)) %>%
  ungroup()

dpt_coverage <- niger_child %>%
  filter(DPT1 %in% c(0, 1), DPT2 %in% c(0, 1), DPT3 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(
    dpt1_coverage = weighted.mean(DPT1 == 1, wt, na.rm = TRUE),
    dpt2_coverage = weighted.mean(DPT2 == 1, wt, na.rm = TRUE),
    dpt3_coverage = weighted.mean(DPT3 == 1, wt, na.rm = TRUE)
  ) %>%
  ungroup()

coverage <- left_join(vas_coverage, dpt_coverage, by = c("region_id", "residence", "child_age_dummy"))

# ANCV coverage # 
ANCV_1 <- niger_child %>%
  filter(ANCV_min1 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(ANCV_min1 = weighted.mean(ANCV_min1 == 1, wt, na.rm = TRUE)) %>%
  ungroup()

ANCV_4 <- niger_child %>%
  filter(ANCV_min4 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(ANCV_min4 = weighted.mean(ANCV_min4 == 1, wt, na.rm = TRUE)) %>%
  ungroup()

ANCV <- left_join(ANCV_1, ANCV_4, by = c("region_id", "residence", "child_age_dummy"))

coverage2 <- left_join(coverage, ANCV, by = c("region_id", "residence", "child_age_dummy"))

coverage2 <- coverage2 %>% 
  distinct(region_id, residence, child_age_dummy, .keep_all = TRUE)

coverage2 <- coverage2 %>% 
  drop_na(child_age_dummy)

proportions_hh = read.csv("results/niger_hhproportions.csv")
proportions_ch = read.csv("results/niger_chproportions.csv")
proportions = left_join(proportions_hh, proportions_ch, by = c("region_id", "residence", "country"))

proportions <- proportions %>% 
  distinct(region_id, residence, age_group, .keep_all = TRUE)

proportions <- proportions %>%
  mutate(child_age_dummy = ifelse(age_group == "Older 24 months", 1, 0))

final_data <- left_join(proportions, coverage2, by = c("region_id", "residence", "child_age_dummy"))
final_data = final_data[, -c(1, 6)]; final_data

rm(list=setdiff(ls(), c("niger_child", "final_data")))


# GIS # AVERAGE DISTANCE TO FACILITIES # # # # # # # # # # # # # # # # # # # # # # # # # # #
names(niger_child)
names(final_data)
niger_hh_GIS = st_read("data_files/Niger/GPS/NIGE61FL_DHS_hous/NIGE61FL.shp") # Household-GPS points
# niger_fa_GIS_raw = openxlsx::read.xlsx("data_files/Niger/GPS/NIger_CSI_coordonnees_geographiques.xlsx"); names(niger_fa_GIS_raw)
# 
# niger_fa_GIS_raw = niger_fa_GIS_raw[!is.na(niger_fa_GIS_raw$Latitude) & !is.na(niger_fa_GIS_raw$Longitude), ]
# summary(niger_fa_GIS_raw$Latitude); summary(niger_fa_GIS_raw$Longitude)
# 
# niger_fa_GIS = st_as_sf(niger_fa_GIS_raw, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")
# niger_fa_GIS
# summary(niger_fa_GIS$geometry)
# 
# st_write(niger_fa_GIS, "data_files/Niger/GPS/NIger_CSI_coordonnees_geographiques.gpkg")
niger_fa_GIS = st_read("data_files/Niger/GPS/NIger_CSI_coordonnees_geographiques.gpkg") # Facility-GPS points
niger = openxlsx::read.xlsx("results/niger_hh.xlsx")

st_crs(niger_fa_GIS) <- 4326
mapview(niger_fa_GIS, color = "red") + mapview(niger_hh_GIS, color = "blue")
max(niger_hh_GIS$DHSCLUST) == max(niger_child$cluster)

niger_hh_GIS = rename(niger_hh_GIS, cluster = DHSCLUST)

niger_hh = left_join(niger, niger_hh_GIS, by = "cluster"); names(niger_hh)
max(niger_hh$cluster)

distance_nearest_facility_mts = st_distance(niger_hh_GIS, niger_fa_GIS)
distance_nearest_facility_mts = as.data.frame(distance_nearest_facility_mts)
distance_nearest_facility_mts = apply(distance_nearest_facility_mts, 1, FUN = min)

# niger_fa_GIS2 = st_drop_geometry(niger_fa_GIS)
# path = 'C:\\Users\\stefa\\Documents\\Code\\ / /'
# openxlsx::write.xlsx(niger_fa_GIS2,  paste(path,'niger_fa_GIS.xlsx', sep = ''))
# rm(niger_fa_GIS2)

niger_fa_GIS$amenity = "CSI"
table(niger_fa_GIS$amenity)

niger_fa_GIS$FacilityType = niger_fa_GIS$amenity
niger_fa_GIS$Ownership <- "Public"; table(niger_fa_GIS$Ownership)

niger_fa_public = niger_fa_GIS %>%
  filter(Ownership == "Public"); table(niger_fa_public$Ownership)

distance_nearest_pubfacility_mts = st_distance(niger_hh_GIS, niger_fa_public)
distance_nearest_pubfacility_mts = as.data.frame(distance_nearest_pubfacility_mts)
distance_nearest_pubfacility_mts = apply(distance_nearest_pubfacility_mts, 1, FUN = min)

niger_hh_GIS$distance_nearest_facility_mts = distance_nearest_facility_mts
niger_hh_GIS$distance_nearest_facility_km = distance_nearest_facility_mts / 1000

niger_hh_GIS$distance_nearest_pubfacility_mts = distance_nearest_pubfacility_mts
niger_hh_GIS$distance_nearest_pubfacility_km = distance_nearest_pubfacility_mts / 1000

summary(niger_hh_GIS$distance_nearest_facility_km)
summary(niger_hh_GIS$distance_nearest_pubfacility_km)

niger_hh <- left_join(niger_hh, niger_hh_GIS[c("cluster", "distance_nearest_facility_km", "distance_nearest_pubfacility_km")], by = "cluster")
niger_ch <- left_join(niger_child, niger_hh_GIS[c("cluster", "distance_nearest_facility_km", "distance_nearest_pubfacility_km")], by = "cluster")
summary(niger_hh$distance_nearest_facility_km)
summary(niger_ch$distance_nearest_facility_km)
summary(niger_hh$distance_nearest_pubfacility_km)
summary(niger_ch$distance_nearest_pubfacility_km)


# DISTANCE GROUPS DUMMY VARIABLE # # # # # # # # # # # # # # # # # # # # # # # # # # #
niger_hh$distance_group = cut(niger_hh$distance_nearest_facility_km, 
                             breaks = quantile(niger_hh$distance_nearest_facility_km, probs = 0:10/10, na.rm = TRUE), 
                             include.lowest = TRUE, labels = FALSE)
table(niger_hh$distance_group)
summary(niger_hh$distance_nearest_facility_km)

library(dplyr)
library(Hmisc)
niger_hh_aggregated <- niger_hh %>%
  group_by(region_id, residence) %>%
  summarise(
    min_distance_km = weighted.mean(distance_nearest_facility_km, wt, na.rm = TRUE, trim = 0),
    mean_distance_km  = weighted.mean(distance_nearest_facility_km, wt, na.rm = TRUE),
    median_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.5, na.rm = TRUE),
    q1_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.25, na.rm = TRUE),
    q3_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.75, na.rm = TRUE),
    max_distance_km = max(distance_nearest_facility_km, na.rm = TRUE),
    Percentile_5 = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.05, na.rm = TRUE),
    Percentile_95 = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.95, na.rm = TRUE),
    distance_public_km = wtd.quantile(distance_nearest_pubfacility_km, weights = wt, probs = 0.5, na.rm = TRUE),
    distance_group = as.integer(names(sort(table(distance_group), decreasing = TRUE)[1]))
  ) %>%
  ungroup()

final_data = as.data.frame(final_data)
final_data$region_id = as.character(final_data$region_id)
niger_hh_aggregated = as.data.frame(niger_hh_aggregated)
niger_hh_aggregated$region_id = as.character(niger_hh_aggregated$region_id)

final_data <- left_join(final_data, niger_hh_aggregated, by = c("region_id", "residence"))

wealth_aggregated <- niger_hh %>%
  group_by(region_id, residence) %>%
  summarise(
    Total_Weight = sum(wt),  # Sum of weights for normalization
    Weighted_Poorer = sum(wt * (wealth == 1)),
    Weighted_Richest = sum(wt * (wealth == 5)),
    Poorer = Weighted_Poorer / Total_Weight,  # Proportion of Poorer
    Richest = Weighted_Richest / Total_Weight,  # Proportion of Richest
    .groups = 'drop'
  )
wealth_aggregated$region_id <- as.character(wealth_aggregated$region_id)  # Ensure region_id is character if necessary
wealth_aggregated = wealth_aggregated[, -c(3, 4, 5)]; wealth_aggregated

final_data <- left_join(final_data, wealth_aggregated, by = c("region_id", "residence")); final_data

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(final_data,  paste(path,'niger_coverage.xlsx', sep = ''))


# FINAL FILTERED CHILDREN DATASET # # # # # # # # # # # # # # # # # # # # # # # # # # #
final_data$residence_binary <- ifelse(final_data$residence == "Urban", 1, 0)
niger_ch$residence_binary <- as.numeric(niger_ch$residence_binary)
niger_ch$region_id <- as.numeric(niger_ch$region_id)
final_data$residence_binary <- as.numeric(as.character(final_data$residence_binary))
final_data$region_id <- as.numeric(as.character(final_data$region_id))

summary(final_data$vas_coverage)
hist(final_data$vas_coverage)

quantile_breaks <- quantile(final_data$vas_coverage, probs = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)
final_data$vas_coverage_group <- cut(final_data$vas_coverage,
                                     breaks = quantile_breaks,
                                     include.lowest = TRUE,
                                     labels = FALSE,
                                     right = TRUE)

table(final_data$vas_coverage_group, useNA = "ifany")
final_data

names(final_data)
names(niger_ch)
nrow(niger_ch); nrow(final_data)

keys <- c("region_id", "residence", "country", "child_age_dummy")
niger_ch_2 <- merge(niger_ch, final_data, by = keys, all.x = TRUE); names(niger_ch_2)

table(niger_ch_2$region_id)
table(niger_ch_2$distance_group) # 1 == less coverage, to 10 == more coverage, quantile(1:10)
summary(niger_ch_2$distance_nearest_facility_km)
table(niger_ch_2$wealth)
table(niger_ch_2$distance_facility)
table(niger_ch_2$vas_coverage_group) # 1 == -50% coverage, to 5 == +90% coverage, quantile(1:6)

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
write.csv(niger_ch_2, paste(path,'niger_ch_final.csv', sep = ''))
st_write(niger_ch_2, paste(path,'niger_ch_final.gpkg', sep = ''), append = FALSE)

# # # DISTANCE GROUPS BY REGION # # # # # # # # # # # # # # # # # # # # # # # # # # #
# summarized_data <- niger_child_distance %>%
#   group_by(region_id, distance_group, vas_coverage_group) %>%
#   summarise(weighted_children_perc = sum(children_perc * wt, na.rm = TRUE), .groups = 'drop')
# 
# total_weights <- summarized_data %>%
#   group_by(region_id) %>%
#   summarise(total_weight = sum(weighted_children_perc, na.rm = TRUE), .groups = 'drop')
# 
# summarized_data <- left_join(summarized_data, total_weights, by = "region_id")
# 
# summarized_data <- summarized_data %>%
#   mutate(weighted_children_perc = (weighted_children_perc / total_weight) * 100)
# 
# wide_data <- pivot_wider(summarized_data,
#                          names_from = region_id, 
#                          values_from = weighted_children_perc,
#                          names_prefix = "Region_",
#                          values_fill = list(weighted_children_perc = 0))
# 
# wide_data[is.na(wide_data)] <- 0
# wide_data <- wide_data %>%
#   mutate(across(starts_with("Region_"), ~./sum(.) * 100))
# print(wide_data, n = 22)
# 
# path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
# openxlsx::write.xlsx(wide_data,  paste(path,'niger_distance_groups_byregion.xlsx', sep = ''))
# 

rm(list=setdiff(ls(), c("niger_ch_2", "niger_fa_GIS", "niger_hh_GIS")))


# # # ESTIMATE DE IMPACT ON VAS COVERAGE # # #
# Analyze the impact of distance in VAS coverage
niger = niger_ch_2; names(niger)

niger = rename(niger, vas = child_vitA_last6m,
              residence_binary = residence_binary.x
); names(niger)

niger$vas = case_when(niger$vas == 2 ~ 0, 
                     niger$vas == 3 ~ 0,
                     TRUE ~ niger$vas)
niger$vas = as.integer(niger$vas)
class(niger); class(niger$vas)
table(niger$vas)

table(niger$wealth)
niger$wealth_factor <- factor(niger$wealth,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# BINOMIAL MODEL DISTANCE & WEALTH #
form1 = vas ~ median_distance_km + wealth_factor + median_distance_km:wealth_factor
form1 = as.formula(form1)

probit1 = glm(form1, family = binomial(link = "probit"), data = niger, weights = wt, x = TRUE)
summary(probit1)
summary(niger$vas - probit1$fitted.values) 
summary(niger$median_distance_km)

coefficients1 = probit1$coefficients; coefficients1
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect1 = erer::maBina(probit1, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect1)


# BINOMIAL MODEL DISTANCE #
form2 = vas ~ median_distance_km
form2 = as.formula(form2)

probit2 = glm(form2, family = binomial(link = "probit"), data = niger, weights = wt, x = TRUE)
summary(probit2)
summary(niger$vas - probit2$fitted.values) 
summary(niger$median_distance_km)

coefficients2 = probit2$coefficients; coefficients2 
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect2 = erer::maBina(probit2, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect2)


# BINOMIAL ln(MODEL DISTANCE) & ln(WEALTH) #
form3 = vas ~ log(median_distance_km) + log(wealth)
form3 = as.formula(form3)

probit3 = glm(form3, family = binomial(link = "probit"), data = niger, weights = wt, x = TRUE)
summary(probit3)
summary(niger$vas - probit3$fitted.values) 
summary(niger$median_distance_km)

coefficients3 = probit3$coefficients; coefficients3
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect3 = erer::maBina(probit3, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect3)


# Save results
setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/"); getwd()
dir.create("niger_probit")

coefficients1 = as.table(coefficients1)
coefficients2 = as.table(coefficients2)
coefficients3 = as.table(coefficients3)
coefficients1 = as.data.frame(coefficients1)
coefficients2 = as.data.frame(coefficients2)
coefficients3 = as.data.frame(coefficients3)
coefficients1 = rename(coefficients1, Variable = Var1, Coefficient = Freq); coefficients1 
coefficients2 = rename(coefficients2, Variable = Var1, Coefficient = Freq); coefficients2
coefficients3 = rename(coefficients3, Variable = Var1, Coefficient = Freq); coefficients3

openxlsx::write.xlsx(coefficients1, 'niger_probit/niger_coefdistancewealth.xlsx')
openxlsx::write.xlsx(coefficients2, 'niger_probit/niger_coefdistance.xlsx')
openxlsx::write.xlsx(coefficients3, 'niger_probit/niger_coeflog.xlsx')

ma_effect1 <- as.data.frame(ma_effect1$out,
                            rownames_to_column = c("(Intercept)", "median_distance_km",
                                                   "wealth_factorPoorer", "wealth_factorMiddle",
                                                   "wealth_factorRicher", "wealth_factorRichest",
                                                   "median_distance_km:wealth_factorPoorer",
                                                   "median_distance_km:wealth_factorMiddle",
                                                   "median_distance_km:wealth_factorRicher",
                                                   "median_distance_km:wealth_factorRichest"))
ma_effect1 = zoo::fortify.zoo(ma_effect1)
ma_effect1$Index <- rownames(ma_effect1); ma_effect1


ma_effect2 <- as.data.frame(ma_effect2$out,
                            row.names = c("(Intercept)", "median_distance_km"))
ma_effect2 = zoo::fortify.zoo(ma_effect2)
ma_effect2$Index <- rownames(ma_effect2); ma_effect2


ma_effect3 <- as.data.frame(ma_effect3$out,
                            row.names = c("(Intercept)", "log(median_distance_km)", "log(wealth)"))
ma_effect3 = zoo::fortify.zoo(ma_effect3)
ma_effect3$Index <- rownames(ma_effect3); ma_effect3


openxlsx::write.xlsx(ma_effect1, 'niger_probit/niger_medistancewealth.xlsx')
openxlsx::write.xlsx(ma_effect2, 'niger_probit/niger_medistance.xlsx')
openxlsx::write.xlsx(ma_effect3, 'niger_probit/niger_melog.xlsx')



rm(list=setdiff(ls(), "niger"))

# # Model with random intercepts for clusters
# form1 = vas ~ median_distance_km + wealth_factor + (1 | cluster)
# probit1 = glmer(form1, family = binomial(link = "probit"), data = niger, weights = wt); summary(probit1)
# 
# # Model with interaction term and random intercepts
# form2 = vas ~ median_distance_km * wealth_factor + (1 | cluster)
# probit2 = glmer(form2, family = binomial(link = "probit"), data = niger, weights = wt); summary(probit2)
# 
# anova(probit1, probit2)


# # # SPATIAL REGRESSION # # #
# niger_sf = niger %>%
#  dplyr::select( "country", "country_code", "region_id", "region", "geometry", "residence_binary", "residence",
#           "children_perc", "caseid", "midx", "cluster", "household", "weight", "wt", "psu", "sample_stratum",
#           "children_prop", "wealth", "wealth_factor",
#           "distance_facility", "distance_group", "distance_nearest_facility_km",
#           "min_distance_km", "mean_distance_km", "median_distance_km", "max_distance_km",
#           "child_age","child_is_under_59", "child_vitA_recent", "vas", "vas_coverage", "vas_coverage_group",
#           "DPT1", "DPT2", "DPT3", "months_ANCV", "amount_ANCV", "ANCV_min1", "ANCV_min4")
# names(niger_sf)
# summary(niger_sf$geometry)
# niger_sf <- st_as_sf(niger_sf); class(niger_sf)
# mapview(niger_sf)
#
# niger_sf <- niger_sf %>%
#   dplyr::filter(!is.na(median_distance_km), !is.na(wealth_factor), !is.na(vas_coverage)) %>%
#   dplyr::mutate(coord_na = is.na(st_coordinates(.)[,1])) %>%
#   dplyr::filter(!coord_na) %>%
#   dplyr::select(-coord_na)
#
# form = vas_coverage ~ median_distance_km + wealth_factor
# ols = lm(form, niger_sf)
# summary(ols)
#
# coords <- st_coordinates(niger_sf)
# sum(is.na(coords))
# coords <- coords[complete.cases(coords), ]
#
# set.seed(123)
# coords_jittered <- jitter(coords, amount = 0.0001)
# knn <- spdep::knearneigh(coords, k = 4)
# nb <- spdep::knn2nb(knn, row.names = NULL)
# lw <- spdep::nb2listw(nb, style = "B", zero.policy = TRUE)
#
# sar_model <- spatialreg::lagsarlm(vas_coverage ~ median_distance_km + wealth_factor, data = niger_sf, listw = lw, zero.policy = TRUE, na.action = na.omit)
#

# rm(list=setdiff(ls(), c("niger", "ols")))
#
# # # # DECISION TREE # # #
table(niger$region)
niger$region_name = case_when(niger$region_id == 1 ~ "Agadez",
                             niger$region_id == 2 ~ "Diffa",
                             niger$region_id == 3 ~ "Dosso",
                             niger$region_id == 4 ~ "Maradi",
                             niger$region_id == 5 ~ "Tahoua",
                             niger$region_id == 6 ~ "Tillaberi",
                             niger$region_id == 7 ~ "Zinder",
                             niger$region_id == 8 ~ "Niamey")
table(niger$region_name)
names(niger)

# Probit by regions
summary(niger$vas_coverage) # children's VAS coverage in %
table(niger$vas) # dummy 0 == NO Vit A delivered, 1 == YES Vit A delivered
table(niger$region_id)
table(niger$region_name)
table(niger$residence) 
table(niger$residence_binary) # Rural == 0, Urban == 1
summary(niger$median_distance_km) # median distance housholds to nearest medical facility
table(niger$wealth_factor) # children's household wealth
table(niger$wealth)

niger$region_id <- as.factor(niger$region_id)
niger$residence_rural = ifelse(niger$residence_binary == 0, 1, 0)

form = vas ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id
form = as.formula(form); form

probit_reg = glm(form, family = binomial(link = "probit"), data = niger, weights = wt, x = TRUE)

summary(probit_reg)
confint(probit_reg, level = 0.95)


# Beta Regression by regions
library(betareg)
niger$vas_coverage_scaled = niger$vas_coverage / 100
beta_reg = betareg(vas_coverage_scaled ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id, data = niger)
summary(beta_reg)

confint(beta_reg, level = 0.95)

# Transformed OLS by regions
niger$vas_coverage_logit = log(niger$vas_coverage_scaled / (1 - niger$vas_coverage_scaled))
ols_reg = lm(vas_coverage_logit ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id, data = niger)
summary(ols_reg)

# compare
AIC(probit_reg, beta_reg, ols_reg)


setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/"); getwd()
niger_limits = st_read("data_files/Niger/niger_limits/gadm41_niger.shp"); names(niger_limits)

# Random Forest
library(caret)
library(raster)
library(sf) 
library(doParallel)
library(foreach)
names(niger)
summary(niger$geometry)
names(niger_limits)
summary(niger_limits$geometry)

table(niger$region_id)
table(niger$region_name)
table(niger_limits$NAME_1)

niger_limits$NAME_1 = case_when(niger_limits$NAME_1 == "Tillabéry" ~ "Tillaberi",
                             TRUE ~ niger_limits$NAME_1)

table(niger$region_name)
table(niger_limits$NAME_1)

niger_limits <- niger_limits %>%
  mutate(region_id = case_when(
    NAME_1 == "Agadeze"  ~ 1,
    NAME_1 == "Diffa"  ~ 2,
    NAME_1 == "Dosso"  ~ 3,
    NAME_1 == "Maradi"    ~ 4,
    NAME_1 == "Tahoua"      ~ 5,
    NAME_1 == "Tillaberi"   ~ 6,
    NAME_1 == "Zinder"  ~ 7,
    NAME_1 == "Niamey"  ~ 8,))
table(niger_limits$region_id)

summary(niger$vas_coverage)
niger$vas_coverage = replace(niger$vas_coverage, is.na(niger$vas_coverage), 0); summary(niger$vas_coverage)
table(niger$child_age_dummy) # if child is above 24 months old == 1, 0 if under
summary(niger$median_distance_km) # median distance to nearest facility
table(niger$wealth_factor) # children living in household's wealth
table(niger$residence_rural) # == 1 if children lives in rural, == 0 if urban

set.seed(123)
form = vas_coverage ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id
form = as.formula(form)

vars <- c("vas_coverage", "child_age_dummy", "median_distance_km", "wealth_factor", "residence_rural", "region_id")
sapply(niger[vars], function(x) sum(is.na(x))) # Check for missing values in these variables

impute_value <- function(x) {
  if(is.numeric(x)) {
    return(ifelse(is.na(x), median(x, na.rm = TRUE), x))
  } else {
    mode <- names(which.max(table(x)))
    return(ifelse(is.na(x), mode, x))
  }
}

niger[vars] <- lapply(niger[vars], impute_value)
sapply(niger[vars], function(x) sum(is.na(x)))

split <- createDataPartition(niger$vas_coverage, p = 0.75, list = FALSE)[,1]
train_set <- niger[split, ]
test_set <- niger[-split, ]

sapply(train_set[vars], function(x) sum(is.na(x)))

numCores <- parallel::detectCores()
registerDoParallel(cores = numCores)
train_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

{
  model <- train(form, 
                 data = train_set, 
                 method = "rf",
                 trControl = train_control)
  print(model)
  stopImplicitCluster()
}

predictions <- predict(model, newdata = test_set)
summary(predictions)
test_set$predicted_vas_coverage <- predictions
region_coverage <- aggregate(predicted_vas_coverage ~ region_id, data = test_set, mean)

unique(test_set$region_id)
unique(niger_limits$region_id)

niger_limits$region_id <- as.character(niger_limits$region_id)
region_coverage$region_id <- as.character(region_coverage$region_id)

niger_limits <- merge(niger_limits, region_coverage, by = "region_id", all.x = TRUE)
str(niger_limits)

summary(niger_limits$predicted_vas_coverage)
niger_limits$predicted_vas_coverage[is.na(niger_limits$predicted_vas_coverage)] <- 0

library(viridis)
summary(niger_limits$predicted_vas_coverage)
breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 1)
color_count <- length(breaks) - 1
colors <- viridis::viridis(color_count)

coverage_map <- mapview(niger_limits, zcol = "predicted_vas_coverage",
                        col.regions = colors,
                        at = breaks, alpha.regions = 1)
print(coverage_map)

library(ggplot2)
library(sf)
gg_map <- ggplot() +
  geom_sf(data = niger_limits, aes(fill = predicted_vas_coverage), colour = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Predicted VAS Coverage %",
    breaks = breaks[-length(breaks)],
    labels = scales::percent(breaks[-length(breaks)]),
    limits = c(0, 1),
    guide = guide_legend(title.position = "top")
  ) +
  labs(
    title = "Random Forest Model: Predicted VAS Coverage",
    subtitle = "Vas_Coverage = Child_Age + Distance_NearFa + HH_Wealth_Factor + Residence_Rural + Region_Incidence",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(gg_map)
ggsave("Maps/Predicted_VAS_Coverage_niger.png", gg_map, width = 10, height = 8, dpi = 300)


# niger_limits_utm <- st_transform(niger_limits, crs = 32632)
# library(raster)
# cell_size <- sqrt(250000)
# raster_template <- raster(extent(niger_limits_utm), res = c(cell_size, cell_size))
# crs(raster_template) <- crs(niger_limits_utm)
# rasterized_data <- rasterize(niger_limits_utm, raster_template, field = "predicted_vas_coverage", fun = mean, background = NA)
# 
# library(raster)
# writeRaster(rasterized_data, filename = "high_res_vas_coverage.tif", format = "GTiff")
# 
# library(rasterVis)
# levelplot(rasterized_data, col.regions = viridis::viridis(100), margin = FALSE)
# 
# summary(niger_limits)
# 
rm(list=setdiff(ls(), c("niger", "niger_limits")))



# # # # # # # # # Maps # # # # # # # # # 

table(niger$cluster) # clusters that group children/households across survey
summary(niger$vas_coverage) # Vit A Sup Coverage in %
names(niger) # children/households points dataset
names(niger_limits) # country regions layer dataset

library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(Hmisc)

# Histogram 
hist(niger$vas_coverage)
hist_vas = ggplot(niger, aes(x=vas_coverage)) +
  geom_histogram(bins=30, fill=rgb(1, 0, 0, 0.5), color="black") +
  scale_x_continuous(breaks=seq(0, 1, by=0.1), limits=c(0, 1)) +
  labs(x = "VAS Coverage (%)", 
       y = "Number of Children (6-59 months)", 
       title = "Niger Children VAS Coverage Distribution",
       caption = "Data source: DHS Survey Datasets") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(plot.caption = element_text(hjust=0, face="italic"))  # Align and style the caption text
print(hist_vas)
ggsave("results/histogram_vascov_niger.png", hist_vas, width = 10, height = 8, dpi = 300)


# Vas_Coverage by clusters
cluster_summary <- niger %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary)

cluster_summary$distance[is.na(cluster_summary$distance)] = 4.23054
cluster_summary$distance_pub[is.na(cluster_summary$distance_pub)] = 4.6536
summary(cluster_summary)

niger_clusters <- niger %>%
  distinct(cluster, .keep_all = TRUE) %>%
  st_as_sf()  

cluster_summary <- cluster_summary %>%
  left_join(niger_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary <- st_as_sf(cluster_summary, sf_column_name = "geometry"); str(cluster_summary)

niger_limits <- st_as_sf(niger_limits)

summary(cluster_summary$vas_coverage)

library(RColorBrewer)
breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1) 
labels <- scales::percent(breaks)

gg_map_vas <- ggplot() +
  geom_sf(data = niger_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Niger Average VAS Coverage by Cluster | Children 6-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas)
ggsave("Maps/Cluster_VAS_Coverage_niger.png", gg_map_vas, width = 10, height = 8, dpi = 300)


# Vas_Coverage by clusters for children under and above 24 months old
table(niger$child_age_dummy) # comes from ifelse(niger_child$child_age >= 24, 1, 0)

# under 24 months
cluster_summary_under24 <- niger %>%
  filter(child_age_dummy == 0, vas_coverage > 0) %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_under24)

cluster_summary_under24$distance[is.na(cluster_summary_under24$distance)] = 3.93070
cluster_summary_under24$distance_pub[is.na(cluster_summary_under24$distance_pub)] = 4.3238
summary(cluster_summary_under24)

cluster_summary_under24 <- cluster_summary_under24 %>%
  left_join(niger_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary_under24 <- st_as_sf(cluster_summary_under24, sf_column_name = "geometry")


# above 24 months
cluster_summary_above24 <- niger %>%
  filter(child_age_dummy == 1, vas_coverage > 0) %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_above24)

cluster_summary_above24$distance[is.na(cluster_summary_above24$distance)] = 4.40825
cluster_summary_above24$distance_pub[is.na(cluster_summary_above24$distance_pub)] = 4.8491
summary(cluster_summary_above24)

cluster_summary_above24 <- cluster_summary_above24 %>%
  left_join(niger_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary_above24 <- st_as_sf(cluster_summary_above24, sf_column_name = "geometry")


gg_map_vas_under24 <- ggplot() +
  geom_sf(data = niger_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_under24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Niger Average VAS Coverage by Cluster | Children 6-24 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_under24)
ggsave("Maps/VAS_Coverage_Under24_niger.png", gg_map_vas_under24, width = 10, height = 8, dpi = 300)


summary(cluster_summary_above24$vas_coverage)
gg_map_vas_above24 <- ggplot() +
  geom_sf(data = niger_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_above24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Niger Average VAS Coverage by Cluster | Children 24-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_above24)
ggsave("Maps/VAS_Coverage_Above24_niger.png", gg_map_vas_above24, width = 10, height = 8, dpi = 300)


# Histogram
hist_vas_sep = ggplot() +
  geom_histogram(data = cluster_summary_under24, aes(x = vas_coverage, fill = "6-24 months"), bins = 30, alpha = 0.5) +
  geom_histogram(data = cluster_summary_above24, aes(x = vas_coverage, fill = "24-59 months"), bins = 30, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  scale_fill_manual(values = c("6-24 months" = rgb(1, 0, 0, 0.5), "24-59 months" = rgb(0, 0, 1, 0.5)),
                    name = "Age Group", 
                    labels = c("6-24 months", "24-59 months")) +
  labs(x = "VAS Coverage (%)", 
       y = "Children",
       title = "VAS Coverage Distribution by Age Group",
       subtitle = "Red: 6-24 months, Blue: 24-59 months") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Age Group",
                             override.aes = list(colour = c("red", "blue"), size = 4))) +
  theme(legend.position = "topright") 
print(hist_vas_sep)
ggsave("results/histogram_vassep_niger.png", hist_vas_sep, width = 10, height = 8, dpi = 300)


# For Nearest Facility by Clusters with adjusted scale
gg_map_distance <- ggplot() +
  geom_sf(data = niger_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = distance, color = distance, size = total_weight), shape = 21, alpha = 0.8) +
  scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                        values = rescale(c(0, 60, 125), to = c(0, 1)),
                        name = "Distance to nearest facility (km)",
                        limits = c(0, 125),
                        breaks = c(0, 30, 60, 90, 125),
                        labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
  scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                       values = rescale(c(0, 60, 125), to = c(0, 1))) +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Niger Distance to Nearest Health Facility by Cluster",
    subtitle = "Colored by distance in kilometers",
    caption = "Source: GADM | Ministry of Public Health"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
print(gg_map_distance)
ggsave("Maps/Distance_to_Facility_niger.png", gg_map_distance, width = 12, height = 10, dpi = 300)

# For Nearest Public Facility by Clusters
# gg_map_distance_pub <- ggplot() +
#   geom_sf(data = niger_limits, fill = NA, color = "gray", size = 0.2) +
#   geom_sf(data = cluster_summary, aes(fill = distance_pub, color = distance_pub, size = total_weight), shape = 21, alpha = 0.8) +
#   scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
#                         values = rescale(c(0, 60, 125), to = c(0, 1)),
#                         name = "Distance to public facility (km)",
#                         limits = c(0, 125),
#                         breaks = c(0, 30, 60, 90, 125),
#                         labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
#   scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
#                        values = rescale(c(0, 60, 125), to = c(0, 1))) +
#   scale_size(range = c(3, 15), name = "Total Children Weight") +
#   labs(
#     title = "Distance to Nearest Public Health Facility by Cluster",
#     subtitle = "Colored by Distance in kilometers",
#     caption = "Source: DHS Survey Datasets"
#   ) +
#   theme_minimal() +
#   guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
# gg_map_distance_pub
# ggsave("Maps/Distance_to_Public_Facility_niger.png", gg_map_distance_pub, width = 12, height = 10, dpi = 300)

# Facility locations and type
niger_fa_GIS = st_read("data_files/Niger/GPS/NIger_CSI_coordonnees_geographiques.gpkg") # Facility-GPS points
names(niger_fa_GIS)

niger_fa_GIS$amenity = "CSI"
table(niger_fa_GIS$amenity)

niger_fa_GIS$FacilityType = niger_fa_GIS$amenity
niger_fa_GIS$Ownership <- "Public"; table(niger_fa_GIS$Ownership)
table(niger_fa_GIS$FacilityType)
table(niger_fa_GIS$Ownership)

niger_fa_GIS = rename(niger_fa_GIS, geometry = geom); names(niger_fa_GIS)
st_crs(niger_fa_GIS) <- 4326
summary(niger_fa_GIS$geometry)
niger_fa_GIS <- niger_fa_GIS[!is.na(st_geometry(niger_fa_GIS)), ]


gg_facility_map <- ggplot() +
  geom_sf(data = niger_limits, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = niger_fa_GIS, aes(color = Ownership, shape = FacilityType), size = 2) +  # Ensure aes() includes variables as intended
  scale_color_manual(values = c("Public" = "purple")) +
  scale_shape_manual(values = c("CSI" = 19)) +
  labs(title = "Public Health Centres Location in Niger",
       caption = "Source: GADM | Ministry of Public Health",
       color = "Ownership",
       shape = "Facility Type") +
  theme_minimal()
print(gg_facility_map)
ggsave("Maps/Facility_classification_niger.png", gg_facility_map, width = 12, height = 10, dpi = 300)



# Amount of facilities per region
summary(niger_fa_GIS$geometry) # facility coords
table(niger_limits$NAME_1) # regions/provinces
summary(niger_limits$geometry)
table(niger_fa_GIS$Ownership) # public/private
table(niger_fa_GIS$FacilityType) #hospital/other

niger_fa_GIS <- st_as_sf(niger_fa_GIS)
niger_limits <- st_as_sf(niger_limits)
facilities_with_regions <- st_join(niger_fa_GIS, niger_limits, join = st_within)

fa_per_reg_total <- facilities_with_regions %>%
  group_by(NAME_1, FacilityType) %>%
  summarise(Total_Facilities = n(), .groups = 'drop') %>%
  spread(key = FacilityType, value = Total_Facilities, fill = 0)

fa_per_reg_total <- fa_per_reg_total %>%
  group_by(NAME_1) %>%
  summarise(
    Health_Centres = sum(CSI, na.rm = TRUE),
  )
fa_per_reg_total <- fa_per_reg_total %>%
  rename(Region = NAME_1)

total_row <- fa_per_reg_total %>%
  summarise(
    Region = "Total",
    Health_Centres = sum(Health_Centres, na.rm = TRUE),
  )
fa_per_reg_total <- bind_rows(fa_per_reg_total, total_row); fa_per_reg_total
fa_per_reg_total = st_drop_geometry(fa_per_reg_total)

openxlsx::write.xlsx(fa_per_reg_total, "results/niger_Health_Facilities_Per_Region.xlsx", rowNames = FALSE)


# Road lengths (KM), population and transport costs
fuel_price = 1.1 # pump price for diesel fuel (US$ per liter) # we assume == U$D 1.10
fuel_cost_per_km <- (fuel_price / 12) # As a general rule, a gasoline car consumes one liter per 12 kilometers, which translates into a consumption of 4 to 12 liters of fuel per 100 kilometers.

names(niger)
summary(niger$geometry)
summary(niger_limits$geometry)

niger_limits = st_read("data_files/Niger/niger_limits/gadm41_NER_0.shp"); names(niger_limits)
niger_roads = st_read("data_files/Niger/niger_roads/niger_roads.shp"); names(niger_roads) # niger Roads DATA #
table(niger_roads$NTLCLASS)
niger_roads2 = subset(niger_roads, NTLCLASS == "Niger", drop = FALSE)
niger_roads = niger_roads2; rm(niger_roads2)
mapview(niger_limits, color = "red") + mapview(niger_roads, color = "green")
summary(niger_roads$LENGTH_KM) # I should calculate the transport costs for niger






niger_popul = openxlsx::read.xlsx("data_files/niger/DRC_population and density.xlsx")
colnames(niger_popul) <- c("Province", "Pop_density_per_km2_2019", "Area_km2", "Population_2019", "Old_region"); names(niger_popul)

table(niger$region_name)
table(niger_limits$NAME_1)
table(niger_popul$Old_region) # Old regions match with raster data
table(niger_popul$Province) # New regions

niger_roads <- st_transform(niger_roads, st_crs(niger_limits))
roads_by_province <- st_join(niger_roads, niger_limits, join = st_intersects)
table(roads_by_province$NAME_1)

province_road_lengths <- roads_by_province %>%
  group_by(NAME_1) %>%
  dplyr::summarize(total_road_length_km = sum(LENGTH_KM, na.rm = TRUE)); summary(province_road_lengths$total_road_length_km)

table(province_road_lengths$NAME_1)
table(niger_popul$Province)

province_data <- left_join(province_road_lengths, niger_popul, by = c("NAME_1" = "Province")); province_data

province_data <- province_data %>%
  mutate(per_capita_transport_cost = (total_road_length_km * fuel_cost_per_km) / Population_2019); province_data

summary(province_data$total_road_length_km)
table(province_data$NAME_1)
table(niger_limits$NAME_1)

province_data_no_geom <- st_drop_geometry(province_data)

merged_data <- left_join(niger_limits, province_data_no_geom, by = "NAME_1")

merged_data = rename(merged_data, Province = NAME_1); names(merged_data)
merged_data$total_road_length_km <- round(merged_data$total_road_length_km, 0)
summary(merged_data$total_road_length_km)

colors <- c("darkgreen", "#1B9E77", "orange", "red", "darkred")
values <- rescale(c(0, 0.5, 1))
gg_map_costs = ggplot(data = merged_data) +
  geom_sf(aes(fill = per_capita_transport_cost), color = "white") +
  geom_sf_text(aes(label = paste(Province, '\nPopulation:', Population_2019, '\nRoad Km:', round(total_road_length_km))), 
               size = 3, check_overlap = TRUE) +
  scale_fill_gradientn(colors = colors, 
                       values = values, 
                       name = "Per Capita\nTransport Cost", 
                       labels = scales::comma, 
                       limits = range(merged_data$per_capita_transport_cost, na.rm = TRUE)) +
  labs(title = "Per Capita Transport Costs",
       subtitle = "By province in the Democratic Republic of the niger",
       caption = "Source: DHS Survey Datasets | Global Roads Open Access Data Set (NASA)") +
  theme_minimal()
print(gg_map_costs)
ggsave("Maps/transport_costs_niger.png", gg_map_costs, width = 12, height = 10, dpi = 300)

rm(list=ls())

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# LIBERIA #
{
  #liberia_IR <- read_dta("data_files/liberia/CDIR61FL.DTA") # Individual Women's Data - Individual Recode (IR)
  #liberia_BR <- read_dta("data_files/liberia/CDBR61FL.DTA") # Births' data - Birth's Recode (BR)
}

# HH %% # 

liberia_HR <- read_dta("data_files/Liberia/LBHR7AFL.DTA") # Household Data - Household Recode (HR)
liberia_HR$country = "Liberia"

liberia <- liberia_HR %>%
  group_by(hhid) %>%
  summarise(
    country = country,
    country_code = hv000,
    sample_year = hv007,
    sample_month = hv006,
    weight = hv005,
    wt = (hv005/1000000),
    psu = hv021,
    sample_stratum = hv022,
    sample_domain = hv023,
    cluster = hv001,
    household = hv002,
    wealth = hv270,
    # geo
    region_id = hv024,
    region = case_when(hv024 == 1 ~ "Region 1", 
                       hv024 == 2 ~ "Region 2",
                       hv024 == 3 ~ "Region 3", 
                       hv024 == 4 ~ "Region 4",
                       hv024 == 5 ~ "Region 5"),
    residence_id = hv025,
    residence = ifelse(hv025 == 1, "Urban", "Rural")
  ) 

liberia$residence_binary <- ifelse(liberia$residence_id == 1, 1, 0) # Convert to binary: 1 for Urban, 0 for Rural

liberia_design <- svydesign(id = ~psu, weights = ~wt, data = liberia, nest = TRUE)

urban_households_proportion <- plyr::ddply(liberia, ~region_id , summarise , mean = weighted.mean(residence_binary, wt)) 
rural_households_proportion = 1 - urban_households_proportion
rural_households_proportion$region_id = 1:5

proportions_urban <- urban_households_proportion %>% 
  dplyr::select(region_id, mean) %>% 
  rename(residence_prop = mean) %>% 
  mutate(residence = "Urban")

proportions_rural <- rural_households_proportion %>% 
  dplyr::select(region_id, mean) %>% 
  rename(residence_prop = mean) %>% 
  mutate(residence = "Rural")

proportions <- bind_rows(proportions_urban, proportions_rural) %>%
  arrange(region_id, residence)

liberia <- liberia %>%
  left_join(proportions, by = c("region_id", "residence")) %>%
  mutate(
    residence_perc = residence_prop * 100
  )

proportions$country = "Liberia"

table(liberia$residence_perc)
table(liberia$residence_prop)

liberia = as.data.frame(liberia)
proportions = as.data.frame(proportions)

proportions = rename(proportions, household_prop = residence_prop)
proportions

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(liberia,  paste(path,'liberia_hh.xlsx', sep = ''))
write.csv(proportions, paste(path,'liberia_hhproportions.csv', sep = ''))
openxlsx::write.xlsx(proportions,  paste(path,'liberia_hhproportions.xlsx', sep = ''))

rm(list=ls())

# # #

# CH %% # 

liberia_KR <- read_dta("data_files/Liberia/LBKR7AFL.DTA") # Children's Data - Children's Recode (KR)
liberia_KR$country = "Liberia"

liberia_child <- liberia_KR %>%
  group_by(caseid) %>%
  summarise(midx,
            country = country,
            country_code = v000,
            sample_month = v006,
            sample_year = v007,
            cluster = v001,
            household = v002,
            area = v004,
            weight = v005,
            wt = (v005/1000000), # Number of units in the population that a sampled unit represents
            psu = v021,
            sample_stratum = v022,
            sample_domain = v023,
            wealth = v190,
            # geo
            region_id = v024,
            residence_id = v025,
            region = case_when(v024 == 1 ~ "Region 1", 
                               v024 == 2 ~ "Region 2",
                               v024 == 3 ~ "Region 3", 
                               v024 == 4 ~ "Region 4",
                               v024 == 5 ~ "Region 5"),
            residence = ifelse(v025 == 1, "Urban", "Rural"),
            distance_facility = v467d, # 0 No problem 1 Big problem 2 Not a big problem
            # child
            child_age = hw1, # Children age in months
            # Vit A variables
            child_vitA_recent = h33, # Child was taken to a medical facility for treatment of the fever and/or cough and received vitamin A (most recent)
            child_vitA_last6m = h34, # Received or not a vitamin A dose in form of an ampoule, a capsule or syrup in last 6 months
            birth_vitA_a2m = m54, # Received Vitamin A dose in first 2 months after delivery
            # DPT (Diphteria, pertussis and tetanus vacccination) variables
            DPT1 = h3,
            DPT2 = h5,
            DPT3 = h7,
            # ANCV (Antenatal care visits)
            months_ANCV = m13, # Months pregnant at first antenatal visit 
            amount_ANCV = m14, # Antenatal visits during pregnancy
  )

names(liberia_child)
liberia_child_2 = subset(liberia_child, child_age >= 6)
summary(liberia_child_2$child_age)
liberia_child = liberia_child_2; rm(liberia_child_2)

liberia_child$residence_binary <- ifelse(liberia_child$residence_id == 1, 1, 0)

liberia_child$child_age_dummy <- ifelse(liberia_child$child_age >= 24, 1, 0)

children_older_24 <- filter(liberia_child, child_age_dummy == 1)
children_younger_24 <- filter(liberia_child, child_age_dummy == 0)

urban_older_24 <- plyr::ddply(children_older_24, ~region_id, summarise, mean = weighted.mean(residence_binary, wt))
rural_older_24 <- transform(urban_older_24, mean = 1 - mean)

urban_younger_24 <- plyr::ddply(children_younger_24, ~region_id, summarise, mean = weighted.mean(residence_binary, wt))
rural_younger_24 <- transform(urban_younger_24, mean = 1 - mean)

urban_older_24$age_group <- "Older 24 months"
urban_younger_24$age_group <- "Younger 24 months"
rural_older_24$age_group <- "Older 24 months"
rural_younger_24$age_group <- "Younger 24 months"

urban_children_proportion <- bind_rows(urban_younger_24, urban_older_24)
rural_children_proportion <- bind_rows(rural_younger_24, rural_older_24)

proportions_urban <- urban_children_proportion %>%
  rename(children_prop = mean) %>%
  mutate(children_perc = children_prop * 100,
         residence = "Urban")

proportions_rural <- rural_children_proportion %>%
  rename(children_prop = mean) %>%
  mutate(children_perc = children_prop * 100,
         residence = "Rural")

proportions <- bind_rows(proportions_urban, proportions_rural) %>%
  arrange(region_id, residence, age_group)

children_count <- liberia_child %>%
  mutate(residence = ifelse(residence_binary == 1, "Urban", "Rural")) %>%
  group_by(region_id, residence) %>%
  dplyr::summarize(total_in_group = sum(wt, na.rm = TRUE), .groups = 'drop')

proportions <- proportions %>%
  mutate(
    child_age_dummy = ifelse(age_group == "Older 24 months", 1, 0),
    residence_binary = ifelse(residence == "Urban", 1, 0)
  )

total_weighted_count <- liberia_child %>%
  group_by(child_age_dummy, residence_binary) %>%
  dplyr::summarize(total_wt = sum(wt, na.rm = TRUE), .groups = 'drop')

children_count <- liberia_child %>%
  mutate(residence = ifelse(residence_binary == 1, "Urban", "Rural")) %>%
  group_by(region_id, residence) %>%
  dplyr::summarize(total_in_group = sum(wt, na.rm = TRUE), .groups = 'drop')

proportions <- proportions %>%
  left_join(children_count, by = c("region_id", "residence")) %>%
  left_join(total_weighted_count, by = c("child_age_dummy", "residence_binary"))

proportions <- proportions %>%
  mutate(children_across_regions = (children_prop * total_in_group) / total_wt)

total_children_across <- sum(proportions$children_across_regions, na.rm = TRUE)
proportions <- proportions %>%
  mutate(children_across_regions = (children_across_regions / total_children_across) * 100)

proportions$country = "Liberia"
print(proportions)
proportions = as.data.frame(proportions)

sum(proportions$children_across_regions, na.rm = TRUE)

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(liberia_child,  paste(path,'liberia_ch.xlsx', sep = ''))
write.csv(proportions, paste(path,'liberia_chproportions.csv', sep = ''))
openxlsx::write.xlsx(proportions,  paste(path,'liberia_chproportions.xlsx', sep = ''))

rm(list=setdiff(ls(), "liberia_child"))

# # # 

# VAS # 
table(liberia_child$child_vitA_last6m)
summary(liberia_child$child_vitA_last6m)
liberia_child <- liberia_child %>%
  mutate(child_vitA_last6m = case_when(child_vitA_last6m == 8 ~ 2, 
                                       child_vitA_last6m == 9 ~ 2, 
                                       is.na(child_vitA_last6m) ~ 3, 
                                       TRUE ~ child_vitA_last6m)) 
table(liberia_child$child_vitA_last6m) # 0 = No, 1 = Yes, 2 = Doesnt Know, 3 = No Data
summary(liberia_child$child_vitA_last6m)

# DPT Vaccination #
# 0 = No, 1 = Vaccination date on card, 2 = Reported by mother, 3 = Vaccination marked on card, 8 = Doesnt know
table(liberia_child$DPT1) 
summary(liberia_child$DPT1)
table(liberia_child$DPT2) 
summary(liberia_child$DPT2)
table(liberia_child$DPT3)
summary(liberia_child$DPT3)

{
  liberia_child <- liberia_child %>%
    mutate(DPT1 = case_when(DPT1 == 2 ~ 1,
                            DPT1 == 3 ~ 1,
                            TRUE ~ DPT1)) 
  liberia_child <- liberia_child %>%
    mutate(DPT1 = case_when(DPT1 == 8 ~ 2, 
                            DPT1 == 9 ~ 2,
                            is.na(DPT1) ~ 3,
                            TRUE ~ DPT1)) 
  liberia_child <- liberia_child %>%
    mutate(DPT2 = case_when(DPT2 == 2 ~ 1,
                            DPT2 == 3 ~ 1,
                            TRUE ~ DPT2)) 
  liberia_child <- liberia_child %>%
    mutate(DPT2 = case_when(DPT2 == 8 ~ 2, 
                            DPT2 == 9 ~ 2,
                            is.na(DPT2) ~ 3,
                            TRUE ~ DPT2)) 
  liberia_child <- liberia_child %>%
    mutate(DPT3 = case_when(DPT3 == 2 ~ 1,
                            DPT3 == 3 ~ 1,
                            TRUE ~ DPT3)) 
  liberia_child <- liberia_child %>%
    mutate(DPT3 = case_when(DPT3 == 8 ~ 2,
                            DPT3 == 9 ~ 2,
                            is.na(DPT3) ~ 3,
                            TRUE ~ DPT3))
}

# 0 = No, 1 = Yes, 2 = Doesnt Know, 3 = No Data
table(liberia_child$DPT1) 
summary(liberia_child$DPT1)
table(liberia_child$DPT2) 
summary(liberia_child$DPT2)
table(liberia_child$DPT3)
summary(liberia_child$DPT3)


# ANCV (Antenatal care visits)
table(liberia_child$amount_ANCV)
liberia_child$ANCV_min1 = ifelse(liberia_child$amount_ANCV >= 1, 1, 0) # At least 1 antenatal visit
liberia_child$ANCV_min4 = ifelse(liberia_child$amount_ANCV >= 4, 1, 0) # At least 4 antenatal visits 
table(liberia_child$ANCV_min1)
table(liberia_child$ANCV_min4)
# 0 = No, 1 = Yes


# VAS and DPT coverages #
vas_coverage <- liberia_child %>%
  filter(child_vitA_last6m %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(vas_coverage = weighted.mean(child_vitA_last6m == 1, wt, na.rm = TRUE)) %>%
  ungroup()

dpt_coverage <- liberia_child %>%
  filter(DPT1 %in% c(0, 1), DPT2 %in% c(0, 1), DPT3 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(
    dpt1_coverage = weighted.mean(DPT1 == 1, wt, na.rm = TRUE),
    dpt2_coverage = weighted.mean(DPT2 == 1, wt, na.rm = TRUE),
    dpt3_coverage = weighted.mean(DPT3 == 1, wt, na.rm = TRUE)
  ) %>%
  ungroup()

coverage <- left_join(vas_coverage, dpt_coverage, by = c("region_id", "residence", "child_age_dummy"))

# ANCV coverage # 
ANCV_1 <- liberia_child %>%
  filter(ANCV_min1 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(ANCV_min1 = weighted.mean(ANCV_min1 == 1, wt, na.rm = TRUE)) %>%
  ungroup()

ANCV_4 <- liberia_child %>%
  filter(ANCV_min4 %in% c(0, 1)) %>%
  group_by(region_id, residence, child_age_dummy) %>%
  summarise(ANCV_min4 = weighted.mean(ANCV_min4 == 1, wt, na.rm = TRUE)) %>%
  ungroup()

ANCV <- left_join(ANCV_1, ANCV_4, by = c("region_id", "residence", "child_age_dummy"))

coverage2 <- left_join(coverage, ANCV, by = c("region_id", "residence", "child_age_dummy"))

coverage2 <- coverage2 %>% 
  distinct(region_id, residence, child_age_dummy, .keep_all = TRUE)

coverage2 <- coverage2 %>% 
  drop_na(child_age_dummy)

proportions_hh = read.csv("results/liberia_hhproportions.csv")
proportions_ch = read.csv("results/liberia_chproportions.csv")
proportions = left_join(proportions_hh, proportions_ch, by = c("region_id", "residence", "country"))

proportions <- proportions %>% 
  distinct(region_id, residence, age_group, .keep_all = TRUE)

proportions <- proportions %>%
  mutate(child_age_dummy = ifelse(age_group == "Older 24 months", 1, 0))

final_data <- left_join(proportions, coverage2, by = c("region_id", "residence", "child_age_dummy"))
final_data = final_data[, -c(1, 6)]; final_data

rm(list=setdiff(ls(), c("liberia_child", "final_data")))


# GIS # AVERAGE DISTANCE TO FACILITIES # # # # # # # # # # # # # # # # # # # # # # # # # # #
names(liberia_child)
names(final_data)
liberia_hh_GIS = st_read("data_files/Liberia/GPS/LBGE7AFL_DHS_hous/LBGE7AFL2.shp") # Household-GPS points
liberia_fa_GIS = st_read("data_files/Liberia/GPS/facilities/hotosm_lbr_health_facilities_points_shp.shp") # Facility-GPS points
liberia = openxlsx::read.xlsx("results/liberia_hh.xlsx")

st_crs(liberia_fa_GIS) <- 4326
mapview(liberia_fa_GIS, color = "red") + mapview(liberia_hh_GIS, color = "blue")
max(liberia_hh_GIS$DHSCLUST) == max(liberia_child$cluster)

liberia_hh_GIS = rename(liberia_hh_GIS, cluster = DHSCLUST)

liberia_hh = left_join(liberia, liberia_hh_GIS, by = "cluster"); names(liberia_hh)
max(liberia_hh$cluster)

distance_nearest_facility_mts = st_distance(liberia_hh_GIS, liberia_fa_GIS)
distance_nearest_facility_mts = as.data.frame(distance_nearest_facility_mts)
distance_nearest_facility_mts = apply(distance_nearest_facility_mts, 1, FUN = min)

table(liberia_fa_GIS$amenity)
liberia_fa_GIS = liberia_fa_GIS[liberia_fa_GIS$amenity %in% c("hospital", "clinic"), ]; table(liberia_fa_GIS$amenity)

liberia_fa_GIS$FacilityType = liberia_fa_GIS$amenity
liberia_fa_GIS$Ownership <- "Public"

table(liberia_fa_GIS$FacilityType)
table(liberia_fa_GIS$Ownership)

liberia_fa_public = liberia_fa_GIS %>%
  filter(Ownership == "Public"); table(liberia_fa_public$Ownership)

distance_nearest_pubfacility_mts = st_distance(liberia_hh_GIS, liberia_fa_public)
distance_nearest_pubfacility_mts = as.data.frame(distance_nearest_pubfacility_mts)
distance_nearest_pubfacility_mts = apply(distance_nearest_pubfacility_mts, 1, FUN = min)

liberia_hh_GIS$distance_nearest_facility_mts = distance_nearest_facility_mts
liberia_hh_GIS$distance_nearest_facility_km = distance_nearest_facility_mts / 1000

liberia_hh_GIS$distance_nearest_pubfacility_mts = distance_nearest_pubfacility_mts
liberia_hh_GIS$distance_nearest_pubfacility_km = distance_nearest_pubfacility_mts / 1000

summary(liberia_hh_GIS$distance_nearest_facility_km)
summary(liberia_hh_GIS$distance_nearest_pubfacility_km)

liberia_hh <- left_join(liberia_hh, liberia_hh_GIS[c("cluster", "distance_nearest_facility_km", "distance_nearest_pubfacility_km")], by = "cluster")
liberia_ch <- left_join(liberia_child, liberia_hh_GIS[c("cluster", "distance_nearest_facility_km", "distance_nearest_pubfacility_km")], by = "cluster")
summary(liberia_hh$distance_nearest_facility_km)
summary(liberia_ch$distance_nearest_facility_km)
summary(liberia_hh$distance_nearest_pubfacility_km)
summary(liberia_ch$distance_nearest_pubfacility_km)


# DISTANCE GROUPS DUMMY VARIABLE # # # # # # # # # # # # # # # # # # # # # # # # # # #
liberia_hh$distance_group = cut(liberia_hh$distance_nearest_facility_km, 
                              breaks = quantile(liberia_hh$distance_nearest_facility_km, probs = 0:10/10, na.rm = TRUE), 
                              include.lowest = TRUE, labels = FALSE)
table(liberia_hh$distance_group)
summary(liberia_hh$distance_nearest_facility_km)

library(dplyr)
library(Hmisc)
liberia_hh_aggregated <- liberia_hh %>%
  group_by(region_id, residence) %>%
  summarise(
    min_distance_km = weighted.mean(distance_nearest_facility_km, wt, na.rm = TRUE, trim = 0),
    mean_distance_km  = weighted.mean(distance_nearest_facility_km, wt, na.rm = TRUE),
    median_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.5, na.rm = TRUE),
    q1_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.25, na.rm = TRUE),
    q3_distance_km = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.75, na.rm = TRUE),
    max_distance_km = max(distance_nearest_facility_km, na.rm = TRUE),
    Percentile_5 = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.05, na.rm = TRUE),
    Percentile_95 = wtd.quantile(distance_nearest_facility_km, weights = wt, probs = 0.95, na.rm = TRUE),
    distance_public_km = wtd.quantile(distance_nearest_pubfacility_km, weights = wt, probs = 0.5, na.rm = TRUE),
    distance_group = as.integer(names(sort(table(distance_group), decreasing = TRUE)[1]))
  ) %>%
  ungroup()

final_data = as.data.frame(final_data)
final_data$region_id = as.character(final_data$region_id)
liberia_hh_aggregated = as.data.frame(liberia_hh_aggregated)
liberia_hh_aggregated$region_id = as.character(liberia_hh_aggregated$region_id)

final_data <- left_join(final_data, liberia_hh_aggregated, by = c("region_id", "residence"))

wealth_aggregated <- liberia_hh %>%
  group_by(region_id, residence) %>%
  summarise(
    Total_Weight = sum(wt),  # Sum of weights for normalization
    Weighted_Poorer = sum(wt * (wealth == 1)),
    Weighted_Richest = sum(wt * (wealth == 5)),
    Poorer = Weighted_Poorer / Total_Weight,  # Proportion of Poorer
    Richest = Weighted_Richest / Total_Weight,  # Proportion of Richest
    .groups = 'drop'
  )
wealth_aggregated$region_id <- as.character(wealth_aggregated$region_id)  # Ensure region_id is character if necessary
wealth_aggregated = wealth_aggregated[, -c(3, 4, 5)]; wealth_aggregated

final_data <- left_join(final_data, wealth_aggregated, by = c("region_id", "residence")); final_data

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
openxlsx::write.xlsx(final_data,  paste(path,'liberia_coverage.xlsx', sep = ''))


# FINAL FILTERED CHILDREN DATASET # # # # # # # # # # # # # # # # # # # # # # # # # # #
final_data$residence_binary <- ifelse(final_data$residence == "Urban", 1, 0)
liberia_ch$residence_binary <- as.numeric(liberia_ch$residence_binary)
liberia_ch$region_id <- as.numeric(liberia_ch$region_id)
final_data$residence_binary <- as.numeric(as.character(final_data$residence_binary))
final_data$region_id <- as.numeric(as.character(final_data$region_id))

summary(final_data$vas_coverage)
hist(final_data$vas_coverage)

quantile_breaks <- quantile(final_data$vas_coverage, probs = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)
final_data$vas_coverage_group <- cut(final_data$vas_coverage,
                                     breaks = quantile_breaks,
                                     include.lowest = TRUE,
                                     labels = FALSE,
                                     right = TRUE)

table(final_data$vas_coverage_group, useNA = "ifany")
final_data

names(final_data)
names(liberia_ch)
nrow(liberia_ch); nrow(final_data)

keys <- c("region_id", "residence", "country", "child_age_dummy")
liberia_ch_2 <- merge(liberia_ch, final_data, by = keys, all.x = TRUE); names(liberia_ch_2)

table(liberia_ch_2$region_id)
table(liberia_ch_2$distance_group) # 1 == less coverage, to 10 == more coverage, quantile(1:10)
summary(liberia_ch_2$distance_nearest_facility_km)
table(liberia_ch_2$wealth)
table(liberia_ch_2$distance_facility)
table(liberia_ch_2$vas_coverage_group) # 1 == -50% coverage, to 5 == +90% coverage, quantile(1:6)

path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
write.csv(liberia_ch_2, paste(path,'liberia_ch_final.csv', sep = ''))
st_write(liberia_ch_2, paste(path,'liberia_ch_final.gpkg', sep = ''), append = FALSE)

# # # DISTANCE GROUPS BY REGION # # # # # # # # # # # # # # # # # # # # # # # # # # #
# summarized_data <- liberia_child_distance %>%
#   group_by(region_id, distance_group, vas_coverage_group) %>%
#   summarise(weighted_children_perc = sum(children_perc * wt, na.rm = TRUE), .groups = 'drop')
# 
# total_weights <- summarized_data %>%
#   group_by(region_id) %>%
#   summarise(total_weight = sum(weighted_children_perc, na.rm = TRUE), .groups = 'drop')
# 
# summarized_data <- left_join(summarized_data, total_weights, by = "region_id")
# 
# summarized_data <- summarized_data %>%
#   mutate(weighted_children_perc = (weighted_children_perc / total_weight) * 100)
# 
# wide_data <- pivot_wider(summarized_data,
#                          names_from = region_id, 
#                          values_from = weighted_children_perc,
#                          names_prefix = "Region_",
#                          values_fill = list(weighted_children_perc = 0))
# 
# wide_data[is.na(wide_data)] <- 0
# wide_data <- wide_data %>%
#   mutate(across(starts_with("Region_"), ~./sum(.) * 100))
# print(wide_data, n = 22)
# 
# path = 'C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/'
# openxlsx::write.xlsx(wide_data,  paste(path,'liberia_distance_groups_byregion.xlsx', sep = ''))
# 

rm(list=setdiff(ls(), c("liberia_ch_2", "liberia_fa_GIS", "liberia_hh_GIS")))


# # # ESTIMATE DE IMPACT ON VAS COVERAGE # # #
# Analyze the impact of distance in VAS coverage
liberia = liberia_ch_2; names(liberia)

liberia = rename(liberia, vas = child_vitA_last6m,
               residence_binary = residence_binary.x
); names(liberia)

liberia$vas = case_when(liberia$vas == 2 ~ 0, 
                      liberia$vas == 3 ~ 0,
                      TRUE ~ liberia$vas)
liberia$vas = as.integer(liberia$vas)
class(liberia); class(liberia$vas)
table(liberia$vas)

table(liberia$wealth)
liberia$wealth_factor <- factor(liberia$wealth,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# BINOMIAL MODEL DISTANCE & WEALTH #
form1 = vas ~ median_distance_km + wealth_factor + median_distance_km:wealth_factor
form1 = as.formula(form1)

probit1 = glm(form1, family = binomial(link = "probit"), data = liberia, weights = wt, x = TRUE)
summary(probit1)
summary(liberia$vas - probit1$fitted.values) 
summary(liberia$median_distance_km)

coefficients1 = probit1$coefficients; coefficients1
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect1 = erer::maBina(probit1, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect1)


# BINOMIAL MODEL DISTANCE #
form2 = vas ~ median_distance_km
form2 = as.formula(form2)

probit2 = glm(form2, family = binomial(link = "probit"), data = liberia, weights = wt, x = TRUE)
summary(probit2)
summary(liberia$vas - probit2$fitted.values) 
summary(liberia$median_distance_km)

coefficients2 = probit2$coefficients; coefficients2 
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect2 = erer::maBina(probit2, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect2)


# BINOMIAL ln(MODEL DISTANCE) & ln(WEALTH) #
form3 = vas ~ log(median_distance_km) + log(wealth)
form3 = as.formula(form3)

probit3 = glm(form3, family = binomial(link = "probit"), data = liberia, weights = wt, x = TRUE)
summary(probit3)
summary(liberia$vas - probit3$fitted.values) 
summary(liberia$median_distance_km)

coefficients3 = probit3$coefficients; coefficients3
# For every one-unit increase in distance_group, vas_coverage is expected to decrease by B1 == %%
ma_effect3 = erer::maBina(probit3, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value); print(ma_effect3)


# Save results
setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/results/"); getwd()
dir.create("liberia_probit")

coefficients1 = as.table(coefficients1)
coefficients2 = as.table(coefficients2)
coefficients3 = as.table(coefficients3)
coefficients1 = as.data.frame(coefficients1)
coefficients2 = as.data.frame(coefficients2)
coefficients3 = as.data.frame(coefficients3)
coefficients1 = rename(coefficients1, Variable = Var1, Coefficient = Freq); coefficients1 
coefficients2 = rename(coefficients2, Variable = Var1, Coefficient = Freq); coefficients2
coefficients3 = rename(coefficients3, Variable = Var1, Coefficient = Freq); coefficients3

openxlsx::write.xlsx(coefficients1, 'liberia_probit/liberia_coefdistancewealth.xlsx')
openxlsx::write.xlsx(coefficients2, 'liberia_probit/liberia_coefdistance.xlsx')
openxlsx::write.xlsx(coefficients3, 'liberia_probit/liberia_coeflog.xlsx')

ma_effect1 <- as.data.frame(ma_effect1$out,
                            rownames_to_column = c("(Intercept)", "median_distance_km",
                                                   "wealth_factorPoorer", "wealth_factorMiddle",
                                                   "wealth_factorRicher", "wealth_factorRichest",
                                                   "median_distance_km:wealth_factorPoorer",
                                                   "median_distance_km:wealth_factorMiddle",
                                                   "median_distance_km:wealth_factorRicher",
                                                   "median_distance_km:wealth_factorRichest"))
ma_effect1 = zoo::fortify.zoo(ma_effect1)
ma_effect1$Index <- rownames(ma_effect1); ma_effect1


ma_effect2 <- as.data.frame(ma_effect2$out,
                            row.names = c("(Intercept)", "median_distance_km"))
ma_effect2 = zoo::fortify.zoo(ma_effect2)
ma_effect2$Index <- rownames(ma_effect2); ma_effect2


ma_effect3 <- as.data.frame(ma_effect3$out,
                            row.names = c("(Intercept)", "log(median_distance_km)", "log(wealth)"))
ma_effect3 = zoo::fortify.zoo(ma_effect3)
ma_effect3$Index <- rownames(ma_effect3); ma_effect3


openxlsx::write.xlsx(ma_effect1, 'liberia_probit/liberia_medistancewealth.xlsx')
openxlsx::write.xlsx(ma_effect2, 'liberia_probit/liberia_medistance.xlsx')
openxlsx::write.xlsx(ma_effect3, 'liberia_probit/liberia_melog.xlsx')



rm(list=setdiff(ls(), "liberia"))

# # Model with random intercepts for clusters
# form1 = vas ~ median_distance_km + wealth_factor + (1 | cluster)
# probit1 = glmer(form1, family = binomial(link = "probit"), data = liberia, weights = wt); summary(probit1)
# 
# # Model with interaction term and random intercepts
# form2 = vas ~ median_distance_km * wealth_factor + (1 | cluster)
# probit2 = glmer(form2, family = binomial(link = "probit"), data = liberia, weights = wt); summary(probit2)
# 
# anova(probit1, probit2)


# # # SPATIAL REGRESSION # # #
# liberia_sf = liberia %>%
#  dplyr::select( "country", "country_code", "region_id", "region", "geometry", "residence_binary", "residence",
#           "children_perc", "caseid", "midx", "cluster", "household", "weight", "wt", "psu", "sample_stratum",
#           "children_prop", "wealth", "wealth_factor",
#           "distance_facility", "distance_group", "distance_nearest_facility_km",
#           "min_distance_km", "mean_distance_km", "median_distance_km", "max_distance_km",
#           "child_age","child_is_under_59", "child_vitA_recent", "vas", "vas_coverage", "vas_coverage_group",
#           "DPT1", "DPT2", "DPT3", "months_ANCV", "amount_ANCV", "ANCV_min1", "ANCV_min4")
# names(liberia_sf)
# summary(liberia_sf$geometry)
# liberia_sf <- st_as_sf(liberia_sf); class(liberia_sf)
# mapview(liberia_sf)
#
# liberia_sf <- liberia_sf %>%
#   dplyr::filter(!is.na(median_distance_km), !is.na(wealth_factor), !is.na(vas_coverage)) %>%
#   dplyr::mutate(coord_na = is.na(st_coordinates(.)[,1])) %>%
#   dplyr::filter(!coord_na) %>%
#   dplyr::select(-coord_na)
#
# form = vas_coverage ~ median_distance_km + wealth_factor
# ols = lm(form, liberia_sf)
# summary(ols)
#
# coords <- st_coordinates(liberia_sf)
# sum(is.na(coords))
# coords <- coords[complete.cases(coords), ]
#
# set.seed(123)
# coords_jittered <- jitter(coords, amount = 0.0001)
# knn <- spdep::knearneigh(coords, k = 4)
# nb <- spdep::knn2nb(knn, row.names = NULL)
# lw <- spdep::nb2listw(nb, style = "B", zero.policy = TRUE)
#
# sar_model <- spatialreg::lagsarlm(vas_coverage ~ median_distance_km + wealth_factor, data = liberia_sf, listw = lw, zero.policy = TRUE, na.action = na.omit)
#

# rm(list=setdiff(ls(), c("liberia", "ols")))
#
# # # # DECISION TREE # # #
table(liberia$region)
liberia$region_name = case_when(liberia$region_id == 1 ~ "North Western",
                              liberia$region_id == 2 ~ "South Central",
                              liberia$region_id == 3 ~ "South Eastern A",
                              liberia$region_id == 4 ~ "South Eastern B",
                              liberia$region_id == 5 ~ "North Central")
table(liberia$region_name)
names(liberia)

# Probit by regions
summary(liberia$vas_coverage) # children's VAS coverage in %
table(liberia$vas) # dummy 0 == NO Vit A delivered, 1 == YES Vit A delivered
table(liberia$region_id)
table(liberia$region_name)
table(liberia$residence) 
table(liberia$residence_binary) # Rural == 0, Urban == 1
summary(liberia$median_distance_km) # median distance housholds to nearest medical facility
table(liberia$wealth_factor) # children's household wealth
table(liberia$wealth)

liberia$region_id <- as.factor(liberia$region_id)
liberia$residence_rural = ifelse(liberia$residence_binary == 0, 1, 0)

form = vas ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id
form = as.formula(form); form

probit_reg = glm(form, family = binomial(link = "probit"), data = liberia, weights = wt, x = TRUE)

summary(probit_reg)
confint(probit_reg, level = 0.95)


# Beta Regression by regions
library(betareg)
liberia$vas_coverage_scaled = liberia$vas_coverage / 100
beta_reg = betareg(vas_coverage_scaled ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id, data = liberia)
summary(beta_reg)

confint(beta_reg, level = 0.95)

# Transformed OLS by regions
liberia$vas_coverage_logit = log(liberia$vas_coverage_scaled / (1 - liberia$vas_coverage_scaled))
ols_reg = lm(vas_coverage_logit ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id, data = liberia)
summary(ols_reg)

# compare
AIC(probit_reg, beta_reg, ols_reg)


setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/"); getwd()
liberia_limits = st_read("data_files/Liberia/liberia_limits/gadm41_liberia.shp"); names(liberia_limits)

# Random Forest
library(caret)
library(raster)
library(sf) 
library(doParallel)
library(foreach)
names(liberia)
summary(liberia$geometry)
names(liberia_limits)
summary(liberia_limits$geometry)

table(liberia$region_id)
table(liberia$region_name)
table(liberia_limits$NAME_1)

summary(liberia$vas_coverage)
liberia$vas_coverage = replace(liberia$vas_coverage, is.na(liberia$vas_coverage), 0); summary(liberia$vas_coverage)
table(liberia$child_age_dummy) # if child is above 24 months old == 1, 0 if under
summary(liberia$median_distance_km) # median distance to nearest facility
table(liberia$wealth_factor) # children living in household's wealth
table(liberia$residence_rural) # == 1 if children lives in rural, == 0 if urban

set.seed(123)
form = vas_coverage ~ child_age_dummy + median_distance_km + wealth_factor + residence_rural + region_id
form = as.formula(form)

vars <- c("vas_coverage", "child_age_dummy", "median_distance_km", "wealth_factor", "residence_rural", "region_id")
sapply(liberia[vars], function(x) sum(is.na(x))) # Check for missing values in these variables

impute_value <- function(x) {
  if(is.numeric(x)) {
    return(ifelse(is.na(x), median(x, na.rm = TRUE), x))
  } else {
    mode <- names(which.max(table(x)))
    return(ifelse(is.na(x), mode, x))
  }
}

liberia[vars] <- lapply(liberia[vars], impute_value)
sapply(liberia[vars], function(x) sum(is.na(x)))

split <- createDataPartition(liberia$vas_coverage, p = 0.75, list = FALSE)[,1]
train_set <- liberia[split, ]
test_set <- liberia[-split, ]

sapply(train_set[vars], function(x) sum(is.na(x)))

numCores <- parallel::detectCores()
registerDoParallel(cores = numCores)
train_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

{
  model <- train(form, 
                 data = train_set, 
                 method = "rf",
                 trControl = train_control)
  print(model)
  stopImplicitCluster()
}

predictions <- predict(model, newdata = test_set)
summary(predictions)
test_set$predicted_vas_coverage <- predictions
region_coverage <- aggregate(predicted_vas_coverage ~ region_id, data = test_set, mean)

unique(test_set$region_id)
unique(liberia_limits$region_id)

liberia_limits$region_id <- as.character(liberia_limits$region_id)
region_coverage$region_id <- as.character(region_coverage$region_id)

liberia_limits <- merge(liberia_limits, region_coverage, by = "region_id", all.x = TRUE)
str(liberia_limits)

summary(liberia_limits$predicted_vas_coverage)
liberia_limits$predicted_vas_coverage[is.na(liberia_limits$predicted_vas_coverage)] <- 0

library(viridis)
summary(liberia_limits$predicted_vas_coverage)
breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 1)
color_count <- length(breaks) - 1
colors <- viridis::viridis(color_count)

coverage_map <- mapview(liberia_limits, zcol = "predicted_vas_coverage",
                        col.regions = colors,
                        at = breaks, alpha.regions = 1)
print(coverage_map)

library(ggplot2)
library(sf)
gg_map <- ggplot() +
  geom_sf(data = liberia_limits, aes(fill = predicted_vas_coverage), colour = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Predicted VAS Coverage %",
    breaks = breaks[-length(breaks)],
    labels = scales::percent(breaks[-length(breaks)]),
    limits = c(0, 1),
    guide = guide_legend(title.position = "top")
  ) +
  labs(
    title = "Random Forest Model: Predicted VAS Coverage",
    subtitle = "Vas_Coverage = Child_Age + Distance_NearFa + HH_Wealth_Factor + Residence_Rural + Region_Incidence",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(gg_map)
ggsave("Maps/Predicted_VAS_Coverage_liberia.png", gg_map, width = 10, height = 8, dpi = 300)


# liberia_limits_utm <- st_transform(liberia_limits, crs = 32632)
# library(raster)
# cell_size <- sqrt(250000)
# raster_template <- raster(extent(liberia_limits_utm), res = c(cell_size, cell_size))
# crs(raster_template) <- crs(liberia_limits_utm)
# rasterized_data <- rasterize(liberia_limits_utm, raster_template, field = "predicted_vas_coverage", fun = mean, background = NA)
# 
# library(raster)
# writeRaster(rasterized_data, filename = "high_res_vas_coverage.tif", format = "GTiff")
# 
# library(rasterVis)
# levelplot(rasterized_data, col.regions = viridis::viridis(100), margin = FALSE)
# 
# summary(liberia_limits)
# 
rm(list=setdiff(ls(), c("liberia", "liberia_limits")))



# # # # # # # # # Maps # # # # # # # # # 

table(liberia$cluster) # clusters that group children/households across survey
summary(liberia$vas_coverage) # Vit A Sup Coverage in %
names(liberia) # children/households points dataset
names(liberia_limits) # country regions layer dataset

library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(Hmisc)

# Histogram 
hist(liberia$vas_coverage)
hist_vas = ggplot(liberia, aes(x=vas_coverage)) +
  geom_histogram(bins=30, fill=rgb(1, 0, 0, 0.5), color="black") +
  scale_x_continuous(breaks=seq(0, 1, by=0.1), limits=c(0, 1)) +
  labs(x = "VAS Coverage (%)", 
       y = "Number of Children (6-59 months)", 
       title = "liberia Children VAS Coverage Distribution",
       caption = "Data source: DHS Survey Datasets") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(plot.caption = element_text(hjust=0, face="italic"))  # Align and style the caption text
print(hist_vas)
ggsave("results/histogram_vascov_liberia.png", hist_vas, width = 10, height = 8, dpi = 300)


# Vas_Coverage by clusters
cluster_summary <- liberia %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary)

cluster_summary$distance[is.na(cluster_summary$distance)] = 4.23054
cluster_summary$distance_pub[is.na(cluster_summary$distance_pub)] = 4.6536
summary(cluster_summary)

liberia_clusters <- liberia %>%
  distinct(cluster, .keep_all = TRUE) %>%
  st_as_sf()  

cluster_summary <- cluster_summary %>%
  left_join(liberia_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary <- st_as_sf(cluster_summary, sf_column_name = "geometry"); str(cluster_summary)

liberia_limits <- st_as_sf(liberia_limits)

summary(cluster_summary$vas_coverage)

library(RColorBrewer)
breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1) 
labels <- scales::percent(breaks)

gg_map_vas <- ggplot() +
  geom_sf(data = liberia_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Liberia Average VAS Coverage by Cluster | Children 6-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas)
ggsave("Maps/Cluster_VAS_Coverage_liberia.png", gg_map_vas, width = 10, height = 8, dpi = 300)


# Vas_Coverage by clusters for children under and above 24 months old
table(liberia$child_age_dummy) # comes from ifelse(liberia_child$child_age >= 24, 1, 0)

# under 24 months
cluster_summary_under24 <- liberia %>%
  filter(child_age_dummy == 0, vas_coverage > 0) %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_under24)

cluster_summary_under24$distance[is.na(cluster_summary_under24$distance)] = 3.93070
cluster_summary_under24$distance_pub[is.na(cluster_summary_under24$distance_pub)] = 4.3238
summary(cluster_summary_under24)

cluster_summary_under24 <- cluster_summary_under24 %>%
  left_join(liberia_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary_under24 <- st_as_sf(cluster_summary_under24, sf_column_name = "geometry")


# above 24 months
cluster_summary_above24 <- liberia %>%
  filter(child_age_dummy == 1, vas_coverage > 0) %>%
  group_by(cluster) %>%
  filter(vas_coverage != 0) %>%
  summarise(
    vas_coverage = median(vas_coverage),
    distance = distance_nearest_facility_km,
    distance_pub = (distance_nearest_pubfacility_km*1.10),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  ); summary(cluster_summary_above24)

cluster_summary_above24$distance[is.na(cluster_summary_above24$distance)] = 4.40825
cluster_summary_above24$distance_pub[is.na(cluster_summary_above24$distance_pub)] = 4.8491
summary(cluster_summary_above24)

cluster_summary_above24 <- cluster_summary_above24 %>%
  left_join(liberia_clusters %>% dplyr::select(cluster, geometry), by = "cluster")

cluster_summary_above24 <- st_as_sf(cluster_summary_above24, sf_column_name = "geometry")


gg_map_vas_under24 <- ggplot() +
  geom_sf(data = liberia_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_under24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Liberia Average VAS Coverage by Cluster | Children 6-24 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_under24)
ggsave("Maps/VAS_Coverage_Under24_liberia.png", gg_map_vas_under24, width = 10, height = 8, dpi = 300)


summary(cluster_summary_above24$vas_coverage)
gg_map_vas_above24 <- ggplot() +
  geom_sf(data = liberia_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary_above24, aes(fill = vas_coverage, size = total_weight), shape = 21, alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  
                       values = rescale(breaks, to = c(0, 1)),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, 1),
                       name = "VAS Coverage") +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Liberia Average VAS Coverage by Cluster | Children 24-59 months old",
    subtitle = "Vitamin A Supplementation Coverage",
    caption = "Data source: GADM | DHS Survey Datasets"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = guide_colorbar())
print(gg_map_vas_above24)
ggsave("Maps/VAS_Coverage_Above24_liberia.png", gg_map_vas_above24, width = 10, height = 8, dpi = 300)


# Histogram
hist_vas_sep = ggplot() +
  geom_histogram(data = cluster_summary_under24, aes(x = vas_coverage, fill = "6-24 months"), bins = 30, alpha = 0.5) +
  geom_histogram(data = cluster_summary_above24, aes(x = vas_coverage, fill = "24-59 months"), bins = 30, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  scale_fill_manual(values = c("6-24 months" = rgb(1, 0, 0, 0.5), "24-59 months" = rgb(0, 0, 1, 0.5)),
                    name = "Age Group", 
                    labels = c("6-24 months", "24-59 months")) +
  labs(x = "VAS Coverage (%)", 
       y = "Children",
       title = "VAS Coverage Distribution by Age Group",
       subtitle = "Red: 6-24 months, Blue: 24-59 months") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Age Group",
                             override.aes = list(colour = c("red", "blue"), size = 4))) +
  theme(legend.position = "topright") 
print(hist_vas_sep)
ggsave("results/histogram_vassep_liberia.png", hist_vas_sep, width = 10, height = 8, dpi = 300)


# For Nearest Facility by Clusters with adjusted scale
gg_map_distance <- ggplot() +
  geom_sf(data = liberia_limits, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = cluster_summary, aes(fill = distance, color = distance, size = total_weight), shape = 21, alpha = 0.8) +
  scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                        values = rescale(c(0, 60, 125), to = c(0, 1)),
                        name = "Distance to nearest facility (km)",
                        limits = c(0, 125),
                        breaks = c(0, 30, 60, 90, 125),
                        labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
  scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
                       values = rescale(c(0, 60, 125), to = c(0, 1))) +
  scale_size(range = c(3, 15), name = "Total Children Weight") +
  labs(
    title = "Liberia Distance to Nearest Health Facility by Cluster",
    subtitle = "Colored by distance in kilometers",
    caption = "Source: GADM | Ministry of Public Health"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
print(gg_map_distance)
ggsave("Maps/Distance_to_Facility_liberia.png", gg_map_distance, width = 12, height = 10, dpi = 300)

# For Nearest Public Facility by Clusters
# gg_map_distance_pub <- ggplot() +
#   geom_sf(data = liberia_limits, fill = NA, color = "gray", size = 0.2) +
#   geom_sf(data = cluster_summary, aes(fill = distance_pub, color = distance_pub, size = total_weight), shape = 21, alpha = 0.8) +
#   scale_color_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
#                         values = rescale(c(0, 60, 125), to = c(0, 1)),
#                         name = "Distance to public facility (km)",
#                         limits = c(0, 125),
#                         breaks = c(0, 30, 60, 90, 125),
#                         labels = c("0 km", "30 km", "60 km", "90 km", "125 km")) +
#   scale_fill_gradientn(colors = c("#1B9E77", "yellow", "#D95F02"), 
#                        values = rescale(c(0, 60, 125), to = c(0, 1))) +
#   scale_size(range = c(3, 15), name = "Total Children Weight") +
#   labs(
#     title = "Distance to Nearest Public Health Facility by Cluster",
#     subtitle = "Colored by Distance in kilometers",
#     caption = "Source: DHS Survey Datasets"
#   ) +
#   theme_minimal() +
#   guides(color = guide_colorbar(), size = guide_legend(), fill = FALSE)
# gg_map_distance_pub
# ggsave("Maps/Distance_to_Public_Facility_liberia.png", gg_map_distance_pub, width = 12, height = 10, dpi = 300)


# region_mapping <- data.frame(
#   NAME_1 = c("Bomi", "Grand Cape Mount", "Gbapolu", "Montserrado", "Margibi",
#              "Grand Gedeh", "River Gee", "Grand Kru", "Maryland", "Sinoe", "Rivercess",
#              "Bong", "Lofa", "Nimba", "Grand Bassa"), 
#   DHS_region = c("North Western", "North Western", "North Western", 
#                  "South Central", "South Central",
#                  "South Eastern A", "South Eastern A", "South Eastern A", "South Eastern A",
#                  "South Eastern B", "South Eastern B",
#                  "North Central", "North Central", "North Central",
#                  "South Central")  
# )
# 
# liberia_limits <- st_as_sf(liberia_limits)
# liberia_limits <- merge(liberia_limits, region_mapping, by = "NAME_1", all.x = TRUE)
# 
# names(liberia)
# nrow(liberia) # 2273 children observations
# summary(liberia$vas) # Need something like this:
# 
# weighted_quantile <- function(x, w, probs) {
#   wq <- wtd.quantile(x, weights = w, probs = probs, na.rm = TRUE)
#   return(wq)
# }
# region_summary <- liberia %>%
#   group_by(region_name, region_id) %>%
#   summarise(
#     mean_vas_coverage = weighted.mean(vas, wt, na.rm = TRUE),
#     median_vas_coverage = weighted_quantile(vas, wt, probs = 0.5),
#     min_vas_coverage = weighted_quantile(vas, wt, probs = 0),
#     q1_vas_coverage = weighted_quantile(vas, wt, probs = 0.25),
#     q3_vas_coverage = weighted_quantile(vas, wt, probs = 0.75),
#     max_vas_coverage = weighted_quantile(vas, wt, probs = 1),
#     .groups = 'drop'
#   )
# region_summary = as.data.frame(region_summary)















# Facility locations and type
liberia_fa_GIS = st_read("data_files/liberia/GPS/liberia_CSI_coordonnees_geographiques.gpkg") # Facility-GPS points
names(liberia_fa_GIS)

liberia_fa_GIS$amenity = "CSI"
table(liberia_fa_GIS$amenity)

liberia_fa_GIS$FacilityType = liberia_fa_GIS$amenity
liberia_fa_GIS$Ownership <- "Public"; table(liberia_fa_GIS$Ownership)
table(liberia_fa_GIS$FacilityType)
table(liberia_fa_GIS$Ownership)

liberia_fa_GIS = rename(liberia_fa_GIS, geometry = geom); names(liberia_fa_GIS)
st_crs(liberia_fa_GIS) <- 4326
summary(liberia_fa_GIS$geometry)
liberia_fa_GIS <- liberia_fa_GIS[!is.na(st_geometry(liberia_fa_GIS)), ]


gg_facility_map <- ggplot() +
  geom_sf(data = liberia_limits, fill = "lightgray", color = "black", size = 0.2) +
  geom_sf(data = liberia_fa_GIS, aes(color = Ownership, shape = FacilityType), size = 2) +  # Ensure aes() includes variables as intended
  scale_color_manual(values = c("Public" = "purple")) +
  scale_shape_manual(values = c("CSI" = 19)) +
  labs(title = "Public Health Centres Location in liberia",
       caption = "Source: GADM | Ministry of Public Health",
       color = "Ownership",
       shape = "Facility Type") +
  theme_minimal()
print(gg_facility_map)
ggsave("Maps/Facility_classification_liberia.png", gg_facility_map, width = 12, height = 10, dpi = 300)



# Amount of facilities per region
summary(liberia_fa_GIS$geometry) # facility coords
table(liberia_limits$NAME_1) # regions/provinces
summary(liberia_limits$geometry)
table(liberia_fa_GIS$Ownership) # public/private
table(liberia_fa_GIS$FacilityType) #hospital/other

liberia_fa_GIS <- st_as_sf(liberia_fa_GIS)
liberia_limits <- st_as_sf(liberia_limits)
facilities_with_regions <- st_join(liberia_fa_GIS, liberia_limits, join = st_within)

fa_per_reg_total <- facilities_with_regions %>%
  group_by(NAME_1, FacilityType) %>%
  summarise(Total_Facilities = n(), .groups = 'drop') %>%
  spread(key = FacilityType, value = Total_Facilities, fill = 0)

fa_per_reg_total <- fa_per_reg_total %>%
  group_by(NAME_1) %>%
  summarise(
    Health_Centres = sum(CSI, na.rm = TRUE),
  )
fa_per_reg_total <- fa_per_reg_total %>%
  rename(Region = NAME_1)

total_row <- fa_per_reg_total %>%
  summarise(
    Region = "Total",
    Health_Centres = sum(Health_Centres, na.rm = TRUE),
  )
fa_per_reg_total <- bind_rows(fa_per_reg_total, total_row); fa_per_reg_total
fa_per_reg_total = st_drop_geometry(fa_per_reg_total)

openxlsx::write.xlsx(fa_per_reg_total, "results/liberia_Health_Facilities_Per_Region.xlsx", rowNames = FALSE)


# Road lengths (KM), population and transport costs
fuel_price = 1.1 # pump price for diesel fuel (US$ per liter) # we assume == U$D 1.10
fuel_cost_per_km <- (fuel_price / 12) # As a general rule, a gasoline car consumes one liter per 12 kilometers, which translates into a consumption of 4 to 12 liters of fuel per 100 kilometers.

names(liberia)
summary(liberia$geometry)
summary(liberia_limits$geometry)

liberia_limits = st_read("data_files/liberia/liberia_limits/gadm41_NER_0.shp"); names(liberia_limits)
liberia_roads = st_read("data_files/liberia/liberia_roads/liberia_roads.shp"); names(liberia_roads) # liberia Roads DATA #
table(liberia_roads$NTLCLASS)
liberia_roads2 = subset(liberia_roads, NTLCLASS == "liberia", drop = FALSE)
liberia_roads = liberia_roads2; rm(liberia_roads2)
mapview(liberia_limits, color = "red") + mapview(liberia_roads, color = "green")
summary(liberia_roads$LENGTH_KM) # I should calculate the transport costs for liberia






liberia_popul = openxlsx::read.xlsx("data_files/liberia/DRC_population and density.xlsx")
colnames(liberia_popul) <- c("Province", "Pop_density_per_km2_2019", "Area_km2", "Population_2019", "Old_region"); names(liberia_popul)

table(liberia$region_name)
table(liberia_limits$NAME_1)
table(liberia_popul$Old_region) # Old regions match with raster data
table(liberia_popul$Province) # New regions

liberia_roads <- st_transform(liberia_roads, st_crs(liberia_limits))
roads_by_province <- st_join(liberia_roads, liberia_limits, join = st_intersects)
table(roads_by_province$NAME_1)

province_road_lengths <- roads_by_province %>%
  group_by(NAME_1) %>%
  dplyr::summarize(total_road_length_km = sum(LENGTH_KM, na.rm = TRUE)); summary(province_road_lengths$total_road_length_km)

table(province_road_lengths$NAME_1)
table(liberia_popul$Province)

province_data <- left_join(province_road_lengths, liberia_popul, by = c("NAME_1" = "Province")); province_data

province_data <- province_data %>%
  mutate(per_capita_transport_cost = (total_road_length_km * fuel_cost_per_km) / Population_2019); province_data

summary(province_data$total_road_length_km)
table(province_data$NAME_1)
table(liberia_limits$NAME_1)

province_data_no_geom <- st_drop_geometry(province_data)

merged_data <- left_join(liberia_limits, province_data_no_geom, by = "NAME_1")

merged_data = rename(merged_data, Province = NAME_1); names(merged_data)
merged_data$total_road_length_km <- round(merged_data$total_road_length_km, 0)
summary(merged_data$total_road_length_km)

colors <- c("darkgreen", "#1B9E77", "orange", "red", "darkred")
values <- rescale(c(0, 0.5, 1))
gg_map_costs = ggplot(data = merged_data) +
  geom_sf(aes(fill = per_capita_transport_cost), color = "white") +
  geom_sf_text(aes(label = paste(Province, '\nPopulation:', Population_2019, '\nRoad Km:', round(total_road_length_km))), 
               size = 3, check_overlap = TRUE) +
  scale_fill_gradientn(colors = colors, 
                       values = values, 
                       name = "Per Capita\nTransport Cost", 
                       labels = scales::comma, 
                       limits = range(merged_data$per_capita_transport_cost, na.rm = TRUE)) +
  labs(title = "Per Capita Transport Costs",
       subtitle = "By province in the Democratic Republic of the liberia",
       caption = "Source: DHS Survey Datasets | Global Roads Open Access Data Set (NASA)") +
  theme_minimal()
print(gg_map_costs)
ggsave("Maps/transport_costs_liberia.png", gg_map_costs, width = 12, height = 10, dpi = 300)

rm(list=ls())

































