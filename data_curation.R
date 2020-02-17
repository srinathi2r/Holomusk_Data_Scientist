#### Loading the necessary packages #####
rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(stringr)
setwd("/Users/srinath/Downloads/dataScienceTask/")

### Loading all the csv files ####
glucose_data = read.csv("T_glucose.csv")
creat_data = read.csv("T_creatinine.csv")
meds_data = read.csv("T_meds.csv")
hgb_data = read.csv("T_HGB.csv")
ldl_data = read.csv("T_ldl.csv")

dbp_data = read.csv("T_DBP.csv")
sbp_data = read.csv("T_SBP.csv")

demo_data = read.csv("T_demo.csv")
stage_data = read.csv("T_stage.csv")


###### Dealing with Glucose Data #######
# Aggregating to Patient-level and computing summary statistcs

agg_glucose_data = glucose_data %>% group_by(id) %>%
  summarise(mean_hba1c = mean(value),
            first_hba1c = first(value),
            latest_hba1c = last(value))

# Computing the change in HBA1C levels from the first to last readings
agg_glucose_data = agg_glucose_data %>% mutate(change_in_hba1c = latest_hba1c - first_hba1c)


######## Dealing with Medicines ########
# Aggregating at patient, drug level
agg_pat_drug = meds_data %>% select(id, drug) %>% group_by(id, drug) %>% distinct()

# Aggregating at patient level and collapsing the drug names as csv
agg_drugs_data = agg_pat_drug %>% group_by(id) %>%
  summarise(drugs = paste0(drug, ",", collapse = " "),
            n_drugs = n())
# Only 272 patients have drugs info. Assuming that the rest of the 28 patients take no drugs
agg_drugs_data = left_join(select(demo_data, id), agg_drugs_data, by = "id")
agg_drugs_data$n_drugs[is.na(agg_drugs_data$n_drugs)] = 0
agg_drugs_data$drugs[is.na(agg_drugs_data$drugs)] = ''

## Deducing the DHL comorbidities based on the drugs
# 1. metformin, canagliflozin, dapagliflozin = Diabetes
# 2. atenolol, bisoprolol, irbesartan, labetalol, losartan, carvedilol
# 2. metoprolol, nebivolol, olmesartan, propranolol, telmisartan, valsartan = Hypertension
# 3. lovastatin, pitavastatin, pravastatin, rosuvastatin, simvastatin = HyperLipidemia

agg_drugs_data = agg_drugs_data %>% mutate(diabetes = ifelse(str_detect(agg_drugs_data$drugs, 'metformin'), 1,
                                                             ifelse(str_detect(agg_drugs_data$drugs, 'canagliflozin'), 1,
                                                                    ifelse(str_detect(agg_drugs_data$drugs, 'dapagliflozin'), 1, 0))))
agg_drugs_data = agg_drugs_data %>% mutate(hl = ifelse(str_detect(agg_drugs_data$drugs, 'lovastatin'), 1,
                                                       ifelse(str_detect(agg_drugs_data$drugs, 'pitavastatin'), 1,
                                                              ifelse(str_detect(agg_drugs_data$drugs, 'pravastatin'), 1,
                                                                     ifelse(str_detect(agg_drugs_data$drugs, 'rosuvastatin'), 1,
                                                                            ifelse(str_detect(agg_drugs_data$drugs, 'simvastatin'), 1, 0))))))

agg_drugs_data = agg_drugs_data %>% mutate(ht = ifelse(str_detect(agg_drugs_data$drugs, 'atenolol'), 1, 
                                                       ifelse(str_detect(agg_drugs_data$drugs, 'bisoprolol'), 1, 
                                                              ifelse(str_detect(agg_drugs_data$drugs, 'irbesartan'), 1, 
                                                                     ifelse(str_detect(agg_drugs_data$drugs, 'labetalol'), 1, 
                                                                            ifelse(str_detect(agg_drugs_data$drugs, 'losartan'), 1, 
                                                                                   ifelse(str_detect(agg_drugs_data$drugs, 'carvedilol'), 1, 
                                                                                          ifelse(str_detect(agg_drugs_data$drugs, 'metoprolol'), 1, 
                                                                                                 ifelse(str_detect(agg_drugs_data$drugs, 'nebivolol'), 1, 
                                                                                                        ifelse(str_detect(agg_drugs_data$drugs, 'olmesartan'), 1, 
                                                                                                               ifelse(str_detect(agg_drugs_data$drugs, 'propranolol'), 1, 
                                                                                                                      ifelse(str_detect(agg_drugs_data$drugs, 'telmisartan'), 1, 
                                                                                                                             ifelse(str_detect(agg_drugs_data$drugs, 'valsartan'), 1, 0)))))))))))))



###### Dealing with Creatnine Data #######
# Aggregating to Patient-level and computing summary statistcs

agg_creat_data = creat_data %>% group_by(id) %>%
  summarise(mean_creat = mean(value),
            first_creat = first(value),
            latest_creat = last(value))

# Computing the change in Creatnine levels from the first to last readings
agg_creat_data = agg_creat_data %>% mutate(change_in_creat = latest_creat - first_creat)


###### Dealing with Haemoglobin Data #######
# Aggregating to Patient-level and computing summary statistcs

agg_hgb_data = hgb_data %>% group_by(id) %>%
  summarise(mean_hgb = mean(value),
            first_hgb = first(value),
            latest_hgb = last(value))

# Computing the change in Heamoglobin levels from the first to last readings
agg_hgb_data = agg_hgb_data %>% mutate(change_in_hgb = latest_hgb - first_hgb)

###### Dealing with LDL Data #######
# Aggregating to Patient-level and computing summary statistcs

agg_ldl_data = ldl_data %>% group_by(id) %>%
  summarise(mean_ldl = mean(value),
            first_ldl = first(value),
            latest_ldl = last(value))

# Computing the change in LDL levels from the first to last readings
agg_ldl_data = agg_ldl_data %>% mutate(change_in_ldl = latest_ldl - first_ldl)


###### Dealing with Diastolic BP Data #######
# Aggregating to Patient-level and computing summary statistcs

agg_dbp_data = dbp_data %>% group_by(id) %>%
  summarise(mean_dbp = mean(value),
            first_dbp = first(value),
            latest_dbp = last(value))

# Computing the change in Diastolic BP levels from the first to last readings
agg_dbp_data = agg_dbp_data %>% mutate(change_in_dbp = latest_dbp - first_dbp)

###### Dealing with Systolic BP Data #######
# Aggregating to Patient-level and computing summary statistcs

agg_sbp_data = sbp_data %>% group_by(id) %>%
  summarise(mean_sbp = mean(value),
            first_sbp = first(value),
            latest_sbp = last(value))

# Computing the change in Sysstolic BP levels from the first to last readings
agg_sbp_data = agg_sbp_data %>% mutate(change_in_sbp = latest_sbp - first_sbp)

###### Merging all the above curated datasets with the demographics and stage data
merged_data = left_join(demo_data, agg_dbp_data, by = "id")
merged_data = left_join(merged_data, agg_sbp_data, by = "id")
merged_data = left_join(merged_data, agg_glucose_data, by = "id")
merged_data = left_join(merged_data, agg_creat_data, by = "id")
merged_data = left_join(merged_data, agg_hgb_data, by = "id")
merged_data = left_join(merged_data, agg_ldl_data, by = "id")
merged_data = left_join(merged_data, agg_drugs_data, by = "id")
merged_data = left_join(merged_data, stage_data, by = "id")

setwd("/Users/srinath/Downloads/dataScienceTask/Holomusk_Data_Scientist/")
save(merged_data, file = "merged_data.RData")
