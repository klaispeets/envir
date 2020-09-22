#install.packages("dlookr") 
#install.packages("validate") 
#install.packages("data.table") 
#install.packages("stringr") 
#install.packages("tidyr")
rm(list=ls())

library(dlookr) 
library(validate) 
library(data.table) 
library(stringr) 
library(tidyr)

setwd("~/work/keskkonnamin/andmekvaliteedi_kontroll")

registry_waste_facility_address = read.csv("registry_waste_facility_address.csv", sep = ";", stringsAsFactors = F)
registry_waste_facility_log = read.csv("registry_waste_facility_log.csv", stringsAsFactors = F, sep = ";")
registry_waste_facility = read.csv("registry_waste_facility.csv", stringsAsFactors = F, sep = ";")
registry_waste_treatment_facility = read.csv("registry_waste_treatment_facility.csv", stringsAsFactors = F, sep = ";")
registry_well_group = read.csv("registry_well_group.csv", stringsAsFactors = F, sep = ";")
registry_well = read.csv("registry_well.csv", stringsAsFactors = F, sep = ";")


registry_waste_facility_log %>% diagnose_report(output_format = "html", output_file = "rregistry_waste_facility_log.html")

#Exploratory Data ANalysis Report
final %>% eda_report(output_format = "html", output_file = "EDA_Report.html")

#Existence rules
existence_rules = validator(exists_id = !is.na(variable))
check = confront(table, existence_rules, key = "variable") 
summary(check)

#Uniqueness rules
uniqueness_rules = validator(unique_id = is_unique(variable))

#Vastavuse reeglid

rules3 = validator(
  tyyp1 = variable_type1 %in% c('text1', 'text2'), 
  tyyp2 = variable_type2 %in% c('text1','text2'))

check = confront(table, rules3) 
output = as.data.frame(check) 
output[output$value == FALSE,]

#Mitme tabeli vaheline seos
tech_classifier_rules = validator(tech_data_conformance = tena_id %in% codelist)
check = confront(eh_tehna, tech_classifier_rules, 
                 ref = list(codelist = kl_tehna$id), key="tena_id")
summary(check)


#Kuupäeva formaadi kontroll
date_rules = validator(
  start_date_syntax = grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", eh_alust_kp),
  created_date_syntax = grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}\ [0-
                              9]{2}\\:[0-9]{2}\\:[0-9]{2}\\.[0-9]{6}$", date_created))

check = confront(eh_ehitised, date_rules) 
summary(check)



energy_class_rules = validator(
  mandatory_energy_class = if (esmane_kasutus >= 2020 & hoone_tyyp ==
                               'VAIKEELAMUD' & koetav_pind > 220) 
    energia_klass == 'ENERGIAKL_A')

check = confront(hoone_energia_margised, energy_class_rules, key="id") 
summary(check)


conditional_rules = validator(
  condition1 = if (!is.na(koetav_pind)) koetav_pind <= kasulik_pind)
check = confront(eh_ehitised, conditional_rules, key="id")

#Reegli kirjeldus: Omavalitsus on seotud vaid ühe maakonnaga.
dependency_rules = validator( dependency_county = omavalitsus ~ maakond)
check = confront(eh_ehitised, dependency_rules, key="ehr_kood") 
summary(check)



#Kõik korraga
eh_ehitised_rules = existence_rules + uniqueness_rules + ehitised_rules + pattern_rules + conditional_rules + dependency_rules
check = confront(eh_ehitised, eh_ehitised_rules, key="ehr_kood") 
summary(check)



