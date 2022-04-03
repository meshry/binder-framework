
hmda1 <- fread(file = "msamd_47894_actions_taken_1-3_loan_purposes_1_yr20.csv" , header = TRUE) %>%
  filter(!is.na(census_tract)) %>% 
  mutate(census_tract = as.character(census_tract))
hmda2 <- fread(file = "msamd_47894_actions_taken_1-3_loan_purposes_1_yr19.csv" , header = TRUE) %>%
  filter(!is.na(census_tract)) %>% 
  mutate(census_tract = as.character(census_tract)) 
hmda3 <- fread(file = "msamd_47894_actions_taken_1-3_loan_purposes_1_yr18.csv" , header = TRUE) %>%
  filter(!is.na(census_tract)) %>% 
  mutate(census_tract = as.character(census_tract))

library(readxl)
TRACT_ZIP_HUD1 <- read_excel("TRACT_ZIP_HUD_2020.xlsx",
                            col_types = c("text", "text", "numeric")) %>% 
  filter(TRACT %in% as.character(hmda1$census_tract)) %>% 
  rename(census_tract = TRACT) %>% 
  mutate(census_tract = as.character(census_tract)) 

length(unique(TRACT_ZIP_HUD1$census_tract))

TRACT_ZIP_HUD1 <- TRACT_ZIP_HUD1 %>% 
  group_by(census_tract) %>% 
  mutate(Max= max(RES_RATIO), N=n()) %>% 
  ungroup() %>%
  filter(RES_RATIO== Max) %>%
  select(census_tract, ZIP)

length(unique(TRACT_ZIP_HUD1$census_tract))


TRACT_ZIP_HUD2 <- read_excel("TRACT_ZIP_HUD_2019.xlsx",
                             col_types = c("text", "text", "numeric")) %>% 
  filter(TRACT %in% as.character(hmda2$census_tract)) %>% 
  rename(census_tract = TRACT) %>% 
  mutate(census_tract = as.character(census_tract)) 

length(unique(TRACT_ZIP_HUD2$census_tract))

TRACT_ZIP_HUD2 <- TRACT_ZIP_HUD2 %>% 
  group_by(census_tract) %>% 
  mutate(Max= max(RES_RATIO), N=n()) %>% 
  ungroup() %>%
  filter(RES_RATIO== Max) %>%
  select(census_tract, ZIP) %>%
  distinct(census_tract, .keep_all= TRUE)

length(unique(TRACT_ZIP_HUD2$census_tract))



TRACT_ZIP_HUD3 <- read_excel("TRACT_ZIP_HUD_2018.xlsx",
                             col_types = c("text", "text", "numeric")) %>% 
  filter(tract %in% as.character(hmda3$census_tract)) %>% 
  rename(census_tract = tract, ZIP = zip , RES_RATIO = res_ratio) %>% 
  mutate(census_tract = as.character(census_tract)) 

length(unique(TRACT_ZIP_HUD3$census_tract))

TRACT_ZIP_HUD3 <- TRACT_ZIP_HUD3 %>% 
  group_by(census_tract) %>% 
  mutate(Max= max(RES_RATIO), N=n()) %>% 
  ungroup() %>%
  filter(RES_RATIO== Max) %>%
  select(census_tract, ZIP) %>%
  distinct(census_tract, .keep_all= TRUE)

length(unique(TRACT_ZIP_HUD3$census_tract))


hmda1 <- merge(hmda1, TRACT_ZIP_HUD1, by= "census_tract")
hmda2 <- merge(hmda2, TRACT_ZIP_HUD2, by= "census_tract")
hmda3 <- merge(hmda3, TRACT_ZIP_HUD3, by= "census_tract")

rm(TRACT_ZIP_HUD1, TRACT_ZIP_HUD2, TRACT_ZIP_HUD3)

hmda <-  rbind( rbind(hmda1,hmda2), hmda3)
rm(hmda1, hmda2, hmda3)

hmda <- hmda %>% 
  filter(conforming_loan_limit == "C" &
           derived_loan_product_type %in% c("Conventional:First Lien",
                                            "FHA:First Lien") &
           preapproval == 2 &
           reverse_mortgage == 2&
           `open-end_line_of_credit` == 2&
           business_or_commercial_purpose == 2 &
           negative_amortization == 2 &
           interest_only_payment == 2 &
           balloon_payment == 2 &
           other_nonamortizing_features ==2 &
           occupancy_type == 1 & 
           derived_dwelling_category == "Single Family (1-4 Units):Site-Built") %>%
  mutate(coapplicant = case_when(
    `co-applicant_credit_score_type` %in% c(1:8) ~ 1 , 
    
    `co-applicant_ethnicity-1`       %in% c(1,2, 11,12,13,14)  ~ 1 , 
    `co-applicant_ethnicity-2`       %in% c(1,2, 11,12,13,14)  ~ 1 , 
    `co-applicant_ethnicity-3`       %in% c(1,2, 11,12,13,14)  ~ 1 , 
    `co-applicant_ethnicity-4`       %in% c(1,2, 11,12,13,14)  ~ 1 , 
    `co-applicant_ethnicity-5`       %in% c(1,2, 11,12,13,14)  ~ 1 , 
    
    `co-applicant_race-1`            %in% c(1,2,21,22,23,24,25,26,27,3, 4, 41, 42, 43, 44,5) ~ 1 , 
    `co-applicant_race-2`            %in% c(1,2,21,22,23,24,25,26,27,3, 4, 41, 42, 43, 44,5) ~ 1 , 
    `co-applicant_race-3`            %in% c(1,2,21,22,23,24,25,26,27,3, 4, 41, 42, 43, 44,5) ~ 1 , 
    `co-applicant_race-4`            %in% c(1,2,21,22,23,24,25,26,27,3, 4, 41, 42, 43, 44,5) ~ 1 , 
    `co-applicant_race-5`            %in% c(1,2,21,22,23,24,25,26,27,3, 4, 41, 42, 43, 44,5) ~ 1 , 
    
    `co-applicant_sex`               %in% c(1,2,6) ~ 1 ,
    
    
    `co-applicant_age`               %in% c("25-34","45-54","55-64","65-74","35-44",">74","<25" ) ~ 1 ,
    `co-applicant_age_above_62`      %in% c("No" , "Yes") ~ 1 , 
    TRUE ~ 0)) %>%
  select(-c(derived_dwelling_category, purchaser_type,
            `derived_msa-md`, loan_purpose, lien_status, 
            hoepa_status, prepayment_penalty_term,
            intro_rate_period, construction_method, 
            manufactured_home_land_property_interest, 
            manufactured_home_secured_property_type,
            `co-applicant_credit_score_type`,  
            multifamily_affordable_units, total_units,
            `applicant_ethnicity-1`, `applicant_ethnicity-2`, 
            `applicant_ethnicity-3`, `applicant_ethnicity-4`, 
            `applicant_ethnicity-5`, 
            `co-applicant_ethnicity-1`, `co-applicant_ethnicity-2`, 
            `co-applicant_ethnicity-3` ,  `co-applicant_ethnicity-4` , 
            `co-applicant_ethnicity-5` , 
           `applicant_race-1`, `applicant_race-2`, 
           `applicant_race-3`, `applicant_race-4`, 
            `applicant_race-5`,
           `co-applicant_race-1`, `co-applicant_race-2` , 
           `co-applicant_race-3`, `co-applicant_race-4` ,
           `co-applicant_race-5`, 
           applicant_ethnicity_observed, `co-applicant_ethnicity_observed`, 
           applicant_race_observed, `co-applicant_race_observed`,
           applicant_sex, `co-applicant_sex`, `co-applicant_age`,
           applicant_sex_observed, `co-applicant_sex_observed`,
           applicant_age_above_62, `co-applicant_age_above_62`,
           `aus-1` ,`aus-2`, `aus-3`, `aus-4`, `aus-5`, 
           `denial_reason-1`, `denial_reason-2`, 
           `denial_reason-3`, `denial_reason-4`,
           total_points_and_fees, origination_charges, discount_points,
           lender_credits, applicant_credit_score_type, 
           submission_of_application, initially_payable_to_institution,
           conforming_loan_limit,derived_loan_product_type,
            preapproval,reverse_mortgage,`open-end_line_of_credit` ,
            business_or_commercial_purpose, negative_amortization,
            interest_only_payment, balloon_payment, 
            other_nonamortizing_features, occupancy_type))


hmda <- hmda %>% select(-c(census_tract, state_code, county_code)) %>%
  mutate(ethnicity =
           recode(derived_ethnicity, 
                  `Ethnicity Not Available` = "NotAvailable" , 
                  `Free Form Text Only`     = NA_character_ ,
                  `Hispanic or Latino`      = "Hispanic" ,
                  Joint                     = "Mixed",
                  `Not Hispanic or Latino`  = "NotHispanic")) %>% 
  select(-c(derived_ethnicity)) %>% 
  mutate(race = recode(derived_race, 
                       `2 or more minority races`         = "Mixed" , 
                       `American Indian or Alaska Native` = "Other" ,
                       `Black or African American`        = "Black" ,
                       `Free Form Text Only`              = NA_character_  ,
                       `Native Hawaiian or Other Pacific Islander` = "Other" ,
                       `Race Not Available`               = "NotAvailable", 
                       Joint                               = "Mixed")) %>% 
  select(-c(derived_race)) %>% 
  mutate(sex = recode(derived_sex, 
                      `Sex Not Available` = "NotAvailable")) %>%
  select(-c(derived_sex)) %>%
  mutate(action_taken = ifelse(test = action_taken == 3,yes = 0, no = action_taken), 
         Conventional = ifelse(test = loan_type == 1, yes = 1, no = 0)) %>%
  select(-c(loan_type)) 

hmda <- hmda %>% 
  mutate(debt_to_income_ratio = 
           case_when(
             debt_to_income_ratio == "<20%"       ~ "<20%" ,
             debt_to_income_ratio == ">60%"       ~ ">60%" ,
             debt_to_income_ratio == "20%-<30%"   ~ "20%-<30%",
             debt_to_income_ratio == "30%-<36%"   ~ "30%-<36%" ,
             debt_to_income_ratio == "36"         ~ "36%-<41%" ,
             debt_to_income_ratio == "37"         ~ "36%-<41%" ,
             debt_to_income_ratio == "38"         ~ "36%-<41%" ,
             debt_to_income_ratio == "39"         ~ "36%-<41%" ,
             debt_to_income_ratio == "40"         ~ "36%-<41%" ,
             debt_to_income_ratio == "41"         ~ "41%-<46%" ,
             debt_to_income_ratio == "42"         ~ "41%-<46%" ,
             debt_to_income_ratio == "43"         ~ "41%-<46%" ,
             debt_to_income_ratio == "44"         ~ "41%-<46%" ,
             debt_to_income_ratio == "45"         ~ "41%-<46%" ,
             debt_to_income_ratio == "46"         ~ "46%-<50%" ,
             debt_to_income_ratio == "47"         ~ "46%-<50%" ,
             debt_to_income_ratio == "48"         ~ "46%-<50%" ,
             debt_to_income_ratio == "49"         ~ "46%-<50%" ,
             debt_to_income_ratio == "50%-60%"    ~ "50%-60%"))
  

hmda <- hmda %>% 
  mutate(applicant_age = case_when(
    applicant_age == "<25"            ~ "<25" ,
    applicant_age ==  ">74"           ~ ">74" ,
    applicant_age ==  "25-34"         ~ "25-34",
    applicant_age ==  "30%-<36%"      ~ "30%-<36%" ,
    applicant_age ==  "35-44"         ~ "35-44" ,
    applicant_age ==  "45-54"         ~ "45-54" ,
    applicant_age ==  "55-64"         ~ "55-64" ,
    applicant_age ==  "65-74"         ~ "65-74" ,
    applicant_age ==  "8888 "         ~ NA_character_ ))

hmda <- hmda %>% mutate_at(c('loan_to_value_ratio', 'interest_rate', 'rate_spread',
                             'total_loan_costs', 'loan_term', 'property_value'), as.numeric)
hmda <- hmda %>% 
  drop_na(loan_to_value_ratio, loan_term, property_value, 
          income, debt_to_income_ratio, applicant_age, 
          ethnicity, race, sex)
hmdazero <- hmda %>% 
  filter(action_taken==0) 

hmdaone  <- hmda %>% 
  filter(action_taken==1) %>%
  drop_na(interest_rate, rate_spread, total_loan_costs)

hmda <- rbind(hmdaone, hmdazero)

rm(hmdaone, hmdazero)

hmda <- hmda %>% 
  filter(loan_amount > 20000 & loan_amount <= 1000000 & 
           loan_to_value_ratio <= 100 & 
           (total_loan_costs<=40000 | is.na(total_loan_costs)) &
           loan_term >= 120 &
           property_value >= 40000 & property_value <= 1000000 &
           income >= 10 & income <= 1000 )
hmda <- hmda %>% 
  group_by(lei) %>% 
  mutate(N=n()) %>% 
  filter(N>=100) %>% 
  ungroup() %>%
  select(-c(N)) 

write_csv(hmda,"hmda_full.csv" )
library(fastDummies)
hmda <- dummy_cols(.data = hmda, 
                   select_columns = c("activity_year" ,"debt_to_income_ratio" ,
                                      "applicant_age" ,
                                      "ethnicity" , "race", 'sex') ,
                   remove_first_dummy = TRUE , 
                   ignore_na = TRUE,remove_selected_columns = TRUE)

hmda