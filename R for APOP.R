# Load packages -----------------------------------------------------------

library(anesr) 
library(devtools) 
library(dplyr)
library(here)
library(tidyverse)
library(haven)


# Read and recode ANES 2020  from local file ------------------------------

anes_2020 <- read_csv(here("anes_2020.csv"))


# Rename ANES 2020 for free trade ------------------------------

anes_2020 <- anes_2020 %>% rename(
  party_id_orig = V201018,
  free_trade = V202359, 
  free_trade_intensity = V202360, 
  free_trade_relationships = V202362,
  free_trade_job = V202365, 
  free_trade_intl = V202367x)

# Create party ID variable (ANES 2020) ------------------------------

anes_2020 <- anes_2020 %>%  mutate( 
  pid_chr = case_when(
    party_id_orig == 2 ~ "rep",
    party_id_orig == 1 ~ "dem",
    party_id_orig == 4 ~ "ind"),
  republican = if_else(pid_chr == "rep", "yes", "no"))

#  Re-code the intensities, 7-point free trade scale from -3 to 3 for opinion in free trade---------


anes_2020 <- anes_2020 %>%  mutate (opinion_free_trade = case_when( free_trade == 3 ~ 0,
                                                                    free_trade == 1 & free_trade_intensity == 1 ~ 3,  
                                                                    free_trade == 1 &  free_trade_intensity == 2 ~ 2,
                                                                    free_trade == 1 &  free_trade_intensity == 3 ~ 1,
                                                                    free_trade == 2 &  free_trade_intensity == 1 ~ -3,
                                                                    free_trade == 2 &  free_trade_intensity == 2 ~ -2,
                                                                    free_trade == 2 &  free_trade_intensity == 3 ~ -1))                                                   ))




#calculation of Stabdard Deviation for each party (ANES-2020)----

anes_2020 %>% group_by(pid_chr) %>%
  summarise(sd_free_trade = sd(opinion_free_trade, na.rm = TRUE))

# Same process for read and re-code ANES 2016  from local file ------------------------------
anes_2016 <- read_dta("~/anes_timeseries_2016.dta")


anes_2016<- anes_2016 %>% rename(party_id_orig = V161019,
  free_trade = V162176, 
  free_trade_intensity = V162176a)

anes_2016 <- anes_2016 %>%  mutate(
    pid_chr = case_when(
    party_id_orig == 2 ~ "rep",
    party_id_orig == 1 ~ "dem",
    party_id_orig == 4 ~ "ind"),
    republican = if_else(pid_chr == "rep", "yes", "no"))
    

  
  
anes_2016 <- anes_2016 %>%  mutate(opinion_free_trade = case_when( free_trade == 3 ~ 0,
                                                                      free_trade == 1 & free_trade_intensity == 1 ~ 3,  
                                                                      free_trade == 1 &  free_trade_intensity == 2 ~ 2,
                                                                      free_trade == 1 &  free_trade_intensity == 3 ~ 1,
                                                                      free_trade == 2 &  free_trade_intensity == 1 ~ -3,
                                                                      free_trade == 2 &  free_trade_intensity == 2 ~ -2,
                                                                      free_trade == 2 &  free_trade_intensity == 3 ~ -1))

                                   
                                   
anes_2020 <- anes_2020 %>%  mutate (opinion_free_trade = case_when( free_trade == 3 ~ 0,
                                                                                                       free_trade == 1 & free_trade_intensity == 1 ~ 3,  
                                                                                                       free_trade == 1 &  free_trade_intensity == 2 ~ 2,
                                                                                                       free_trade == 1 &  free_trade_intensity == 3 ~ 1,
                                                                                                       free_trade == 2 &  free_trade_intensity == 1 ~ -3,
                                                                                                       free_trade == 2 &  free_trade_intensity == 2 ~ -2,
                                                                                                       free_trade == 2 &  free_trade_intensity == 3 ~ -1))                                                    ))



# Some basic statistic -------
table(anes_2016$opinion_free_trade) 
table(anes_2020$opinion_free_trade) 

hist(anes_2016$opinion_free_trade)
hist(anes_2020$opinion_free_trade)

boxplot(anes_2020$opinion_free_trade, anes_2016$opinion_free_trade)
t.test (anes_2016$opinion_free_trade, anes_2020$opinion_free_trade) 

#Sample calculation of SD for each party------
anes_2020 %>% group_by(pid_chr) %>%
  summarise(sd_free_trade = sd(opinion_free_trade, na.rm = TRUE))

anes_2016 %>% group_by(pid_chr) %>%
  summarise(sd_free_trade = sd(opinion_free_trade, na.rm = TRUE))

#Sample calculation of mean for each party----

anes_2016 %>% group_by(pid_chr) %>% summarise(mean_free_trade = mean(opinion_free_trade, na.rm = TRUE))

anes_2020 %>% group_by(pid_chr) %>%
  summarise(mean_free_trade = mean(opinion_free_trade, na.rm = TRUE))
  
