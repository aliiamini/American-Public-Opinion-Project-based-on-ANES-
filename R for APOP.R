# Load packages -----------------------------------------------------------

library(devtools) 
library(dplyr)
library(here)
library(tidyverse)
library(haven)
library(ggplot2)
# In the following steps I code for 2020 firs and then I repeat the process for 2016---------


# Read ANES 2020 and 2016 from local file ------------------------------

anes_2020 <- read.csv(here("anes_2020.csv"))
anes_2016 <- read_dta("~/anes_timeseries_2016.dta")


# Rename variable  for free trade ------------------------------

anes_2020 <- anes_2020 %>% rename(
  party_id_orig = V201018,
  free_trade = V202359, 
  free_trade_intensity = V202360, 
  free_trade_relationships = V202362,
  free_trade_job = V202365, 
  free_trade_intl = V202367x)


anes_2016<- anes_2016 %>% rename(party_id_orig = V161019,
                                 free_trade = V162176, 
                                 free_trade_intensity = V162176a)



# Create party ID variable ------------------------------

anes_2020 <- anes_2020 %>%  mutate( 
  pid_chr = case_when(
    party_id_orig == 2 ~ "rep",
    party_id_orig == 1 ~ "dem",
    party_id_orig == 4 ~ "ind"),
  republican = if_else(pid_chr == "rep", "yes", "no"))

anes_2016 <- anes_2016 %>%  mutate(
  pid_chr = case_when(
    party_id_orig == 2 ~ "rep",
    party_id_orig == 1 ~ "dem",
    party_id_orig == 4 ~ "ind"),
  republican = if_else(pid_chr == "rep", "yes", "no"))

#  Re-code the intensities, 7-point free trade scale from -3 to 3 for opinion in free trade---------


anes_2020 <- anes_2020 %>% 
  mutate (opinion_free_trade = case_when( free_trade == 3 ~ 0,
                              free_trade == 1 & free_trade_intensity == 1 ~ 3,  
                             free_trade == 1 &  free_trade_intensity == 2 ~ 2,
                              free_trade == 1 &  free_trade_intensity == 3 ~ 1,
                                free_trade == 2 &  free_trade_intensity == 1 ~ -3,
                               free_trade == 2 &  free_trade_intensity == 2 ~ -2,
                              free_trade == 2 &  free_trade_intensity == 3 ~ -1))  

anes_2016 <- anes_2016 %>% 
  mutate(opinion_free_trade = case_when( free_trade == 3 ~ 0,
                             free_trade == 1 & free_trade_intensity == 1 ~ 3,  
                             free_trade == 1 &  free_trade_intensity == 2 ~ 2,
                             free_trade == 1 &  free_trade_intensity == 3 ~ 1,
                             free_trade == 2 &  free_trade_intensity == 1 ~ -3,
                             free_trade == 2 &  free_trade_intensity == 2 ~ -2,
                             free_trade == 2 &  free_trade_intensity == 3 ~ -1))



#calculation of Standard Deviation and mean for each party in free  trade-------

anes_2020 %>% group_by(pid_chr) %>%
  summarise(sd_free_trade = sd(opinion_free_trade, na.rm = TRUE))

anes_2020 %>% group_by(pid_chr) %>%
  summarise(mean_free_trade = mean(opinion_free_trade, na.rm = TRUE))



anes_2016 %>% group_by(pid_chr) %>%
  summarise(sd_free_trade = sd(opinion_free_trade, na.rm = TRUE))

anes_2016 %>% group_by(pid_chr) %>% 
  summarise(mean_free_trade = mean(opinion_free_trade, na.rm = TRUE))
# t-test----
t.test (anes_2016$opinion_free_trade, anes_2020$opinion_free_trade)

#visualization----
ggplot(data = anes_2016,
       mapping = aes(x=anes_2016$opinion_free_trade, fill = anes_2016$pid_chr)) +
  geom_bar(position = "dodge") + scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()

ggplot(data = anes_2020,
       mapping = aes(x=anes_2020$opinion_free_trade, fill = anes_2020$pid_chr)) +
  geom_bar(position = "dodge") + scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()

boxplot(anes_2016$opinion_free_trade, anes_2020$opinion_free_trade,
        names = c('2016', '2020'), col=c('red', 'blue'), main='Public oppinion in health care')


ggplot(anes_2016, aes(anes_2016$pid_chr,anes_2016$opinion_free_trade)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))

ggplot(anes_2020, aes(anes_2020$pid_chr,anes_2020$opinion_free_trade)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))

-------------------------------------------------------------------------------------------------


#create a variable for opinion_health_care-----

anes_2020 <- anes_2020 %>% 
  mutate (opinion_health_care = case_when( V202380x == 4 ~ 0,
                                         V202380x == 1  ~ 3,  
                                         V202380x == 2  ~ 2,
                                         V202380x == 3  ~ 1,
                                         V202380x == 7  ~ -3,
                                         V202380x == 6  ~ -2,
                                         V202380x == 5 ~ -1)) 

anes_2016 <- anes_2016 %>% 
  mutate(opinion_health_care= case_when( V162193x == 4 ~ 0,
                                         V162193x == 1  ~ 3,  
                                         V162193x == 2  ~ 2,
                                         V162193x == 3  ~ 1,
                                         V162193x == 7  ~ -3,
                                         V162193x == 6  ~ -2,
                                         V162193x == 5 ~ -1)) 


#calculation of Standard Deviation and mean for each party in health care----

anes_2020 %>% group_by(pid_chr) %>%
  summarise(sd_health_care = sd(opinion_health_care, na.rm = TRUE))

anes_2020 %>% group_by(pid_chr) %>%
  summarise(mean_health_care = mean(opinion_health_care, na.rm = TRUE))


anes_2016 %>% group_by(pid_chr) %>%
  summarise(sd_health_care = sd(opinion_health_care, na.rm = TRUE))

anes_2016 %>% group_by(pid_chr) %>%
  summarise(mean_health_care = mean(opinion_health_care, na.rm = TRUE))


# t-test-----
t.test(anes_2016$opinion_health_care, anes_2020$opinion_health_care)


#visualization----


ggplot(data = anes_2016,
       mapping = aes(x=anes_2016$opinion_health_care, fill = anes_2016$pid_chr), col= c('blue', 'green', 'red','white')) +
  geom_bar(position = "dodge")+ scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()

ggplot(data = anes_2020,
       mapping = aes(x=anes_2020$opinion_health_care, fill = anes_2020$pid_chr)) +
  geom_bar(position = "dodge")+ scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()


boxplot(anes_2016$opinion_health_care,anes_2020$opinion_health_care, 
        names = c('2016', '2020'), col=c('red', 'blue'), main='Public oppinion in health care')


ggplot(anes_2016, aes(anes_2016$pid_chr,anes_2016$opinion_health_care)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))

ggplot(anes_2020, aes(anes_2020$pid_chr,anes_2020$opinion_health_care)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))



------------------------------------------------------------------------------------------------------


  




# create a variable for building war at Mexico borders-------------
anes_2020 <- anes_2020 %>% 
  mutate (opinion_wall_Mexico = case_when( V201426x == 4 ~ 0,
                                           V201426x == 1  ~ 3,  
                                           V201426x == 2  ~ 2,
                                           V201426x == 3  ~ 1,
                                           V201426x == 7  ~ -3,
                                           V201426x == 6  ~ -2,
                                           V201426x == 5 ~ -1)) 


anes_2016 <- anes_2016 %>% 
  mutate (opinion_wall_Mexico = case_when( V161196x == 4 ~ 0,
                                           V161196x == 1  ~ 3,  
                                           V161196x == 2  ~ 2,
                                           V161196x == 3  ~ 1,
                                           V161196x == 7  ~ -3,
                                           V161196x == 6  ~ -2,
                                           V161196x == 5 ~ -1))


#calculation of Standard Deviation and mean for each party in  building war at Mexico -------

anes_2020 %>% group_by(pid_chr) %>%
  summarise(sd_wall_Mexico  = sd(opinion_wall_Mexico , na.rm = TRUE))


anes_2020 %>% group_by(pid_chr) %>%
  summarise(mean_wall_Mexico  = mean(opinion_wall_Mexico , na.rm = TRUE))


anes_2016 %>% group_by(pid_chr) %>%
  summarise(sd_wall_Mexico  = sd(opinion_wall_Mexico , na.rm = TRUE))


anes_2016 %>% group_by(pid_chr) %>%
  summarise(mean_wall_Mexico  = mean(opinion_wall_Mexico , na.rm = TRUE))

# t.test----------
t.test(anes_2016$opinion_wall_Mexico, anes_2020$opinion_wall_Mexico)


# visualization---------

ggplot(data = anes_2016,
       mapping = aes(x=anes_2016$opinion_wall_Mexico, fill = anes_2016$pid_chr)) +
  geom_bar(position = "dodge")+scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()

ggplot(data = anes_2020,
       mapping = aes(x=anes_2020$opinion_wall_Mexico, fill = anes_2020$pid_chr)) +
  geom_bar(position = "dodge") +scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()


boxplot(anes_2016$opinion_wall_Mexico,anes_2020$opinion_wall_Mexico, 
        names = c('2016', '2020'), col=c('red', 'blue'), main='Public oppinion in health care')


ggplot(anes_2016, aes(anes_2016$pid_chr,anes_2016$opinion_wall_Mexico)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))

ggplot(anes_2020, aes(anes_2020$pid_chr,anes_2020$opinion_wall_Mexico)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))



------------------------------------------------------------------------------------------------------------------






--------------------------------------------------------------------------------------------------------------------
# create a variable for PRESIDENT HANDLING JOB scale -2, -1, +1, +2-------------------

anes_2020 <- anes_2020 %>% 
  mutate (opinion_persident_job = case_when(V201129x == 3 ~ -1,
                                            V201129x == 4 ~ -2,
                                            V201129x == 2  ~ 1,
                                            V201129x == 1  ~ 2)) 



anes_2016 <- anes_2016 %>% 
  mutate (opinion_persident_job = case_when(V161082x == 3 ~ -1,
                                            V161082x == 4 ~ -2,
                                            V161082x == 2  ~ 1,
                                            V161082x == 1  ~ 2))

#calculation of Standard Deviation and mean for PRESIDENT HANDLING JOB -------



anes_2020 %>% group_by(pid_chr) %>%
  summarise(sd_opinion_persident_job = sd(opinion_persident_job, na.rm = TRUE))

anes_2020 %>% group_by(pid_chr) %>%
  summarise(mean_opinion_persident_job = mean(opinion_persident_job, na.rm = TRUE))


anes_2016 %>% group_by(pid_chr) %>%
  summarise(sd_opinion_persident_job = sd(opinion_persident_job, na.rm = TRUE))

anes_2016 %>% group_by(pid_chr) %>%
  summarise(mean_opinion_persident_job = mean(opinion_persident_job, na.rm = TRUE))

# t.test-------------
t.test(anes_2016$opinion_persident_job, anes_2020$opinion_persident_job)




# visualization--------------

ggplot(data = anes_2016,
       mapping = aes(x=anes_2016$opinion_persident_job, fill = anes_2016$pid_chr)) +
  geom_bar(position = "dodge")+scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()

ggplot(data = anes_2020,
       mapping = aes(x=anes_2020$opinion_persident_job, fill = anes_2020$pid_chr)) +
  geom_bar(position = "dodge")+scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()

boxplot(anes_2016$opinion_persident_job,anes_2020$opinion_persident_job, 
        names = c('2016', '2020'), col=c('red', 'blue'), main='Public oppinion in health care')


ggplot(anes_2016, aes(anes_2016$pid_chr,anes_2016$opinion_persident_job)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))

ggplot(anes_2020, aes(anes_2020$pid_chr,anes_2020$opinion_persident_job)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))



----------------------------------------
  # create a variable for rising temperatures-------------

anes_2020 <- anes_2020 %>% 
  mutate (opinion_rising_temperatures = case_when( V201401 == 3    ~ 0,
                                                   V201401 == 1 & V201402 == 1 ~ 3,  
                                                   V201401 == 1 & V201402 == 2 ~ 2,
                                                   V201401 == 1  & V201402 == 3 ~ 1,
                                                   V201401 == 2 & V201402 == 1 ~ -3,
                                                   V201401 == 2 &V201402 == 2 ~ -2,
                                                   V201401 == 2 & V201402 == 3 ~ -1))  
anes_2016 <- anes_2016 %>% 
  mutate (opinion_rising_temperatures = case_when( V161225x == 4 ~ 0,
                                                   V161225x == 1  ~ 3,  
                                                   V161225x == 2  ~ 2,
                                                   V161225x == 3  ~ 1,
                                                   V161225x == 7  ~ -3,
                                                   V161225x == 6  ~ -2,
                                                   V161225x == 5 ~ -1))


#calculation of Standard Deviation and mean for each party in  rising temperatures -------

anes_2020 %>% group_by(pid_chr) %>%
  summarise(sd_rising_temperatures  = sd(opinion_rising_temperatures , na.rm = TRUE))

anes_2020 %>% group_by(pid_chr) %>%
  summarise(mean_rising_temperatures  = mean(opinion_rising_temperatures , na.rm = TRUE))


anes_2016 %>% group_by(pid_chr) %>%
  summarise(sd_rising_temperatures  = sd(opinion_rising_temperatures , na.rm = TRUE))

anes_2016 %>% group_by(pid_chr) %>%
  summarise(mean_rising_temperatures  = mean(opinion_rising_temperatures , na.rm = TRUE))

#t-test----------

t.test(anes_2016$opinion_rising_temperatures, anes_2020$opinion_rising_temperatures)

#visualization--------
ggplot(data = anes_2016,
       mapping = aes(x=anes_2016$opinion_rising_temperatures, fill = anes_2016$pid_chr)) +
  geom_bar(position = "dodge")+scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()

ggplot(data = anes_2020,
       mapping = aes(x=anes_2020$opinion_rising_temperatures, fill = anes_2020$pid_chr)) +
  geom_bar(position = "dodge")+scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()


boxplot(anes_2016$opinion_rising_temperatures, anes_2020$opinion_rising_temperature, 
        names = c('2016', '2020'), col=c('red', 'blue'), main='Public oppinion in health care')


ggplot(anes_2016, aes(anes_2016$pid_chr, anes_2016$opinion_free_trade)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))

ggplot(anes_2020, aes(anes_2020$pid_chr,anes_2020$opinion_rising_temperature)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))






-----------------------------------------
  
  #create a variable for opinion_vote_mail for 2020
  anes_2020 <- anes_2020 %>% 
  mutate (opinion_vote_mail = case_when( V201356x == 4 ~ 0,
                                         V201356x == 1  ~ 3,  
                                         V201356x == 2  ~ 2,
                                         V201356x == 3  ~ 1,
                                         V201356x == 7  ~ -3,
                                         V201356x == 6  ~ -2,
                                         V201356x == 5 ~ -1)) 






#calculation of Standard Deviation and mean for each party in vote mail  trade (only for ANES-2020)----

anes_2020 %>% group_by(pid_chr) %>%
  summarise(sd_vote_mail = sd(opinion_vote_mail, na.rm = TRUE))



anes_2020 %>% group_by(pid_chr) %>%
  summarise(mean_vote_mail = mean(opinion_vote_mail, na.rm = TRUE))

ggplot(data = anes_2020,
       mapping = aes(x=anes_2020$opinion_vote_mail, fill = anes_2020$pid_chr)) +
  geom_bar(position = "dodge")+scale_fill_manual(values=c('blue', 'green', 'red', 'white')) +theme_dark()


ggplot(anes_2020, aes(anes_2020$pid_chr,anes_2020$opinion_vote_mail)) + 
  geom_boxplot(col=c('blue','green', 'red', 'white'))


boxplot(anes_2020$opinion_vote_mail, 
         col=c('blue'), main='Public oppinion vote by mail')
# End of codes -----





