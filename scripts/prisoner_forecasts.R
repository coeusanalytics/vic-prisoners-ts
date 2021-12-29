
#########################################################
# CHANGE HISTORY
#########################################################

# v1.0
# - Initialised.
# - Produced first model for forecasting male and female 
# - Evaluated forecast accuracy

#########################################################
# PACKAGES
#########################################################

# data import
library(readxl)

# data manipulation
library(tidyverse)
library(lubridate)
library(stringr)
library(magrittr)
library(naniar)

# time series tools
library(tsibble)
library(fable)
library(fabletools)

# other tools
library(tictoc)
library(here) 

#########################################################
# DATA IMPORT
#########################################################

# read the prisoner numbers dataset
prison_raw <- readxl::read_excel(here::here("./data/Monthly time series prisoner and offender data Nov21.xlsx"),
                                 skip = 3,
                                 col_names = FALSE,
                                 sheet = "Table 2 - Prisoner receptions")

# horrible code but will do for now
# clean up column names
prison_colnames <- cbind(
  prison_raw %>%
    slice(1) %>%
    unlist() %>% 
    tibble() %>% 
    fill(everything()) %>% 
    `colnames<-`("col_names1") %>% 
    mutate(col_names1 = replace(col_names1, row_number() == 1, "Month")),
  prison_raw %>%
    slice(2) %>%
    unlist() %>% 
    tibble() %>% 
    fill(everything()) %>% 
    `colnames<-`("col_names2") %>% 
    mutate(col_names2 = replace(col_names2, row_number() == 1, "Month"))
) %>% 
  mutate(final_names = ifelse(col_names1 == "Month", "Month", paste0(col_names1, "_", col_names2))) %>% 
  select(final_names) %>% 
  pull(final_names)

# set column names
prison_raw <- prison_raw %>% 
  slice(-c(1:2)) %>% 
  `colnames<-`(prison_colnames) %>% 
  janitor::clean_names()

#########################################################
# DATA TRANSFORMATIONS
#########################################################

# transform original data set
prison_data_long <- prison_raw %>% 
  mutate(across(male_receptions_sentenced_at_reception:all_receptions_total, ~ as.numeric(.))) %>% 
  mutate(month = lubridate::as_date(month)) %>% 
  tidyr::pivot_longer(data = .,
                      cols = 2:ncol(.),
                      names_to = "population_name",
                      values_to = "prisoner_count") 

# group by only gender
prison_pop_data <- prison_data_long %>% 
  filter(str_detect(population_name, "male|female"), !str_detect(population_name, "total")) %>% 
  mutate(population_name2 = stringr::str_replace_all(population_name, "_receptions|_at_reception", "")) %>% 
  separate(population_name2, into = c("gender", "sentence_status_reception"), remove = TRUE) %>% 
  select(month, gender, sentence_status_reception, prisoner_count) %>% 
  mutate(gender = tools::toTitleCase(gender),
         sentence_status_reception = tools::toTitleCase(sentence_status_reception))

# convert into a tsibble for ease of modelling
prison_pop_data_ts <- prison_pop_data %>% 
  mutate(month = tsibble::yearmonth(month)) %>% 
  as_tsibble(index = month, key = c("gender", "sentence_status_reception"))

#########################################################
# MODELLING
#########################################################

# fit multiple models
mod1 <- prison_pop_data_ts %>% 
  filter(month <= yearmonth("2020 Nov")) %>%
  model(base = ETS(prisoner_count),
        arima1 = ARIMA(prisoner_count)) 

# produce future predictions of prison receptions
fc1 <- mod1 %>% 
  forecast(h = 12)

#########################################################
# DATA VISUALISATION
# autoplot() is convenient but prefer to built the
# ggplot2 base from the ground up - will do
#########################################################

# compare the forecast vs actual for 12-month period
fc1 %>% 
  autoplot(
    prison_pop_data_ts %>% 
      filter(month >= yearmonth("2010 Nov"), month <= yearmonth("2021 Nov")),
    alpha = 0.7, level = 90
  ) +
  theme_bw() +
  labs(title = "Prisoner reception numbers",
       subtitle = "Grouped by gender and sentence status at reception",
       x = "Month",
       y = "Number of prisoners",
       caption = "Source: Victoria State Government - Justice and Community Safety
       
       @coeus-analytics") +
  facet_wrap(gender ~ sentence_status_reception, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
#########################################################
# EVALUATING FORECAST ACCURACY
#########################################################

# filter on the time window for evaluation
prison_test <- prison_pop_data_ts %>% 
  filter(month >= yearmonth("2010 Jan"), month <= yearmonth("2021 Nov"))

# evaluate accuracy of forecast
accuracy(fc1, prison_test)
