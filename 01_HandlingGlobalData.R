############################################################
#### Get Global Corona Data
############################################################
# Load required packages
library(tidyverse)
library(lubridate)
library(RCurl)
library(countrycode)
library(wppExplorer)
library(rworldmap)
library(wbstats)

############################################################
#### Download and Clean Required Data
############################################################
# Specify the Github links to the different files of the John Hopkins University
links <- list(
    Infections  = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  , Deaths      = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  , Recoveries  = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
)

# Load data and do some cleaning
dat_world <- links %>%
  lapply(read_csv)  %>%
  bind_rows(., .id = "Case") %>%
  gather(key = "Date", value = "ConfirmedCases", 6:ncol(.)) %>%
  mutate(ConfirmedCases = replace_na(ConfirmedCases, 0)) %>%
  rename(Country = "Country/Region", Region = "Province/State") %>%
  dplyr::select(-c(Region, Long, Lat)) %>%
  mutate(Country = gsub("Diamond Princess", "Cruise Ships", Country)) %>%
  mutate(Country = gsub("MS Zaandam", "Cruise Ships", Country)) %>%
  mutate(Country = gsub("Kosovo", "Serbia", Country)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(Date = update(Date, year = 2020)) %>%
  mutate(CountryCode = countrycode(
      Country
    , origin = "country.name"
    , destination = "iso3c"
    )
  ) %>%
  mutate(CountryCodeShort = countrycode(
      Country
    , origin = "country.name"
    , destination = "iso2c"
  )) %>%
  arrange(Country)

# Prepare population estimates (we need to merge two sources to get estimates
# for all countries)
pop1 <- "tpop" %>%
  wpp.indicator() %>%
  subset(Year == 2020) %>%
  rename(CountryCodeShort = charcode, Population = value) %>%
  dplyr::select(-Year)

pop2 <- "SP.POP.TOTL" %>%
  wb(indicator = ., startdate = 2018, enddate = 2020) %>%
  subset(date == 2018) %>%
  slice(-c(1:46)) %>%
  rename(CountryCodeShort = iso2c, Population = value) %>%
  dplyr::select(CountryCodeShort, Population) %>%
  subset(!(CountryCodeShort %in% pop1$CountryCodeShort)) %>%
  mutate(Population = Population / 1000)

# Put estimates together
pop <- rbind(pop1, pop2)

# Assign population estimates to the COVID data
dat_world <- left_join(dat_world, pop, by = "CountryCodeShort")

# We are still missing population estimates for some entries. We assign this
# manually
dat_world$Population[dat_world$Country == "Cruise Ships"] <- 3000
dat_world$Population[dat_world$Country == "Holy See"] <- 900

# Let's summarize the data by Country, Case, and Date
dat_world <- dat_world %>%
  group_by(Country, Case, Date) %>%
  summarize(
      ConfirmedCases  = sum(ConfirmedCases)
    , Population      = min(Population) * 1000
  ) %>%
  ungroup()
