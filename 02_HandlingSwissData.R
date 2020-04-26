############################################################
#### Get Corona Data for Switzerland (by Canton)
############################################################
# Load required packages
library(tidyverse)

############################################################
#### Load Data
############################################################
dat_infections <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland_openzh.csv")
dat_deaths <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_fatalities_switzerland_openzh.csv")
dat_deaths$AI <- as.double(dat_deaths$AI)

# Function to fill NAs
fillNA <- function(x){
  x[1, 2:ncol(x)] <- 0
  for (i in 2:ncol(x)){
    for (j in 2:nrow(x)){
      if (is.na(x[j, i])){
        x[j, i] <- x[j-1, i]
      }
    }
  }
  return(x)
}

# Fill NA's
dat_infections <- fillNA(dat_infections)
dat_deaths <- fillNA(dat_deaths)

# Remove CH column
dat_infections$CH <- NULL
dat_deaths$CH <- NULL

# Prepare infections
dat_infections <- dat_infections %>%
  gather(key = "Code", value = "ConfirmedCases", 2:ncol(.)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Case = "Infections")

# Prepare deaths
dat_deaths <- dat_deaths %>%
  gather(key = "Code", value = "ConfirmedCases", 2:ncol(.)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Case = "Deaths")

# Put the data on infections, deaths and recovieries together
dat_che <- rbind(dat_infections, dat_deaths)

# Prepare dataframe that shows cantonal names
canton <- list(
    "Aargau" = "AG"
  , "Appenzell Innerrhoden" =	"AI"
  , "Appenzell Ausserrhoden" = "AR"
  , "Bern" = "BE"
  , "Basel-Landschaft" = "BL"
  , "Basel-Stadt" = "BS"
  , "Fribourg" = "FR"
  , "Genève" = "GE"
  , "Glarus" = "GL"
  , "Graubünden" = "GR"
  , "Jura" = "JU"
  , "Lucerne" = "LU"
  , "Neuchâtel"	= "NE"
  , "Nidwalden" = "NW"
  , "Obwalden" = "OW"
  , "Sankt Gallen" = "SG"
  , "Schaffhausen" = "SH"
  , "Solothurn" = "SO"
  , "Schwyz" = "SZ"
  , "Thurgau" = "TG"
  , "Ticino" = "TI"
  , "Uri" = "UR"
  , "Vaud" = "VD"
  , "Valais" = "VS"
  , "Zug" = "ZG"
  , "Zürich" = "ZH"
)
canton <- data.frame(do.call(rbind, canton))
canton$Canton <- rownames(canton)
names(canton) <- c("Code", "Canton")
rownames(canton) <- NULL

# Join cantons to our data
dat_che <- left_join(dat_che, canton)
