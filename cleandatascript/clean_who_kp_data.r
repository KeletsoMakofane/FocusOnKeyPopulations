library(tidyr)
library(stringr)
library(dplyr)

varnames <- read.csv("input_data/raw_data/varnames.csv")
data.mentions <- read.csv("input_data/raw_data/2018 07 04 Tables File kpmentions.csv")
data.msm      <- read.csv("input_data/raw_data/2018 07 04 Tables File MSM.csv") %>% mutate(pop = "msm") 
data.pris     <- read.csv("input_data/raw_data/2018 07 04 Tables File PRIS.csv") %>% mutate(pop = "pris") 
data.pwid     <- read.csv("input_data/raw_data/2018 07 04 Tables File PWID.csv") %>% mutate(pop = "pwid") 
data.sw       <- read.csv("input_data/raw_data/2018 07 04 Tables File SW.csv") %>% mutate(pop = "sw")
data.tg       <- read.csv("input_data/raw_data/2018 07 04 Tables File TG.csv") %>% mutate(pop = "tg")

names(data.mentions) <- c("country", "sw", "msm", "tg", "pwid", "pris", "alpha3")
names(data.msm)   <- c("country", varnames$name, "alpha3", "pop")
names(data.pris)  <- c("country", varnames$name, "alpha3", "pop")
names(data.pwid)  <- c("country", varnames$name, "alpha3", "pop")
names(data.sw)    <- c("country", varnames$name, "alpha3", "pop")
names(data.tg)    <- c("country", varnames$name, "alpha3", "pop")

data.mentions     <- data.mentions %>%
  pivot_longer(cols = 2:6, names_to = "pop", values_to = "value") %>%
  rename(mention = value)

data.indicators <- data.msm %>%
  rbind(data.pris) %>%
  rbind(data.pwid) %>%
  rbind(data.sw) %>%
  rbind(data.tg) %>%
  mutate(country = str_extract(country, "([^}]+?)(?= \\()")) %>%
  filter(country != "Grand total") %>%
  mutate(country = str_replace(country, fixed("Congo"),"Republic of the Congo")) %>%
  mutate(country = str_replace(country, fixed("Cote d'Ivoire"),  "Côte d’Ivoire")) %>%
  mutate(country = str_replace(country, fixed("Gambia"),  "The Gambia")) %>%
  mutate(country = str_replace(country, fixed("United Republic of Tanzania"),  "Tanzania, United Republic of")) %>%
  mutate(country = str_replace(country, fixed("Nambia"), "Namibia"))


data.who <- data.indicators %>%
  left_join(data.mentions, by = c("alpha3", "pop"))

write.csv(data.who, "input_data/who_data.csv")


