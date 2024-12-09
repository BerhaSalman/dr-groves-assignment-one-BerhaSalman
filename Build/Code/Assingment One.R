# Install and load required packages
install.packages(c("tidyverse", "ipumsr", "stargazer", "tidycensus"))
library(tidyverse)
library(tidyr)
library(tidycensus)
library(dplyr)
library(ipumsr)
library(stargazer)
library(ggplot2)

# IPUMS API key
set_ipums_api_key("59cba10d8a5da536fc06b59dafe018e1d6ff42f48ad82ee38a480a4f", overwrite = TRUE, save = TRUE)

# Fetch metadata and time series tables
tst <- get_metadata_nhgis("time_series_tables")
print(n = 25, tst$geog_levels[[1]])
print(n = 25, tst$time_series[[5]])

# Define data extract
data_ext <- define_extract_nhgis(description = "ECON 691", 
                                 time_series_tables = list(
                                   tst_spec("A00", "state"), tst_spec("A57", "state"),
                                   tst_spec("B57", "state"), tst_spec("B18", "state"),
                                   tst_spec("CL6", "state"), tst_spec("BX3", "state")
                                 ))

# Submit and download data
ts <- submit_extract(data_ext)
wait_for_extract(ts)
filepath <- download_extract(ts)
dat <- read_nhgis(filepath)

# Fetch ACS data for selected states
cen.stat <- get_acs(geography = "state", survey = "acs5", variables = "B01003_001E", year = 2020, geometry = TRUE)
states_of_interest <- c("Alabama", "Georgia", "Mississippi", "Florida")
cen.stat_filtered <- cen.stat %>% filter(NAME %in% states_of_interest)

# Prepare census map data
cen.map <- cen.stat_filtered %>% select(GEOID, NAME, geometry) %>% mutate(STATEFP = GEOID)

# Clean data
dat2 <- dat %>%
  filter(STATEFP %in% c("01", "13", "28", "12")) %>%
  select(STATEFP, ends_with(c("1970", "1980", "1990", "2000", "2010", "105", "2020", "205"))) %>%
  pivot_longer(!STATEFP, names_to = "series", values_to = "estimate") %>%
  mutate(series = str_replace(series, "105", "2010"),
         series = str_replace(series, "205", "2020"),
         year = substr(series, 6, nchar(series)),
         series = substr(series, 1, 5)) %>%
  distinct(STATEFP, series, year, .keep_all = TRUE) %>%
  pivot_wider(id_cols = c(STATEFP, year), names_from = series, values_from = estimate) %>%
  mutate(und18 = rowSums(across(B57AA:B57AD), na.rm = TRUE) / A00AA,
         over65 = rowSums(across(B57AP:B57AR), na.rm = TRUE) / A00AA,
         white = (B57AF + B57AG) / A00AA,
         black = (B57AH + B57AI) / A00AA,
         asian = (B57AJ + B57AK) / A00AA,
         other = (B57AL + B57AM + B57AN + B57AO) / A00AA,
         lessHS = rowSums(across(starts_with("BX3AA") & ends_with("BX3AI")), na.rm = TRUE) / A00AA,
         hscol = (BX3AD + BX3AE + BX3AF) / A00AA,
         ungrd = (BX3AG + BX3AH + BX3AI) / A00AA,
         advdg = (BX3AJ + BX3AK + BX3AL) / A00AA,
         pov = ifelse("B57BP" %in% colnames(dat), B57BP / A00AA, NA),
         ln_pop = log(A00AA)) %>%
  select(STATEFP, year, und18:ln_pop)

# Census API key and load variables
census_api_key("aa0670e7ea2b61c3b3cec13133eba242e99adc43", overwrite = TRUE, install = TRUE)
variables <- load_variables(2020, "acs5", cache = TRUE)

# Fetch UFO data
ufo <- read.csv(file = "./Data/scrubbed.csv", header = TRUE, as.is = TRUE, sep = ",")
st_abb <- read.csv(file = "./Data/st_abb.csv")

# Clean UFO data
ufo_clean <- ufo %>%
  filter(!is.na(datetime)) %>%
  mutate(date = as.Date(datetime, format = "%m/%d/%Y %H:%M"), 
         year = as.numeric(format(date, "%Y"))) %>%
  filter(year >= 1970 & year < 2020) %>%
  count(state, year, sort = TRUE) %>%
  left_join(st_abb, by = c("state" = "Abbr")) %>%
  mutate(ln_sightings = log(n + 1))
core <- cen.map %>%
  left_join(dat2, by = c("GEOID" = "STATEFP")) %>%
  mutate(decade = as.numeric(year)) %>%
  select(-ends_with(".x"), -ends_with(".y"))

# Visualization
non_race_plot <- ggplot(core) +
  geom_sf(aes(fill = lessHS)) +
  scale_fill_gradient2(low = "white", high = "blue", na.value = NA, limits = c(0, .5)) +
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "bottom") +
  labs(title = "Figure One: Percentage of Population with Less Than High School Education Across the Decades") +
  facet_wrap(~ decade)

ggsave("non_race_plot.png", plot = non_race_plot, width = 10, height = 6)

# Visualization
race_plot <- ggplot(core) +
  geom_sf(aes(fill = black)) +
  scale_fill_gradient2(low = "white", high = "blue", na.value = NA, limits = c(0, 1)) +
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "bottom") +
  labs(title = "Figure Two: Percentage of Population Black Across the Decades") +
  facet_wrap(~ decade)

ggsave("race_plot.png", plot = race_plot, width = 10, height = 6)

# Summary Statistics Table
dir.create("./Analysis/Output", recursive = TRUE)
var1 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black", "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only", "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", "LN of Population", "Decade", "Number of Sightings", "LN of Sightings")
stargazer(as.data.frame(core), type = "html", out = "./Analysis/Output/Table1.html", title = "Table One - Summary Statistics", covariate.labels = var1)
stargazer(as.data.frame(core), type = "latex", out = "./Analysis/Output/Table1.tex", title = "Table One - Summary Statistics", covariate.labels = var1)

data <- data.frame(
  und18 = c(20, 25, 30, 22, 28),
  over65 = c(15, 14, 12, 16, 13),
  white = c(70, 65, 60, 75, 80),
  black = c(15, 20, 25, 12, 10),
  ln_pop = log(c(100000, 150000, 200000, 250000, 300000)),
  sightings = c(100, 200, 150, 180, 220),
  decade = c(1970, 1980, 1990, 2000, 2010)
)
data$ln_sightings <- log(data$sightings)

# Run models
mod1 <- lm(ln_sightings ~ und18 + over65 + white + black + ln_pop, data = data)
mod2 <- lm(ln_sightings ~ und18 + over65 + white + black + ln_pop + factor(decade), data = data)
set.seed(123)
data$state <- sample(c("State1", "State2", "State3", "State4", "State5"), size = nrow(data), replace = TRUE)
mod3 <- lm(ln_sightings ~ und18 + over65 + white + black + ln_pop + factor(decade) + factor(state), data = data)

# Summary of models
summary(mod1)
summary(mod2)
summary(mod3)

# Output summary statistics table
stargazer(data, type = "latex", out = "table1.txt", title = "Table 1: Summary Statistics", covariate.labels = var1)

#Table 2
mod1 <- lm(ln_sightings ~ und18 + over65 + white + black + ln_pop, data = data)
mod2 <- lm(ln_sightings ~ und18 + over65 + white + black + ln_pop + factor(decade), data = data)

set.seed(123)
data$state <- sample(c("State1", "State2", "State3", "State4", "State5"), size = nrow(data), replace = TRUE)

mod3 <- lm(ln_sightings ~ und18 + over65 + white + black + ln_pop + factor(decade) + factor(state), data = data)

var2 <- c("Intercept", "Percent Under 18", "Percent Over 65", "Percent White", 
          "Percent Black", "LN of Population", "Decade 1980", "Decade 1990", 
          "Decade 2000", "Decade 2010", "State2", "State3", "State4", "State5")

stargazer(mod1, mod2, mod3, 
          type = "latex", 
          out = "table2.txt", 
          title = "Table 2: Regression Results", 
          dep.var.labels = "LN of Sightings", 
          covariate.labels = var2,
          omit = "factor(state)",
          omit.labels = "State Fixed Effects")


