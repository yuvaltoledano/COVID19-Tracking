# Load required libraries:
library(pacman)
p_load(tidyverse, janitor, lubridate, zoo, ggplot2, cowplot, RcppRoll, here, ggthemes, MMWRweek, gmailr)

# Read in and process data:
raw_data <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

raw_data <- raw_data %>%
  clean_names() %>%
  select(date_rep, year_week, cases_weekly, deaths_weekly, countries_and_territories, pop_data2019, continent_exp, notification_rate_per_100000_population_14_days) %>%
  rename(date = date_rep, country = countries_and_territories, population = pop_data2019, continent = continent_exp) %>%
  mutate(date = dmy(date))

# Clean continent column:
raw_data$continent[raw_data$continent == "America" & raw_data$country == "United_States_of_America"] <- "USA"
raw_data$continent[raw_data$continent == "America" & raw_data$country != "United_States_of_America"] <- "America - ex USA"
raw_data$continent[raw_data$continent == "Asia" & raw_data$country == "China"] <- "China"
raw_data$continent[raw_data$continent == "Asia" & raw_data$country != "China"] <- "Asia - ex China"

western_europe <- c("United_Kingdom", "Ireland", "Netherlands", "Belgium", "Luxembourg", "France", "Spain", "Portugal", "Germany", "Switzerland", "Italy", "Austria", "Norway", "Sweden", "Finland", "Denmark", "Monaco", "San_Marino", "Holy_See", "Liechtenstein", "Iceland", "Gibraltar", "Guernsey", "Jersey")
raw_data$continent[raw_data$continent == "Europe" & raw_data$country %in% western_europe] <- "Western Europe"
raw_data$continent[raw_data$continent == "Europe"] <- "Rest of Europe"

# Add new cases to master raw dataframe:
raw_data_master <- read_csv(here("Data Files", "Raw data_new version.csv"))
new_entries <- anti_join(raw_data, raw_data_master)
raw_data_master <- bind_rows(raw_data_master, new_entries)

# Remove old entries from master raw dataframe:
if(all_equal(raw_data, raw_data_master) != TRUE) {
  obsolte_entries <- anti_join(raw_data_master, raw_data)
  raw_data_master <- anti_join(raw_data_master, obsolte_entries)
}

# Write results back to csv:
raw_data_master <- raw_data_master %>%
  arrange(country, date) %>%
  write_csv(here("Data Files", "Raw data_new version.csv"))

rm(raw_data_master)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Filter for relevant countries:
relevant_countries <- c("Germany", "Netherlands", "Sweden", "Spain", "Italy", "United_Kingdom", "Ireland", "United_States_of_America", "Russia", "France", "Belgium", "Greece")

clean_data <- raw_data %>%
  filter(country %in% relevant_countries) %>%
  arrange(country, date)

# Add calculated columns:
clean_data <- clean_data %>%
  group_by(country) %>%
  mutate(cum_cases = cumsum(cases_weekly),
         cum_cases_per100000 = cum_cases / (population / 100000),
         weekly_cases_per100000 = cases_weekly / (population / 100000),
         cum_deaths = cumsum(deaths_weekly),
         cum_deaths_per100000 = cum_deaths / (population / 100000),
         weekly_deaths_per100000 = deaths_weekly / (population / 100000)) %>%
  ungroup()

# Write results back to csv:
as_of_date <- max(clean_data$date)
output_file_name <- paste(as_of_date, " Clean data.csv", sep = "")
write_csv(clean_data, here("Data Files", output_file_name))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set caption:
chart_caption <- paste("Source: ECDC data as of", as_of_date, sep = " ")

# Plot cases:
plot_weekly_cases <- ggplot(clean_data, aes(x = date, y = cases_weekly)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 cases",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly cases",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_cases, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_cases <- ggplot(clean_data, aes(x = date, y = cum_cases)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative cases",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_per100000 <- ggplot(clean_data, aes(x = date, y = cum_cases_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative cases per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_weekly_cases_per100000 <- ggplot(clean_data, aes(x = date, y = weekly_cases_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly cases per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_cases_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plot deaths:
plot_weekly_deaths <- ggplot(clean_data, aes(x = date, y = deaths_weekly)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 deaths",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly deaths",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_deaths, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths <- ggplot(clean_data, aes(x = date, y = cum_deaths)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative deaths",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_per100000 <- ggplot(clean_data, aes(x = date, y = cum_deaths_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative deaths per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_weekly_deaths_per100000 <- ggplot(clean_data, aes(x = date, y = weekly_deaths_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly deaths per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_deaths_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Cases and deaths charts by continent

# Prepare data:
population_continent <- raw_data %>%
  select(country, continent, population) %>%
  distinct() %>%
  group_by(continent) %>%
  summarize(population_continent_total = sum(population, na.rm = TRUE)) %>%
  filter(population_continent_total > 0)

clean_data_continent <- raw_data %>%
  group_by(date, continent) %>%
  mutate(total_continent_weekly_cases = sum(cases_weekly),
         total_continent_weekly_deaths = sum(deaths_weekly)) %>%
  ungroup() %>%
  select(date, continent, total_continent_weekly_cases, total_continent_weekly_deaths) %>%
  distinct() %>%
  arrange(continent, date) %>%
  left_join(population_continent, by = "continent") %>%
  filter(continent != "Other")

clean_data_continent <- clean_data_continent %>%
  group_by(continent) %>%
  mutate(cum_cases = cumsum(total_continent_weekly_cases),
         cum_cases_per1000000 = cum_cases / (population_continent_total / 1000000),
         weekly_cases_per1000000 = total_continent_weekly_cases / (population_continent_total / 1000000),
         cum_deaths = cumsum(total_continent_weekly_deaths),
         cum_deaths_per1000000 = cum_deaths / (population_continent_total / 1000000),
         weekly_deaths_per1000000 = total_continent_weekly_deaths / (population_continent_total / 1000000)) %>%
  ungroup()

# Plot cases:
plot_weekly_cases_continent <- ggplot(clean_data_continent, aes(x = date, y = total_continent_weekly_cases)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~continent, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 cases",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly cases by continent",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_cases_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_weekly_cases_continent_per1000000 <- ggplot(clean_data_continent, aes(x = date, y = weekly_cases_per1000000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~continent) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 cases per 1,000,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly cases per 1,000,000 inhabitants by continent",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_cases_continent_per1000000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_per1000000_continent <- ggplot(clean_data_continent, aes(x = date, y = cum_cases_per1000000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~continent) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases per 1,000,000 inhabitants",
       caption = chart_caption)
  
file_name <- paste(as_of_date, " Cumulative cases per 1,000,000 inhabitants by continent",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases_per1000000_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_continent <- ggplot(clean_data_continent, aes(x = date, y = cum_cases)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~continent, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative cases by continent",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

# Plot deaths:
plot_weekly_deaths_continent <- ggplot(clean_data_continent, aes(x = date, y = total_continent_weekly_deaths)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~continent, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 deaths",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly deaths by continent",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_deaths_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_weekly_deaths_per1000000_continent <- ggplot(clean_data_continent, aes(x = date, y = weekly_deaths_per1000000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~continent) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 deaths per 1,000,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly deaths per 1,000,000 inhabitants by continent",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_deaths_per1000000_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_per1000000_continent <- ggplot(clean_data_continent, aes(x = date, y = cum_deaths_per1000000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~continent) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths per 1,000,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative deaths per 1,000,000 inhabitants by continent",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths_per1000000_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_continent <- ggplot(clean_data_continent, aes(x = date, y = cum_deaths)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~continent, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative deaths by continent",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Read in and process hospital and ICU data:
hospital_data <- read_csv("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv")

hospital_data <- hospital_data %>%
  clean_names() %>%
  select(country, indicator, year_week, date, value) %>%
  mutate(year = str_sub(year_week, 1, 4),
         year = as.numeric(year),
         week = str_sub(year_week, 7, 8),
         week = as.numeric(week),
         MMWRweek_date = MMWRweek2Date(year, week, 2),
         date = if_else(is.na(date), MMWRweek_date, date)) %>%
  select(country, indicator, date, value)

# Create daily occupancy charts:
relevant_countries <- c("Germany", "Netherlands", "Sweden", "Spain", "Italy", "United_Kingdom", "Ireland", "France", "Belgium")

plot_daily_hospital_occupancy <- hospital_data %>%
  filter(country %in% relevant_countries, indicator == "Daily hospital occupancy") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Daily hospital occupancy",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily hospital occupancy",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_hospital_occupancy, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_daily_icu_occupancy <- hospital_data %>%
  filter(country %in% relevant_countries, indicator == "Daily ICU occupancy") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Daily ICU occupancy",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily ICU occupancy",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_icu_occupancy, path = here("Charts"), scale = 1, width = 15, height = 10)

# Create weekly admissions charts:
plot_weekly_hospital_admissions_per100000 <- hospital_data %>%
  filter(country %in% relevant_countries, indicator == "Weekly new hospital admissions per 100k") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly new hospital admissions per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly new hospital admissions per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_hospital_admissions_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_weekly_icu_admissions_per100000 <- hospital_data %>%
  filter(country %in% relevant_countries, indicator == "Weekly new ICU admissions per 100k") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly new ICU admissions per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Weekly new ICU admissions per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_weekly_icu_admissions_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Read in and process Germany vaccination data:
vaccination_data <- read_tsv("https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv")

vaccination_data <- vaccination_data %>%
  clean_names() %>%
  rename(cum_vacs = dosen_kumulativ,
         new_vacs_all = dosen_differenz_zum_vortag,
         new_vacs_first_dose_all = dosen_erst_differenz_zum_vortag,
         new_vacs_second_dose_all = dosen_zweit_differenz_zum_vortag,
         cum_vacs_pfizer = dosen_biontech_kumulativ,
         cum_vacs_moderna = dosen_moderna_kumulativ,
         cum_persons_vaccinated_first_dose = personen_erst_kumulativ,
         cum_persons_vaccinated_second_dose = personen_voll_kumulativ,
         pct_pop_vaccinated_first_dose = impf_quote_erst,
         pct_pop_vaccinated_second_dose = impf_quote_voll,
         cum_vacs_elderly = indikation_alter_dosen,
         cum_vacs_medical_profession = indikation_beruf_dosen,
         cum_vacs_medical_condition = indikation_medizinisch_dosen,
         cum_vacs_care_homes = indikation_pflegeheim_dosen,
         cum_vacs_elderly_first_dose = indikation_alter_erst,
         cum_vacs_medical_profession_first_dose = indikation_beruf_erst,
         cum_vacs_medical_condition_first_dose = indikation_medizinisch_erst,
         cum_vacs_care_homes_first_dose = indikation_pflegeheim_erst,
         cum_vacs_elderly_second_dose = indikation_alter_voll,
         cum_vacs_medical_profession_second_dose = indikation_beruf_voll,
         cum_vacs_medical_condition_second_dose = indikation_medizinisch_voll,
         cum_vacs_care_homes_second_dose = indikation_pflegeheim_voll)

# Add calculated columns:
vaccination_data <- vaccination_data %>%
  mutate(new_vacs_pfizer = cum_vacs_pfizer - lag(cum_vacs_pfizer, n = 1L, default = 0),
         new_vacs_moderna = cum_vacs_moderna - lag(cum_vacs_moderna, n = 1L, default = 0),
         new_vacs_eldery = cum_vacs_elderly - lag(cum_vacs_elderly, n = 1L, default = 0),
         new_vacs_medical_profession = cum_vacs_medical_profession - lag(cum_vacs_medical_profession, n = 1L, default = 0),
         new_vacs_medical_condition = cum_vacs_medical_condition - lag(cum_vacs_medical_condition, n = 1L, default = 0),
         new_vacs_care_homes = cum_vacs_care_homes - lag(cum_vacs_care_homes, n = 1L, default = 0))

# Set chart caption:
as_of_date_vaccinations <- max(vaccination_data$date)
chart_caption_vaccinations <- paste("Source: Bundesministerium für Gesundheit data as of", as_of_date_vaccinations, sep = " ")

# Create charts:
plot_new_vacs_type <- vaccination_data %>%
  select(date, new_vacs_pfizer, new_vacs_moderna) %>%
  pivot_longer(cols = contains("vacs"), names_to = "type_of_vac") %>%
  ggplot(aes(x = date, y = value, fill = type_of_vac)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Daily administered vacs in Germany by type",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Daily administered vacs in Germany by type",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_new_vacs_type, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_new_vacs_recipient <- vaccination_data %>%
  select(date, new_vacs_eldery, new_vacs_medical_profession, new_vacs_medical_condition, new_vacs_care_homes) %>%
  pivot_longer(cols = contains("vacs"), names_to = "type_of_recipient") %>%
  ggplot(aes(x = date, y = value, fill = type_of_recipient)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Daily administered vacs in Germany by recipient",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Daily administered vacs in Germany by recipient",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_new_vacs_recipient, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_vacs_proportions <- vaccination_data %>%
  select(date, pct_pop_vaccinated_first_dose, pct_pop_vaccinated_second_dose) %>%
  rename(`First dose` = pct_pop_vaccinated_first_dose, `Both doses` = pct_pop_vaccinated_second_dose) %>%
  pivot_longer(cols = contains("dose"), names_to = "Dose") %>%
  ggplot(aes(x = date, y = value, color = Dose)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Proportions of German population vaccinated",
       caption = chart_caption_vaccinations)
  
file_name <- paste(as_of_date_vaccinations, " Cumulative oroportions of German population vaccinated",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_vacs_proportions, path = here("Charts"), scale = 1, width = 15, height = 10)