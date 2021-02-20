# Load required libraries:
library(pacman)
p_load(tidyverse, janitor, lubridate, zoo, ggplot2, cowplot, here, MMWRweek)

# Read in case data:
raw_data <- read_csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv")

# Process case data:
case_data_all <- raw_data %>%
  # Clean column names:
  clean_names() %>%
  # Select relevant columns:
  select(year_week, country, continent, indicator, weekly_count, rate_14_day, cumulative_count, population) %>%
  filter(indicator == "cases") %>%
  # Convert year-week format into dates:
  mutate(year = str_sub(year_week, 1, 4),
         year = as.numeric(year),
         week = str_sub(year_week, 6, 8),
         week = as.numeric(week),
         date = MMWRweek2Date(year, week, 2)) %>%
  # Delete temporary helper columns:
  select(-one_of("year", "week", "year_week", "indicator")) %>%
  rename(weekly_cases = weekly_count,
         cumulative_cases = cumulative_count,
         cases_14day_rollsum_per100000 = rate_14_day)

# Process deaths data:
deaths_data_all <- raw_data %>%
  # Clean column names:
  clean_names() %>%
  # Select relevant columns:
  select(year_week, country, continent, indicator, weekly_count, rate_14_day, cumulative_count, population) %>%
  filter(indicator == "deaths") %>%
  # Convert year-week format into dates:
  mutate(year = str_sub(year_week, 1, 4), 
         year = as.numeric(year),
         week = str_sub(year_week, 6, 8),
         week = as.numeric(week),
         date = MMWRweek2Date(year, week, 2)) %>%
  # Delete temporary helper columns:
  select(-one_of("year", "week", "year_week", "indicator")) %>%
  rename(weekly_deaths = weekly_count,
         cumulative_deaths = cumulative_count,
         deaths_14day_rollsum_per1000000 = rate_14_day)

# Join case and deaths datasets:
clean_data_countries_all <- left_join(case_data_all, deaths_data_all) %>%
  relocate(date, .before = country) %>%
  relocate(population, .before = weekly_cases)
  
# Add new cases to master raw dataframe:
clean_data_countries_master <- read_csv(here("Data Files", "Clean data_all_master.csv"))
new_entries <- anti_join(clean_data_countries_all, clean_data_countries_master)
clean_data_countries_master <- bind_rows(clean_data_countries_master, new_entries)

# Remove old entries from master raw dataframe:
if(all_equal(clean_data_countries_all, clean_data_countries_master) != TRUE) {
  obsolte_entries <- anti_join(clean_data_countries_master, clean_data_countries_all)
  clean_data_countries_master <- anti_join(clean_data_countries_master, obsolte_entries)
}

# Write results back to csv:
clean_data_countries_master <- clean_data_countries_master %>%
  arrange(country, date) %>%
  write_csv(here("Data Files", "Clean data_all_master.csv"))

rm(list = c("clean_data_countries_master", "case_data_all", "deaths_data_all", "raw_data", "new_entries"))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Filter for relevant countries:
relevant_countries <- c("Germany", "Netherlands", "Sweden", "Spain", "Italy", "United Kingdom", "Ireland", "United States", "Russia", "France", "Belgium", "Greece")

clean_data_countries_filtered <- clean_data_countries_all %>%
  filter(country %in% relevant_countries) %>%
  arrange(country, date)

# Add calculated columns:
clean_data_countries_filtered <- clean_data_countries_filtered %>%
  group_by(country) %>%
  mutate(weekly_cases_per100000 = weekly_cases / (population / 100000),
         cum_cases_per100000 = cumulative_cases / (population / 100000),
         weekly_deaths_per100000 = weekly_deaths / (population / 100000),
         cum_deaths_per100000 = cumulative_deaths / (population / 100000)) %>%
  ungroup()

# Write results back to csv:
as_of_date_cases_deaths <- max(clean_data_countries_filtered$date)
output_file_name <- paste(as_of_date_cases_deaths, " Clean data_filtered.csv", sep = "")
write_csv(clean_data_countries_filtered, here("Data Files", output_file_name))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set caption:
chart_caption_cases_deaths <- paste("Source: ECDC data as of", as_of_date_cases_deaths, sep = " ")

# Plot cases:
plot_cases_7day_rollsum <- ggplot(clean_data_countries_filtered, aes(x = date, y = weekly_cases)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cases_7day_rollsum_per100000 <- ggplot(clean_data_countries_filtered, aes(x = date, y = weekly_cases_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_country_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cases_14day_rollsum_per100000 <- ggplot(clean_data_countries_filtered, aes(x = date, y = cases_14day_rollsum_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_14day sum_country_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_14day_rollsum_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_cases <- ggplot(clean_data_countries_filtered, aes(x = date, y = cumulative_cases)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum cases_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_per100000 <- ggplot(clean_data_countries_filtered, aes(x = date, y = cum_cases_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum cases_country_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plot deaths:
plot_deaths_7day_rollsum <- ggplot(clean_data_countries_filtered, aes(x = date, y = weekly_deaths)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 deaths",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths_7day sum_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_7day_rollsum, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_deaths_7day_rollsum_per100000 <- ggplot(clean_data_countries_filtered, aes(x = date, y = weekly_deaths_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths_7day sum_country_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_7day_rollsum_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_deaths_14day_rollsum_per1000000 <- ggplot(clean_data_countries_filtered, aes(x = date, y = deaths_14day_rollsum_per1000000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 deaths per 1,000,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths_14day sum_country_per1M",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_14day_rollsum_per1000000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths <- ggplot(clean_data_countries_filtered, aes(x = date, y = cumulative_deaths)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum deaths_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_per100000 <- ggplot(clean_data_countries_filtered, aes(x = date, y = cum_deaths_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum deaths_country_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Cases and deaths charts by continent

# Process data:
clean_data_continent <- clean_data_countries_all %>%
  filter(str_detect(country, "total") | country == "United States" | country == "China") %>%
  group_by(country) %>%
  mutate(weekly_cases_per100000 = weekly_cases / (population / 100000),
         cum_cases_per100000 = cumulative_cases / (population / 100000),
         weekly_deaths_per100000 = weekly_deaths / (population / 100000),
         cum_deaths_per100000 = cumulative_deaths / (population / 100000)) %>%
  ungroup()

# Plot cases:
plot_cases_7day_rollsum_continent <- ggplot(clean_data_continent, aes(x = date, y = weekly_cases)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cases_7day_rollsum_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = weekly_cases_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_continent_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_continent_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cases_14day_rollsum_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = cases_14day_rollsum_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_14day sum_continent_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_14day_rollsum_continent_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_continent <- ggplot(clean_data_continent, aes(x = date, y = cumulative_cases)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum cases_continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = cum_cases_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum cases_continent_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases_continent_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

#TODO - fix continent-level death charts

# Plot deaths:
plot_weekly_deaths_continent <- ggplot(clean_data_continent, aes(x = date, y = weekly_deaths)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 deaths",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths - weekly - continent", ".png", sep = "")
ggsave(filename = file_name, plot = plot_weekly_deaths_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_weekly_deaths_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = weekly_deaths_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Weekly COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths - weekly - per 100,000 - by continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_weekly_deaths_continent_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_deaths_14day_rollsum__continent_per1000000 <- ggplot(clean_data_continent, aes(x = date, y = deaths_14day_rollsum_per1000000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 deaths per 1,000,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths -  14day rollsum - per 1,000,000 - by continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_14day_rollsum__continent_per1000000, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_continent <- ggplot(clean_data_continent, aes(x = date, y = cumulative_deaths)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cumulative deaths - continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths_continent, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = cum_deaths_per100000)) +
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cumulative deaths  - per 100,000 - by continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths_continent_per100000, path = here("Charts"), scale = 1, width = 15, height = 10)

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
as_of_date_hospital <- max(hospital_data$date)
chart_caption_hospital <- paste("Source: ECDC data as of", as_of_date_hospital, sep = " ")

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
       caption = chart_caption_hospital)

file_name <- paste(as_of_date_hospital, " Daily hospital occupancy",  ".png", sep = "")
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
       caption = chart_caption_hospital)

file_name <- paste(as_of_date_hospital, " Daily ICU occupancy",  ".png", sep = "")
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
       caption = chart_caption_hospital)

file_name <- paste(as_of_date_hospital, " Weekly new hospital admissions per 100,000 inhabitants",  ".png", sep = "")
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
       caption = chart_caption_hospital)

file_name <- paste(as_of_date_hospital, " Weekly new ICU admissions per 100,000 inhabitants",  ".png", sep = "")
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
         cum_vacs_astrazeneca = dosen_astrazeneca_kumulativ,
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
         new_vacs_astrazeneca = cum_vacs_astrazeneca - lag(cum_vacs_astrazeneca, n = 1L, default = 0),
         new_vacs_elderly = cum_vacs_elderly - lag(cum_vacs_elderly, n = 1L, default = 0),
         new_vacs_medical_profession = cum_vacs_medical_profession - lag(cum_vacs_medical_profession, n = 1L, default = 0),
         new_vacs_medical_condition = cum_vacs_medical_condition - lag(cum_vacs_medical_condition, n = 1L, default = 0),
         new_vacs_care_homes = cum_vacs_care_homes - lag(cum_vacs_care_homes, n = 1L, default = 0),
         new_vacs_all_7day_rollsum = rollsum(new_vacs_all, 7, fill = NA, align = "right"),
         new_vacs_pfizer_7day_rollsum = rollsum(new_vacs_pfizer, 7, fill = NA, align = "right"),
         new_vacs_moderna_7day_rollsum = rollsum(new_vacs_moderna, 7, fill = NA, align = "right"),
         new_vacs_astrazeneca_7day_rollsum = rollsum(new_vacs_astrazeneca, 7, fill = NA, align = "right"))

# Set chart caption:
as_of_date_vaccinations <- max(vaccination_data$date)
chart_caption_vaccinations <- paste("Source: Bundesministerium für Gesundheit data as of", as_of_date_vaccinations, sep = " ")

# Create charts:
plot_new_vacs_7day_rollsum <- vaccination_data %>%
  select(date, new_vacs_all_7day_rollsum, new_vacs_pfizer_7day_rollsum, new_vacs_moderna_7day_rollsum, new_vacs_astrazeneca_7day_rollsum) %>%
  rename(All = new_vacs_all_7day_rollsum, Pfizer = new_vacs_pfizer_7day_rollsum, Moderna = new_vacs_moderna_7day_rollsum, AstraZeneca = new_vacs_astrazeneca_7day_rollsum) %>%
  pivot_longer(cols = c("All", "Pfizer", "Moderna", "AstraZeneca"), names_to = "Type of vaccine") %>%
  ggplot(aes(x = date, y = value)) +
  facet_wrap(~`Type of vaccine`, scales = "free_y") + 
  geom_line(color = "cadetblue", size = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum administered vacs in Germany",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " New vacs 7-day rolling sum",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_new_vacs_7day_rollsum, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_new_vacs_dose <- vaccination_data %>%
  select(date, new_vacs_first_dose_all, new_vacs_second_dose_all) %>%
  rename(`First dose` = new_vacs_first_dose_all, `Second dose` = new_vacs_second_dose_all) %>%
  pivot_longer(cols = contains("dose"), names_to = "Dose") %>%
  ggplot(aes(x = date, y = value, fill = `Dose`)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Daily administered vacs in Germany by dose",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Daily administered vacs in Germany by dose",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_new_vacs_dose, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_new_vacs_type <- vaccination_data %>%
  select(date, new_vacs_pfizer, new_vacs_moderna, new_vacs_astrazeneca) %>%
  rename(Pfizer = new_vacs_pfizer, Moderna = new_vacs_moderna, AstraZeneca = new_vacs_astrazeneca) %>%
  pivot_longer(cols = c("Pfizer", "Moderna", "AstraZeneca"), names_to = "Type of vaccine") %>%
  ggplot(aes(x = date, y = value, fill = `Type of vaccine`)) +
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
  select(date, new_vacs_elderly, new_vacs_medical_profession, new_vacs_medical_condition, new_vacs_care_homes) %>%
  rename(`Vacs for elderly` = new_vacs_elderly, `Vacs for medical professionals` = new_vacs_medical_profession, `Vacs for pre-existing conditions` = new_vacs_medical_condition, `Vacs for nursing home residents` = new_vacs_care_homes) %>%
  pivot_longer(cols = contains("Vacs"), names_to = "Type of recipient") %>%
  ggplot(aes(x = date, y = value, fill = `Type of recipient`)) +
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

plot_cum_vacs <- vaccination_data %>%
  select(date, cum_vacs_pfizer, cum_vacs_moderna, cum_vacs_astrazeneca) %>%
  rename(Pfizer = cum_vacs_pfizer, Moderna = cum_vacs_moderna, AstraZeneca = cum_vacs_astrazeneca) %>%
  pivot_longer(cols = c("Pfizer", "Moderna", "AstraZeneca"), names_to = "Type of vaccine") %>%
  ggplot(aes(x = date, y = value, fill = `Type of vaccine`)) +
  geom_area() +
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative number of vaccinations administered",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Cumulative vaccinations administered",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cum_vacs, path = here("Charts"), scale = 1, width = 15, height = 10)

plot_cum_vacs_proportions <- vaccination_data %>%
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
  
file_name <- paste(as_of_date_vaccinations, " Cumulative proportion of German population vaccinated",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cum_vacs_proportions, path = here("Charts"), scale = 1, width = 15, height = 10)

# Read in and process Germany vaccine delivery data:
vaccine_delivery_data <- read_tsv("https://impfdashboard.de/static/data/germany_deliveries_timeseries.tsv")

vaccine_delivery_data <- vaccine_delivery_data %>%
  clean_names() %>%
  rename(vaccine_type = impfstoff,
         delivered_doses = dosen) %>%
  mutate(vaccine_type = recode(vaccine_type,
                               "comirnaty" = "Pfizer",
                               "moderna" = "Moderna",
                               "astra" = "AstraZeneca")) %>%
  group_by(vaccine_type) %>%
  mutate(cum_delivered_doses = cumsum(delivered_doses)) %>%
  ungroup()

plot_vaccine_deliveries <- vaccine_delivery_data %>%
  rename(`Type of vaccine` = vaccine_type) %>%
  ggplot(aes(x = date, y = delivered_doses, fill = `Type of vaccine`)) + 
  geom_col() +
  scale_x_date(date_breaks = "5 days") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Delivered vaccine doses by type",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Delivered vaccine doses by type",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_vaccine_deliveries, path = here("Charts"), scale = 1, width = 15, height = 10)