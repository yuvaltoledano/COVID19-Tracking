# Load required libraries:
library(pacman)
p_load(tidyverse, janitor, lubridate, zoo, ggplot2, cowplot, here, MMWRweek)

# Read in data:
raw_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", guess_max = 100000)

# Add new cases to master raw dataframe:
raw_data_master <- read_csv(here("Data Files", "Cases and deaths", "Raw data_master_owid.csv"), guess_max = 100000)
new_entries <- anti_join(raw_data, raw_data_master)
raw_data_master <- bind_rows(raw_data_master, new_entries)

# Remove old entries from master raw dataframe:
if(all_equal(raw_data, raw_data_master) != TRUE) {
  obsolete_entries <- anti_join(raw_data_master, raw_data)
  raw_data_master <- anti_join(raw_data_master, obsolete_entries)
}

# Write results back to csv:
raw_data_master <- raw_data_master %>%
  arrange(location, date) %>%
  write_csv(here("Data Files", "Cases and deaths", "Raw data_master_owid.csv"))

rm(list = c("raw_data_master", "new_entries", "obsolete_entries"))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Filter for relevant countries:
relevant_countries <- c("Germany", "Netherlands", "Sweden", "Spain", "Italy", "United Kingdom", "Ireland", "United States", "Israel", "France", "Belgium", "Greece", "Austria", "Denmark", "Finland", "Norway", "Portugal", "Switzerland", "Ukraine")

raw_data_filtered <- raw_data %>%
  filter(location %in% relevant_countries) %>%
  arrange(location, date)

# Add calculated columns:
raw_data_filtered <- raw_data_filtered %>%
  group_by(location) %>%
  mutate(new_cases_7day_rollsum = rollsum(new_cases, 7, fill = NA, align = "right"),
         new_cases_7day_rollsum_per_100k = new_cases_7day_rollsum / (population / 100000),
         new_cases_14day_rollsum = rollsum(new_cases, 14, fill = NA, align = "right"),
         new_cases_14ay_rollsum_per_100k = new_cases_14day_rollsum / (population / 100000),
         new_deaths_7day_rollsum = rollsum(new_deaths, 7, fill = NA, align = "right"),
         new_deaths_7day_rollsum_per_100k = new_deaths_7day_rollsum / (population / 100000),
         new_deaths_14day_rollsum = rollsum(new_deaths, 14, fill = NA, align = "right"),
         new_deaths_14day_rollsum_per_100k = new_deaths_14day_rollsum / (population / 100000)) %>%
  ungroup()

# Write results back to csv:
as_of_date_cases_deaths <- max(raw_data_filtered$date)
output_file_name <- paste(as_of_date_cases_deaths, " Raw data_filtered_owid.csv", sep = "")
write_csv(raw_data_filtered, here("Data Files", "Cases and deaths", output_file_name))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plot cases:
plot_cases_7day_rollsum <- ggplot(raw_data_filtered, aes(x = date, y = new_cases_7day_rollsum)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cases_7day_rollsum_per_100k <- ggplot(raw_data_filtered, aes(x = date, y = new_cases_7day_rollsum_per_100k)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_country_per_100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_per_100k, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_cases <- ggplot(raw_data_filtered, aes(x = date, y = total_cases)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Total cases_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_per_million <- ggplot(raw_data_filtered, aes(x = date, y = total_cases_per_million)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases per 1,000,000 inhabitants",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Total cases_country_per_million",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases_per_million, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_reproduction_rate <- ggplot(raw_data_filtered, aes(x = date, y = reproduction_rate)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "COVID-19 reproduction rate",
       caption = paste("Source: Arroyo Marioli et al. (2020) (https://doi.org/10.2139/ssrn.3581633) data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Reproduction rate_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_reproduction_rate, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_stringency_index <- ggplot(raw_data_filtered, aes(x = date, y = stringency_index)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Government response stringency index",
       caption = paste("Source: Oxford COVID-19 Government Response Tracker as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Stringency index_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_stringency_index, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plot deaths:
plot_deaths_7day_rollsum <- ggplot(raw_data_filtered, aes(x = date, y = new_deaths_7day_rollsum)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 deaths",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " New deaths_7day sum_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_7day_rollsum, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_deaths_7day_rollsum_per_100k <- ggplot(raw_data_filtered, aes(x = date, y = new_deaths_7day_rollsum_per_100k)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 deaths per 100,000 inhabitants",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " New deaths_7day sum_country_per_100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_7day_rollsum_per_100k, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths <- ggplot(raw_data_filtered, aes(x = date, y = total_deaths)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Total deaths_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_per_million <- ggplot(raw_data_filtered, aes(x = date, y = total_deaths_per_million)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths per 1,000,000 inhabitants",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Total deaths_country_per_million",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths_per_million, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Cases and deaths charts by continent:
raw_data_continent <- raw_data %>%
  filter(iso_code %in% c("OWID_AFR", "OWID_SAM", "OWID_NAM", "OWID_EUR", "OWID_EUN", "OWID_ASI", "OWID_OCE", "OWID_WRL")) %>%
  group_by(location) %>%
  mutate(new_cases_7day_rollsum = rollsum(new_cases, 7, fill = NA, align = "right"),
         new_cases_7day_rollsum_per_million = new_cases_7day_rollsum / (population / 1000000),
         new_cases_14day_rollsum = rollsum(new_cases, 14, fill = NA, align = "right"),
         new_cases_14day_rollsum_per_million = new_cases_14day_rollsum / (population / 1000000),
         new_deaths_7day_rollsum = rollsum(new_deaths, 7, fill = NA, align = "right"),
         new_deaths_7day_rollsum_per_million = new_deaths_7day_rollsum / (population / 1000000),
         new_deaths_14day_rollsum = rollsum(new_deaths, 14, fill = NA, align = "right"),
         new_deaths_14day_rollsum_per_million = new_deaths_14day_rollsum / (population / 1000000)) %>%
  ungroup()

# Plot cases:
plot_cases_7day_rollsum_continent <- ggplot(raw_data_continent, aes(x = date, y = new_cases_7day_rollsum)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_continent, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cases_7day_rollsum_continent_per_million <- ggplot(raw_data_continent, aes(x = date, y = new_cases_7day_rollsum_per_million)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases per 1,000,000 inhabitants",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_continent_per_million",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_continent_per_million, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_continent <- ggplot(raw_data_continent, aes(x = date, y = total_cases)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Total cases_continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases_continent, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_continent_per_million <- ggplot(raw_data_continent, aes(x = date, y = total_cases_per_million)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases per 100,000 inhabitants",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Total cases_continent_per_million",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases_continent_per_million, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

# Plot deaths:
plot_deaths_7day_rollsum_continent <- ggplot(raw_data_continent, aes(x = date, y = new_deaths_7day_rollsum)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 deaths",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " New deaths_7day sum_continent", ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_7day_rollsum_continent, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_deaths_7day_rollsum_continent_per_million <- ggplot(raw_data_continent, aes(x = date, y = new_deaths_7day_rollsum_per_million)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 deaths per 1,000,000 inhabitants",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " New deaths_7day sum_continent_per_million",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_7day_rollsum_continent_per_million, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_continent <- ggplot(raw_data_continent, aes(x = date, y = total_deaths)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Total deaths_continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths_continent, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_continent_per_million <- ggplot(raw_data_continent, aes(x = date, y = total_deaths_per_million)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths per 1,000,000 inhabitants",
       caption = paste("Source: Center for Systems Science and Engineering at Johns Hopkins University data as of", as_of_date_cases_deaths, sep = " "))

file_name <- paste(as_of_date_cases_deaths, " Total deaths_continent_per_million",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths_continent_per_million, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Cases and deaths charts by German Bundesländer

# Read in case data:
bundeslander_case_data <- read_csv("https://opendata.ecdc.europa.eu/covid19/subnationalcaseweekly/csv")

# Process data:
bundeslander_case_data <- bundeslander_case_data %>%
  clean_names() %>%
  select(year_week, country, region_name, rate_14_day_per_100k) %>%
  filter(country == "Germany") %>%
  mutate(year = str_sub(year_week, 1, 4),
         year = as.numeric(year),
         week = str_sub(year_week, 7, 8),
         week = as.numeric(week),
         date = MMWRweek2Date(year, week, 2)) %>%
  select(-one_of("year", "week", "year_week")) %>%
  rename(cases_14day_rollsum_per_100k = rate_14_day_per_100k)

# Create chart:
as_of_date_bundeslander <- max(bundeslander_case_data$date)

plot_cases_14day_rollsum_per_100k_bundeslander <- ggplot(bundeslander_case_data, aes(x = date, y = cases_14day_rollsum_per_100k)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~region_name) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = paste("Source: ECDC data as of", as_of_date_bundeslander, sep = " "))

file_name <- paste(as_of_date_bundeslander, " New cases_14day sum_bundeslander_per_100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_14day_rollsum_per_100k_bundeslander, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)