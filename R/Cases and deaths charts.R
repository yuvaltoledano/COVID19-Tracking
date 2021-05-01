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
relevant_countries <- c("Germany", "Netherlands", "Sweden", "Spain", "Italy", "United Kingdom", "Ireland", "United States", "Israel", "France", "Belgium", "Greece")

raw_data_filtered <- raw_data %>%
  filter(location %in% relevant_countries) %>%
  arrange(location, date)

# Add calculated columns:
raw_data_filtered <- raw_data_filtered %>%
  group_by(location) %>%
  mutate(new_cases_7day_rollsum = rollsum(new_cases, 7, fill = NA, align = "right"),
         new_cases_7day_rollsum_per100000 = new_cases_7day_rollsum / (population / 100000),
         new_deaths_7day_rollsum = rollsum(new_deaths, 7, fill = NA, align = "right"),
         new_deaths_7day_rollsum_per100000 = new_deaths_7day_rollsum / (population / 100000)) %>%
  ungroup()

# Write results back to csv:
as_of_date_cases_deaths <- max(raw_data_filtered$date)
output_file_name <- paste(as_of_date_cases_deaths, " Raw data_filtered.csv", sep = "")
write_csv(raw_data_filtered, here("Data Files", "Cases and deaths", output_file_name))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set caption:
chart_caption_date <- paste("Source: OWID data as of", as_of_date_cases_deaths, sep = " ")

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

plot_cases_7day_rollsum_per100000 <- ggplot(raw_data_filtered, aes(x = date, y = new_cases_7day_rollsum_per100000)) +
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
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_per100000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

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

plot_cumulative_cases_per1000000 <- ggplot(raw_data_filtered, aes(x = date, y = total_cases_per_million)) +
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
ggsave(filename = file_name, plot = plot_cumulative_cases_per1000000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

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

plot_deaths_7day_rollsum_per100000 <- ggplot(raw_data_filtered, aes(x = date, y = new_deaths_7day_rollsum_per100000)) +
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
ggsave(filename = file_name, plot = plot_deaths_7day_rollsum_per100000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

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

plot_cumulative_deaths_per100000 <- ggplot(raw_data_filtered, aes(x = date, y = total_deaths_per_million)) +
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
ggsave(filename = file_name, plot = plot_cumulative_deaths_per100000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Cases and deaths charts by continent

# Process data:
clean_data_continent <- clean_data_countries_all %>%
  filter(str_detect(country, "total") | country == "United States Of America" | country == "China") %>%
  group_by(country) %>%
  mutate(weekly_cases_per100000 = weekly_cases / (population / 100000),
         cum_cases_per100000 = cumulative_cases / (population / 100000),
         weekly_deaths_per100000 = weekly_deaths / (population / 100000),
         cum_deaths_per100000 = cumulative_deaths / (population / 100000)) %>%
  ungroup()

# Plot cases:
plot_cases_7day_rollsum_continent <- ggplot(clean_data_continent, aes(x = date, y = weekly_cases)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_continent, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cases_7day_rollsum_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = weekly_cases_per100000)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_7day sum_continent_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_7day_rollsum_continent_per100000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cases_14day_rollsum_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = cases_14day_rollsum_per100000)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New cases_14day sum_continent_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_14day_rollsum_continent_per100000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_continent <- ggplot(clean_data_continent, aes(x = date, y = cumulative_cases)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum cases_continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases_continent, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_cases_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = cum_cases_per100000)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum cases_continent_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_cases_continent_per100000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

# Plot deaths:
plot_weekly_deaths_continent <- ggplot(clean_data_continent, aes(x = date, y = weekly_deaths)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 deaths",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths_7day sum_continent", ".png", sep = "")
ggsave(filename = file_name, plot = plot_weekly_deaths_continent, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_weekly_deaths_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = weekly_deaths_per100000)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths_7day sum_continent_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_weekly_deaths_continent_per100000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_deaths_14day_rollsum__continent_per1000000 <- ggplot(clean_data_continent, aes(x = date, y = deaths_14day_rollsum_per1000000)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 deaths per 1,000,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " New deaths_14day sum_continent_per1M",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_deaths_14day_rollsum__continent_per1000000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_continent <- ggplot(clean_data_continent, aes(x = date, y = cumulative_deaths)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum deaths_continent",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths_continent, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

plot_cumulative_deaths_continent_per100000 <- ggplot(clean_data_continent, aes(x = date, y = cum_deaths_per100000)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Cumulative COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption_cases_deaths)

file_name <- paste(as_of_date_cases_deaths, " Cum deaths_continent_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cumulative_deaths_continent_per100000, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)

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
  rename(cases_14day_rollsum_per100000 = rate_14_day_per_100k)

# Create chart:
as_of_date_bundeslander <- max(bundeslander_case_data$date)
chart_caption_bundeslander <- paste("Source: ECDC data as of", as_of_date_bundeslander, sep = " ")

plot_cases_14day_rollsum_per100000_bundeslander <- ggplot(bundeslander_case_data, aes(x = date, y = cases_14day_rollsum_per100000)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~region_name) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_bundeslander)

file_name <- paste(as_of_date_bundeslander, " New cases_14day sum_bundeslander_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_14day_rollsum_per100000_bundeslander, path = here("Charts", "Cases and deaths"), scale = 1, width = 15, height = 10)