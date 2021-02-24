# Load required libraries:
library(pacman)
p_load(tidyverse, janitor, lubridate, zoo, ggplot2, cowplot, here, MMWRweek)

# Read in data:
raw_data <- read_csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv")

# Process case data:
case_data_all <- raw_data %>%
  clean_names() %>%
  select(year_week, country, continent, indicator, weekly_count, rate_14_day, cumulative_count, population) %>%
  filter(indicator == "cases") %>%
  mutate(year = str_sub(year_week, 1, 4),
         year = as.numeric(year),
         week = str_sub(year_week, 6, 8),
         week = as.numeric(week),
         date = MMWRweek2Date(year, week, 2)) %>%
  select(-one_of("year", "week", "year_week", "indicator")) %>%
  rename(weekly_cases = weekly_count,
         cumulative_cases = cumulative_count,
         cases_14day_rollsum_per100000 = rate_14_day)

# Process deaths data:
deaths_data_all <- raw_data %>%
  clean_names() %>%
  select(year_week, country, continent, indicator, weekly_count, rate_14_day, cumulative_count, population) %>%
  filter(indicator == "deaths") %>%
  mutate(year = str_sub(year_week, 1, 4), 
         year = as.numeric(year),
         week = str_sub(year_week, 6, 8),
         week = as.numeric(week),
         date = MMWRweek2Date(year, week, 2)) %>%
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
  obsolete_entries <- anti_join(clean_data_countries_master, clean_data_countries_all)
  clean_data_countries_master <- anti_join(clean_data_countries_master, obsolete_entries)
}

# Write results back to csv:
clean_data_countries_master <- clean_data_countries_master %>%
  arrange(country, date) %>%
  write_csv(here("Data Files", "Clean data_all_master.csv"))

rm(list = c("clean_data_countries_master", "case_data_all", "deaths_data_all", "raw_data", "new_entries", "obsolete_entries"))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  geom_line(color = "cadetblue", size = 1.2) +
  facet_wrap(~region_name) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "14-day rolling sum COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption_bundeslander)

file_name <- paste(as_of_date_bundeslander, " New cases_14day sum_bundeslander_per100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_cases_14day_rollsum_per100000_bundeslander, path = here("Charts"), scale = 1, width = 15, height = 10)