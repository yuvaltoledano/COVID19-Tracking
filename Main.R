# Load required libraries:
library(pacman)
p_load(tidyverse, janitor, lubridate, zoo, ggplot2, cowplot)

# Read in and process data:
raw_data <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

raw_data <- raw_data %>%
  clean_names() %>%
  select(date_rep, cases, deaths, countries_and_territories, pop_data2018) %>%
  rename(date = date_rep, country = countries_and_territories, population = pop_data2018) %>%
  mutate(date = dmy(date))

# Add new cases to master raw data frame:
raw_data_master <- read_csv("E:/Programming projects/COVID19-Tracking/Data Files/Raw data.csv")
new_entries <- anti_join(raw_data, raw_data_master)
raw_data_master <- bind_rows(raw_data_master, new_entries)

# Remove old entries from master raw dataframe:
obsolte_entries <- anti_join(raw_data_master, raw_data)
raw_data_master <- anti_join(raw_data_master, obsolte_entries)
all_equal(raw_data, raw_data_master)

# Write results back to csv:
write_csv(raw_data_master, "E:/Programming projects/COVID19-Tracking/Data Files/Raw data.csv")
rm(list = c("new_entries", "raw_data_master", "obsolte_entries"))

# Filter for relevant countries:
relevant_countries <- c("Germany", "Netherlands", "Israel", "Spain", "Italy", "United_Kingdom", "Ireland", "United_States_of_America", "Russia", "France")

clean_data <- raw_data %>%
  filter(country %in% relevant_countries) %>%
  arrange(country, date)

# Calculate 7-day rolling averages and cumulatives:
clean_data <- clean_data %>%
  group_by(country) %>%
  mutate(cumulative_cases = cumsum(cases),
         cases_per100000 = cumulative_cases / (population / 100000),
         cases_7day_rollmean = rollmean(cases, 7, fill = NA, align = "right"),
         cumulative_deaths = cumsum(deaths),
         deaths_per100000 = cumulative_deaths / (population / 100000),
         deaths_7day_rollmean = rollmean(deaths, 7, fill = NA, align = "right"))

# Write results back to csv:
current_date <- today()
output_file_directory <- "E:/Programming projects/COVID19-Tracking/Data Files/"
output_file_name <- paste(current_date, " Clean data", sep = "")
output_file_name <- paste(output_file_name, "csv", sep = ".")
output_file_path <- paste(output_file_directory, output_file_name, sep = "")
write_csv(clean_data, output_file_path)

# Plot:
plot_daily_cases <- ggplot(clean_data, aes(x = date, y = cases)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Daily new COVID-19 cases",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Daily cases",  ".png", sep = "")
chart_file_path <- "E:/Programming projects/COVID19-Tracking/Charts/"
ggsave(filename =  file_name, plot = plot_daily_cases, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_cases <- ggplot(clean_data, aes(x = date, y = cumulative_cases)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Cumulative reported COVID-19 cases",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Cumulative cases",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_cases_log10 <- ggplot(clean_data, aes(x = date, y = cumulative_cases)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(trans = 'log10', labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Cumulative reported COVID-19 cases log10 scale",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Cumulative cases log10",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases_log10, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_cases_per100000 <- ggplot(clean_data, aes(x = date, y = cases_per100000)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases per 100,000",
       title = "Cumulative reported COVID-19 cases per 100,000 inhabitants",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Cumulative cases per 100000",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases_per100000, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_cases_rolling_average <- ggplot(clean_data, aes(x = date, y = cases_7day_rollmean)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Daily new COVID-19 cases (7-day rolling average)",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Daily cases rolling average",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_cases_rolling_average, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_deaths <- ggplot(clean_data, aes(x = date, y = deaths)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "Daily new COVID-19 deaths",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Daily deaths",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_deaths, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_deaths <- ggplot(clean_data, aes(x = date, y = cumulative_deaths)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "Cumulative reported COVID-19 deaths",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Cumulative deaths",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_deaths_log10 <- ggplot(clean_data, aes(x = date, y = cumulative_deaths)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(trans = 'log10', labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "Cumulative reported COVID-19 deaths log10 scale",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Cumulative deaths log10",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths_log10, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_deaths_per100000 <- ggplot(clean_data, aes(x = date, y = deaths_per100000)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths per 100,000",
       title = "Cumulative reported COVID-19 deaths per 100,000 inhabitants",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Cumulative deaths per 100000",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths_per100000, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_deaths_rolling_average <- ggplot(clean_data, aes(x = date, y = deaths_7day_rollmean)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "Daily new COVID-19 deaths (7-day rolling average)",
       caption = "Source: ECDC")

file_name <- paste(current_date, " Daily deaths rolling average",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_deaths_rolling_average, path = chart_file_path, scale = 1, width = 15, height = 10)