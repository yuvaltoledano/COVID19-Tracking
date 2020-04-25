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

# Write results back to csv:
current_date <- today()
output_file_directory <- "E:/Programming projects/COVID19-Tracking/Data/"
output_file_name <- paste(current_date, " Raw data", sep = "")
output_file_name <- paste(output_file_name, "csv", sep = ".")
output_file_path <- paste(output_file_directory, output_file_name, sep = "")
write_csv(raw_data, output_file_path)

# Filter for relevant countries:
relevant_countries <- c("Germany", "Netherlands", "Israel", "Spain", "Italy", "United_Kingdom", "Irelant", "United_States_of_America", "Russia", "China")

clean_data <- raw_data %>%
  filter(country %in% relevant_countries) %>%
  arrange(country, date)

clean_data <- clean_data %>%
  group_by(country) %>%
  mutate(cumulative_cases = cumsum(cases),
         cases_7day_rollmean = rollmean(cases, 7, fill = NA, align = "right"),
         cumulative_deaths = cumsum(deaths),
         deaths_7day_rollmean = rollmean(deaths, 7, fill = NA, align = "right"))

# Write results back to csv:
output_file_directory <- "E:/Programming projects/COVID19-Tracking/Data/"
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
ggsave(filename =  file_name, plot = plot_daily_cases, path = file_path, scale = 1, width = 15, height = 10)

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