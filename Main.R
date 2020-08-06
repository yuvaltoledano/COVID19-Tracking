# Load required libraries:
library(pacman)
p_load(tidyverse, janitor, lubridate, zoo, ggplot2, cowplot, RcppRoll)

# Read in and process data:
raw_data <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

raw_data <- raw_data %>%
  clean_names() %>%
  select(date_rep, cases, deaths, countries_and_territories, pop_data2019) %>%
  rename(date = date_rep, country = countries_and_territories, population = pop_data2019) %>%
  mutate(date = dmy(date))

# Add new cases to master raw data frame:
raw_data_master <- read_csv("E:/Programming projects/COVID19-Tracking/Data Files/Raw data.csv")
new_entries <- anti_join(raw_data, raw_data_master)
raw_data_master <- bind_rows(raw_data_master, new_entries)

# Remove old entries from master raw dataframe:
if(all_equal(raw_data, raw_data_master) != TRUE) {
  obsolte_entries <- anti_join(raw_data_master, raw_data)
  raw_data_master <- anti_join(raw_data_master, obsolte_entries)
}

raw_data_master <- raw_data_master %>%
  arrange(country, date)

# Write results back to csv:
write_csv(raw_data_master, "E:/Programming projects/COVID19-Tracking/Data Files/Raw data.csv")
rm(raw_data_master)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Filter for relevant countries:
relevant_countries <- c("Germany", "Netherlands", "Israel", "Spain", "Italy", "United_Kingdom", "Ireland", "United_States_of_America", "Russia", "France", "Belgium")

clean_data <- raw_data %>%
  filter(country %in% relevant_countries) %>%
  arrange(country, date)

# Calculate 7-day rolling averages, sums, and cumulatives:
clean_data <- clean_data %>%
  group_by(country) %>%
  mutate(cum_cases = cumsum(cases),
         cum_cases_per100000 = cum_cases / (population / 100000),
         cases_per100000 = cases / (population / 100000),
         cases_7day_rollmean = rollmean(cases, 7, fill = NA, align = "right"),
         cases_7day_rollsum = rollsum(cases, 7, fill = NA, align = "right"),
         cases_14day_rollmean = rollmean(cases, 14, fill = NA, align = "right"),
         cases_14day_rollsum = rollsum(cases, 14, fill = NA, align = "right"),
         cases_14day_rollmean_per100000 = cases_14day_rollmean / (population / 100000),
         cases_14day_rollsum_per100000 = cases_14day_rollsum / (population / 100000),
         cumulative_deaths = cumsum(deaths),
         deaths_per100000 = cumulative_deaths / (population / 100000),
         deaths_7day_rollmean = rollmean(deaths, 7, fill = NA, align = "right"),
         deaths_7day_rollsum = rollsum(deaths, 7, fill = NA, align = "right"),
         deaths_14day_rollmean = rollmean(deaths, 14, fill = NA, align = "right"),
         deaths_14day_rollsum = rollsum(deaths, 14, fill = NA, align = "right"),
         deaths_14day_rollmean_per100000 = deaths_14day_rollmean / (population / 100000),
         deaths_14day_rollsum_per100000 = deaths_14day_rollsum / (population / 100000))

# Write results back to csv:
as_of_date <- max(clean_data$date)
output_file_directory <- "E:/Programming projects/COVID19-Tracking/Data Files/"
output_file_name <- paste(as_of_date, " Clean data", sep = "")
output_file_name <- paste(output_file_name, "csv", sep = ".")
output_file_path <- paste(output_file_directory, output_file_name, sep = "")
write_csv(clean_data, output_file_path)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set caption:
chart_caption <- paste("Source: ECDC data as of", as_of_date, sep = " ")

# Plot cases:
plot_daily_cases <- ggplot(clean_data, aes(x = date, y = cases)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Daily new COVID-19 cases",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily cases",  ".png", sep = "")
chart_file_path <- "E:/Programming projects/COVID19-Tracking/Charts/"
ggsave(filename =  file_name, plot = plot_daily_cases, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_cases <- ggplot(clean_data, aes(x = date, y = cum_cases)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Cumulative reported COVID-19 cases",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative cases",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_cases_log10 <- ggplot(clean_data, aes(x = date, y = cum_cases)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(trans = 'log10', labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Cumulative reported COVID-19 cases log10 scale",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative cases log10",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases_log10, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_cases_per100000 <- ggplot(clean_data, aes(x = date, y = cum_cases_per100000)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases per 100,000",
       title = "Cumulative reported COVID-19 cases per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative cases per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_cases_per100000, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_cases_7day_rolling_average <- ggplot(clean_data, aes(x = date, y = cases_7day_rollmean)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Daily new COVID-19 cases (7-day rolling average)",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily cases 7-day rolling average",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_cases_7day_rolling_average, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_cases_7day_rolling_sum <- ggplot(clean_data, aes(x = date, y = cases_7day_rollsum)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "COVID-19 cases 7-day rolling sum",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cases 7-day rolling sum",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_cases_7day_rolling_sum, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_cases_14day_rolling_average <- ggplot(clean_data, aes(x = date, y = cases_14day_rollmean)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Daily new COVID-19 cases (14-day rolling average)",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily cases 14-day rolling average",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_cases_14day_rolling_average, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_cases_14day_rolling_average_per100000 <- ggplot(clean_data, aes(x = date, y = cases_14day_rollmean_per100000)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "Daily new COVID-19 cases (14-day rolling average) per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily cases 14-day rolling average per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_cases_14day_rolling_average_per100000, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_cases_14day_rolling_sum_per100000 <- ggplot(clean_data, aes(x = date, y = cases_14day_rollsum_per100000)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported cases",
       title = "COVID-19 cases 14-day rolling sum per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cases 14-day rolling sum per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_cases_14day_rolling_sum_per100000, path = chart_file_path, scale = 1, width = 15, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plot deaths:
plot_daily_deaths <- ggplot(clean_data, aes(x = date, y = deaths)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "Daily new COVID-19 deaths",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily deaths",  ".png", sep = "")
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
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative deaths",  ".png", sep = "")
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
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative deaths log10",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths_log10, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_cumulative_deaths_per100000 <- ggplot(clean_data, aes(x = date, y = deaths_per100000)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths per 100,000",
       title = "Cumulative reported COVID-19 deaths per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Cumulative deaths per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_cumulative_deaths_per100000, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_deaths_7day_rolling_average <- ggplot(clean_data, aes(x = date, y = deaths_7day_rollmean)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "Daily new COVID-19 deaths (7-day rolling average)",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily deaths 7-day rolling average",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_deaths_7day_rolling_average, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_deaths_7day_rolling_sum <- ggplot(clean_data, aes(x = date, y = deaths_7day_rollsum)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "COVID-19 deaths (7-day rolling sum)",
       caption = chart_caption)

file_name <- paste(as_of_date, " Deaths 7-day rolling sum",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_deaths_7day_rolling_sum, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_deaths_14day_rolling_average <- ggplot(clean_data, aes(x = date, y = deaths_14day_rollmean)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "Daily new COVID-19 deaths (14-day rolling average)",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily deaths 14-day rolling average",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_deaths_14day_rolling_average, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_deaths_14day_rolling_average_per100000 <- ggplot(clean_data, aes(x = date, y = deaths_14day_rollmean_per100000)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "Daily new COVID-19 deaths (14-day rolling average) per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Daily deaths 14-day rolling average per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_deaths_14day_rolling_average_per100000, path = chart_file_path, scale = 1, width = 15, height = 10)

plot_daily_deaths_14day_rolling_sum_per100000 <- ggplot(clean_data, aes(x = date, y = deaths_14day_rollsum_per100000)) +
  geom_line(color = "forest green", size = 1.2) +
  facet_wrap(~country) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "Reported deaths",
       title = "COVID-19 deaths 14-day rolling sum per 100,000 inhabitants",
       caption = chart_caption)

file_name <- paste(as_of_date, " Deaths 14-day rolling sum per 100,000 inhabitants",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_daily_deaths_14day_rolling_sum_per100000, path = chart_file_path, scale = 1, width = 15, height = 10)
