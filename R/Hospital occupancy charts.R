# Load required libraries:
library(pacman)
p_load(tidyverse, janitor, lubridate, zoo, ggplot2, cowplot, here, MMWRweek)

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

# Save data as csv:
as_of_date_hospital <- max(hospital_data$date)
output_file_name <- paste(as_of_date_hospital, " Hospital occupancy data.csv", sep = "")
write_csv(hospital_data, here("Data Files", "Hospital occupancy", output_file_name))

# Create daily occupancy charts:
relevant_countries <- c("Germany", "Netherlands", "Sweden", "Spain", "Italy", "United_Kingdom", "Ireland", "France", "Belgium")
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
ggsave(filename =  file_name, plot = plot_daily_hospital_occupancy, path = here("Charts", "Hospital occupancy"), scale = 1, width = 15, height = 10)

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
ggsave(filename =  file_name, plot = plot_daily_icu_occupancy, path = here("Charts", "Hospital occupancy"), scale = 1, width = 15, height = 10)

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
ggsave(filename =  file_name, plot = plot_weekly_hospital_admissions_per100000, path = here("Charts", "Hospital occupancy"), scale = 1, width = 15, height = 10)

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
ggsave(filename =  file_name, plot = plot_weekly_icu_admissions_per100000, path = here("Charts", "Hospital occupancy"), scale = 1, width = 15, height = 10)
