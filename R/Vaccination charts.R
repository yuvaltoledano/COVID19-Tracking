# Load required libraries:
library(pacman)
p_load(tidyverse, janitor, lubridate, zoo, ggplot2, cowplot, here, MMWRweek)

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

# Save data as csv:
as_of_date_vaccinations <- max(vaccination_data$date)
output_file_name <- paste(as_of_date_vaccinations, " Germany vaccination data.csv", sep = "")
write_csv(vaccination_data, here("Data Files", "Vaccinations", output_file_name))

# Set chart caption:
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
       title = "7-day rolling sum administered vaccinations in Germany",
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
       title = "Daily administered vaccinations in Germany by dose",
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
       title = "Daily administered vaccinations in Germany by type",
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
       title = "Daily administered vaccinations in Germany by recipient",
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

file_name <- paste(as_of_date_vaccinations, " Cumulative vacs administered",  ".png", sep = "")
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

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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