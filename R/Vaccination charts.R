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
         cum_vacs_age_indication = indikation_alter_dosen,
         cum_vacs_profession_indication = indikation_beruf_dosen,
         cum_vacs_medical_indication = indikation_medizinisch_dosen,
         cum_vacs_care_homes = indikation_pflegeheim_dosen,
         cum_vacs_age_indication_first_dose = indikation_alter_erst,
         cum_vacs_profession_indication_first_dose = indikation_beruf_erst,
         cum_vacs_medical_indication_first_dose = indikation_medizinisch_erst,
         cum_vacs_care_homes_first_dose = indikation_pflegeheim_erst,
         cum_vacs_age_indication_second_dose = indikation_alter_voll,
         cum_vacs_profession_indication_second_dose = indikation_beruf_voll,
         cum_vacs_medical_indication_second_dose = indikation_medizinisch_voll,
         cum_vacs_care_homes_second_dose = indikation_pflegeheim_voll)

# Add calculated columns:
vaccination_data <- vaccination_data %>%
  mutate(new_vacs_pfizer = cum_vacs_pfizer - lag(cum_vacs_pfizer, n = 1L, default = 0),
         new_vacs_moderna = cum_vacs_moderna - lag(cum_vacs_moderna, n = 1L, default = 0),
         new_vacs_astrazeneca = cum_vacs_astrazeneca - lag(cum_vacs_astrazeneca, n = 1L, default = 0),
         new_vacs_age_indication = cum_vacs_age_indication - lag(cum_vacs_age_indication, n = 1L, default = 0),
         new_vacs_profession_indication = cum_vacs_profession_indication - lag(cum_vacs_profession_indication, n = 1L, default = 0),
         new_vacs_medical_indication = cum_vacs_medical_indication - lag(cum_vacs_medical_indication, n = 1L, default = 0),
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
  facet_wrap(~`Type of vaccine`) + 
  geom_line(color = "#00BFC4", size = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum administered vaccinations in Germany",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " New vacs 7-day rolling sum",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_new_vacs_7day_rollsum, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

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
ggsave(filename =  file_name, plot = plot_new_vacs_dose, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

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
ggsave(filename =  file_name, plot = plot_new_vacs_type, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

plot_new_vacs_recipient <- vaccination_data %>%
  select(date, new_vacs_age_indication, new_vacs_profession_indication, new_vacs_medical_indication, new_vacs_care_homes) %>%
  rename(`Vacs for age indication` = new_vacs_age_indication, `Vacs for professional indication` = new_vacs_profession_indication, `Vacs for medical indication` = new_vacs_medical_indication, `Vacs for nursing home residents` = new_vacs_care_homes) %>%
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
ggsave(filename =  file_name, plot = plot_new_vacs_recipient, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

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
ggsave(filename =  file_name, plot = plot_cum_vacs, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

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
ggsave(filename =  file_name, plot = plot_cum_vacs_proportions, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Read in and process Germany vaccine delivery data:
vaccine_delivery_data <- read_tsv("https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv")

vaccine_delivery_data <- vaccine_delivery_data %>%
  clean_names() %>%
  rename(vaccine_type = impfstoff,
         delivered_doses = dosen,
         bundesland = region) %>%
  mutate(vaccine_type = recode(vaccine_type,
                               "comirnaty" = "Pfizer",
                               "moderna" = "Moderna",
                               "astra" = "AstraZeneca"),
         bundesland = recode(bundesland,
                             "DE-BW" = "Baden-Württemberg",
                             "DE-BY" = "Bayern",
                             "DE-BE" = "Berlin",
                             "DE-BB" = "Brandenburg",
                             "DE-HB" = "Bremen",
                             "DE-HH" = "Hamburg",
                             "DE-HE" = "Hessen",
                             "DE-MV" = "Mecklenburg-Vorpommern",
                             "DE-NI" = "Niedersachsen",
                             "DE-NW" = "Nordrhein-Westfalen",
                             "DE-RP" = "Rheinland-Pfalz",
                             "DE-SL" = "Saarland",
                             "DE-SN" = "Sachsen",
                             "DE-ST" = "Sachsen-Anhalt",
                             "DE-SH" = "Schleswig-Holstein",
                             "DE-TH" = "Thüringen")) %>%
  group_by(date, vaccine_type) %>%
  mutate(total_delivered_doses_on_date = sum(delivered_doses),
         pct_delivered_bundesland = delivered_doses / total_delivered_doses_on_date * 100) %>%
  ungroup()

plot_vaccine_deliveries <- vaccine_delivery_data %>%
  select(-one_of("delivered_doses", "bundesland", "pct_delivered_bundesland")) %>%
  distinct() %>%
  rename(`Type of vaccine` = vaccine_type) %>%
  ggplot(aes(x = date, y = total_delivered_doses_on_date, fill = `Type of vaccine`)) + 
  geom_col() +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Delivered vaccine doses by type",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Delivered vaccine doses by type",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_vaccine_deliveries, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Compare delivered doses vs. used doses per vaccine type

# Process data:
vaccines_delivered_temp <- vaccine_delivery_data %>%
  select(-one_of("delivered_doses", "bundesland", "pct_delivered_bundesland")) %>%
  distinct() %>%
  rename(delivered_vaccine_doses = total_delivered_doses_on_date)

dates_vec <- seq(min(vaccine_delivery_data$date), max(vaccination_data$date), by = "days")

vaccines_delivered <- tibble(dates_vec) %>%
  rename(date = dates_vec) %>%
  left_join(vaccines_delivered_temp, by = "date") %>%
  mutate(vaccine_type = replace_na(vaccine_type, "No delivery"),
         delivered_vaccine_doses = replace_na(delivered_vaccine_doses, 0)) %>%
  pivot_wider(names_from = vaccine_type, values_from = delivered_vaccine_doses) %>%
  select(-"No delivery") %>%
  mutate(Pfizer = replace_na(Pfizer, 0),
         Moderna = replace_na(Moderna, 0),
         AstraZeneca = replace_na(AstraZeneca, 0)) %>%
  rename(delivered_doses_pfizer = Pfizer,
         delivered_doses_moderna = Moderna,
         delivered_doses_astrazeneca = AstraZeneca)

vaccinations_administered <- vaccination_data %>%
  arrange(date) %>%
  select(date, new_vacs_pfizer, new_vacs_moderna,  new_vacs_astrazeneca) %>%
  rename(administered_doses_pfizer = new_vacs_pfizer,
         administered_doses_moderna = new_vacs_moderna,
         administered_doses_astrazeneca = new_vacs_astrazeneca) %>%
  add_row(date = min(vaccine_delivery_data$date), administered_doses_pfizer = 0, administered_doses_moderna = 0, administered_doses_astrazeneca = 0, .before = 1)

vaccination_capacity <- left_join(vaccinations_administered, vaccines_delivered, by = "date")

rm(list = c("vaccines_delivered_temp", "vaccinations_administered", "vaccines_delivered", "dates_vec"))

vaccination_capacity <- vaccination_capacity %>%
  mutate(cum_administered_doses_pfizer = cumsum(administered_doses_pfizer),
         cum_administered_doses_moderna = cumsum(administered_doses_moderna),
         cum_administered_doses_astrazeneca = cumsum(administered_doses_astrazeneca),
         cum_delivered_doses_pfizer = cumsum(delivered_doses_pfizer),
         cum_delivered_doses_moderna = cumsum(delivered_doses_moderna),
         cum_delivered_doses_astrazeneca = cumsum(delivered_doses_astrazeneca))

# Save data as csv:
output_file_name <- paste(as_of_date_vaccinations, " Germany vaccine capacity data.csv", sep = "")
write_csv(vaccination_capacity, here("Data Files", "Vaccinations", output_file_name))

# Create charts:
plot_vaccine_capacity_pfizer <- vaccination_capacity %>%
  select(date, cum_delivered_doses_pfizer, cum_administered_doses_pfizer) %>%
  pivot_longer(cols = contains("cum"), names_to = "Metric") %>%
  mutate(Metric = recode(Metric,
                         "cum_delivered_doses_pfizer" = "Pfizer - cumulative delivered doses",
                         "cum_administered_doses_pfizer" = "Pfizer - cumulative administered doses")) %>%
  ggplot(aes(x = date, y = value, color = Metric)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Pfizer vaccine capacity",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Vaccine capacity - Pfizer",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_vaccine_capacity_pfizer, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

plot_vaccine_capacity_moderna <- vaccination_capacity %>%
  select(date, cum_delivered_doses_moderna, cum_administered_doses_moderna) %>%
  pivot_longer(cols = contains("cum"), names_to = "Metric") %>%
  mutate(Metric = recode(Metric,
                         "cum_delivered_doses_moderna" = "Moderna - cumulative delivered doses",
                         "cum_administered_doses_moderna" = "Moderna - cumulative administered doses")) %>%
  ggplot(aes(x = date, y = value, color = Metric)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Moderna vaccine capacity",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Vaccine capacity - Moderna",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_vaccine_capacity_moderna, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

plot_vaccine_capacity_astrazeneca <- vaccination_capacity %>%
  select(date, cum_delivered_doses_astrazeneca, cum_administered_doses_astrazeneca) %>%
  pivot_longer(cols = contains("cum"), names_to = "Metric") %>%
  mutate(Metric = recode(Metric,
                         "cum_delivered_doses_astrazeneca" = "AstraZeneca - cumulative delivered doses",
                         "cum_administered_doses_astrazeneca" = "AstraZeneca - cumulative administered doses")) %>%
  ggplot(aes(x = date, y = value, color = Metric)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "AstraZeneca vaccine capacity",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Vaccine capacity - AstraZeneca",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_vaccine_capacity_astrazeneca, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)