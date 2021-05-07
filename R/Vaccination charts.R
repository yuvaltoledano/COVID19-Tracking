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
         cum_vacs_pfizer_first_dose = dosen_biontech_erst_kumulativ,
         cum_vacs_pfizer_second_dose = dosen_biontech_zweit_kumulativ,
         cum_vacs_moderna = dosen_moderna_kumulativ,
         cum_vacs_moderna_first_dose = dosen_moderna_erst_kumulativ,
         cum_vacs_moderna_second_dose = dosen_moderna_zweit_kumulativ,
         cum_vacs_astrazeneca = dosen_astrazeneca_kumulativ,
         cum_vacs_astrazeneca_first_dose = dosen_astrazeneca_erst_kumulativ,
         cum_vacs_astrazeneca_second_dose = dosen_astrazeneca_zweit_kumulativ,
         cum_vacs_jj = dosen_johnson_kumulativ,
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
         cum_vacs_care_homes_second_dose = indikation_pflegeheim_voll,
         cum_vacs_vaccination_centres = dosen_dim_kumulativ,
         cum_vacs_doctors_offices = dosen_kbv_kumulativ)

# Add calculated columns:
vaccination_data <- vaccination_data %>%
  mutate(new_vacs_pfizer = cum_vacs_pfizer - lag(cum_vacs_pfizer, n = 1L, default = 0),
         new_vacs_pfizer_first_dose = cum_vacs_pfizer_first_dose - lag(cum_vacs_pfizer_first_dose, n = 1L, default = 0),
         new_vacs_pfizer_second_dose = cum_vacs_pfizer_second_dose - lag(cum_vacs_pfizer_second_dose, n = 1L, default = 0),
         new_vacs_moderna = cum_vacs_moderna - lag(cum_vacs_moderna, n = 1L, default = 0),
         new_vacs_moderna_first_dose = cum_vacs_moderna_first_dose - lag(cum_vacs_moderna_first_dose, n = 1L, default = 0),
         new_vacs_moderna_second_dose = cum_vacs_moderna_second_dose - lag(cum_vacs_moderna_second_dose, n = 1L, default = 0),
         new_vacs_astrazeneca = cum_vacs_astrazeneca - lag(cum_vacs_astrazeneca, n = 1L, default = 0),
         new_vacs_astrazeneca_first_dose = cum_vacs_astrazeneca_first_dose - lag(cum_vacs_astrazeneca_first_dose, n = 1L, default = 0),
         new_vacs_astrazeneca_second_dose = cum_vacs_astrazeneca_second_dose - lag(cum_vacs_astrazeneca_second_dose, n = 1L, default = 0),
         new_vacs_jj = cum_vacs_jj - lag(cum_vacs_jj, n = 1L, default = 0),
         new_vacs_age_indication = cum_vacs_age_indication - lag(cum_vacs_age_indication, n = 1L, default = 0),
         new_vacs_profession_indication = cum_vacs_profession_indication - lag(cum_vacs_profession_indication, n = 1L, default = 0),
         new_vacs_medical_indication = cum_vacs_medical_indication - lag(cum_vacs_medical_indication, n = 1L, default = 0),
         new_vacs_care_homes = cum_vacs_care_homes - lag(cum_vacs_care_homes, n = 1L, default = 0),
         new_vacs_vaccination_centres = cum_vacs_vaccination_centres - lag(cum_vacs_vaccination_centres, n = 1L, default = 0),
         new_vacs_doctors_offices = cum_vacs_doctors_offices - lag(cum_vacs_doctors_offices, n = 1L, default = 0),
         new_vacs_all_7day_rollsum = rollsum(new_vacs_all, 7, fill = NA, align = "right"),
         new_vacs_pfizer_7day_rollsum = rollsum(new_vacs_pfizer, 7, fill = NA, align = "right"),
         new_vacs_moderna_7day_rollsum = rollsum(new_vacs_moderna, 7, fill = NA, align = "right"),
         new_vacs_astrazeneca_7day_rollsum = rollsum(new_vacs_astrazeneca, 7, fill = NA, align = "right"),
         new_vacs_jj_7day_rollsum = rollsum(new_vacs_jj, 7, fill = NA, align = "right"))

# Save data as csv:
as_of_date_vaccinations <- max(vaccination_data$date)
output_file_name <- paste(as_of_date_vaccinations, " Germany vaccination data.csv", sep = "")
write_csv(vaccination_data, here("Data Files", "Vaccinations", output_file_name))

# Set chart caption:
chart_caption_vaccinations <- paste("Source: Bundesministerium für Gesundheit data as of", as_of_date_vaccinations, sep = " ")

# Create charts:
plot_new_vacs_7day_rollsum <- vaccination_data %>%
  select(date, new_vacs_all_7day_rollsum, new_vacs_pfizer_7day_rollsum, new_vacs_moderna_7day_rollsum, new_vacs_astrazeneca_7day_rollsum, new_vacs_jj_7day_rollsum) %>%
  rename(All = new_vacs_all_7day_rollsum, Pfizer = new_vacs_pfizer_7day_rollsum, Moderna = new_vacs_moderna_7day_rollsum, AstraZeneca = new_vacs_astrazeneca_7day_rollsum, `J&J` = new_vacs_jj_7day_rollsum) %>%
  pivot_longer(cols = c("All", "Pfizer", "Moderna", "AstraZeneca", "J&J"), names_to = "Type of vaccine") %>%
  ggplot(aes(x = date, y = value)) +
  facet_wrap(~`Type of vaccine`, scales = "free") + 
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
  select(date, new_vacs_pfizer, new_vacs_moderna, new_vacs_astrazeneca, new_vacs_jj) %>%
  rename(Pfizer = new_vacs_pfizer, Moderna = new_vacs_moderna, AstraZeneca = new_vacs_astrazeneca, `J&J` = new_vacs_jj) %>%
  pivot_longer(cols = c("Pfizer", "Moderna", "AstraZeneca", "J&J"), names_to = "Type of vaccine") %>%
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

plot_new_vacs_location <- vaccination_data %>%
  select(date, new_vacs_vaccination_centres, new_vacs_doctors_offices) %>%
  rename(`Vaccination centre` = new_vacs_vaccination_centres, `Doctor's office` = new_vacs_doctors_offices) %>%
  pivot_longer(cols = c("Vaccination centre", "Doctor's office"), names_to = "Place of vaccination") %>%
  ggplot(aes(x = date, y = value, fill = `Place of vaccination`)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Daily administered vaccinations in Germany by place of vaccination",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Daily administered vacs in Germany by location",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_new_vacs_location, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

plot_cum_vacs <- vaccination_data %>%
  select(date, cum_vacs_pfizer, cum_vacs_moderna, cum_vacs_astrazeneca, cum_vacs_jj) %>%
  rename(Pfizer = cum_vacs_pfizer, Moderna = cum_vacs_moderna, AstraZeneca = cum_vacs_astrazeneca, JJ = cum_vacs_jj) %>%
  pivot_longer(cols = c("Pfizer", "Moderna", "AstraZeneca", "JJ"), names_to = "Type of vaccine") %>%
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
                               "astra" = "AstraZeneca",
                               "johnson" = "J&J"),
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
         AstraZeneca = replace_na(AstraZeneca, 0),
         `J&J` = replace_na(`J&J`, 0)) %>%
  rename(delivered_doses_pfizer = Pfizer,
         delivered_doses_moderna = Moderna,
         delivered_doses_astrazeneca = AstraZeneca,
         delivered_doses_jj = `J&J`)

vaccinations_administered <- vaccination_data %>%
  arrange(date) %>%
  select(date, new_vacs_pfizer, new_vacs_moderna,  new_vacs_astrazeneca, new_vacs_jj) %>%
  rename(administered_doses_pfizer = new_vacs_pfizer,
         administered_doses_moderna = new_vacs_moderna,
         administered_doses_astrazeneca = new_vacs_astrazeneca,
         administered_doses_jj = new_vacs_jj) %>%
  add_row(date = min(vaccine_delivery_data$date), administered_doses_pfizer = 0, administered_doses_moderna = 0, administered_doses_astrazeneca = 0, administered_doses_jj = 0, .before = 1)

vaccination_capacity <- left_join(vaccinations_administered, vaccines_delivered, by = "date")

rm(list = c("vaccines_delivered_temp", "vaccinations_administered", "vaccines_delivered", "dates_vec"))

vaccination_capacity <- vaccination_capacity %>%
  mutate(cum_administered_doses_pfizer = cumsum(administered_doses_pfizer),
         cum_administered_doses_moderna = cumsum(administered_doses_moderna),
         cum_administered_doses_astrazeneca = cumsum(administered_doses_astrazeneca),
         cum_administered_doses_jj = cumsum(administered_doses_jj),
         cum_delivered_doses_pfizer = cumsum(delivered_doses_pfizer),
         cum_delivered_doses_moderna = cumsum(delivered_doses_moderna),
         cum_delivered_doses_astrazeneca = cumsum(delivered_doses_astrazeneca),
         cum_delivered_doses_jj = cumsum(delivered_doses_jj))

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

plot_vaccine_capacity_jj <- vaccination_capacity %>%
  select(date, cum_delivered_doses_jj, cum_administered_doses_jj) %>%
  pivot_longer(cols = contains("cum"), names_to = "Metric") %>%
  mutate(Metric = recode(Metric,
                         "cum_delivered_doses_jj" = "J&J - cumulative delivered doses",
                         "cum_administered_doses_jj" = "J&J - cumulative administered doses")) %>%
  ggplot(aes(x = date, y = value, color = Metric)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "J&J vaccine capacity",
       caption = chart_caption_vaccinations)

file_name <- paste(as_of_date_vaccinations, " Vaccine capacity - JJ",  ".png", sep = "")
ggsave(filename =  file_name, plot = plot_vaccine_capacity_jj, path = here("Charts", "Vaccinations"), scale = 1, width = 16, height = 10)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Compare vaccination rates across countries
vaccination_data_owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", guess_max = 100000)

# Filter for relevant countries:
relevant_countries <- c("Germany", "Italy", "United Kingdom", "United States", "Israel", "France", "Belgium", "European Union")

vaccination_data_owid_filtered <- vaccination_data_owid %>%
  select(location, date, population, total_vaccinations, people_vaccinated, people_fully_vaccinated, new_vaccinations, new_vaccinations_smoothed, total_vaccinations_per_hundred, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, new_vaccinations_smoothed_per_million) %>%
  filter(location %in% relevant_countries, date > ymd("2020-12-20")) %>%
  arrange(location, date)

rm(vaccination_data_owid)

# Add calculated columns:
vaccination_data_owid_filtered <- vaccination_data_owid_filtered %>%
  group_by(location) %>%
  mutate(new_vacs_7day_rollsum = rollsum(new_vaccinations, 7, fill = NA, align = "right"),
         new_vacs_7day_rollsum_per_100k = new_vacs_7day_rollsum / (population / 100000),
         new_vacs_14day_rollsum = rollsum(new_vaccinations, 14, fill = NA, align = "right"),
         new_vacs_14ay_rollsum_per_100k = new_vacs_14day_rollsum / (population / 100000)) %>%
  ungroup()

# Save data as csv:
as_of_date_owid_vaccinations <- max(vaccination_data_owid_filtered$date)
output_file_name <- paste(as_of_date_owid_vaccinations, " National vaccination rates.csv", sep = "")
write_csv(vaccination_data_owid_filtered, here("Data Files", "Vaccinations", output_file_name))

# Plot vaccinations:

plot_new_vacs_7day_rollsum <- ggplot(vaccination_data_owid_filtered, aes(x = date, y = new_vacs_7day_rollsum)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum vaccinations administered",
       caption = paste("Source: National government reports data as of", as_of_date_owid_vaccinations, sep = " "))

file_name <- paste(as_of_date_owid_vaccinations, " New vacs_7day sum_country",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_new_vacs_7day_rollsum, path = here("Charts", "Vaccinations"), scale = 1, width = 15, height = 10)

plot_new_vacs_7day_rollsum_per_100k <- ggplot(vaccination_data_owid_filtered, aes(x = date, y = new_vacs_7day_rollsum_per_100k)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location,) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "7-day rolling sum vaccinations administered per 100,000 inhabitants",
       caption = paste("Source: National government reports data as of", as_of_date_owid_vaccinations, sep = " "))

file_name <- paste(as_of_date_owid_vaccinations, " New vacs_7day sum_country_per_100k",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_new_vacs_7day_rollsum_per_100k, path = here("Charts", "Vaccinations"), scale = 1, width = 15, height = 10)

plot_people_vaccinated_per_hundred <- ggplot(vaccination_data_owid_filtered, aes(x = date, y = people_vaccinated_per_hundred)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Percent of population vaccinated",
       caption = paste("Source: National government reports data as of", as_of_date_owid_vaccinations, sep = " "))

file_name <- paste(as_of_date_owid_vaccinations, " Proportion of population vaccinated",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_people_vaccinated_per_hundred, path = here("Charts", "Vaccinations"), scale = 1, width = 15, height = 10)

plot_people_fully_vaccinated_per_hundred <- ggplot(vaccination_data_owid_filtered, aes(x = date, y = people_fully_vaccinated_per_hundred)) +
  geom_line(color = "#00BFC4", size = 1.2) +
  facet_wrap(~location) + 
  scale_y_continuous(labels = scales::comma) +
  theme_cowplot() + 
  background_grid() +
  labs(x = "Date",
       y = "",
       title = "Percent of population fully vaccinated",
       caption = paste("Source: National government reports data as of", as_of_date_owid_vaccinations, sep = " "))

file_name <- paste(as_of_date_owid_vaccinations, " Proportion of population fully vaccinated",  ".png", sep = "")
ggsave(filename = file_name, plot = plot_people_fully_vaccinated_per_hundred, path = here("Charts", "Vaccinations"), scale = 1, width = 15, height = 10)