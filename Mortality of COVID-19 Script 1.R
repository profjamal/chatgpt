# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(gt)

# Load the dataset from the URL
url <- "https://github.com/MoH-Malaysia/covid19-public/raw/main/epidemic/linelist/linelist_deaths.csv"
covid_data <- read.csv(url)

# Identify and store duplicate rows before cleaning
duplicate_rows <- covid_data[duplicated(covid_data), ]

# Clean the dataset by removing duplicates
covid_data_cleaned <- covid_data %>%
  distinct()

# Summarize the removed duplicates
if (nrow(duplicate_rows) > 0) {
  removed_duplicates_table <- duplicate_rows %>%
    gt() %>%
    tab_header(
      title = "Summary of Removed Duplicates"
    )
  
  # Display the table of removed duplicates
  print(removed_duplicates_table)
} else {
  print("No duplicates were found and removed.")
}

# Ensure date columns are treated correctly
covid_data_cleaned <- covid_data_cleaned %>%
  mutate(across(c(date_dose1, date_dose2, date_dose3), as.Date, format = "%Y-%m-%d"))

# Categorize individuals into vaccinated and non-vaccinated
covid_data_cleaned <- covid_data_cleaned %>%
  mutate(vaccinated = case_when(
    !is.na(date_dose1) | !is.na(date_dose2) | !is.na(date_dose3) ~ "Vaccinated",
    TRUE ~ "Non-Vaccinated"
  ))

# Ensure the date variable is in Date format
covid_data_cleaned <- covid_data_cleaned %>%
  mutate(date = ymd(date))

# Define vaccination status (non-vaccinated means no vaccination at all)
covid_data_cleaned <- covid_data_cleaned %>%
  mutate(vaccinated = ifelse(is.na(date_dose1) & is.na(date_dose2) & is.na(date_dose3), "No", "Yes"))

# 1. Mortality rate by year
mortality_by_year <- covid_data_cleaned %>%
  group_by(year = year(date)) %>%
  summarize(Deaths = n()) %>%
  mutate(Mortality_Rate = round(Deaths / sum(Deaths) * 100, 1))

# Publication-Ready Table for Mortality by Year using gt
mortality_by_year_table <- mortality_by_year %>%
  gt() %>%
  tab_header(
    title = "Mortality Rate by Year"
  ) %>%
  cols_label(
    year = "Year",
    Deaths = "Total Deaths",
    Mortality_Rate = "Mortality Rate (%)"
  ) %>%
  fmt_number(
    columns = c(Mortality_Rate),
    decimals = 1
  )

# Display the table
print(mortality_by_year_table)

# 2. Mortality rate by sex
mortality_by_sex <- covid_data_cleaned %>%
  group_by(male) %>%
  summarize(Deaths = n()) %>%
  mutate(Mortality_Rate = round(Deaths / sum(Deaths) * 100, 1),
         Sex = ifelse(male == 1, "Male", "Female")) %>%
  select(Sex, Deaths, Mortality_Rate)

# Publication-Ready Table for Mortality by Sex using gt
mortality_by_sex_table <- mortality_by_sex %>%
  gt() %>%
  tab_header(
    title = "Mortality Rate by Sex"
  ) %>%
  cols_label(
    Sex = "Sex",
    Deaths = "Total Deaths",
    Mortality_Rate = "Mortality Rate (%)"
  ) %>%
  fmt_number(
    columns = c(Mortality_Rate),
    decimals = 1
  )

# Display the table
print(mortality_by_sex_table)

# 3. Mortality rate between vaccination status
mortality_by_vaccination <- covid_data_cleaned %>%
  group_by(vaccinated) %>%
  summarize(Deaths = n()) %>%
  mutate(Mortality_Rate = round(Deaths / sum(Deaths) * 100, 1))

# Publication-Ready Table for Mortality by Vaccination Status using gt
mortality_by_vaccination_table <- mortality_by_vaccination %>%
  gt() %>%
  tab_header(
    title = "Mortality Rate by Vaccination Status"
  ) %>%
  cols_label(
    vaccinated = "Vaccinated",
    Deaths = "Total Deaths",
    Mortality_Rate = "Mortality Rate (%)"
  ) %>%
  fmt_number(
    columns = c(Mortality_Rate),
    decimals = 1
  )

# Display the table
print(mortality_by_vaccination_table)

# 4. Time series of deaths
time_series_deaths <- covid_data_cleaned %>%
  group_by(date) %>%
  summarize(Deaths = n())

# Plot time series of deaths
ggplot(time_series_deaths, aes(x = date, y = Deaths)) +
  geom_line(color = "blue") +
  labs(title = "Time Series of Deaths", x = "Date", y = "Number of Deaths") +
  theme_minimal()

# 5. Time series split between vaccinated and non-vaccinated
time_series_vaccination_status <- covid_data_cleaned %>%
  group_by(date, vaccinated) %>%
  summarize(Deaths = n())

# Plot time series split by vaccination status
ggplot(time_series_vaccination_status, aes(x = date, y = Deaths, color = vaccinated)) +
  geom_line() +
  labs(title = "Time Series of Deaths by Vaccination Status", x = "Date", y = "Number of Deaths", color = "Vaccinated") +
  theme_minimal()

# 6. Focus on vaccinated only, split time series for doses
time_series_by_doses <- covid_data_cleaned %>%
  filter(vaccinated == "Yes") %>%
  mutate(doses = ifelse(!is.na(date_dose3), "3 Doses", ifelse(!is.na(date_dose2), "2 Doses", "1 Dose"))) %>%
  group_by(date, doses) %>%
  summarize(Deaths = n())

# Plot time series for vaccinated only, split by doses
ggplot(time_series_by_doses, aes(x = date, y = Deaths, color = doses)) +
  geom_line() +
  labs(title = "Time Series of Deaths by Doses (Vaccinated Only)", x = "Date", y = "Number of Deaths", color = "Doses") +
  theme_minimal()

