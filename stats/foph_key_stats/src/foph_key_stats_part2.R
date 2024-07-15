# https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-fakten-zu-spitaelern/qualitaetsindikatoren-der-schweizer-akutspitaeler/qualitaetsindikatoren-dokumentation.html
# Bundesamt für Gesundheit (BAG) - (Federal office for public health)
# Key figures for Swiss hospitals

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(purrr)  # Ensure this is loaded
library(tidyr)
library(scales)  # For additional formatting options
library(stringi)

# MTMyMjY=2022.csv
# Daten «Qualitätsindikatoren der Schweizer Akutspitäler 2022» nach Betrieb
# MTcxMDQxMzIyNw==2022.csv
# Daten «Qualitätsindikatoren der Schweizer Akutspitäler 2022» nach Standort

# Define the paths to your files
file_paths <- c(
	"../data/Mjg1NzE=2010.csv",
	"../data/Mjg1ODE=2011.csv",
	"../data/Mjg1OTA=2012.csv",
	"../data/Mjg1OTc=2013.csv",
	"../data/Mjg2MDY=2014.csv",
	"../data/Mjg2MzE=2015.csv",
	"../data/Mjg2Mzk=2016.csv",
	"../data/Mjg2NTU=2017.csv",
	"../data/Mjg2NzI=2018.csv",
	"../data/MTYyMTI0MTA0MA==2019.csv",
	"../data/MTY0OTA3NjMxOA==2020.csv",
	"../data/MTY4MDcxMjYzMQ==2021.csv",
	"../data/MTcxMDQxMzIyNw==2022.csv"
)

read_my_data <- function(file_path) {
	year <- str_extract(basename(file_path), "\\d{4}")
	# Assuming the file might be in UTF-8 incorrectly read as Windows-1252
	df <- read_csv2(file_path, locale = locale(encoding = "Windows-1252"))
	
	nombre_de_cas_column <- paste("nombre de cas", year)
	
	if (!nombre_de_cas_column %in% names(df)) {
		stop(paste("Column", nombre_de_cas_column, "not found in", file_path))
	}
	
	# Clean and convert the case numbers
	df <- df %>%
		mutate(across(all_of(nombre_de_cas_column), ~str_remove(as.character(.), "^<"))) %>%
		mutate(across(all_of(nombre_de_cas_column), ~replace(.x, .x == "", NA))) %>%
		mutate(across(all_of(nombre_de_cas_column), as.numeric, na.rm = TRUE)) %>%
		select(institution, indicator, all_of(nombre_de_cas_column))
	
	# Correct encoding from UTF-8 to UTF-8, assuming the data was misinterpreted
	# df <- df %>%
		# mutate(across(where(is.character), ~iconv(.x, from = "Windows-1252", to = "UTF-8")))
	
	df <- df %>%
		mutate(institution = stringi::stri_trans_general(institution, "Latin-ASCII"))
	
	return(df)
}

# Use purrr's map_df with safely to read data and handle potential errors
results <- map_df(file_paths, safely(read_my_data), .id = "file")


# Extract data frames and errors
data_frames <- results$result
errors <- results$error

# Handle errors if needed
if (any(sapply(errors, function(x) !is.null(x)))) {
	warning("Errors encountered in some files:")
	print(errors)
} else {
	# Combine all successful data frames
	combined_df <- bind_rows(data_frames)
	print(combined_df)
}

# Combine all successful data frames
combined_df <- bind_rows(data_frames) %>%
	distinct()  # Remove duplicate rows if any

df <- combined_df 

# df <- df %>%
	# mutate(institution = stringi::stri_trans_general(institution, "Latin-ASCII"))

names(df)
df$institution %>% unique() %>% head()

# Clean and convert all "nombre de cas YYYY" columns
df_cleaned <- df %>%
	mutate(across(matches("^nombre de cas \\d{4}$"), ~str_remove(as.character(.), "^<"))) %>%  # Remove '<' from start
	mutate(across(matches("^nombre de cas \\d{4}$"), ~str_extract(.x, "[0-9.]+"))) %>%  # Extract only numeric part
	mutate(across(matches("^nombre de cas \\d{4}$"), as.numeric))  # Convert to numeric

# Now check to see if all columns are correctly formatted as numeric
str(df_cleaned)

# Apply pivot_longer on the cleaned dataframe
df_long <- df_cleaned %>%
	pivot_longer(
		cols = matches("^nombre de cas \\d{4}$"),  # Matches columns starting with 'nombre de cas' followed by a year
		names_to = "year",
		names_prefix = "nombre de cas ",  # This removes 'nombre de cas ' from the year value
		values_to = "cases"
	)

# Check the result to ensure it's as expected
print(head(df_long))

names(df_long)

max(df_long$year)

# Function to extract the group based on specified patterns
extract_group <- function(indicator) {
	# Regular expression to match the first letter and optionally a period followed by a number
	str_extract(indicator, "^[A-Z]?")
}

extract_subgroup <- function(indicator) {
	# Regular expression to match the first letter and optionally a period followed by a number
	str_extract(indicator, "^[A-Z](\\.[0-9])?")
}

# Add the extracted group and subgroup to the dataframe
df_long <- df_long %>%
	mutate(
		subgroup = extract_subgroup(indicator),
		group = extract_group(indicator),
		indicator_main = if_else(group == subgroup, as.character(indicator), NA_character_)
	) %>%
	group_by(group) %>%
	# Fill the indicator_main for all members of the same group
	mutate(indicator_main = ifelse(is.na(indicator_main), first(na.omit(indicator_main)), indicator_main)) %>%
	ungroup()

df_long <- df_long %>%
	na.omit()

# fix accent letters ----
# Replacement list
replacements <- c("A¤" = "a", "Ã¤" = "a", "ä" = "a", "Ã©" = "e", "é" = "e",
									"ZA 1/4rich"= "Zurich", "A´"="o", "A¨"="e")

# Applying replacements
df_long <- df_long %>%
	mutate(across(where(is.character), function(col) {
		for (pattern in names(replacements)) {
			col <- str_replace_all(col, fixed(pattern), replacements[pattern])
		}
		return(col)
	}))


# better names ----
df_long <- df_long %>%
	separate(institution, into = c("institution", "institution_full"), sep = " - ", extra = "merge", fill = "right")

# Remove text within parentheses including the parentheses themselves
df_long <- df_long %>%
	mutate(institution = str_replace_all(institution, "\\s*\\([^\\)]+\\)", ""))

# Assuming df_long is your large dataset
# Define the mapping for unifying institution names
institution_mapping <- c(
	"Universitats-Kinderspital Zurich das Spital der Eleonorenstiftung" = "Universitats-Kinderspital Zurich",
	"Kinderspital Zurich" = "Universitats-Kinderspital Zurich"
	# Add other mappings as needed
)

# Apply the mapping to the 'institution' column in df_long
df_long <- df_long %>%
	mutate(institution = recode(institution, !!!institution_mapping))

saveRDS(df_long, file ="../data/df_long_processed_2010_2022.Rds")

# read data ----
df_long <- readRDS(file = "../data/df_long_processed_2010_2022.Rds")

# get kispi ----
df_kispi_2022 <- df_long %>%
	filter(year == 2022) %>%
	filter(str_detect(institution, "Kinderspital Z")) %>%  # Partial matching on institution name
	ungroup()

df_kispi_2022_cases <- sum(df_kispi_2022$cases)

tmp_j2 <- df_kispi_2022 %>%
    filter(subgroup == "J.2")

df_kispi_2022_cases_j2 <- sum(tmp_j2$cases)

# Summarize data by 'indicator_main' and 'subgroup' (assuming 'subgroup' is the detail level)
p <- df_kispi_2022 %>%
	filter(year == 2022) %>%
	filter(str_detect(institution, "Kinderspital Z")) %>% # Partial matching
	group_by(indicator_main, subgroup) %>%
	summarise(total_cases = sum(`cases`, na.rm = TRUE)) %>%
	ungroup() %>%
# Create a stacked bar plot
	ggplot(aes(x = indicator_main, y = total_cases, fill = subgroup)) +
	geom_bar(stat = "identity", position = "stack") +
	labs(
		title = "Kispi 2022: Total cases per main indicator by subgroup",
		subtitle = paste0("Total cases:", df_kispi_2022_cases, "\nTotal J.2. sepsis:", df_kispi_2022_cases_j2),
		x = "Main indicator",
		y = "Total number of cases",
		fill = "Subgroup"
	) +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
	scale_x_discrete(labels = function(x) str_wrap(x, width = 30))  # Wrap text to avoid overly long labels

# Print the plot
print(p)

ggsave("../output/p_cases_per_indicator_kispi_2022.pdf", plot = p, width = 16, height = 6)

# kispi all -----
# Summarize data by 'indicator_main' and 'subgroup' (assuming 'subgroup' is the detail level)
df_kispi_sepsis <- df_long %>%
	# filter(year >= 2010) %>%
	filter(str_detect(institution, "Kinderspital Z")) %>%  # Partial matching on institution name
	filter(subgroup == "J.2") %>%
	filter(cases > 0)

# Filter and summarize the data
df_kispi_sepsis_combined <- df_kispi_sepsis %>%
	group_by(indicator, year) %>%
	summarise(total_cases = sum(cases, na.rm = TRUE), .groups = 'drop') %>%
	mutate(indicator = as.character(indicator)) %>%  # Ensure indicator is character type for consistency
	bind_rows(
		group_by(., year) %>%
			summarise(total_cases = sum(total_cases, na.rm = TRUE), .groups = 'drop') %>%
			mutate(indicator = "sepsis_total_mortalitat")
	)

library(ggrepel)

# Plot the data
p_ks <- ggplot(df_kispi_sepsis_combined, aes(x = as.factor(year), y = total_cases, color = indicator, group = indicator)) + 
	geom_point() +
	geom_line() +
	geom_line(data = df_kispi_sepsis_combined %>% filter(indicator == "sepsis_total_mortalitat"), 
						aes(x = as.factor(year), y = total_cases, group = 1), color = "black") +
	ggrepel::geom_text_repel(data = df_kispi_sepsis_combined %>% 
													 	filter(indicator == "sepsis_total_mortalitat"),
						aes(label = total_cases), vjust = -0.5, color = "black",
						nudge_y = 2, max.overlaps = Inf,
						direction = "y", hjust = "top"
						) +
	labs(
		title = "Kispi: Total cases of sepsis per main indicator by subgroup",
		x = "Year",
		y = "Total number of cases",
		color = "Indicator"
	) +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
	scale_x_discrete(labels = function(x) str_wrap(x, width = 30))  # Wrap text to avoid overly long labels

# Print the plot
print(p_ks)

ggsave("../output/p_cases_sepsis_kispi_yearly.pdf", plot = p_ks)

# get kispi forecast ----

library(dplyr)
library(ggplot2)
library(stringr)
library(ggrepel)
library(forecast)

# Assuming df_kispi_sepsis_combined is already created
# Forecast future values for each indicator
df_kispi_sepsis$year <- as.numeric(df_kispi_sepsis$year)

# Forecast future values using Poisson regression
df_forecast <- df_kispi_sepsis %>%
	filter(year > 2014) %>%
	group_by(indicator) %>%
	do({
		model <- glm(cases ~ year, data = ., family = poisson)
		future_years <- data.frame(year = (max(.$year) + 1):2030)
		predictions <- predict(model, newdata = future_years, type = "response")
		data.frame(year = future_years$year, Forecast = predictions)
	}) %>%
	ungroup()

# Combine historical and forecasted data for plotting
df_combined <- bind_rows(
	df_kispi_sepsis %>% select(year, indicator, cases),
	df_forecast %>% rename(cases = Forecast) 
)

df_combined <- df_combined %>% mutate(cases = round(cases, 0))


# Get total count for new forecasted data
df_combined <- df_combined %>%
	group_by(indicator, year) %>%
	summarise(total_cases = sum(cases, na.rm = TRUE), .groups = 'drop') %>%
	mutate(indicator = as.character(indicator)) %>%  # Ensure indicator is character type for consistency
	bind_rows(
		group_by(., year) %>%
			summarise(total_cases = sum(total_cases, na.rm = TRUE), .groups = 'drop') %>%
			mutate(indicator = "sepsis_total_mortalitat")
	)

# Plot the data
p_ks_forecast <- df_combined %>%
	# filter(cases >= 0) %>%
	ggplot(aes(x = year, y = total_cases, color = indicator, group = indicator)) + 
	geom_point() +
	geom_line() +
	geom_vline(xintercept = 2024, linetype = "dotted", alpha = 0.6) 

# Print the plot
print(p_ks_forecast)

# reduction ----

# Assuming df_combined is your existing dataset with forecasts up to 2030
# Add a reduction factor that starts at 1.0 in 2024 and decreases by 0.1 each year
saving_pc <- 0.05
df_combined <- df_combined %>%
	mutate(
		ReductionFactor = 1.0 - ((year - 2024) * saving_pc),
		Reduced_total_cases = if_else(year >= 2024, total_cases * ReductionFactor, total_cases)
	)

# Ensure that the reduction factor does not go below 0
# And only get values for sepsis_total_mortalitat
df_combined <- df_combined %>%
	mutate(
		ReductionFactor = pmax(ReductionFactor, 0),
		Reduced_total_cases = if_else(year >= 2024, total_cases * ReductionFactor, NA),
		Reduced_total_cases = if_else(indicator == "sepsis_total_mortalitat", Reduced_total_cases, NA)
	)

# Check the changes and make sure the reduced total_casess are correct
print(df_combined)

max_reduction_factor <- 1 - min(df_combined$ReductionFactor, na.rm = TRUE)
max_reduction_factor <- round(max_reduction_factor, digits = 2)
max_year <- max(df_combined$year, na.rm = TRUE)
df$Inst
saving_pc
max_reduction_factor

df_combined$year <- as.numeric(df_combined$year)
df_combined$Reduced_total_cases <- as.numeric(df_combined$Reduced_total_cases) %>% round(digits = 0)

# Plot the data
p_ks_forecast <- df_combined %>%
	ggplot(aes(x = year, y = total_cases, color = indicator, group = indicator)) + 
	geom_point() +
	geom_point(aes(y = Reduced_total_cases), shape = 21, fill = "red", color="black") +
	geom_line(aes(y = Reduced_total_cases), linetype = "dotted", color="red") +
	geom_line() +
	geom_vline(xintercept = 2024, linetype = "dotted" ) +
	geom_line(data = df_combined %>% filter(indicator == "sepsis_total_mortalitat"), aes(x = year, y = total_cases, group = 1), color = "black") +
	geom_text_repel(data = df_combined %>%
									filter(indicator == "sepsis_total_mortalitat"),
									aes(label = total_cases), vjust = 0.5, color = "black",
									nudge_x = 0, 
									nudge_y = 3,
									max.overlaps = Inf,
									direction = "y", hjust = "top",
									alpha = 0.6
	) +
	geom_text_repel(data = df_combined %>%
										filter(Reduced_total_cases > 0),
									aes(label = Reduced_total_cases,
											y = Reduced_total_cases
											), color = "red",
									nudge_x = 0,
									nudge_y = -3, max.overlaps = Inf,
									direction = "y", hjust = "bottom",
									alpha = 0.6
	) +
	labs(
		title = "Kispi projections - Total cases of sepsis 2010-2030",
		subtitle = paste0("Time series Poisson regression\n",
											saving_pc*100, "% per year\n", max_reduction_factor*100, "% by ", max_year),
		x = "Year",
		y = "Total number of cases",
		color = "Indicator",
		caption = paste0("Data source: Bundesamt für Gesundheit (BAG),\n",
										 "https://www.bag.admin.ch/\n"
										 # "Universitäts-Kinderspital Zürich"
		)
	) +
	theme_minimal() 

# Print the plot
print(p_ks_forecast)

ggsave("../output/p_cases_sepsis_kispi_yearly_forecast.pdf", plot = p_ks_forecast, width =12, height = 5)


# universitat ----

# Summarize data by 'indicator_main' and 'subgroup' (assuming 'subgroup' is the detail level)
df_uni_sepsis <- df_long %>%
	filter(str_detect(institution, "Universit") | str_detect(institution, "Kinderspital Z")) %>%
	# Partial matching on institution name
	filter(subgroup == "J.2") #%>%

unique(df_uni_sepsis$institution)


# Define a custom labeller function to wrap facet labels
wrap_labeller <- labeller(
  institution = function(x) str_wrap(x, width = 30)
)

p_us <- df_uni_sepsis %>%
	ungroup() %>%
	group_by(institution, indicator, year) %>%
	summarise(total_cases = sum(`cases`)) %>% 
	filter(total_cases > 0) %>%
	ggplot(aes(x = year, y = total_cases, color = indicator, group = indicator)) +  # Add group aesthetic
	geom_point() +
	geom_line() +
	facet_wrap(~ institution, labeller = wrap_labeller) +
	labs(
		title = "Total cases of sepsis in University hospitals per main indicator by subgroup",
		fill = "Subgroup"
	) +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

p_us

ggsave("../output/p_cases_sepsis_uni_yearly.pdf", plot = p_us, width =12, height = 5)

