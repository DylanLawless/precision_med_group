# https://www.bag.admin.ch/bag/de/home/zahlen-und-statistiken/zahlen-fakten-zu-spitaelern/kennzahlen-der-schweizer-spitaeler.html
# Bundesamt für Gesundheit (BAG) - (Federal office for public health)
# Key figures for Swiss hospitals
# 
# Dokumente
# 2022
# Publikation «Kennzahlen der Schweizer Spitäler 2022» (PDF, de fr it)
# Beschreibung der Kennzahlen der Schweizer Spitäler 2022 (PDF, deutsch)
# Daten «Kennzahlen der Schweizer Spitäler 2022» (XLSX, de fr it)
#  Zugehörigkeit zur kantonalen Spitalliste ab 2008 (XLSX, de fr it)
#  Zeitreihe der «Kennzahlen der Schweizer Spitäler» ab 2008 (XLSX, de fr it)
# 
# 2022
# Publication « Key figures of the Swiss hospitals 2022 » (PDF, de fr it)
# Description of the key figures of the Swiss hospitals in 2022 (PDF, German)
# Data « Key figures of the Swiss hospitals 2022 » (XLSX, de fr it)
#  Belonging to the cantonal hospital list from 2008 (XLSX, de fr it)
#  Time series of the « Key figures of the Swiss hospitals » from 2008 (XLSX, de fr it)

library(dplyr)
library(ggplot2)
library(lubridate)

# year data
# df <-  readxl::read_xlsx("../data/kzp22_data.xlsx", sheet = 5 )
# df <- df |> filter(Adr == "Steinwiesstrasse 75")

# Read the data from the specified Excel sheet
df <- read.csv2("../data/kzp22_KZ_TimeSerie.csv", sep = ",", header = TRUE)


# Assuming df$JAHR contains integer or numeric years like 2024, 2025, etc.
# This will create a date with the first day of the specified year
df$Year <- make_date(year = df$JAHR, month = 1, day = 1)

class(df$AustStatMSA)

# kispi data
df <- df %>%
	filter(Adr == "Steinwiesstrasse 75")

df <- df %>%
	filter(JAHR >= 2010 )


df2 <- df |> 
	select(Year, JAHR, Inst, AwT, AwBesold, AwSonst, EtMedL, 
			 PtageStatMSA, PtageStatP, PtageStatR, PtageStatB, 
			 AustStatMSA, AustStatP, AustStatR, AustStatB)
#  Total expense
# Personnel expenses
# Other operational costs
# income from medical costs
# Total number of care days Acute/Psy/Reha/GH
# Total number of administrative cases (excluding cases in long-term treatment) that were terminated  Discharges during the year (inpatient) – Acute/Psy/Reha/GH

# cases ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Assuming df2 is your dataset and it's properly formatted.
# Name the columns in each group
# care_days_columns <- c("PtageStatMSA", "PtageStatP", "PtageStatR", "PtageStatB")
# discharges_columns <- c("AustStatMSA", "AustStatP", "AustStatR", "AustStatB")

care_days_columns <- c("PtageStatMSA")
discharges_columns <- c("AustStatMSA")


# Pivot the data to long format and add group names
df_patients <- df2 %>%
	pivot_longer(
		cols = c(care_days_columns, discharges_columns),
		names_to = "Type",
		values_to = "Value"
	) %>%
	mutate(
		Group = case_when(
			Type %in% care_days_columns ~ "Care Days",
			Type %in% discharges_columns ~ "Discharges",
			TRUE ~ NA_character_
		)
	)

# Check the structure of the reshaped data
print(head(df_patients))

# Now plot the data
p <- df_patients %>%
	ggplot(aes(x = Year, y = Value, color = Type)) +
	geom_point() +
	geom_line() +
	facet_wrap(~ Group, scales = "free_y") +
	labs(
		title = "Care Days and Discharges over Time",
		x = "Year",
		y = "Count",
		color = "Care Type"
	) +
	theme_minimal()

print(p)

# forecast care days ----

df_patient_forecast <- df_patients %>%
	group_by(Type) %>%
	do({
		model <- lm(Value ~ JAHR, data = .)
		future_years <- data.frame(JAHR = (max(.$JAHR) + 1):2030)
		predictions <- predict(model, newdata = future_years, interval = "prediction")
		data.frame(JAHR = future_years$JAHR, Forecast = predictions[,1])
	}) %>%
	ungroup()

# Combine historical and forecasted data for plotting
df_combined <- bind_rows(
	df_patients %>% select(JAHR, Type, Value),
	df_patient_forecast %>% rename(Value = Forecast)
)

# Step 4: Plot the results
ggplot(df_combined, aes(x = JAHR, y = Value, color = Type)) +
	geom_line() +
	geom_point(data = df_combined %>% filter(JAHR > 2022), shape = 21, fill = "white") +
	labs(title = "Historical and Forecasted Care Days and Discharges ",
			 x = "Year",
			 y = "Count",
			 color = "Care Type") +
	theme_minimal()

# Assuming df_combined is your existing dataset with forecasts up to 2030
# Add a reduction factor that starts at 1.0 in 2024 and decreases by 0.1 each year
saving_pc <- 0.01
df_combined <- df_combined %>%
	mutate(
		ReductionFactor = 1.0 - ((JAHR - 2024) * 0.01),
		Reduced_Value = if_else(JAHR >= 2024, Value * ReductionFactor, Value)
	)

# Ensure that the reduction factor does not go below 0
df_combined <- df_combined %>%
	mutate(
		ReductionFactor = pmax(ReductionFactor, 0),
		Reduced_Value = if_else(JAHR >= 2024, Value * ReductionFactor, NA)
	)

# Check the changes and make sure the reduced values are correct
print(df_combined)

max_reduction_factor <- 1 - min(df_combined$ReductionFactor, na.rm = TRUE)
max_reduction_factor <- round(max_reduction_factor, digits = 2)
max_year <- max(df_combined$JAHR, na.rm = TRUE)
df$Inst

# Plot the original and reduced values for comparison
p_cost <- df_combined |>
	ggplot( aes(x = JAHR, y = Value, group = Type)) +
	geom_line(aes(color = Type)) +
	geom_line(aes(y = Reduced_Value, linetype = "Reduced"), color = "grey") +
	geom_point(aes(color = Type)) +
	geom_point(aes(y = Reduced_Value), shape = 21, fill = "white") +
	labs(title = paste0("Expense projections - time series linear model"),
			 subtitle = paste0("\n",
			 									saving_pc, "% per year\n", max_reduction_factor, "% by ", max_year),
			 x = "Year",
			 y = "Expense Value (CHF)",
			 color = "Expense Type",
			 linetype = "Condition",
			 caption = paste0("Data source: Bundesamt für Gesundheit (BAG), kzp22_KZ_TimeSerie\n",
			 								 "https://www.bag.admin.ch/\n",
			 								 "Universitäts-Kinderspital Zürich")) +
	geom_vline(xintercept = 2024, linetype = "dotted", alpha = 0.4) +
	theme_minimal()  +
	scale_y_continuous(labels = scales::comma) 

p_cost
ggsave("../output/p_cost.pdf", plot = p_cost)





# expenses ----
# Total expense
# Personnel expenses
# Operational costs
# Income from medical costs

library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming df2 is your dataset and it's properly formatted.
# Name the columns in each group
expense_columns <- c("AwT", "AwBesold", "AwSonst", "EtMedL")

# Pivot the data to long format for expense columns and rename the Type entries
df_expenses <- df2 %>%
	pivot_longer(
		cols = expense_columns, # Focus only on expense columns
		names_to = "Type",
		values_to = "Value"
	) %>%
	mutate(Type = case_when(
		Type == "AwT"      ~ "Total expense",
		Type == "AwBesold" ~ "Personnel expenses",
		Type == "AwSonst"  ~ "Operational costs",
		Type == "EtMedL"   ~ "Income from medical",
		TRUE ~ Type  # Default case to handle unexpected values
	))

# Display the updated dataframe to verify changes
print(df_expenses)

# forecast expenses ----

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)

# Step 2: Fit a linear model and forecast
df_forecast <- df_expenses %>%
	group_by(Type) %>%
	do({
		model <- lm(Value ~ JAHR, data = .)
		future_years <- data.frame(JAHR = (max(.$JAHR) + 1):2030)
		predictions <- predict(model, newdata = future_years, interval = "prediction")
		data.frame(JAHR = future_years$JAHR, Forecast = predictions[,1])
	}) %>%
	ungroup()

# Combine historical and forecasted data for plotting
df_combined <- bind_rows(
	df_expenses %>% select(JAHR, Type, Value),
	df_forecast %>% rename(Value = Forecast)
)

# Step 4: Plot the results
ggplot(df_combined, aes(x = JAHR, y = Value, color = Type)) +
	geom_line() +
	geom_point(data = df_combined %>% filter(JAHR > 2022), shape = 21, fill = "white") +
	labs(title = "Historical and Forecasted Expenses by Type",
		  x = "Year",
		  y = "Expense Value ($)",
		  color = "Expense Type") +
	theme_minimal()


library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming df_combined is your existing dataset with forecasts up to 2030
# Add a reduction factor that starts at 1.0 in 2024 and decreases by 0.1 each year
saving_pc <- 0.01
df_combined <- df_combined %>%
	mutate(
		ReductionFactor = 1.0 - ((JAHR - 2024) * 0.01),
		Reduced_Value = if_else(JAHR >= 2024, Value * ReductionFactor, Value)
	)

# Ensure that the reduction factor does not go below 0
df_combined <- df_combined %>%
	mutate(
		ReductionFactor = pmax(ReductionFactor, 0),
		Reduced_Value = if_else(JAHR >= 2024, Value * ReductionFactor, NA)
	)

# Check the changes and make sure the reduced values are correct
print(df_combined)

max_reduction_factor <- 1 - min(df_combined$ReductionFactor, na.rm = TRUE)
max_reduction_factor <- round(max_reduction_factor, digits = 2)
max_year <- max(df_combined$JAHR, na.rm = TRUE)
df$Inst

# Plot the original and reduced values for comparison
p_cost <- df_combined |>
ggplot( aes(x = JAHR, y = Value, group = Type)) +
	geom_line(aes(color = Type)) +
	geom_line(aes(y = Reduced_Value, linetype = "Reduced"), color = "grey") +
	geom_point(aes(color = Type)) +
	geom_point(aes(y = Reduced_Value), shape = 21, fill = "white") +
	labs(title = paste0("Expense projections - time series linear model"),
		  subtitle = paste0("\n",
		  						saving_pc, "% per year\n", max_reduction_factor, "% by ", max_year),
		  x = "Year",
		  y = "Expense Value (CHF)",
		  color = "Expense Type",
		  linetype = "Condition",
		  caption = paste0("Data source: Bundesamt für Gesundheit (BAG), kzp22_KZ_TimeSerie\n",
		  "https://www.bag.admin.ch/\n",
		  "Universitäts-Kinderspital Zürich")) +
	geom_vline(xintercept = 2024, linetype = "dotted", alpha = 0.4) +
	theme_minimal()  +
	scale_y_continuous(labels = scales::comma) 

p_cost
ggsave("../output/p_cost.pdf", plot = p_cost)

