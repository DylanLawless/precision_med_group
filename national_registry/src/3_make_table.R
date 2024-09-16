# Load necessary libraries
library(DT)
library(htmlwidgets)

# Step 1: Connect to the SQLite Database
database_path <- "../data/sepsis_registry.db"
conn <- dbConnect(RSQLite::SQLite(), database_path)

# Step 2: Read the Data from SQL
query <- "SELECT * FROM SepsisRegistry"
sepsis_data <- dbGetQuery(conn, query)

# Disconnect from the database
dbDisconnect(conn)

# Step 3: Optionally convert dates to character strings for display in DT
sepsis_data$Date_Admission <- as.Date(sepsis_data$Date_Admission, origin = "1970-01-01")
sepsis_data$Date_Discharge <- as.Date(sepsis_data$Date_Discharge, origin = "1970-01-01")
sepsis_data$Date_Follow_up <- as.Date(sepsis_data$Date_Follow_up, origin = "1970-01-01")

sepsis_data$Date_Admission <- format(sepsis_data$Date_Admission, "%Y-%m-%d")
sepsis_data$Date_Discharge <- format(sepsis_data$Date_Discharge, "%Y-%m-%d")
sepsis_data$Date_Follow_up <- format(sepsis_data$Date_Follow_up, "%Y-%m-%d")

# Step 4: Create a DT datatable
datatable_output <- datatable(sepsis_data, options = list(pageLength = 5, autoWidth = TRUE), 
															filter = 'top', class = 'cell-border stripe')

# Step 5: Save the DT output as an HTML file
saveWidget(datatable_output, '../data/sepsis_registry_table.html', selfcontained = TRUE)
