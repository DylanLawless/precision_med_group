# Load the RSQLite package
library(RSQLite)

# Specify the database name
dbname <- "DNAsnake_IVDR.db"

# Check if the database already exists and delete it if it does
if (file.exists(dbname)) {
	file.remove(dbname)
}

# Create a new SQLite database
db <- dbConnect(SQLite(), dbname = dbname)

 
# Create tables
dbExecute(db, "CREATE TABLE Samples (sample_id TEXT PRIMARY KEY, collection_date DATE, source TEXT)")
dbExecute(db, "CREATE TABLE Runs (run_id TEXT PRIMARY KEY, sample_id TEXT, run_date DATE, status TEXT, FOREIGN KEY(sample_id) REFERENCES Samples(sample_id))")
dbExecute(db, "CREATE TABLE Analyses (analysis_id INTEGER PRIMARY KEY, run_id TEXT, analysis_type TEXT, result TEXT, FOREIGN KEY(run_id) REFERENCES Runs(run_id))")
dbExecute(db, "CREATE TABLE ReferenceGenomes (reference_id TEXT PRIMARY KEY, name TEXT, version TEXT)")
dbExecute(db, "CREATE TABLE SystemComponents (component_id INTEGER PRIMARY KEY, run_id TEXT, component_name TEXT, component_version TEXT, FOREIGN KEY(run_id) REFERENCES Runs(run_id))")
dbExecute(db, "CREATE TABLE GitLog (log_id INTEGER PRIMARY KEY, run_id TEXT, component_id INTEGER, commit_id TEXT, FOREIGN KEY(run_id) REFERENCES Runs(run_id), FOREIGN KEY(component_id) REFERENCES SystemComponents(component_id))")
dbExecute(db, "CREATE TABLE Metadata (metadata_id INTEGER PRIMARY KEY, run_id TEXT, key TEXT, value TEXT, FOREIGN KEY(run_id) REFERENCES Runs(run_id))")
dbExecute(db, "CREATE TABLE AuditTrail (audit_id INTEGER PRIMARY KEY, run_id TEXT, event TEXT, event_time DATETIME, details TEXT, FOREIGN KEY(run_id) REFERENCES Runs(run_id))")

# Insert sample data
dbExecute(db, "INSERT INTO Samples (sample_id, collection_date, source) VALUES ('XYZ_001', '2024-07-15', 'Laboratory A')")
dbExecute(db, "INSERT INTO Runs (run_id, sample_id, run_date, status) VALUES ('RUN123', 'XYZ_001', '2024-07-16', 'Completed')")
dbExecute(db, "INSERT INTO Analyses (run_id, analysis_type, result) VALUES ('RUN123', 'Variant Detection', 'Success')")
dbExecute(db, "INSERT INTO ReferenceGenomes (reference_id, name, version) VALUES ('REF001', 'GRCh38.p13', 'v1.0')")
dbExecute(db, "INSERT INTO Metadata (run_id, key, value) VALUES ('RUN123', 'Reference Genome', 'REF001')")
dbExecute(db, "INSERT INTO SystemComponents (run_id, component_name, component_version) VALUES ('RUN123', 'GATK', 'v4.0')")
dbExecute(db, "INSERT INTO GitLog (run_id, component_id, commit_id) VALUES ('RUN123', 1, 'a1b2c3d')")
dbExecute(db, "INSERT INTO Metadata (run_id, key, value) VALUES ('RUN123', 'Pipeline Config', 'Default')")
dbExecute(db, "INSERT INTO AuditTrail (run_id, event, event_time, details) VALUES ('RUN123', 'Compliance Check', '2024-07-17 12:00:00', 'Passed initial compliance checks')")

# Function to fetch run details, joining necessary tables to get a comprehensive overview
getRunDetails <- function(run_id) {
	query <- sprintf("SELECT r.run_id, r.run_date, r.status, a.analysis_type, a.result, m.key, m.value, sc.component_name, sc.component_version, gl.commit_id 
                    FROM Runs r 
                    JOIN Analyses a ON r.run_id = a.run_id 
                    JOIN Metadata m ON r.run_id = m.run_id 
                    JOIN SystemComponents sc ON r.run_id = sc.run_id 
                    JOIN GitLog gl ON sc.component_id = gl.component_id
                    WHERE r.run_id = '%s'", run_id)
	dbGetQuery(db, query)
}

dbDisconnect(db)


# quick look at data ----
# Load the necessary libraries
library(DBI)
library(RSQLite)
library(dplyr)

# Connect to the SQLite database
db <- dbConnect(RSQLite::SQLite(), dbname = "DNAsnake_IVDR.db")

# Get a list of all tables in the database
tables <- dbListTables(db)

# Get dimensions of each table and display a sample of rows
table_info <- lapply(tables, function(table_name) {
	# Query to get row count
	total_rows <- dbGetQuery(db, paste("SELECT COUNT(*) AS count FROM", table_name))
	
	# Fetch all data from each table to calculate sample size directly
	full_data <- dbReadTable(db, table_name)
	
	# Determine the minimum of 5 or number of rows in the table
	sample_size <- min(5, nrow(full_data))
	
	# Sample data
	sample_data <- full_data %>% 
		slice_sample(n = sample_size)
	
	# Return a list with the table name, dimensions, and sample data
	list(
		table_name = table_name,
		row_count = total_rows$count,
		sample_data = sample_data
	)
})

# Print the information
for (info in table_info) {
	cat("Table:", info$table_name, "\n")
	cat("Row Count:", info$row_count, "\n")
	print(info$sample_data)
	cat("\n")
}

# Disconnect from the database
dbDisconnect(db)
