# Load RSQLite library
library(RSQLite)

# Instead of ":memory:", specify a path to save the database file
database_path <- "../data/sepsis_registry.db"
conn <- dbConnect(RSQLite::SQLite(), database_path)

# Write the data frame to the SQLite database
dbWriteTable(conn, "SepsisRegistry", sepsis_data, overwrite = TRUE)

# Check the contents of the database (Optional, for verification)
print(dbReadTable(conn, "SepsisRegistry"))

# Disconnect from the database
dbDisconnect(conn)
