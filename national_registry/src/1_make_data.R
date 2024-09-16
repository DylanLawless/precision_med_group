# Load necessary libraries
library(dplyr)
library(DBI)

set.seed(123)  # For reproducibility

# Generate basic data without dates first
sepsis_data <- data.frame(
	Patient_ID = sprintf("%03d", 1:300),  # Unique patient IDs
	Age = sample(0:18, 300, replace = TRUE),  # Random ages between 18 and 90
	Gender = sample(c("M", "F"), 300, replace = TRUE),  # Randomly assign gender
	Diagnosis_Code = sample(c("A41.9", "A41.5", "A40.3", "A41.2"), 300, replace = TRUE),  # Random diagnosis codes
	Source_of_Infection = sample(c("Pulmonary", "Abdominal", "Urinary", "Skin", "Nosocomial"), 300, replace = TRUE),
	Organ_Dysfunction = sample(c("Renal", "Cardiovascular", "Respiratory", "Hepatic", "Multiorgan", "Other"), 300, replace = TRUE),
	Treatment_Administered = sample(c("Antibiotics, Fluids", "Vasopressors", "Antibiotics", "Antibiotics, Oxygen", "ECMO, Antibiotics"), 300, replace = TRUE),
	AMS_Compliance = sample(c("Yes", "No"), 300, replace = TRUE),
	National_Sepsis_Pathway_Used = sample(c("Yes", "No"), 300, replace = TRUE),
	Outcome = sample(c("Survived", "Deceased"), 300, replace = TRUE),
	Sepsis_Standard_Met = sample(c("Yes", "No"), 300, replace = TRUE),
	stringsAsFactors = FALSE  # Avoid factor conversion
)

# Add admission and discharge dates separately
sepsis_data$Date_Admission <- sample(seq(as.Date('2024-01-01'), as.Date('2024-12-31'), by="day"), 300, replace = TRUE)
sepsis_data$Date_Discharge <- sepsis_data$Date_Admission + sample(5:30, 300, replace = TRUE)

# Add follow-up dates conditionally based on the outcome
sepsis_data$Date_Follow_up <- ifelse(sepsis_data$Outcome == "Survived", 
																					 sepsis_data$Date_Discharge + sample(30:90, 300, replace = TRUE), NA)

# Check the updated structure and sample data
str(sepsis_data)
print(head(sepsis_data))


# Print the structure and a sample of the data
str(sepsis_data)
print(head(sepsis_data, n = 5))  # Display the first 5 rows


# Print the data frame
print(sepsis_data)

# extended ----

extended_data <- data.frame(
	study_site = sample(c("Bern", "Zurich", "Geneva"), 300, replace = TRUE),
	sex = sample(c("M", "F"), 300, replace = TRUE),
	age_at_bc_days = sample(0:5000, 300, replace = TRUE),
	age_grp = sample(c("child.less12mt", "child.1y.4y", "child.5y.9y", "child.10y.16y"), 300, replace = TRUE),
	ethnicity = sample(c("caucasian", "mixed", "other"), 300, replace = TRUE),
	category = sample(c("healthy", "comorbidity", "tech.dependent"), 300, replace = TRUE),
	ccc_final = sample(c("2.conditions", "3.conditions", "4.conditions", "5.conditions", "cardial", "ccc.final", "gastrointestinal", "hematologic", "malformation", "malignancy", "metabolic", "neonatal", "neurologic", "renal", "surgery.burn", "tech.dependent"), 300, replace = TRUE),
	hosp_sepsis = sample(c(TRUE, FALSE), 300, replace = TRUE),
	hosp_delay = sample(0:50, 300, replace = TRUE),
	hosp_los = sample(1:30, 300, replace = TRUE),
	hosp_los_bc = sample(1:30, 300, replace = TRUE),
	pathogen_grp = sample(c("saureus", "ecoli", "candida", "viridansgroup",  "paeruginosa",  "spneumoniae", "othergneg", "spyogenes", "hinfluenzae", "klebsiella"), 300, replace = TRUE),
	focus_grp = sample(c("abdominal", "clabsi", "cns", "earnosethroat", "endocarditis", "focus.grp", "osteoarticular", "other", "pneumonia", "primbsi", "skin", "toxic_shock", "uti", "wound"), 300, replace = TRUE),
	picu = sample(c(TRUE, FALSE), 300, replace = TRUE),
	picu_reason = sample(c("sepsis", "trauma", "surgery", "no_picu", "other"), 300, replace = TRUE),
	picu_los = sample(0:20, 300, replace = TRUE),
	picu_los_bc = sample(0:20, 300, replace = TRUE),
	cahai = sample(c("yes", "no"), 300, replace = TRUE),
	death_30_bc = sample(c(TRUE, FALSE), 300, replace = TRUE),
	death_delay = round(sample(0:30, 300, replace = TRUE, prob = c(0.95, rep(0.05, 30)))),  # Ensure integers
	cons05_score_agg = round(runif(300, 0, 15)),  # Rounded to nearest whole number
	pelod_score_agg = round(runif(300, 0, 20)),  # Rounded to nearest whole number
	psofa_score_agg = round(runif(300, 0, 18)),  # Rounded to nearest whole number
	podium_score_agg = round(runif(300, 0, 10))   # Rounded to nearest whole number
)

# Combine with existing sepsis_data if needed or stand-alone
sepsis_data <- bind_cols(sepsis_data, extended_data)  # If combining with existing data

# Verify the structure and print sample data
str(extended_data)
print(head(extended_data))


# Combine with existing sepsis_data
full_sepsis_data <- bind_cols(sepsis_data, extended_data)

# Check the structure and sample data
str(full_sepsis_data)
print(head(full_sepsis_data))
