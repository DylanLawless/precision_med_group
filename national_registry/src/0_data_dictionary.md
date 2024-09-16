# Sepsis Registry Data Dictionary

This data dictionary defines and describes the variables stored in the Sepsis Registry database, which tracks detailed information about sepsis cases for research and quality improvement purposes.

## Table: SepsisRegistry

| Field Name                  | Data Type          | Description                                                                                             |
|-----------------------------|--------------------|---------------------------------------------------------------------------------------------------------|
| `Patient_ID`                | String             | A unique identifier for each patient to ensure privacy and enable tracking.                             |
| `Age`                       | Integer            | Age of the patient at the time of admission.                                                            |
| `Gender`                    | Char(1)            | Gender of the patient (M for male, F for female).                                                       |
| `Admission_Date`            | Date               | Date when the patient was admitted to the hospital.                                                     |
| `Discharge_Date`            | Date               | Date when the patient was discharged from the hospital.                                                 |
| `Diagnosis_Code`            | ICD-10 Code        | ICD-10 code for sepsis (e.g., A41.9 for sepsis, unspecified organisms).                                 |
| `Source_of_Infection`       | String             | The likely source or site of the infection leading to sepsis.                                           |
| `Organ_Dysfunction`         | Enum("Renal", "Cardiovascular", "Respiratory", "Hepatic", "Multiorgan", "Other") | Specific organs affected by sepsis.             |
| `Treatment_Administered`    | Text               | Key treatments provided to the patient (e.g., antibiotics, vasopressors, supportive care).              |
| `AMS_Compliance`            | Boolean            | Indicates whether antimicrobial stewardship (AMS) guidelines were followed (Yes/No).                     |
| `National_Sepsis_Pathway_Used` | Boolean         | Indicates if a national sepsis pathway was followed for treatment (Yes/No).                             |
| `Outcome`                   | Enum("Survived", "Deceased") | Patient's condition at discharge.                                           |
| `Follow_up_Date`            | Date               | Date for a follow-up visit or assessment, if applicable.                                                |
| `Sepsis_Standard_Met`       | Boolean            | Indicates whether the minimal national sepsis standard for care was met (Yes/No).                        |

### Notes

- **`Patient_ID`** is used instead of personal identifiers to protect patient confidentiality.
- **`Diagnosis_Code`** uses ICD-10 coding to standardize the classification of sepsis types.
- **`AMS_Compliance`**, **`National_Sepsis_Pathway_Used`**, and **`Sepsis_Standard_Met`** are represented as Boolean to clearly indicate compliance (Yes) or non-compliance (No).
- **`Organ_Dysfunction`** and **`Outcome`** are enumerated to ensure data consistency.

This structure supports comprehensive tracking and analysis of sepsis cases to improve patient outcomes and enhance healthcare quality.


sample.id		Unique identifier for each episode. Used to identify blood samples. Can be used to link between datasets.
episode.nr		Number of previous sepsis episodes registered in the same child. If this variable is used, it needs to be redefined for subprojects that do not use all episodes in the database. Personal identifier for each child. Equals to the sample.id of the first sepsis episode registered for every child. This variable should be used to define the number of children in the study.
personal.id		?Name of the recruiting site
study.site		Self-reported sex of patient.
sex age.at.bc.days		Age at blood culture sampling in days. Day of birth == 0. This is in line with STROBE-NI guidelines (Fitchett et al., 2016, (PMID 27633910))
age.grp		Age group at blood culture sampling.
ethnicity		Self-reported ethnicity
category		Patient risk category
ccc.final		Final classification according to the pediatric complex chronic condition classification system Version 2 (Feudtner et al. (2014). (PMID 25102958)). In children with more than one chronic condition, the number of underlying conditions is shown.
hosp.sepsis		Was sepsis the reason for hospital admission
hosp.delay		Time interval (days) between hospital admission and blood culture sampling. Negative values indicate the blood culture was taken before hospital admission.
hosp.los		Length of hospital stay (days)
hosp.los.bc		Length of hospital stay (days) after blood culture sampling. Only diverges from hosp.los in case the blood culture was taking later than the day of admission. Useful in hospital-acquired sepsis
pathogen.grp		Microorganism detected in blood culture, according to predefined groups
focus.grp		Site of infection according to predefined groups
picu		?Was the patient admitted to the intensive care unit
picu.reason		Is sepsis the reason for intensive care unit or has the patient already been on intensive care before sepsis onset
picu.los		Length of intensive care unit stay (days)
picu.los.bc		Length of intensive care unit stay (days) after blood culture sampling. Only diverges from hosp.los in case the blood culture was taking later than the day of picu admission. Useful in hospital-acquired sepsis if sepsis occurred during picu stay
cahai		Is this episode categorised as community or hospital-acquired sepsis? In newborns, those in whom blood culture was taken <= third day of life (i.e. age.at.bc <= 2)) are classified as early-onset sepsis, otherwise as late-onset sepsis (LOS). LOS in newborns is divided into community-acquired (i.e. hosp.delay <= 2) and hospital-acquired (i.e. hosp.delay > 2) based on the variable hosp.delay
death.30.bc		Did the child die in the first 30 days after blood culture sampling. CAVE some patients experienced more than one sepsis episode in the last thirty days before death. This needs to be accounted for in the data analysis!!
death.delay		Time interval (days) between blood culture sampling and death
cons05.score.agg		Total number of organ failures according to the 2005 consensus definitions (Goldstein et al. (2005) (PMID 15636651)). The classification of organ failures is based on the worst vital signs and the worst lab values during the first 7 days from blood culture sampling on
cons05.cvs.agg		Cardiovascular failure according to the 2005 consensus definitions (Goldstein et al. (2005) (PMID 15636651)). Based on lowest systolic blood pressure values, highest lactate, longest capillary refill time, fluid bolus, and catecholamine use in the first 7 days from blood culture sampling on
cons05.resp.agg		Respiratory failure according to the 2005 consensus definitions (Goldstein et al. (2005) (PMID 15636651)). Based on lowest oxygen saturation / concentration, highest carbon dioxide concentration and ventilation need in the first 7 days from blood culture sampling on
cons05.cns.agg		Central nervous system failure according to the 2005 consensus definitions (Goldstein et al. (2005) (PMID 15636651)). Based on lowest glasgow coma scale in the first 7 days from blood culture sampling on
cons05.ren.agg		Renal failure according to the 2005 consensus definitions (Goldstein et al. (2005) (PMID 15636651)). Based on creatinine concentration in the first 7 days from blood culture sampling on 
cons05.hep.agg		Hepatic failure according to the 2005 consensus definitions (Goldstein et al. (2005) (PMID 15636651)). Based on highest alanine aminotransferase and highest bilirubin the first 7 days from blood culture sampling on 
cons05.hem.agg		Hematological failure according to the 2005 consensus definitions (Goldstein et al. (2005) (PMID 15636651)). Based on lowest platelets and highest inr in the first 7 days from blood culture sampling on
pelod.score.agg		Total number of organ failures according to PELOD-2 definitions (Leteurtre et al. (2013) (PMID 23685639)). The classification of organ failures is based on the worst vital signs and the worst lab values during the first 7 days from blood culture sampling on
pelod.cvs.agg		Cardiovascular failure according to PELOD-2 definitions (Leteurtre et al. (2013) (PMID 23685639)). Based on lowest mean blood pressure values, highest lactate, longest capillary refill time, fluid bolus, and catecholamine use in the first 7 days from blood culture sampling on
pelod.resp.agg		Respiratory failure according to PELOD-2 definitions (Leteurtre et al. (2013) (PMID 23685639)). Based on lowest oxygen saturation / concentration, highest carbon dioxide concentration and ventilation need in the first 7 days from blood culture sampling on
pelod.cns.agg		Central nervous system failure according to PELOD-2 definitions (Leteurtre et al. (2013) (PMID 23685639)). Based on lowest glasgow coma scale in the first 7 days from blood culture sampling on
pelod.ren.agg		Renal failure according to PELOD-2 definitions (Leteurtre et al. (2013) (PMID 23685639)). Based on creatinine concentration in the first 7 days from blood culture sampling on
pelod.hem.agg		Hematological failure according to PELOD-2 definitions (Leteurtre et al. (2013) (PMID 23685639)). Based on lowest platelets and lowest white blood cell count in the first 7 days from blood culture sampling on
psofa.score.agg		Total number of organ failures according to 2017 pSOFA definitions (Matics et al. (2017) (PMID 28783810)). The classification of organ failures is based on the worst vital signs and the worst lab values during the first 7 days from blood culture sampling on
psofa.cvs.agg		Cardiovascular failure according to 2017 pSOFA definitions (Matics et al. (2017) (PMID 28783810)). Based on lowest mean blood pressure values and catecholamine use in the first 7 days from blood culture sampling on 
psofa.resp.agg		Respiratory failure according to 2017 pSOFA definitions (Matics et al. (2017) (PMID 28783810)). Based on lowest oxygen saturation / concentration in the first 7 days from blood culture sampling on
psofa.cns.agg		Central nervous system failure according to 2017 pSOFA definitions (Matics et al. (2017) (PMID 28783810)). Based on lowest glasgow coma scale in the first 7 days from blood culture sampling on
psofa.ren.agg		Renal failure according to 2017 pSOFA definitions (Matics et al. (2017) (PMID 28783810)). Based on creatinine concentration in the first 7 days from blood culture sampling on 
psofa.hep.agg		Hepatic failure according to 2017 pSOFA definitions (Matics et al. (2017) (PMID 28783810)). Based on highest bilirubin the first 7 days from blood culture sampling on 
psofa.hem.agg		Hematological failure according to 2017 pSOFA definitions (Matics et al. (2017) (PMID 28783810)). Based on lowest platelets in the first 7 days from blood culture sampling on
podium.score.agg		What is the total PODIUM score (personal communication L. Schlapbach
podium.cvs.agg		Cardiovascular failure according to PODIUM definitions (personal communication L. Schlapbach). Based on ECMO, catecholamine use, lactate, fastest heart rate and lowest blood pressure values in the first 7 days from blood culture sampling on
podium.resp.agg		Respiratory failure according to PODIUM definitions (personal communication L. Schlapbach).
podium.cns.agg		Based on ECMO, ventilation and lowest oxygen saturation in the first 7 days from blood culture sampling on Central nervous system failure according to PODIUM definitions (personal communication L. Schlapbach). Based on lowest glasgow coma scale in the first 7 days from blood culture sampling on
podium.ren.agg		Renal failure according to PODIUM definitions (personal communication L. Schlapbach). Based on highest creatinine in the first 7 days from blood culture sampling on
podium.hep.agg		Hepatic failure according to PODIUM definitions (personal communication L. Schlapbach). Based on highest bilirubin, ALT in the presence of liver coagulopathy and hepatic encephalopathy in the first 7 days from blood culture sampling on Hematological failure according to PODIUM definitions (personal communication L. Schlapbach).
podium.hem.agg		Based on lowest platelet or lymphocyte count in the first 7 days from blood culture sampling on
podium.coag.agg		Coagulation failure according to PODIUM definitions (personal communication L. Schlapbach). Based on lowest platelet count and highest INR in the first 7 days from blood culture sampling on
podium.imm.agg		podium.imm.agg


study.site - Location identifier (character)
sex - Patient gender (character)
age.at.bc.days - Age in days at baseline/control (integer)
age.grp - Age group classification (character)
ethnicity - Ethnic background (character)
category - Health category (character)
ccc.final - Comorbidity conditions (character)
hosp.sepsis - Indicator of hospital sepsis (logical/boolean)
hosp.delay - Delay in hospital admission (integer)
hosp.los - Length of stay in the hospital (integer)
hosp.los.bc - Length of stay before control (integer)
pathogen.grp - Group of pathogen involved (character)
focus.grp - Focus group of the infection (character)
picu - Admission to PICU (logical/boolean)
picu.reason - Reason for PICU admission (character)
picu.los - Length of stay in PICU (integer)
picu.los.bc - Length of stay in PICU before control (integer)
cahai - Hospital-acquired infection indicator (character)
death.30.bc - Death within 30 days from baseline/control (logical/boolean)
death.delay - Delay in days until death (integer)
cons05.score.agg - Aggregate score of condition at 5% level (numeric)
pelod.score.agg - PELOD scoring aggregate (numeric)
psofa.score.agg - PSOFA scoring aggregate (numeric)
podium.score.agg - PODIUM scoring aggregate (numeric)
Additional sub-scores for each scoring system (numeric)
R Code to Simulate and Integrate Data
