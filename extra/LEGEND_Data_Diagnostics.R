
# Set up data diagnostics for Legend T2dm ----

# remotes::install_github("OHDSI/ROhdsiWebApi")
# remotes::install_github("OHDSI/CohortGenerator")
# remotes::install_github("OHDSI/MethodEvaluation")

baseUrl <- "https://epi.jnj.com:8443/WebAPI"

outputFolder <- "output/main_comparisons"

library(ROhdsiWebApi)
library(tidyr)
library(dplyr)

## Set up variables

dbProfileConnectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                         user = Sys.getenv("OEN_USER"),
                                                                         password = Sys.getenv("OEN_PASSWORD"),
                                                                         server = Sys.getenv("OEN_SERVER"),
                                                                         port = 5432,
                                                                         extraSettings = "ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory")


# Load outcome list for Legend T2dm study ----
outcomes <- read.csv("inst/settings/OutcomesDataDiagnosticsSettings.csv", stringsAsFactors = FALSE)

# Reset all_outcomes_concept_sets to clear duplicates from previous runs
all_outcomes_concept_sets <- data.frame()

# Iterate through each outcome
for (k in 1:nrow(outcomes)) {

  # Read and parse each outcome JSON
  outcome_json_text <- SqlRender::readSql(system.file("cohorts", paste0(outcomes$name[k], ".json"), package = "LegendT2dm"))
  cohort <- RJSONIO::fromJSON(outcome_json_text)

  # Only process non-empty ConceptSets
  if (length(cohort$ConceptSets) > 0) {

    # Temporary storage for the loops
    all_concept_sets <- data.frame()

    for (i in 1:length(cohort$ConceptSets)) {

      # Avoid processing 'Visit' domain concept sets
      if (cohort$ConceptSets[[i]]$expression$items[[1]]$concept$DOMAIN_ID != "Visit") {

        # Authorize and resolve concept set
        ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")
        concept_set <- as.data.frame(ROhdsiWebApi::resolveConceptSet(cohort$ConceptSets[[i]], baseUrl))
        names(concept_set) <- "Concept Id"

        # Add necessary columns
        concept_set$"Concept Set Name" <- cohort$ConceptSets[[i]]$name
        concept_set$"Outcome Cohort Name" <- outcomes$atlasName[k]

        # Append to the local list for this outcome
        all_concept_sets <- rbind(all_concept_sets, concept_set)
      }
    }

    # Append all concept sets of this outcome to the final result
    all_outcomes_concept_sets <- rbind(all_outcomes_concept_sets, all_concept_sets)
  }
}

rm(cohort,concept_set,all_concept_sets,i,k,outcome_json_text)

## Load exposure and target lists ----

distinctClasses <- read.csv("inst/settings/classCohortsToCreate.csv", stringsAsFactors = FALSE) %>%
                    filter(stringr::str_detect(atlasId, "1100000"))

classTcs <- read.csv("inst/settings/classTcosOfInterest.csv", stringsAsFactors = FALSE) %>%
             filter(stringr::str_detect(targetId, "1100000"))

### Resolve concepts for drug classes ----
# Reset all_classes_concept_sets to clear duplicates from previous runs
all_classes_concept_sets <- data.frame()

# Iterate through each class
for (k in 1) { ## each class cohort has the concepts for all classes

  # Read and parse each outcome JSON
  class_json_text <- SqlRender::readSql(system.file("cohorts", "class", paste0("ID",distinctClasses$atlasId[k], ".json"), package = "LegendT2dm"))
  cohort <- RJSONIO::fromJSON(class_json_text)

  # Only process non-empty ConceptSets
  if (length(cohort$ConceptSets) > 0) {

    # Temporary storage for the loops
    all_concept_sets <- data.frame()

    for (i in 1:length(cohort$ConceptSets)) {

      # Avoid processing 'Visit' domain concept sets
      if (cohort$ConceptSets[[i]]$expression$items[[1]]$concept$DOMAIN_ID != "Visit") {

        # Authorize and resolve concept set
        ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")
        concept_set <- as.data.frame(ROhdsiWebApi::resolveConceptSet(cohort$ConceptSets[[i]], baseUrl))
        names(concept_set) <- "Concept Id"

        # Add necessary columns
        concept_set$"Concept Set Name" <- cohort$ConceptSets[[i]]$name

        # Append to the local list for this outcome
        all_concept_sets <- rbind(all_concept_sets, concept_set)
      }
    }

    # Append all concept sets of this outcome to the final result
    all_classes_concept_sets <- rbind(all_classes_concept_sets, all_concept_sets)
  }
}

rm(all_concept_sets,cohort,concept_set,i,k,class_json_text)

## Add atlasId and atlasName to dataframe

all_classes_concept_sets <- all_classes_concept_sets %>%
  mutate(atlasId = case_when(
    `Concept Set Name` == "DPP4 inhibitors" ~ 101100000,
    `Concept Set Name` == "GLP-1 receptor agonists" ~ 201100000,
    `Concept Set Name` == "Sulfonylureas" ~ 401100000,
    `Concept Set Name` == "SGLT2 inhibitors" ~ 301100000
  ))

# Run Data Diagnostics ----

## Default diagnostic criteria ----

minAge <- 18
maxAge <- 100
requiredDurationDays <- 365
requiredDomains = c("condition,drug")

## Set up the empty list

settingsList <- list()
counter <- 0

for(m in 1:nrow(outcomes)){

  outcomeConceptIds <- all_outcomes_concept_sets %>%
                       filter(`Outcome Cohort Name` == outcomes$atlasName[m]) %>%
                       select(`Concept Id`)

  ## target and comparator loop ----

  for(b in 1:nrow(classTcs)){

  counter <- counter + 1

  target <- classTcs$targetName[b]
  targetConceptIds <- all_classes_concept_sets %>%
                      filter(atlasId == classTcs$targetId[b]) %>%
                      select(`Concept Id`)

  comparator <- classTcs$comparatorName[b]
  comparatorConceptIds <- all_classes_concept_sets %>%
                          filter(atlasId == classTcs$comparatorId[b]) %>%
                          select(`Concept Id`)

  analysisName <- paste(target,comparator,outcomes$atlasName[m], sep = "_")

  analysisSettings <- DbDiagnostics::createDataDiagnosticsSettings(
    analysisId = counter,
    analysisName = analysisName,
    minAge = minAge,
    maxAge = maxAge,
    requiredDomains = requiredDomains,
    requiredDurationDays = requiredDurationDays,
    desiredDomains = outcomes$desired_domains[m],
    desiredVisits = outcomes$desired_visits[m],
    targetName = target,
    targetConceptIds = targetConceptIds,
    comparatorName = comparator,
    comparatorConceptIds = comparatorConceptIds,
    outcomeName = outcomes$atlasName[m],
    outcomeConceptIds = outcomeConceptIds
  )

  settingsList[[counter]] <- analysisSettings

  }
}

dbDiagnosticResults <- DbDiagnostics::executeDbDiagnostics(connectionDetails = dbProfileConnectionDetails,
                                                           resultsDatabaseSchema = "public",
                                                           resultsTableName = "sos_ohdsi_network_profile",
                                                           outputFolder = outputFolder,
                                                           dataDiagnosticsSettingsList = settingsList)

dbDiagnosticSummary <- read.csv(paste0(outputFolder,"/data_diagnostics_summary.csv"), stringsAsFactors = F)

