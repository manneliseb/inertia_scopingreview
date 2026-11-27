# This is R code for the project “Lingering over time: How do
# authors define and model (emotional) inertia? A scoping review.”
# For information on this project, see: https://osf.io/uaw4n/

# This script focuses on:
# The first cleaning & wrangling of the raw data

# Packages
library(here)
library(tidyverse)
library(purrr) # for open responses & strings
library(stringr) # for open responses & strings

here::here() # checking location of project

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------

# Data downloaded from Microsoft Forms from primary coder (MAB)
ScopingReview <- readr::read_csv2(
  here::here(
    "data",
    "raw",
    "Data extraction form_ Inertia scoping review (MAB)(Sheet1).csv"
  ),
  locale = locale(encoding = "UTF-8")
) %>%
  # remove unneeded columns
  dplyr::select(
    -c(`Heure de début`, `Heure de fin`, `Adresse de messagerie`, `Nom`)
  )

# Data downloaded from Microsoft Forms from secondary coder (LP)
ScopingReview_LP <- readr::read_csv2(here::here(
  "data",
  "raw",
  "Data extraction form_ Inertia scoping review (LP)(Sheet1).csv"
)) %>%
  # remove unneeded columns
  dplyr::select(
    -c(`Heure de début`, `Heure de fin`, `Adresse de messagerie`, `Nom`)
  )

# Hand-sorted categories for construct items
categorized_data <- readr::read_csv(
  here::here(
    "data",
    "processed",
    "ScopingReview_constructinfo_sorting.csv"
  ),
  locale = locale(encoding = "windows-1252")
) %>%
  select(
    Authors,
    Title,
    Constructs_Coarse,
    RemovedDataPoints_Why,
    CenterPredictors_Categorized,
    Recovery_Cat
  )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Functions ----------------------------------------------------------

# Functions for wrangling

# Recoding written-in responses (via 'other') to a new column
# (Keeping NAs, and taking into account accidental ';' after single entries)
recode_other <- function(x, valid_values) {
  split_vals <- map(
    x,
    ~ if (is.na(.x) || str_squish(.x) == "") {
      NA_character_
    } else {
      # str_remove(";\\s*$") |> # removing trailing ;
      str_split(.x, ";\\s*")[[1]] |>
        str_trim() |>
        (\(z) z[z != ""])()
    }
  )

  other <- map_chr(split_vals, function(vals) {
    if (all(is.na(vals))) {
      return(NA_character_)
    }

    bad <- vals[!vals %in% valid_values]

    if (length(bad) == 0) {
      NA_character_
    } else {
      paste(bad, collapse = ";")
    }
  })

  cleaned <- map_chr(split_vals, function(vals) {
    if (all(is.na(vals))) {
      return(NA_character_)
    }

    good <- vals[vals %in% valid_values]
    bad <- vals[!vals %in% valid_values]

    result <- good

    if (length(bad) > 0) {
      result <- c(result, "Other")
    }

    paste(unique(result), collapse = ";")
  })

  list(
    cleaned = cleaned,
    other = other
  )
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Wrangling Data ----------------------------------------------------------

new_columns <-
  c(
    "Id",
    "Authors",
    "Title",
    "Journal",
    "Reference",
    "SourceLocation",
    "Year",
    "Term_Inertia",
    "Term_Other",
    "ClearDefinition",
    "Definition_Type",
    "Definition_FirstAppears",
    "Definition_NewInfoAppears",
    "Definition_ExactPhrase",
    "Definition_Components",
    "Definition_ComponentsUnique",
    "Constructs",
    "OperationalizeRecovery_YN",
    "OperationalizeRecovery_How",
    "OperationalizeRecovery_How_ExactPhrase",
    "StudyTopic",
    "NParticipants",
    "SamplingDesign_Clear",
    "SamplingDesign_Type",
    "AverageTimeInterval_Reported",
    "AverageTimeInterval_Hours",
    "SamplingDesign_ExactPhrase",
    "StudyDuration_Days",
    "ESMPrompts_perDay",
    "AverageNAnsweredESM_Reported",
    "AverageNAnsweredESM",
    "RemovedDataPoints_CarelessResponding_YN",
    "RemovedDataPoints_CarelessResponding_How",
    "ResponseScales",
    "Distribution_Discussed",
    "Distribution_Exact",
    "variable_Mean",
    "variable_SD",
    "variable_Range",
    "Preregistration",
    "Prereg_workingLink",
    "DataShared",
    "CodeShared",
    "WithinPersonChange_YN",
    "WithinPersonChange_Correlate",
    "WithinPersonChange_HowModeled",
    "WithinPersonChange_Moderator_Step",
    "WithinPersonChange_Moderator_Form",
    "WithinPersonChange_Moderator_ExactInfo",
    "WithinPersonChange_Moderator_Type",
    "WithinPersonChange_Moderator_Distribution_YN",
    "WithinPersonChange_Moderator_Distribution_What",
    "WithinPersonChange_Moderator_BetweenReliability_Discussed",
    "WithinPersonChange_Moderator_BetweenReliability_How",
    "WithinPersonChange_Moderator_WithinReliability_Discussed",
    "WithinPersonChange_Moderator_WithinReliability_How",
    "WithinPersonChange_StatisticalModel",
    "WithinPersonChange_Evidence",
    "WithinPersonChange_Evidence_LinkedWithModerator",
    "WithinPersonChange_Evidence_Moderator_Which",
    "StatisticalModelFamily",
    "IndividualDifferencesHandled",
    "CenterPredictors",
    "CenterPredictors_How",
    "EstimationTechnique",
    "ModelComparison",
    "ModelComparison_HowChosen",
    "ModelComparison_DiffImpliedDefinitions",
    "ModelComparison_ImpliedDefinitions",
    "StatisticalAnalyses_OtherNotes",
    "ExactItems_Specified",
    "ExactItems",
    "Items_Justified_YN",
    "Items_Justified_How",
    "Scores_Calculated_How",
    "Scores_Pooled_How",
    "Scores_Calculated_Justified",
    "Scores_Calculated_Justified_How",
    "Reliability_WithinPerson_YN",
    "Reliability_WithinPerson_Method",
    "Reliability_WithinPerson",
    "Reliability_BetweenPerson",
    "InertiaEstimate_Indicated",
    "InertiaEstimate",
    "Inertia_Correlate_YN",
    "Inertia_Correlate_Which",
    "GeneralNotes",
    "Uncertainties"
  )


# Getting these columns saved for codebook
column_names <- as_tibble(new_columns) %>%
  add_column((colnames(ScopingReview))) %>%
  rename(fullItems = 2, varNames = 1)

# Saving the column names so have clear list of variable names + original question text
#write_excel_csv(column_names, here::here("data", "codebook_columns.csv"), )

# Adding hand-categorized categories for open variables to full dataset
ScopingReview <- ScopingReview %>%
  inner_join(
    categorized_data %>%
      select(
        Title,
        Constructs_Coarse,
        RemovedDataPoints_Why,
        CenterPredictors_Categorized,
        Recovery_Cat
      ),
    by = "Title"
  )

# Renaming columns & adapting relevant columns to integer etc.
ScopingReview <- ScopingReview %>%
  rename(!!!set_names(column_names$fullItems, column_names$varNames)) %>%
  mutate(
    across(c(Id, Year), as.integer)
  ) %>%
  # Creating separate columns for Uni & Country
  separate(
    SourceLocation,
    into = c("SourceUniversity", "SourceCountry"),
    sep = ",\\s*"
  )


# -- Adapting wording ------------------------------------------------------------------

# Removing semi-colons in response options (cause issues later for select-multiple variablesCan yo)
replacements <- list(
  ClearDefinition = c(
    "The authors provide a vague or unclear definition that does not explain or define inertia/the similar concept" = "Unclear"
  ),
  Definition_Components = c(
    "Autocorrelations/autoregressive model (if the authors specifically define inertia as the statistical model)" = "Autocorrelation/AR model",
    "Persisting; tendency to remain in same state" = "Persisting or tendency to remain in same state",
    "“Sticky”; stuck" = "“Sticky” or stuck",
    "Across contexts; spillover" = "Across contexts or spillover"
  ),
  Definition_ComponentsUnique = c(
    "“Sticky”; stuck" = "“Sticky” or stuck",
    "Autocorrelations/autoregressive model (if the authors specifically define inertia as the statistical model)" = "Autocorrelation/AR model",
    # typo in form:
    "inflexibity" = "inflexibility"
  ),
  # Also adapting phrasing in prep for graphs
  Term_Other = c(
    "autocorrelation (state it's \"commonly referred to as inertia/persistence\" but they call it only autocorrelation)" = "Autocorrelation/AR",
    "autocorrelation" = "Autocorrelation/AR",
    "autoregressive effects" = "Autocorrelation/AR",
    "autocorrelates" = "Autocorrelation/AR",
    "autoregressive" = "Autocorrelation/AR",
    "Emotional recovery" = "Recovery",
    "Affective recovery" = "Recovery",
    "recovery" = "Recovery",
    "stability" = "Stability"
  ),
  DataShared = c(
    "Available upon request" = "Upon request",
    "Publicly available (a working link is provided and data can be downloaded directly)" = "Yes",
    "Publicly available (but the authors do not provide a working link)" = "Yes (no working link)",
    "Not available" = "No"
  ),
  Preregistration = c(
    "Yes, after data access but before data analysis (e.g., secondary analysis)" = "Yes (before analysis)",
    "Yes, after data collection but before data access (post-registration)" = "Yes (before data access)",
    "Yes, before data collection (pre-registration)" = "Yes (before data collection)",
    "Yes, but timing unclear" = "Yes (unclear)"
  ),
  SamplingDesign_Clear = c(
    "Unclearly (but can assume a sampling design)" = "Unclear but assumed"
  ),
  AverageTimeInterval_Reported = c(
    "The time interval is too irregular (e.g., a design with exponentially decreasing intervals after an event)" = "Too irregular",
    "Yes, explicitly" = "Yes",
    "Yes, it can be calculated or assumed" = "Yes (assumed or calculated)"
  ),
  AverageNAnsweredESM_Reported = c(
    "Yes, explicitly" = "Yes",
    "Yes, it can be calculated or assumed" = "Yes (assumed or calculated)"
  ),
  WithinPersonChange_Correlate = c(
    "Reduction in clinical symptoms" = "Change in mental health",
    "Relapse (depression)" = "Change in mental health",
    "Transition to a manic or depressive episode" = "Change in mental health",
    "Sudden drop in mental health" = "Change in mental health",
    "Behavior (substance use)" = "Behavior",
    "transition toward recurrence of depression" = "Change in mental health"
  ),
  EstimationTechnique = c(
    "For GAM: nonparametric smooth functions based on thin-plate regression splines" = "Nonparametric smooth functions (GAM)"
  ),
  RemovedDataPoints_CarelessResponding_YN = c(
    "Don't clearly specify for which parts of results they remove data points BUT do remove some due to careless responding" = "Remove data points but unclear at which stage"
  ),
  WithinPersonChange_HowModeled = c(
    "Via a model or method without a moderator (e.g., time-varying VAR or regime-switching)" = "Model/method without moderator",
    "Via a moderator (in one or multiple steps)" = "Via a moderator"
  ),
  StatisticalModelFamily = c(
    "Autoregressive model" = "AR model",
    "Model with autoregressive term but also other main parameters (e.g., ARX)" = "AR model with other main parameter (e.g., ARX)",
    "Comparison across models: constant, linear, quadratic, cubic, asymptotic, and logistic" = "Comparison across models of different exponents",
    "Visual inspection of \"recovery after outlying values\" (page 91)" = "Visual inspection"
  ),
  IndividualDifferencesHandled = c(
    "Treat individual differences as important via second-step group-level analysis" = "Individual then group-level analysis",
    "Single case analysis: research questions focused on individuals" = "Single case analysis",
    "Model each individual and then cluster" = "Individual then cluster"
  ),
  CenterPredictors = c(
    "Not applicable (no multilevel techniques)" = "N/A (no multilevel techniques)"
  ),
  Scores_Calculated_How = c(
    "Aggregate (sum or average) of several items for each measurement occasion" = "Aggregate several items per timepoint",
    "Single item score per measurement" = "Single item",
    "Other way to pool items (Specify in next question)" = "Pooling items (not sum or average)"
  )
)


# Replacing these in the main dataframe
for (col in names(replacements)) {
  for (old in names(replacements[[col]])) {
    ScopingReview[[col]] <-
      str_replace_all(
        ScopingReview[[col]],
        fixed(old),
        replacements[[col]][[old]]
      )
  }
}


# -- Select-multiple columns that include "Other" option ------------------------------------------------

#Indicating the valid response options (pulled from the data extraction questionnaire)
valid_options <- list(
  Definition_Components = c(
    "Autocorrelation/AR model",
    "Self-predicting",
    "Persisting or tendency to remain in same state",
    "Resistance to change or inflexibility",
    "“Sticky” or stuck",
    "Across contexts or spillover",
    "Slower recovery / return to baseline",
    "Trait / person-level characteristic",
    "Within-person change (state-characteristic)"
  ),
  Definition_ComponentsUnique = c(
    "Autocorrelation/AR model",
    "Self-predicting",
    "Persisting",
    "Tendency to remain in same state",
    "Resistance to change",
    "Inflexibility",
    "“Sticky” or stuck",
    "Across contexts",
    "Spillover",
    "Slower recovery / return to baseline",
    "Trait / person-level characteristic",
    "Within-person change (state-characteristic)",
    "Lingering",
    "Carry over",
    "Insensitive/unaffected by context"
  ),
  StudyTopic = c(
    "Physical health and health behaviour",
    "Mental health",
    "Emotions",
    "Romantic or marital relationships",
    "Friendships",
    "Family relationships",
    "General social relationships/interactions",
    "Education",
    "Work/employment",
    "Consumer behaviour",
    "Situations/places",
    "Personality" # Decided to change "Personality states" (as pre-decided option) to "Personality" (to more accurately capture studies this was relevant to...)
  ),
  SamplingDesign_Type = c(
    "Fixed sampling design",
    "Semi-random sampling design",
    "Random sampling design",
    "Event-based sampling design"
  )
)


# Via function defined at start of script:
# Putting any written-in info as "Other" and moving the info to new "Other" column each time

for (col in names(valid_options)) {
  tmp <- recode_other(
    ScopingReview[[col]],
    valid_options[[col]]
  )

  ScopingReview[[col]] <- tmp$cleaned

  ScopingReview[[paste0(col, "_other")]] <- tmp$other

  ScopingReview <- ScopingReview %>%
    relocate(
      all_of(paste0(col, "_other")),
      .after = all_of(col)
    )
}

# -- Formatting around factors and ; ------------------------------------------------

# Removing any ; at the end of strings (Microsoft forms seems to add them but only sometimes??)
ScopingReview <- ScopingReview %>%
  mutate(
    across(
      where(is.character),
      ~ str_remove(.x, "\\s*;+\\s*$")
    ),
    # Removing white space after some resposne options
    CenterPredictors = str_trim(
      CenterPredictors
    )
  )
# Transforming relevant variables to factors
ScopingReview <- ScopingReview %>%
  mutate(
    across(
      c(
        Term_Inertia,
        ClearDefinition,
        OperationalizeRecovery_YN,
        SamplingDesign_Clear,
        AverageTimeInterval_Reported,
        AverageNAnsweredESM_Reported,
        RemovedDataPoints_CarelessResponding_YN,
        Distribution_Discussed,
        # Distribution_Exact, # select multiple so keep as character
        Preregistration,
        Prereg_workingLink,
        DataShared,
        CodeShared,
        WithinPersonChange_YN,
        WithinPersonChange_HowModeled,
        WithinPersonChange_Moderator_Step,
        WithinPersonChange_Moderator_Distribution_YN,
        WithinPersonChange_Moderator_BetweenReliability_Discussed,
        WithinPersonChange_Moderator_WithinReliability_Discussed,
        WithinPersonChange_Evidence,
        WithinPersonChange_Evidence_LinkedWithModerator,
        CenterPredictors,
        ModelComparison,
        ModelComparison_DiffImpliedDefinitions,
        ExactItems_Specified,
        Items_Justified_YN,
        Scores_Calculated_Justified,
        Reliability_WithinPerson_YN,
        EstimationTechnique
      ),
      as.factor
    )
  )

# Checking factors are correct levels etc
lapply(ScopingReview[sapply(ScopingReview, is.factor)], table)

# Also fixing formatting for new categorized variable (in prep for figures)
ScopingReview <- ScopingReview %>%
  mutate(
    Constructs_Coarse = Constructs_Coarse %>%
      # remove spaces around semicolons
      str_replace_all("\\s*;\\s*", ";") %>%
      # Ensure consistent capitalization after each semicolon
      str_replace_all(
        ";\\s*([a-z])",
        ~ paste0(";", str_to_upper(str_sub(.x, -1, -1)))
      ),
    RemovedDataPoints_Why = RemovedDataPoints_Why %>%
      str_replace_all("\\s*;\\s*", ";"),
    CenterPredictors_Categorized = CenterPredictors_Categorized %>%
      str_replace_all("\\s*;\\s*", ";"),
    Recovery_Cat = Recovery_Cat %>%
      str_replace_all("\\s*;\\s*", ";"),
    Definition_NewInfoAppears = Definition_NewInfoAppears %>%
      str_replace_all("\\s*;\\s*", ";"),
    EstimationTechnique = EstimationTechnique %>%
      str_replace_all("\\s*;\\s*", ";")
  )


# -- Categorizing definition components (other)  ------------------------------------------------
# These are the write-in results for definition components: Adding similar together, etc.

# Keep track of placement so can put back into full dataframe later
temp_SR <- ScopingReview %>%
  mutate(row_id = row_number())

# Checking current repsonses
def_writein <- temp_SR %>%
  select(Definition_ComponentsUnique_other) %>%
  filter(
    !is.na(Definition_ComponentsUnique_other),
    Definition_ComponentsUnique_other != ""
  ) %>%
  separate_rows(Definition_ComponentsUnique_other, sep = ";") %>%
  mutate(
    Definition_ComponentsUnique_other = str_trim(
      Definition_ComponentsUnique_other
    )
  ) %>%
  filter(Definition_ComponentsUnique_other != "") %>%
  count(Definition_ComponentsUnique_other, sort = TRUE)


content_cat <- temp_SR %>%
  # Split
  separate_rows(Definition_ComponentsUnique_other, sep = ";") %>%
  mutate(
    Definition_ComponentsUnique_other = str_trim(
      Definition_ComponentsUnique_other
    ),
    Definition_ComponentsUnique_other_cat = replace_values(
      Definition_ComponentsUnique_other,
      # Emotion regulation
      "Alterations in emotional responding" ~ "Emotion regulation",
      "Difficulty regulating emotional states" ~ "Emotion regulation",
      "Regulatory deficit" ~ "Emotion regulation",
      "Rely on one emotion regulation strategy and not shift away from it" ~ "Emotion regulation",
      "ER inertia: heavily relies on that single strategy, rigid regulatory patterns" ~ "Emotion regulation",
      "Failure of regulatory strategies to pull emotions back to baseline after an event has prompted movement away from home base" ~ "Emotion regulation",
      "Less capable to adapt to changing emotional circumstances" ~ "Emotion regulation",
      "Regulatory weakness" ~ "Emotion regulation",
      "impaired emotion regulation" ~ "Emotion regulation",
      "more strongly relies on a [self-control] single strategy" ~ "Emotion regulation",
      "fails to downregulate following perturbation" ~ "Emotion regulation",
      "affective regulation" ~ "Emotion regulation",
      "emotion regulation" ~ "Emotion regulation", # capitalization
      "difficulties in disengaging from RNT once it has been triggered" ~ "Emotion regulation",
      "Difficulties in stress management" ~ "Emotion regulation",
      "Deficits in emotion regulation" ~ "Emotion regulation",
      "regulatory strength" ~ "Emotion regulation",

      # Slow recovery / return to baseline
      "Regulation back to baseline" ~ "Slower recovery / return to baseline",

      # Relating to rate of change
      "Rate of change in emotional states" ~ "Rate of change",
      "Slow to change" ~ "Rate of change",
      "Slower mood shifts" ~ "Rate of change",
      "Speed at which mood shifts" ~ "Rate of change",
      "speed of change" ~ "Rate of change",
      "rate of change of affective state" ~ "Rate of change",

      # Relating to maladaptive
      "Pathological process" ~ "Maladaptive",
      "Marker of psychological maladjustment" ~ "Maladaptive",
      "maladaptive" ~ "Maladaptive",
      "connected to mental disorders" ~ "Maladaptive",
      "Linked to psychological maladjustment" ~ "Maladaptive",
      "Decoupling of emotions from their adaptive purpose" ~ "Maladaptive",

      # Psychological functioning
      "Markers of psychological functioning" ~ "Psychological functioning",
      "Resilience" ~ "Psychological functioning",
      "resilience" ~ "Psychological functioning",

      # Low fluctuations
      "Lower levels of fluctuation" ~ "Little fluctuation",
      "frequency of affective changes" ~ "Little fluctuation",
      "Low dynamism of state emotions" ~ "Little fluctuation",
      "Reduced reactivity" ~ "Little fluctuation",

      # Context less relevant
      "More resistant to external (e.g., events) or internal (e.g., regulatory efforts) forces" ~ "Persist despite context",
      "Persist over time despite external or internal stimuli" ~ "Persist despite context",
      "Unaffected by external stimuli" ~ "Persist despite context",

      # Slow change/recovering after context
      "trouble adapting to changes in external events" ~ "Slow recovering after context",
      "Prolonged experiences of negative affect after a stress event" ~ "Slow recovering after context",
      "Obstacle in recovering from relational events" ~ "Slow recovering after context",

      # Predictability
      "Predictability over time" ~ "Self-predicting",
      "Ability to transfer PA from one moment to the next" ~ "Self-predicting",

      # Resistance to change
      "Resistance to emotional fluctuations" ~ "Resistance to change",

      # Stability
      "propensity to maintain affective states over time" ~ "Stability",
      "temporal stability" ~ "Stability",
      "Stable" ~ "Stability",
      "Likely to remain in a particular emotional state" ~ "Stability",
      "\"maintain current affective trend\"" ~ "Stability",
      "Enduring" ~ "Stability",

      # Perseverating
      "Emotional perseveration" ~ "Perseverate",

      "temporal dependency" ~ "Temporal dependency",

      # Typo
      "Inflexibity" ~ "Inflexibility",

      # Other
      "\"cross-over effect\"" ~ "Other",
      "Used consistently over time" ~ "Other",
      "Variability" ~ "Other",
      "Rigid emotional profiles" ~ "Other",
      "pervasiveness" ~ "Other",
      "Emotional continuity" ~ "Other",
      "durations of emotions" ~ "Other",
      "reduction in the amplitude of oscillations over time" ~ "Other",
      "Attractor strength" ~ "Other",
      "attractor strength" ~ "Other",
      "Specific strategy used consistently over time" ~ "Other",
      "Spillover" ~ "Other"
    )
  ) %>%
  group_by(row_id) %>%
  summarize(
    Definition_ComponentsUnique_other_catnew = paste(
      unique(Definition_ComponentsUnique_other_cat),
      collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Definition_ComponentsUnique_other_catnew = replace_values(
      Definition_ComponentsUnique_other_catnew,
      "NA" ~ NA
    )
  )

ScopingReview <- temp_SR %>%
  left_join(content_cat, by = "row_id") %>%
  select(-row_id)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Preparing LP data for inter-rater reliability ---------------------------

# First removing columns not included for second reviewer (for other project)
LP_columnsNotIncluded <- c(
  "Definition_ComponentsUnique",
  "Definition_FirstAppears",
  "Definition_NewInfoAppears",
  "InertiaEstimate_Indicated",
  "InertiaEstimate",
  "Inertia_Correlate_YN",
  "Inertia_Correlate_Which"
)

# Remmoving unnecessary columns from LP's dataframe
fullNames_lookup_LP <- column_names %>%
  filter(!varNames %in% LP_columnsNotIncluded)

# Renaming columns for LP df, creating integer columns etc.
ScopingReview_LP <- ScopingReview_LP %>%
  rename(
    !!!set_names(fullNames_lookup_LP$fullItems, fullNames_lookup_LP$varNames)
  ) %>%
  mutate(
    across(c(Id, Year), as.integer)
  ) %>%
  # Creating separate columns for Uni & Country
  separate(
    SourceLocation,
    into = c("SourceUniversity", "SourceCountry"),
    sep = ",\\s*"
  )

# Replacing these in the LP dataframe as well
# Removing 'components_unique' as not relevant for LP
replacements_LP <- replacements[-3]

for (col in names(replacements_LP)) {
  for (old in names(replacements_LP[[col]])) {
    ScopingReview_LP[[col]] <-
      str_replace_all(
        ScopingReview_LP[[col]],
        fixed(old),
        replacements_LP[[col]][[old]]
      )
  }
}

# Only including relevant 'valid variables' for LP dataframe
valid_options_LP <- valid_options[-2]

# Putting any written-in info in "Other" column (using fn from other script)
for (col in names(valid_options_LP)) {
  tmp <- recode_other(
    ScopingReview_LP[[col]],
    valid_options_LP[[col]]
  )

  ScopingReview_LP[[col]] <- tmp$cleaned

  ScopingReview_LP[[paste0(col, "_other")]] <- tmp$other

  ScopingReview_LP <- ScopingReview_LP %>%
    relocate(
      all_of(paste0(col, "_other")),
      .after = all_of(col)
    )
}

# Transforming relevant variables to factors
ScopingReview_LP <- ScopingReview_LP %>%
  mutate(
    across(
      c(
        Term_Inertia,
        ClearDefinition,
        OperationalizeRecovery_YN,
        SamplingDesign_Clear,
        AverageTimeInterval_Reported,
        AverageNAnsweredESM_Reported,
        RemovedDataPoints_CarelessResponding_YN,
        Distribution_Discussed,
        # Distribution_Exact, # select multiple so keep as character
        Preregistration,
        Prereg_workingLink,
        DataShared,
        CodeShared,
        WithinPersonChange_YN,
        WithinPersonChange_HowModeled,
        WithinPersonChange_Moderator_Step,
        WithinPersonChange_Moderator_Distribution_YN,
        WithinPersonChange_Moderator_BetweenReliability_Discussed,
        WithinPersonChange_Moderator_WithinReliability_Discussed,
        WithinPersonChange_Evidence,
        WithinPersonChange_Evidence_LinkedWithModerator,
        CenterPredictors,
        ModelComparison,
        ModelComparison_DiffImpliedDefinitions,
        EstimationTechnique,
        ExactItems_Specified,
        Items_Justified_YN,
        Scores_Calculated_Justified,
        Reliability_WithinPerson_YN
      ),
      as.factor
    )
  )

# Removing any ; at the end of strings
ScopingReview_LP <- ScopingReview_LP %>%
  mutate(
    across(
      where(is.character),
      ~ str_remove(.x, "\\s*;+\\s*$")
    )
  )


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5) Preparing general data for inter-rater reliability ----------------------

### Selecting variables for inter-rater comparison
# Note: This involves all variables that are: multiple choice, select-all items, or items that can be answered only by an integer (or “Not reported” or “N/A”), as written in protocol
InterRater_Variables <- c(
  "Title",
  "Authors",
  "Term_Inertia",
  "ClearDefinition",
  "Definition_Type",
  "Definition_Components",
  "OperationalizeRecovery_YN",
  "OperationalizeRecovery_How",
  "StudyTopic",
  "NParticipants",
  "SamplingDesign_Clear",
  "SamplingDesign_Type",
  "AverageTimeInterval_Reported",
  "AverageTimeInterval_Hours",
  "StudyDuration_Days",
  "ESMPrompts_perDay",
  "AverageNAnsweredESM_Reported",
  "AverageNAnsweredESM",
  "RemovedDataPoints_CarelessResponding_YN",
  "ResponseScales",
  "Preregistration",
  "Prereg_workingLink",
  "DataShared",
  "CodeShared",
  "WithinPersonChange_YN",
  "WithinPersonChange_Correlate",
  "WithinPersonChange_HowModeled",
  "WithinPersonChange_Moderator_Step",
  "WithinPersonChange_Moderator_Type",
  "WithinPersonChange_StatisticalModel",
  "WithinPersonChange_Evidence",
  "WithinPersonChange_Evidence_LinkedWithModerator",
  "StatisticalModelFamily",
  "IndividualDifferencesHandled",
  "CenterPredictors",
  "EstimationTechnique",
  "ModelComparison",
  "ModelComparison_DiffImpliedDefinitions"
)

# Selecting just relevant variables & articles for inter-rater reliability
ScopingReview_LP_ir <- ScopingReview_LP %>%
  select(all_of(InterRater_Variables))

ScopingReview_MAB_ir <- ScopingReview %>%
  select(all_of(InterRater_Variables)) %>%
  filter(Authors %in% ScopingReview_LP_ir$Authors)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6) Cleaning numeric data & prepping for export- --------------------------

# Reordering  before new dataframe
ScopingReview <- ScopingReview %>%
  relocate(Constructs_Coarse, .after = Constructs) %>%
  relocate(
    RemovedDataPoints_Why,
    .after = RemovedDataPoints_CarelessResponding_How
  ) %>%
  relocate(CenterPredictors_Categorized, .after = CenterPredictors_How) %>%
  relocate(Recovery_Cat, .after = OperationalizeRecovery_How_ExactPhrase) %>%
  relocate(
    Definition_ComponentsUnique_other_catnew,
    .after = Definition_ComponentsUnique_other
  )

# Making identifiers per article
ScopingReview <- ScopingReview %>%
  mutate(
    # Extract study number if present
    StudyNumber = str_extract(Title, "\\[Study [12]\\]$"),

    # Remove study suffix to identify unique article
    ArticleTitle = str_remove(Title, "\\s*\\[Study [12]\\]$")
  ) %>%
  mutate(
    ArticleID = dense_rank(ArticleTitle)
  ) %>%
  group_by(ArticleID) %>%
  mutate(
    StudySuffix = case_when(
      n() == 1 ~ "",
      row_number() == 1 ~ "_a",
      row_number() == 2 ~ "_b"
    ),
    StudyID = paste0(ArticleID, StudySuffix)
  ) %>%
  ungroup() %>%
  relocate(c(ArticleID, StudyID), .before = Id) %>%
  select(-c(Id, StudyNumber, ArticleTitle, StudySuffix)) %>%
  arrange(ArticleID)


# Cleaning numeric data
ScopingReview_forR <- ScopingReview %>%
  # Number of participants
  mutate(
    NParticipants = replace_values(
      NParticipants,
      "1031, 171" ~ "1031",
      "1139 (wave 1), 906 (wave 2), 797 (wave 3)" ~ "1139",
      "Not clearly reported" ~ NA
    ),
    # StudyDuration_Days
    StudyDuration_Days = replace_values(
      StudyDuration_Days,
      # For weeks: x7
      "1 week" ~ "7",
      "2 weeks" ~ "14",
      "3 weeks" ~ "21",
      "28 weeks" ~ "196",
      # For months: x30.5
      "2 months" ~ "61",
      "4 months" ~ "122",
      "6 months" ~ "183",
      # For multiple values (range), choosing smallest available if possible
      "10+" ~ "10",
      "minimum 60 days" ~ "60",
      "4 sessions of 1 hour each" ~ "4",
      "1-43" ~ "13.9", # Since min is 1, putting the average # given by authors
      "14-21" ~ "14",
      "36-126" ~ "36",
      "7-8" ~ "7",
      # If only info is max, giving that info
      "Up to 64 days" ~ "64"
    ),
    # ESMPrompts_perDay
    ESMPrompts_perDay = replace_values(
      ESMPrompts_perDay,
      "Not reported" ~ NA,
      "Not clear" ~ NA,
      "Minimum 8" ~ "8", # Putting the minimum number here
      "3 (but averaged per day)" ~ "1", # Putting effective # for AR here
    ),
    # AverageNAnsweredESM
    AverageNAnsweredESM = replace_values(
      AverageNAnsweredESM,
      # Keeping smallest in general
      "26 (wave 1); 27.9 (wave 2); 29.2 (wave 3)" ~ "26",
      "Healthy controls (n=35): 33 \nAddiction (n=64): 30.1" ~ "30.1",
      "25.8, 32.4" ~ "25.8",
      # This is a burst design, so taking sum of scheduled & burst beeps
      "127.68 scheduled beeps; 18 episodes; 43.51 burst beeps" ~ "171.19",
      # Treating median as close enough to the mean (e.g., another measure of central tendency)
      "Median: 545" ~ "545"
    ),
    AverageTimeInterval_Hours = replace_values(
      AverageTimeInterval_Hours,
      "N/S" ~ NA,
      "Not reported" ~ NA
    )
  ) %>%
  mutate(across(
    c(
      NParticipants,
      StudyDuration_Days,
      ESMPrompts_perDay,
      AverageNAnsweredESM,
      AverageTimeInterval_Hours
    ),
    as.numeric
  ))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6) Preparing data for export --------------------------------------------

# Removing unneeded rows
ScopingReview_toShare <- ScopingReview %>%
  select(
    -c(
      Definition_Components,
      Definition_Components_other,
      #     OperationalizeRecovery_How,
      Distribution_Discussed,
      Distribution_Exact,
      variable_Mean,
      variable_SD,
      variable_Range,
      Distribution_Discussed,
      Distribution_Exact,
      WithinPersonChange_Moderator_Distribution_YN,
      WithinPersonChange_Moderator_Distribution_What,
      WithinPersonChange_Moderator_BetweenReliability_Discussed,
      WithinPersonChange_Moderator_BetweenReliability_How,
      WithinPersonChange_Moderator_WithinReliability_Discussed,
      WithinPersonChange_Moderator_WithinReliability_How,
      ExactItems_Specified,
      ExactItems,
      Items_Justified_YN,
      Items_Justified_How,
      Scores_Calculated_Justified,
      Scores_Calculated_Justified_How,
      Reliability_WithinPerson_YN,
      Reliability_WithinPerson_Method,
      Reliability_WithinPerson,
      Reliability_BetweenPerson,
      InertiaEstimate_Indicated,
      InertiaEstimate,
      Inertia_Correlate_YN,
      Inertia_Correlate_Which,
      Uncertainties
    )
  )


ScopingReview_forR <- ScopingReview_forR %>%
  select(
    -c(
      Definition_NewInfoAppears,
      Definition_Components,
      Definition_Components_other,
      Constructs,
      # OperationalizeRecovery_How,
      OperationalizeRecovery_How_ExactPhrase,
      Distribution_Discussed,
      Distribution_Exact,
      variable_Mean,
      variable_SD,
      variable_Range,
      RemovedDataPoints_CarelessResponding_How,
      WithinPersonChange_Moderator_Distribution_YN,
      WithinPersonChange_Moderator_Distribution_What,
      WithinPersonChange_Moderator_BetweenReliability_Discussed,
      WithinPersonChange_Moderator_BetweenReliability_How,
      WithinPersonChange_Moderator_WithinReliability_Discussed,
      WithinPersonChange_Moderator_WithinReliability_How,
      ExactItems_Specified,
      ExactItems,
      Items_Justified_YN,
      Items_Justified_How,
      Scores_Calculated_Justified,
      Scores_Calculated_Justified_How,
      Reliability_WithinPerson_YN,
      Reliability_WithinPerson_Method,
      Reliability_WithinPerson,
      Reliability_BetweenPerson,
      InertiaEstimate_Indicated,
      InertiaEstimate,
      Inertia_Correlate_YN,
      Inertia_Correlate_Which,
      Uncertainties
    )
  )


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Saving data ----------------------

# Data for figures
readr::write_csv(
  ScopingReview,
  here::here("data", "processed", "ScopingReview_Alldata_processed.csv")
)

readr::write_rds(
  ScopingReview,
  here::here("data", "processed", "ScopingReview_Alldata_processed.RDS")
)

readr::write_csv(
  ScopingReview_forR,
  here::here("data", "processed", "ScopingReview_forR.csv")
)

readr::write_rds(
  ScopingReview_forR,
  here::here("data", "processed", "ScopingReview_forR.RDS")
)

readr::write_csv(
  ScopingReview_toShare,
  here::here("data", "processed", "ScopingReview_toShare.csv")
)

readr::write_rds(
  ScopingReview_toShare,
  here::here("data", "processed", "ScopingReview_toShare.RDS")
)

# Data for inter-rater computations
# readr::write_csv(
#   ScopingReview_LP_ir,
#   here::here("data", "processed", "ForInterRaterReliability", "ScopingReview_LP_ir.csv")
# )
# readr::write_csv(
#   ScopingReview_MAB_ir,
#   here::here("data", "processed", "ForInterRaterReliability", "ScopingReview_MAB_ir.csv")
# )

# readr::write_rds(
#   ScopingReview_LP_ir,
#   here::here("data", "processed", "ForInterRaterReliability", "ScopingReview_LP_ir.RDS")
# )
# readr::write_rds(
#   ScopingReview_MAB_ir,
#   here::here("data", "processed", "ForInterRaterReliability", "ScopingReview_MAB_ir.RDS")
# )

# # Data for compiling construct info
# readr::write_csv(
#   ScopingReview %>% select(Authors, Title, Constructs, Term_Other),
#   here::here("data", "processed", "ScopingReview_constructinfo.csv")
# )
