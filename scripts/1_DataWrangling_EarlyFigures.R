# Packages
library(here)
library(tidyverse)

here::here() # checking location of project


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------

# Data downloaded from Microsoft Forms
ScopingReview <- readr::read_csv2(here::here(
  "data",
  "Data extraction form_ Inertia scoping review (MAB)(Sheet1).csv"
)) %>%
  # remove unneeded columns
  dplyr::select(
    -c(`Heure de début`, `Heure de fin`, `Adresse de messagerie`, `Nom`)
  )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Wrangling Data ----------------------------------------------------------

# Renaming columns
fullNames_lookup <- tibble(names(ScopingReview)) %>%
  add_column(
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
  ) %>%
  rename(oldNames = 1, newNames = 2)

# Name both column

# Renaming dataframe column names
ScopingReview <- ScopingReview %>%
  rename(!!!set_names(fullNames_lookup$oldNames, fullNames_lookup$newNames)) %>%
  # Creating separate columns for Uni & Country
  separate(
    SourceLocation,
    into = c("SourceUniversity", "SourceCountry"),
    sep = ",\\s*"
  )


# Creating a dataframe with just unique articles (without extra studies)
ScopingReview_unique <- ScopingReview %>%
  distinct(Title, .keep_all = TRUE) %>%
  # Adding a short ID tag for each article
  mutate(
    ArticleID = paste0("A", row_number()),
    Present = 1
  )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Descriptives ----------------------------------------------------------

# USE TERM INERTIA
ScopingReview_unique %>%
  count(Term_Inertia) # 8 no, 22 yes


ScopingReview_InertiaTerm <- ScopingReview_unique %>%
  separate_rows(Definition_ComponentsUnique, sep = ";") %>%
  group_by(Term_Inertia, Definition_ComponentsUnique) %>%
  summarise(n = n(), .groups = "drop") %>%
  # Check, lots of NAs - MAB

  # Getting proportions
  group_by(Term_Inertia) %>%
  mutate(prop = n / sum(n))

# WITHIN-PERSON CHANGE
ScopingReview_unique %>%
  count(WithinPersonChange_YN) # 24 no, 6 yes

# Proportion that found evidence
ScopingReview_unique %>%
  filter(WithinPersonChange_YN == "Yes") %>%
  count(WithinPersonChange_Evidence) %>%
  mutate(prop = n / sum(n))
# 67 % find evidence of w/in person change; 1 no (but within-day AR models); 1 didn't mention

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) FIGURES  ----------------------------------------------------------

country_counts <- ScopingReview_unique %>%
  count(SourceCountry)
# MAB note - need to double check correct naming etc

library(maps)

world <- map_data("world")

map_data_joined <- world %>%
  left_join(country_counts, by = c("region" = "SourceCountry"))


ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(color = "grey50") +
  scale_fill_viridis_c(na.value = "white") +
  coord_quickmap() +
  theme_minimal() +
  labs(fill = "Number of articles")


# Plot with # of articles that looked at within person change

# First setting colors
# (color-blind friendly color scheme from https://sronpersonalpages.nl/~pault/)
code_colors_wpchange <- c(
  "Yes" = "#5AAE61", # green
  "No" = "#9970AB", # purple
  "Not mentioned" = "#E7D4E8", # light purple
  "Not applicable" = "#555555" # grey
)

# postscript(
#   file = here::here("figures", "WithinPersonChange.eps"),
#   width = 8,
#   height = 6
# )
# jpeg(
#   file = here::here("figures", "WithinPersonChange.jpg"),
#   width = 8,
#   height = 6,
#   units = "in",
#   res = 300
# )

ScopingReview_unique %>%
  mutate(
    WithinPersonChange_Evidence = replace_na(
      WithinPersonChange_Evidence,
      "Not applicable"
    )
  ) %>%
  count(WithinPersonChange_YN, WithinPersonChange_Evidence) %>%
  ggplot(aes(
    x = WithinPersonChange_YN,
    y = n,
    fill = WithinPersonChange_Evidence
  )) +
  geom_col() +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  labs(
    x = "Within-person change assessed?",
    y = "Number of articles",
    fill = "Evidence"
  ) +
  theme_bw() +
  scale_fill_manual(
    values = code_colors_wpchange
  )


# dev.off()

# Plot that looks at definition components

# First getting data in long format

defs_long <- ScopingReview_unique %>%
  filter(
    ClearDefinition != "No"
  ) %>%
  # split on the semi-colon
  separate_rows(Definition_ComponentsUnique, sep = "\\s*;\\s*") %>%
  mutate(
    Definition_ComponentsUnique = str_trim(Definition_ComponentsUnique)
  ) %>%
  # Removing any NAs (sometimes have ___; and leads to NAs)
  filter(Definition_ComponentsUnique != "") %>%
  # Trim AR component
  mutate(
    Definition_ComponentsUnique = case_when(
      Definition_ComponentsUnique ==
        "Autocorrelations/autoregressive model (if the authors specifically define inertia as the statistical model)" ~ "Autoregression",
      Definition_ComponentsUnique == "stuck" ~ "Sticky/stuck",
      .default = Definition_ComponentsUnique
    )
  ) %>%
  # Remove extra "Sticky" component (now redundant, was from same component as "stuck")
  filter(Definition_ComponentsUnique != "“Sticky”")

# Plotting
# SIDE-BY-SIDE BARS (Inertia term vs not) (N>1)
defs_long %>%
  count(Term_Inertia, Definition_ComponentsUnique) %>%
  # Get percent; want to graph that?? - MAB (rn not included)
  # mutate(
  #   pct = 100 * n / sum(n)
  # )
  # only include if MORE than 1 definition component
  filter(n > 1) %>%
  ggplot(aes(
    x = reorder(Definition_ComponentsUnique, n),
    y = n,
    fill = Term_Inertia
  )) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x = "Definition component",
    y = "Number of articles"
  ) +
  theme_minimal()

# STACKED BARS - OVERALL DEFINITION COMPONENTS (N>1)
defs_long %>%
  count(Term_Inertia, Definition_ComponentsUnique) %>%
  filter(n > 1) %>%
  ggplot(
    aes(
      x = reorder(Definition_ComponentsUnique, n),
      y = n,
      fill = Term_Inertia
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Definition component",
    y = "Number of articles"
  ) +
  theme_minimal()


# IF WANT FACET WRAP !
defs_long %>%
  count(Term_Inertia, Definition_ComponentsUnique) %>%
  filter(n > 1) %>%
  ggplot(
    aes(
      x = reorder(Definition_ComponentsUnique, n),
      y = n
    )
  ) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  facet_wrap(~Term_Inertia) +
  labs(
    x = "Definition component",
    y = "Number of articles"
  ) +
  theme_minimal()


# LOOKING PER ARTICLE

# Getting binary presence vs absence of specific definition components
defs_article <- defs_long %>%
  add_count(Definition_ComponentsUnique) %>%
  filter(n > 1) %>%
  # removing when just single definition components for now
  distinct(Title, Definition_ComponentsUnique) %>%
  mutate(Present = 1) %>%
  # Making code for articles
  mutate(
    ArticleID = paste0("A", row_number()),
    Present = 1
  )


ggplot(
  defs_article,
  aes(
    x = Definition_ComponentsUnique,
    y = forcats::fct_rev(ArticleID),
    fill = factor(Present)
  )
) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("1" = "#0072B2"),
    guide = "none"
  ) +
  labs(
    x = "Definition component",
    y = "Article"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
# MAB - maybe useful to include above for final version, BUT only if remove lots of n=1 versions...

# Other way to look at combinations:
library(ComplexUpset)

# Convert back to wide format

defs_wide <- defs_article %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = Definition_ComponentsUnique,
    values_from = value,
    values_fill = 0
  )


upset(
  defs_wide,
  intersect = names(defs_wide)[-1]
)
