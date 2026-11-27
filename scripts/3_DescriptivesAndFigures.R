# This is R code for the project “Lingering over time: How do
# authors define and model (emotional) inertia? A scoping review.”
# For information on this project, see: https://osf.io/uaw4n/

# This script focuses on:
# - Examining descriptive statistics
# - Generating figures for the manuscript & supp materials

# Packages
library(here)
library(tidyverse)
library(maps) # to graph a world map with country affiliations of first authors
library(RColorBrewer) # for graphing palettes
library(khroma) # for more graphing palettes
library(patchwork) # to piece together graphs
library(ggpattern) # for stripes in later graphs
library(cowplot) # for one graph with two panels and a common legend
library(ggnewscale) # for one graph with two different patterns for tiles

# Checking in correct folder location
here::here()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) FUNCTIONS  ----------------------------------------------------------

# Function to determine if text on graphs should be white or black
get_text_colour <- function(hex_col) {
  rgb <- col2rgb(hex_col)

  brightness <- (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255

  if_else(brightness < 0.5, "white", "black")
}


# ggplot theme settings
gglayer_theme <- list(
  theme_minimal(base_size = 14),
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)),
    minor_breaks = seq(2, 100, 2)
  ),
  # to remove gap between title & graph (probably bc expects multiple y values)
  scale_y_discrete(expand = expansion(mult = c(0, 0))),
  theme(
    plot.margin = margin(t = 5, b = 25),
    plot.title = element_text(size = 16, hjust = 0, margin = margin(b = 2))
  )
)

gg_row <- list(
  theme_void(base_size = 14),
  scale_x_continuous(expand = expansion(mult = c(0, 0))),
  # to remove gap between title & graph (probably bc expects multiple y values)
  scale_y_discrete(expand = expansion(mult = c(0, 0))),
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.margin = margin(t = 5, b = 25),
    plot.title = element_text(size = 16, hjust = 0, margin = margin(b = 2)),
    plot.title.position = "panel",
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 12), # making sure legend text small enough to fit
    legend.key.width = unit(.5, "cm") # same for size of legend tiles
  )
)


n_annotation <- function(total_n) {
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", total_n),
    hjust = 1.05,
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  )
}

# Reusable function code
make_stacked_row_plot <- function(
  data,
  category,
  title,
  colours,
  width = 0.8,
  reverse_legend = TRUE,
  minimum_label = 2,
  legend_position = "bottom"
) {
  category <- enquo(category)

  plot_data <- data %>%
    count(!!category, name = "n") %>%
    mutate(
      pct = 100 * n / sum(n),
      # Setting the fill & background color to be constrasting
      fill_col = unname(colours[as.character(!!category)]),
      text_color = get_text_colour(fill_col),
      # Only have % showing if it's a large enough # (w/ enough space)
      label = case_when(
        pct < minimum_label ~ "",
        pct >= 6 ~ paste0(round(pct), "%"),
        TRUE ~ as.character(round(pct))
      )
    )

  # Get the total # of included articles/studies for the graph annotation
  total_n <- nrow(data)

  ggplot(
    plot_data,
    aes(
      x = n,
      y = "",
      fill = !!category
    )
  ) +
    geom_col(
      color = "black",
      width = width
    ) +
    geom_text(
      aes(
        label = label,
        color = text_color,
        group = !!category
      ),
      position = position_stack(vjust = 0.5),
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_colour_identity() +
    scale_fill_manual(
      values = colours,
      breaks = intersect(
        levels(plot_data %>% pull(!!category)),
        as.character(unique(plot_data %>% pull(!!category)))
      )
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = title
    ) +
    guides(
      fill = guide_legend(reverse = reverse_legend, nrow = 1)
    ) +
    n_annotation(total_n) +
    coord_cartesian(clip = "off") +
    gg_row
}

# Function for repeated classifying of statistical model
classify_model_category <- function(x) {
  purrr::map_chr(x, \(model) {
    if (is.na(model)) {
      return(NA_character_)
    }

    categories <- character()

    if (
      any(str_detect(
        model,
        fixed(c(
          "AR model",
          "Autocorrelation",
          "Time-varying AR model",
          "Continuous-time AR model",
          "AR model with other main parameter (e.g., ARX)"
        ))
      ))
    ) {
      categories <- c(categories, "AR-based models")
    }

    if (
      str_detect(
        model,
        fixed("Damped linear oscillator model")
      )
    ) {
      categories <- c(categories, "Dynamical systems models")
    }

    if (
      any(str_detect(
        model,
        fixed(c(
          "Comparison across models of different exponents",
          "Visual inspection",
          "Detrended fluctuation analysis"
        ))
      ))
    ) {
      categories <- c(
        categories,
        "Descriptive time series analysis"
      )
    }

    if (
      str_detect(
        model,
        fixed("Survival analysis")
      )
    ) {
      categories <- c(categories, "Survival analysis")
    }

    if (length(categories) == 0) {
      return(NA_character_)
    }

    if (length(categories) > 1) {
      return("Multiple types")
    }

    categories
  })
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Importing Data ----------------------------------------------------------

# Loading data preprocessed in prior wrangling script
ScopingReview_clean <- readr::read_rds(here::here(
  "data",
  "processed",
  "ScopingReview_forR.RDS"
))

# Getting data ready for figures (factors in correct order for legends, shorter terms, etc)
ScopingReview_clean <- ScopingReview_clean %>%
  # Making column for later graphs for Model: AR-based vs not (counting studies with both as 'other' for this...)
  mutate(
    ModelCategory = classify_model_category(StatisticalModelFamily),
    AR_model = if_else(
      ModelCategory == "AR-based models",
      "AR-based models",
      "Other"
    ),
    # Specifying the factor levels so that the graphs haved the desired order of variables
    RemovedDataPoints_CarelessResponding_YN = factor(
      RemovedDataPoints_CarelessResponding_YN,
      levels = c("No", "Remove data points but unclear at which stage", "Yes")
    ),
    ModelComparison = factor(
      ModelComparison,
      levels = c("No", "Yes")
    ),
    ClearDefinition = factor(
      ClearDefinition,
      levels = c("No", "Unclear", "Yes")
    ),
    OperationalizeRecovery_YN = factor(
      OperationalizeRecovery_YN,
      levels = c("No", "Yes")
    ),
    AverageTimeInterval_Reported = factor(
      AverageTimeInterval_Reported,
      levels = c("No", "Yes (assumed or calculated)", "Yes")
    ),
    WithinPersonChange_YN = factor(
      WithinPersonChange_YN,
      levels = c(
        "Yes",
        "No"
      )
    ),
    WithinPersonChange_YN = factor(
      WithinPersonChange_YN,
      levels = c(
        "Yes",
        "No"
      )
    ),
    Preregistration = factor(
      Preregistration,
      levels = c(
        "No",
        "Yes (unclear)",
        "Yes (before analysis)",
        "Yes (before data access)",
        "Yes (before data collection)",
        "Yes"
      )
    ),
    DataShared = factor(
      DataShared,
      levels = c(
        "No",
        "Upon request",
        "Yes (no working link)",
        "Yes"
      )
    ),
    CodeShared = factor(CodeShared, levels = c("No", "Yes")),
    # Adding columns used later on in figures
    DisplayTerm = case_when(
      Term_Inertia == "Yes" ~ "Inertia",
      TRUE ~ as.character(Term_Other)
    ),
    DisplayTerm = factor(
      DisplayTerm,
      levels = c(
        "Damping",
        "Decay",
        "Stability",
        "Recovery",
        "Autocorrelation/AR",
        "Inertia"
      )
    ),
    SamplingDesign_Type_perarticle = case_when(
      SamplingDesign_Type == "Event-based sampling design" ~ "Event-based",
      SamplingDesign_Type == "Fixed sampling design" ~ "Fixed",
      SamplingDesign_Type == "Random sampling design" ~ "Random",
      SamplingDesign_Type == "Semi-random sampling design" ~ "Semi-random",
      is.na(SamplingDesign_Type) ~ NA,
      .default = "Multiple types" # All other options have 2 types of sampling design
    ),
    SamplingDesign_Type_perarticle = factor(
      SamplingDesign_Type_perarticle,
      levels = c(
        "Multiple types",
        "Event-based",
        "Random",
        "Semi-random",
        "Fixed"
      )
    ),
    AverageNAnsweredESM_Reported = factor(
      AverageNAnsweredESM_Reported,
      levels = c(
        "No",
        "Yes (assumed or calculated)",
        "Yes"
      )
    ),
    Definition_FirstAppears = factor(
      Definition_FirstAppears,
      levels = c(
        "NA",
        "Results",
        "Methods",
        "Introduction"
      )
    ),
    OperationalizeRecovery_How = case_when(
      OperationalizeRecovery_How ==
        "Statistically (e.g., discuss speed of return or specific statistical terms such as mean);Conceptually" ~ "Both",
      OperationalizeRecovery_How ==
        "Statistically (e.g., discuss speed of return or specific statistical terms such as mean)" ~ "Statistical",
      OperationalizeRecovery_How ==
        "Conceptually;Statistically (e.g., discuss speed of return or specific statistical terms such as mean)" ~ "Both",
      OperationalizeRecovery_How == "Conceptually" ~ "Conceptual",
      is.na(OperationalizeRecovery_How) ~ "N/A",
      .default = OperationalizeRecovery_How
    ),
    OperationalizeRecovery_How = factor(
      OperationalizeRecovery_How,
      levels = c(
        "N/A",
        "Conceptual",
        "Statistical",
        "Both"
      )
    )
  )


# Creating a dataframe with just unique articles (without extra studies)
ScopingReview_unique <- ScopingReview_clean %>%
  # need to remove the last few characters to separate out if [Study 1] vs [Study 2]
  distinct(ArticleID, .keep_all = TRUE) %>%
  select(-StudyID)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Colors  ----------------------------------------------------------

# Palettes
palette_yn <- RColorBrewer::brewer.pal(11, "PRGn")
palette_inert <- RColorBrewer::brewer.pal(11, "RdYlBu")
palette_nightfall <- khroma::colour("nightfall")(17)
palette_greys <- RColorBrewer::brewer.pal(9, "Greys")
palette_set3 <- RColorBrewer::brewer.pal(12, "Set3")
palette_muted <- khroma::colour("muted")(9) # missing: "#DDDDDD"
palette_sunset <- khroma::colour("sunset")(11) # missing: "#FFFFFF"
palette_light <- khroma::colour("light")(9) # missing: "#FFFFFF"
palette_rainbow <- khroma::colour("discreterainbow")(23)

# Colors for yes/no/gradations
col_yes <- palette_yn[10]
col_yes_2 <- palette_yn[9]
col_yes_3 <- palette_yn[8]
col_yes_4 <- palette_yn[7]

col_no <- palette_greys[5]

col_na <- palette_yn[6] # white

# Colors for inertia/definition graphs
col_inertia <- "#004488"

# Colors for AR-based
col_AR <- palette_muted[6]

# Setting specific colors (for all 'yes/no' graphs)
cols_os <- c(
  # Yes responses
  "Yes" = palette_yn[10],
  "Yes (before data collection)" = col_yes,
  "Yes (before data access)" = col_yes_2,
  "Yes (before analysis)" = col_yes_3,
  "Yes (unclear)" = col_yes_4,
  "Yes (no working link)" = col_yes_3,
  "Upon request" = col_yes_4,
  "Yes (assumed or calculated)" = col_yes_3,
  "Remove data points but unclear at which stage" = col_yes_4,

  # Unclear
  "Unclear" = "#DDDDDD",
  "Unclear but assumed" = "#DDDDDD",
  "Not mentioned" = "#DDDDDD",

  # Negative responses
  "Not available" = palette_yn[2],
  "No" = col_no,

  # Missing
  "N/A" = col_na
)

cols_term <- c(
  "Inertia" = col_inertia,
  "Autocorrelation/AR" = col_AR,
  "Recovery" = palette_inert[2],
  "Stability" = palette_inert[3],
  "Decay" = palette_inert[4],
  "Damping" = palette_inert[5]
)


# Setting specific colors
cols_component <- c(
  "AR-related" = col_AR,
  "Verbal" = palette_set3[4],
  "Context-related" = palette_set3[6],
  "Stable" = palette_set3[12],
  "Interpreted" = palette_set3[11],
  "Other" = palette_greys[5]
)

cols_construct <- c(
  "Emotions" = palette_nightfall[13],
  "ER" = palette_nightfall[11],
  "Other" = palette_nightfall[2]
)

cols_statsmodels <- c(
  "AR-based models" = col_AR,
  "Survival analysis" = palette_muted[8],
  "Descriptive time series analysis" = palette_muted[7],
  "Dynamical systems models" = palette_muted[3],
  "Multiple types" = palette_rainbow[23]
)

cols_statsmodels_gen <- c(
  "AR-based models" = col_AR,
  "AR-based" = col_AR,
  "Other" = palette_muted[5],
  "Unknown" = col_no
)

cols_statsmodel_detailed <- c(
  "AR model" = palette_muted[6],
  "AR model with other main parameter (e.g., ARX)" = palette_muted[9],
  "Autocorrelation" = palette_muted[1],
  "Continuous-time AR model" = palette_sunset[9],
  "Time-varying AR model" = palette_sunset[8], #
  "Damped linear oscillator model" = palette_muted[3],
  "Detrended fluctuation analysis" = palette_muted[5],
  "Survival analysis" = palette_muted[8],
  "Visual inspection" = palette_muted[7],
  # Only for context model (includes stats model details for within-person change articles)
  "AR model with time-varying interaction term" = palette_muted[3],
  # Only for estimation figure
  "Comparison across models of different exponents"
)

cols_centering <- c(
  "Person-mean" = col_yes,
  "Standardized" = col_yes_2,
  "Also group/grand-mean" = col_yes_4,
  "None" = col_no
)

cols_moderator <- c(
  "Model without moderator" = palette_light[1],
  "Via both" = palette_light[6],
  "Moderator (1 step)" = palette_light[7],
  "Moderator (2 steps)" = palette_light[8],
  # Below for recovery how figure
  "Both" = palette_light[6],
  "Statistical" = palette_light[8],
  "Conceptual" = palette_light[1],
  "N/A" = col_no
)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Descriptives ------------------------------------------------------------

## Table 2: Summary statistics -----------------------------------------------
summary_table <- ScopingReview_clean %>%
  select(
    NParticipants,
    AverageNAnsweredESM,
    StudyDuration_Days,
    ESMPrompts_perDay,
    AverageTimeInterval_Hours
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    N = sum(!is.na(Value)),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2)),
    Range = paste0(Min, " - ", Max)
  ) %>%
  select(Variable, Mean, Median, SD, Range, N)

summary_table

#Exported for manuscript:
#write_csv(summary_table, here::here("data", "processed","descriptiveTableForManuscript.csv"))

## Descriptive figures (main manuscript) -------------------------------------------
### 4A: Sampling design: CLEAR -----------------------------------------------------

p_sampling_clear <- make_stacked_row_plot(
  data = ScopingReview_clean,
  category = SamplingDesign_Clear,
  title = "A. Clear reporting of sampling design",
  colours = cols_os
)

### 4B: Sampling design: TYPE  -----------------------------------------------------

sampling_long <- ScopingReview_clean %>%
  select(StudyID, SamplingDesign_Type) %>%
  separate_rows(SamplingDesign_Type, sep = ";") %>%
  filter(!is.na(SamplingDesign_Type)) %>%
  mutate(
    SamplingDesign_Type = recode(
      SamplingDesign_Type,
      "Event-based sampling design" = "Event-based",
      "Fixed sampling design" = "Fixed",
      "Random sampling design" = "Random",
      "Semi-random sampling design" = "Semi-random"
    ),
    SamplingDesign_Type = factor(
      SamplingDesign_Type,
      levels = c("Other", "Event-based", "Random", "Semi-random", "Fixed")
    )
  )


p_sampling_perarticle <- ggplot(
  ScopingReview_clean %>% filter(!is.na(SamplingDesign_Type_perarticle)),
  aes(y = SamplingDesign_Type_perarticle)
) +
  geom_bar() +
  labs(title = "B. Sampling design", y = NULL, x = NULL) +
  gglayer_theme +
  n_annotation(nrow(
    ScopingReview_clean %>% filter(!is.na(SamplingDesign_Type_perarticle))
  )) +
  theme(plot.title.position = "plot") +
  coord_cartesian(clip = "off")


# (Version for supplementary materials that doesn't collapse by article)
p_sampling_bydesign <- ggplot(sampling_long, aes(y = SamplingDesign_Type)) +
  geom_bar() +
  labs(title = "Sampling design (all)", x = "Number of articles", y = NULL) +
  gglayer_theme +
  n_annotation(nrow(sampling_long)) +
  coord_cartesian(clip = "off")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### 4C-E: Open Science -------------------------------------------------------------

p_prereg <- make_stacked_row_plot(
  data = ScopingReview_clean,
  category = Preregistration,
  title = "C. Preregistration",
  colours = cols_os
)

p_datasharing <- make_stacked_row_plot(
  data = ScopingReview_clean,
  category = DataShared,
  title = "D. Data sharing",
  colours = cols_os
)

p_codesharing <- make_stacked_row_plot(
  data = ScopingReview_clean,
  category = CodeShared,
  title = "E. Code sharing",
  colours = cols_os
)

### Supplmentary descriptive figures -----------------------------------------------

### Items --------------------------------------------------------------------------
items_long <- ScopingReview_clean %>%
  select(StudyID, Scores_Calculated_How) %>%
  separate_rows(Scores_Calculated_How, sep = ";") %>%
  filter(!is.na(Scores_Calculated_How)) %>%
  mutate(
    Scores_Calculated_How = recode(
      Scores_Calculated_How,
      "Aggregate several items per timepoint" = "Sum/average several items",
      "Pooling items (not sum or average)" = "Pool via other method"
    ),
    Scores_Calculated_How = factor(
      Scores_Calculated_How,
      levels = c(
        "Do not specify",
        "Not clear",
        "Single item",
        "Pool via other method",
        "Sum/average several items"
      )
    )
  )

p_items <- ggplot(items_long, aes(y = Scores_Calculated_How)) +
  geom_bar() +
  labs(
    title = "",
    y = NULL,
    x = "Number of studies"
  ) +
  gglayer_theme +
  n_annotation(nrow(items_long)) +
  coord_cartesian(clip = "off")


### Removed data points ------------------------------------------------------------

p_removeddata_yn <- make_stacked_row_plot(
  data = ScopingReview_clean,
  category = RemovedDataPoints_CarelessResponding_YN,
  title = "A. Removed datapoints",
  colours = cols_os
)

removeddatapoints_long <- ScopingReview_clean %>%
  select(ArticleID, RemovedDataPoints_Why) %>%
  separate_rows(RemovedDataPoints_Why, sep = ";") %>%
  filter(!is.na(RemovedDataPoints_Why)) %>%
  mutate(
    RemovedDataPoints_Why = recode(
      # typos when categorizing
      RemovedDataPoints_Why,
      "Response time short" = "Response time too short",
      "Response time long" = "Response time too long"
    ),
    RemovedDataPoints_Why = factor(
      RemovedDataPoints_Why,
      levels = c(
        "Not specified",
        "Invalid responses (no further info)",
        "Technical issues",
        "Attention check",
        "Response time (not specified)",
        "Response time too short",
        "Response time too long",
        "Sum/average several items"
      )
    )
  )

p_removeddatapoints_why <- ggplot(
  removeddatapoints_long,
  aes(y = RemovedDataPoints_Why)
) +
  geom_bar() +
  labs(title = "B. Justification for removing datapoints", y = NULL, x = NULL) +
  gglayer_theme +
  n_annotation(nrow(removeddatapoints_long)) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")


### Model comparison ---------------------------------------------------------------

p_modelcomp <- make_stacked_row_plot(
  data = ScopingReview_clean,
  category = ModelComparison,
  title = "Model comparison",
  colours = cols_os
)

### Average number of answered ESM beeps -------------------------------------------

p_averageN_rep <- make_stacked_row_plot(
  data = ScopingReview_clean,
  category = AverageNAnsweredESM_Reported,
  title = "Average answered beeps reported",
  colours = cols_os
)


### Map figure ----------------------------------------------------------

country_counts <- ScopingReview_unique %>%
  count(SourceCountry)

world <- map_data("world")

map_data_joined <- world %>%
  left_join(country_counts, by = c("region" = "SourceCountry"))

# Check all countries in correct format:
ScopingReview_unique$Reference[which(
  ScopingReview_unique$SourceCountry %in% unique(world$region) == FALSE
)]
# Lo et al. -> Hong Kong

# Hong Kong not included in maps package regions so adding it manually:
hong_kong <- data.frame(
  lon = 114.1694,
  lat = 22.3193,
  n = country_counts$n[country_counts$SourceCountry == "Hong Kong"]
)


p_map <- ggplot(
  map_data_joined,
  aes(x = long, y = lat, group = group, fill = n)
) +
  geom_polygon(color = "grey50", linewidth = 0.05) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  geom_point(
    data = hong_kong,
    aes(x = lon, y = lat, fill = n),
    shape = 21,
    size = 2,
    inherit.aes = FALSE
  ) +
  coord_quickmap() +
  theme_void() +
  labs(fill = "Number of articles")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5) DEFINING INERTIA  -------------------------------------------------------

## Figure 5: Constructs Figure ----------------------------------------------------------

### 5A: Specific constructs, by term inertia --------------------------------------------
construct_long <- ScopingReview_unique %>%
  select(
    ArticleID,
    Term_Inertia,
    Constructs_Coarse,
    Year,
    AverageTimeInterval_Hours,
    ModelCategory
  ) %>%
  separate_rows(
    Constructs_Coarse,
    sep = ";"
  ) %>%
  mutate(
    Constructs_Coarse = str_trim(
      Constructs_Coarse
    ),
    Constructs_fill = case_when(
      Constructs_Coarse == "Affect" ~ "Emotions",
      Constructs_Coarse == "Discrete emotions" ~ "Emotions",
      Constructs_Coarse == "ER (intrapersonal)" ~ "ER",
      Constructs_Coarse == "Physical states" ~ "Other",
      Constructs_Coarse == "Psychological symptoms" ~ "Other",
      Constructs_Coarse == "Cognitive states" ~ "Other",
      Constructs_Coarse == "Well-being" ~ "Other",
      Constructs_Coarse == "Interpersonal constructs" ~ "Other",
      .default = Constructs_Coarse
    ),
    Constructs_fill = factor(
      Constructs_fill,
      levels = c("Other", "ER", "Emotions")
    ),
    Constructs_Coarse = factor(
      Constructs_Coarse,
      levels = c(
        "Other",
        "Interpersonal constructs",
        "Physical states",
        "Cognitive states",
        "Psychological symptoms",
        "Well-being",
        "ER (intrapersonal)",
        "Discrete emotions",
        "Affect"
      )
    )
  )


p_constructs <-
  ggplot(
    construct_long,
    aes(
      y = Constructs_Coarse,
      alpha = Term_Inertia,
      fill = Constructs_fill
    )
  ) +
  geom_bar() +
  labs(
    x = "Number of articles",
    y = NULL,
    alpha = "Used term inertia?",
    fill = "Construct category",
    title = "A. Types of constructs"
  ) +
  scale_fill_manual(
    values = c(cols_construct)
  ) +
  scale_alpha_discrete(range = c(.6, 1)) +
  guides(
    alpha = guide_legend(reverse = T)
  ) +
  gglayer_theme +
  n_annotation(nrow(construct_long)) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.margin = margin(b = 0)
  )

# Getting common legend from above plot & p_constructs_per_year
p_constructs_commonlegend <- cowplot::get_legend(
  p_constructs +
    guides(
      fill = guide_legend(nrow = 1, reverse = T),
      alpha = "none"
    )
)

# Removing common legend from p_constructs bc will add it separately
p_constructs <- p_constructs + guides(fill = "none")

### 5A: Constructs per year -------------------------------------------------------------
# Constructs & articles by year
articles_per_year <- construct_long %>%
  distinct(ArticleID, Year) %>%
  count(Year, name = "n_articles")

p_constructs_peryear <- ggplot(
  construct_long,
  aes(
    x = Year,
    fill = Constructs_fill
  )
) +
  geom_bar() +
  geom_line(
    data = articles_per_year,
    aes(x = Year, y = n_articles, group = 1, color = "Number of articles"),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_point(
    data = articles_per_year,
    aes(x = Year, y = n_articles, color = "Number of articles"),
    inherit.aes = F,
    size = 3
  ) +
  scale_fill_manual(
    values = c(cols_construct)
  ) +
  scale_color_manual(
    values = c("Number of articles" = "black"),
    name = NULL
  ) +
  labs(
    x = "Year",
    y = "Number of constructs",
    title = "B. Constructs per year"
  ) +
  gglayer_theme +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    breaks = seq(0, 100, by = 10)
  ) +
  guides(fill = "none") +
  n_annotation(nrow(construct_long)) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.margin = margin(b = 0, l = 30)
  )


## Figure 6: Inertia term & definitions --------------------------------------
### 6A: Term inertia use ---------------------------------------------------------

p_term <- make_stacked_row_plot(
  data = ScopingReview_unique,
  category = DisplayTerm,
  title = "A. Used term inertia",
  colours = cols_term
)

p_term_peryear <- ggplot(
  ScopingReview_unique,
  aes(
    x = Year,
    fill = Term_Inertia
  )
) +
  geom_bar() +
  scale_fill_manual(
    values = c(cols_os)
  ) +
  labs(
    x = "Year",
    y = "Number of articles",
    fill = "Used term inertia"
  ) +
  gglayer_theme +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    breaks = seq(0, 100, by = 10)
  ) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  n_annotation(nrow(ScopingReview_unique)) +
  coord_cartesian(clip = "off")

### 6B: Clear definition -----------------------------------------------------------

p_clear <- make_stacked_row_plot(
  data = ScopingReview_unique,
  category = ClearDefinition,
  title = "B. Clear definition",
  colours = cols_os
)


### 6C: Definition components -----------------------------------------------------------

# DEFINITION COMPONENTS GRAPH
# First combining in a relevant way the check-box responses with the write-in ones

# Reuse this list a few times so defining first
component_context_vars <- c(
  "ArticleID",
  "Term_Inertia",
  "StatisticalModelFamily",
  "AR_model",
  "WithinPersonChange_YN",
  "WithinPersonChange_StatisticalModel",
  "WithinPersonChange_HowModeled",
  "WithinPersonChange_Correlate"
)

components_long <-
  ScopingReview_unique %>%
  select(
    all_of(component_context_vars),
    Definition_ComponentsUnique,
    Definition_ComponentsUnique_other_catnew
  ) %>%
  separate_rows(
    Definition_ComponentsUnique,
    sep = ";"
  ) %>%
  mutate(
    Definition_ComponentsUnique = str_trim(Definition_ComponentsUnique)
  )

other_rows <-
  ScopingReview_unique %>%
  filter(
    str_detect(
      Definition_ComponentsUnique,
      "Other"
    )
  ) %>%
  select(
    all_of(component_context_vars),
    Definition_ComponentsUnique_other_catnew
  ) %>%
  separate_rows(
    Definition_ComponentsUnique_other_catnew,
    sep = ";"
  ) %>%
  transmute(
    across(all_of(component_context_vars)),
    component = str_trim(
      Definition_ComponentsUnique_other_catnew
    )
  )


# Removing 'other' entries from checkbox list
components_nonother <-
  components_long %>%
  filter(
    Definition_ComponentsUnique != "Other"
  ) %>%
  select(
    all_of(component_context_vars),
    ArticleID,
    Term_Inertia,
    component = Definition_ComponentsUnique
  )

# Combining
components_all <-
  bind_rows(
    components_nonother,
    other_rows
  ) %>%
  mutate(ModelCategory = classify_model_category(StatisticalModelFamily))

# Looking at all components
components_all %>%
  distinct(ArticleID, component) %>%
  count(component) %>%
  print(n = 100)

components_all <-
  components_all %>%
  filter(!is.na(component)) %>%
  mutate(
    # Making new column for main figure (keeping trait/state for supp figure)
    component_mainfig = case_when(
      component == "Trait / person-level characteristic" ~ "Other",
      component == "Within-person change (state-characteristic)" ~ "Other",
      .default = component
    ),
    component_cat = case_when(
      component %in%
        c(
          "Autocorrelation/AR model",
          "Self-predicting",
          "Temporal dependency",
          "Slower recovery / return to baseline",
          "Carry over",
          "Rate of change"
        ) ~ "AR-related",

      component %in%
        c(
          "Lingering",
          "Persisting",
          "“Sticky” or stuck",
          "Resistance to change",
          "Perseverate",
          "Spillover"
        ) ~ "Verbal",

      component %in%
        c(
          "Persist despite context",
          "Slow recovering after context",
          "Across contexts"
        ) ~ "Context-related",

      component %in%
        c(
          "Rigidity",
          "Stability",
          # "Consistency",
          "Little fluctuation"
        ) ~ "Stable",

      component %in%
        c(
          "Inflexibility",
          "Emotion regulation",
          "Maladaptive",
          "Psychological functioning"
        ) ~ "Interpreted",

      component %in%
        c(
          "Trait / person-level characteristic",
          "Within-person change (state-characteristic)",
          "Other"
        ) ~ "Other",

      .default = component
    ),

    component_cat_time = as.factor(case_when(
      component == "Trait / person-level characteristic" ~ "Trait",
      component ==
        "Within-person change (state-characteristic)" ~ "Within-person change",
      .default = as.character(component_cat)
    )),
    # Ordering for legend in graph
    component_cat = fct_relevel(
      component_cat,
      c(
        #"Term: Not inertia",
        "AR-related",
        "Verbal",
        "Context-related",
        "Stable",
        "Interpreted",
        "Other"
      )
    )
  )

# Setting order
component_order <- c(
  "Autocorrelation/AR model",
  "Self-predicting",
  "Temporal dependency",
  "Slower recovery / return to baseline",
  "Carry over",
  "Rate of change",

  "Lingering",
  "Persisting",
  "“Sticky” or stuck",
  "Resistance to change",
  "Perseverate",
  "Spillover",

  "Across contexts",
  "Persist despite context",
  "Slow recovering after context",

  "Stability",
  "Rigidity",
  # "Consistency",
  "Little fluctuation",

  "Inflexibility",
  "Emotion regulation",
  "Maladaptive",
  "Psychological functioning",

  "Trait / person-level characteristic",
  "Within-person change (state-characteristic)",
  "Other"
)

components_all <-
  components_all %>%
  mutate(
    component = factor(
      component,
      levels = rev(component_order)
    ),
    component_mainfig = factor(
      component_mainfig,
      levels = rev(component_order[c(1:22, 25)]) # not including trait/state
    )
  )

### 6C: Making figure (definition components) -------------------------------------------

component_alpha_plot <-
  components_all %>%
  distinct(
    ArticleID,
    component_mainfig,
    Term_Inertia,
    component_cat
  )

p_alpha_components <-
  ggplot(
    component_alpha_plot,
    aes(
      # x = n,
      y = component_mainfig,
      fill = component_cat,
      alpha = Term_Inertia
    )
  ) +
  geom_bar() +
  scale_fill_manual(
    values = c(cols_component)
  ) +
  scale_alpha_discrete(range = c(0.5, 1), name = "Used term inertia?") +
  labs(
    x = "Number of articles",
    y = NULL,
    fill = "Definition categories",
    title = "C. Definition components"
  ) +
  guides(alpha = guide_legend(reverse = T)) +
  gglayer_theme +
  n_annotation(nrow(component_alpha_plot)) +
  coord_cartesian(clip = "off")

## Supplementary figures (Definition-related)--------------------------------------------
### Supp: Constructs (detailed) by operationalization  ---------------------------------------------
p_constructs_operationalization <-
  ggplot(
    construct_long,
    aes(
      y = Constructs_Coarse,
      fill = ModelCategory
    )
  ) +
  geom_bar() +
  labs(
    x = "Number of articles",
    y = NULL,
    fill = "Operationalization",
    title = ""
  ) +
  scale_fill_manual(
    values = c(cols_statsmodels)
  ) +
  guides(
    fill = guide_legend(reverse = T)
  ) +
  gglayer_theme +
  n_annotation(nrow(construct_long)) +
  coord_cartesian(clip = "off")

### Supp: Topic Figure ----------------------------------------------------------

topic_long <- ScopingReview_unique %>%
  select(
    ArticleID,
    Term_Inertia,
    StudyTopic,
    Year
  ) %>%
  separate_rows(
    StudyTopic,
    sep = ";"
  ) %>%
  mutate(
    StudyTopic = str_trim(
      StudyTopic
    ),
    StudyTopic = factor(
      StudyTopic,
      levels = c(
        "Other",
        "Situations/places",
        "Education",
        "Work/employment",
        "General social relationships/interactions",
        "Romantic or marital relationships",
        "Family relationships",
        "Physical health and health behaviour",
        "Personality",
        "Mental health",
        "Emotions"
      )
    )
  )


p_topic <-
  ggplot(
    topic_long,
    aes(
      y = StudyTopic,
      alpha = Term_Inertia #,
    )
  ) +
  geom_bar() +
  labs(
    x = "Number of articles",
    y = NULL,
    alpha = "Term inertia",
    title = ""
  ) +
  scale_alpha_discrete(range = c(.6, 1)) +
  gglayer_theme +
  guides(alpha = guide_legend(reverse = T)) +
  n_annotation(nrow(topic_long)) +
  coord_cartesian(clip = "off")


### Supp: Recovery Figure ----------------------------------------------------------

p_recovery_yn <- make_stacked_row_plot(
  data = ScopingReview_unique,
  category = OperationalizeRecovery_YN,
  title = "A. Recovery conceptualized as part of definition",
  colours = cols_os
)

p_recovery_how <- make_stacked_row_plot(
  data = ScopingReview_unique,
  category = OperationalizeRecovery_How,
  title = "B. Type of definition",
  colours = cols_moderator
)

# Recovery components figure
recovery_long <- ScopingReview_unique %>%
  filter(OperationalizeRecovery_YN == "Yes") %>%
  select(
    ArticleID,
    Term_Inertia,
    Recovery_Cat
  ) %>%
  separate_rows(
    Recovery_Cat,
    sep = ";"
  ) %>%
  mutate(
    Recovery_Cat = str_trim(
      Recovery_Cat
    )
  )

p_recovery_defs <-
  ggplot(
    recovery_long,
    aes(y = Recovery_Cat, alpha = Term_Inertia)
  ) +
  geom_bar(fill = col_yes) +
  scale_alpha_discrete(range = c(.5, 1)) +
  labs(
    x = "Number of articles",
    y = NULL,
    alpha = "Term inertia",
    title = "C. How authors describe what recovery returns to"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  gglayer_theme +
  guides(alpha = guide_legend(reverse = T)) +
  n_annotation(nrow(recovery_long)) +
  coord_cartesian(clip = "off")


### Supp: Time Interval Figure ----------------------------------------------------------

p_timeint_yn <- make_stacked_row_plot(
  data = ScopingReview_clean,
  category = AverageTimeInterval_Reported,
  title = "A. Reporting of average time interval",
  colours = cols_os
)

p_timeint_construct <-
  ggplot(
    construct_long %>% filter(!is.na(AverageTimeInterval_Hours)),
    aes(
      y = Constructs_Coarse,
      x = AverageTimeInterval_Hours,
      fill = Constructs_Coarse,
      color = Constructs_Coarse
    )
  ) +
  geom_boxplot(
    width = .15,
    outlier.shape = NA,
    box.color = "black",
    whisker.color = "black",
    alpha = .5,
    median.color = "black",
    staple.color = "black"
  ) +
  geom_jitter(
    width = .85
  ) +
  theme(plot.title = element_text(size = 20), legend.position = "none") +
  labs(
    y = NULL,
    x = NULL,
    title = "B. Average time interval by construct"
  ) +
  gglayer_theme +
  theme() +
  theme(legend.position = "none") +

  scale_x_continuous(expand = c(0, 0), limits = c(0.01, 25))

### Supp: Definition component first appears
p_def_firstappears <-
  ggplot(
    ScopingReview_unique,
    aes(
      y = Definition_FirstAppears,
      alpha = Term_Inertia
    )
  ) +
  geom_bar() +
  gglayer_theme +
  labs(
    x = "Number of articles",
    y = NULL,
    alpha = "Used term inertia?",
    title = ""
  ) +
  scale_alpha_discrete(range = c(.6, 1)) +
  guides(
    alpha = guide_legend(reverse = T)
  ) +
  n_annotation(nrow(construct_long)) +
  coord_cartesian(clip = "off")


### Supp: Definition component co-occurrences -------------------------------------------

# Order for figure
displayed_order <- component_order[c(1:22, 25)]

# Prepare component data
components_all_car <- components_all %>%
  transmute(
    ArticleID,
    component = component_mainfig %>%
      coalesce("Other") %>%
      as.character() %>%
      stringr::str_squish(),
    component_cat = factor(
      component_cat,
      levels = levels(components_all$component_cat)
    )
  ) %>%
  distinct()

# Component-category information for displayed components
component_category_lookup <- components_all_car %>%
  distinct(component, component_cat) %>%
  filter(component %in% displayed_order) %>%
  mutate(
    component_position = match(component, displayed_order)
  ) %>%
  arrange(component_position)

# One row per article-component combination
components_for_counts <- components_all_car %>%
  distinct(ArticleID, component)

# Co-occurrence counts
comp_cooccurrence <- components_for_counts %>%
  inner_join(
    components_for_counts,
    by = "ArticleID",
    relationship = "many-to-many"
  ) %>%
  filter(component.x < component.y) %>%
  count(
    comp1 = component.x,
    comp2 = component.y,
    name = "n"
  )

# Component prevalence/frequency for the diagonal
component_prevalence <- components_for_counts %>%
  count(
    component,
    name = "n_prevalence"
  )

# Build only one triangle, including the diagonal
heatmap_df <- expand_grid(
  x_component = displayed_order,
  y_component = displayed_order
) %>%
  mutate(
    x_pos = match(x_component, displayed_order),
    y_pos = match(y_component, displayed_order)
  ) %>%
  filter(x_pos <= y_pos) %>%
  mutate(
    comp1 = pmin(x_component, y_component),
    comp2 = pmax(x_component, y_component)
  ) %>%
  left_join(
    comp_cooccurrence,
    by = c("comp1", "comp2")
  ) %>%
  left_join(
    component_prevalence,
    by = c("x_component" = "component")
  ) %>%
  mutate(
    diagonal = x_component == y_component,
    value = if_else(diagonal, n_prevalence, n),
    x_component = factor(
      x_component,
      levels = displayed_order
    ),
    y_component = factor(
      y_component,
      levels = rev(displayed_order)
    )
  )

# Category rectangles
n_components <- length(displayed_order)

category_rectangles <- component_category_lookup %>%
  group_by(component_cat) %>%
  summarise(
    first_position = min(component_position),
    last_position = max(component_position),
    .groups = "drop"
  ) %>%
  mutate(
    offset = seq(0.02, by = 0.02, length.out = n()),
    xmin = first_position - 0.5 + offset,
    xmax = last_position + 0.5 - offset,
    ymin = n_components - last_position + 0.5 + offset,
    ymax = n_components - first_position + 1.5 - offset
  )


# Separate diagonal and off-diagonal cells
heatmap_offdiag <- heatmap_df %>%
  filter(!diagonal)

heatmap_diag <- heatmap_df %>%
  filter(diagonal)

# Figure
defcomp_cooc <- ggplot() +
  # Co-occurrence cells
  geom_tile(
    data = heatmap_offdiag,
    aes(
      x = x_component,
      y = y_component,
      fill = value
    ),
    color = "white",
    linewidth = 0.4
  ) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    na.value = "grey95",
    name = "Co-occurring\ndefinition components"
  ) +
  # White diagonal (overall frequency) cells
  geom_tile(
    data = heatmap_diag,
    aes(
      x = x_component,
      y = y_component
    ),
    fill = "white",
    color = "white",
    linewidth = 0.4
  ) +
  # Component-category outlines
  geom_rect(
    data = category_rectangles,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      color = component_cat
    ),
    inherit.aes = FALSE,
    fill = NA,
    linewidth = 1.5
  ) +
  # Co-occurrence labels
  geom_text(
    data = heatmap_offdiag %>%
      filter(!is.na(value)),
    aes(
      x = x_component,
      y = y_component,
      label = value
    ),
    color = "black",
    size = 4
  ) +
  # Prevalence labels on diagonal
  geom_text(
    data = heatmap_diag,
    aes(
      x = x_component,
      y = y_component,
      label = value
    ),
    color = "black",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(
    values = cols_component,
    limits = levels(components_all$component_cat),
    drop = FALSE,
    name = "Component category"
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  coord_equal() +
  labs(
    x = NULL,
    y = NULL,
    title = "A. Co-occurrence of definition components",
    subtitle = "Diagonal (white) = Overall frequency of that definition component"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1
    ),
    panel.grid = element_blank()
  )


### Supp: Definition component (alone vs with others)
# Number of components in each article
article_component_counts <- components_for_counts %>%
  count(
    ArticleID,
    name = "n_components"
  )

# Total prevalence
component_prevalence <- components_for_counts %>%
  count(
    component,
    name = "prevalence"
  )

# Singleton (how often the def components appear on their own) prevalence
component_singletons <- components_for_counts %>%
  left_join(
    article_component_counts,
    by = "ArticleID"
  ) %>%
  filter(
    n_components == 1
  ) %>%
  count(
    component,
    name = "singleton_count"
  )

# Combine
component_summary <- component_prevalence %>%
  left_join(
    component_singletons,
    by = "component"
  ) %>%
  mutate(
    singleton_count = replace_na(
      singleton_count,
      0
    ),
    singleton_rate = singleton_count /
      prevalence,
    component = factor(
      component,
      levels = rev(displayed_order)
    )
  )

# Plotting
defcomp_occur <-
  ggplot(
    component_summary,
    aes(
      y = component
    )
  ) +
  geom_col(
    aes(
      x = prevalence
    ),
    fill = "grey80",
    width = .8
  ) +
  geom_col(
    aes(
      x = singleton_count
    ),
    fill = "black",
    width = .8
  ) +
  geom_text(
    aes(
      x = prevalence + 1,
      label = prevalence
    ),
    hjust = 0,
    size = 3
  ) +
  labs(
    x = "Number of articles",
    y = NULL,
    title = "B. Definition component overall prevalence",
    subtitle = "Black = Appears alone; Grey = Total prevalence"
  ) +
  gglayer_theme

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6) OPERATIONALIZING INERTIA  -----------------------------------------------

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 7: Statistical Models ----------------------------------------------

statisticalmodels_long <- ScopingReview_clean %>%
  separate_rows(
    StatisticalModelFamily,
    sep = ";"
  ) %>%
  mutate(
    StatisticalModelFamily = str_trim(StatisticalModelFamily),
    ModelCategory = classify_model_category(StatisticalModelFamily)
  ) %>%
  dplyr::filter(!is.na(StatisticalModelFamily)) %>%
  mutate(
    StatisticalModelFamily = factor(
      StatisticalModelFamily,
      levels = c(
        "Comparison across models of different exponents",
        "Visual inspection",
        "Detrended fluctuation analysis",
        "Damped linear oscillator model",
        "Survival analysis",
        "Time-varying AR model",
        "Continuous-time AR model",
        "AR model with other main parameter (e.g., ARX)",
        "AR model",
        "Autocorrelation"
      )
    )
  )

statisticalmodels_plot <- statisticalmodels_long %>%
  distinct(
    ArticleID,
    StatisticalModelFamily,
    ModelCategory,
    Term_Inertia,
    .keep_all = TRUE
  ) %>%
  select(
    ArticleID,
    StatisticalModelFamily,
    ModelCategory,
    Term_Inertia,
    AverageTimeInterval_Hours
  )

p_statsmodels <- ggplot(
  statisticalmodels_plot,
  aes(
    y = StatisticalModelFamily,
    fill = ModelCategory,
    alpha = Term_Inertia
  )
) +
  geom_bar() +
  scale_fill_manual(
    values = c(cols_statsmodels)
  ) +
  scale_alpha_discrete(range = c(0.6, 1)) +
  labs(
    x = "Number of articles",
    fill = "Model category",
    alpha = "Used term inertia?",
    y = NULL
  ) +
  gglayer_theme +
  guides(alpha = guide_legend(reverse = T)) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", nrow(ScopingReview_clean)),
    hjust = 1.05,
    vjust = 0.5,
    size = 4,
    fontface = "bold"
  ) +
  coord_cartesian(clip = "off")


## Supplementary material figures (about operationalizations) ----------------

# (This comes with other time interval figure)
p_timeint_operationalization <-
  ggplot(
    statisticalmodels_plot %>% filter(!is.na(AverageTimeInterval_Hours)),
    aes(
      y = StatisticalModelFamily,
      x = AverageTimeInterval_Hours,
      fill = StatisticalModelFamily,
      color = StatisticalModelFamily
    )
  ) +
  geom_boxplot(
    width = .15,
    outlier.shape = NA,
    box.color = "black",
    whisker.color = "black",
    alpha = .5,
    median.color = "black",
    staple.color = "black"
  ) +
  geom_jitter(
    width = .5
  ) +
  theme(plot.title = element_text(size = 20), legend.position = "none") +
  labs(
    y = NULL,
    x = "Average interval (hours)",
    title = "C. Average time interval by statistical model type"
  ) +
  gglayer_theme +
  theme(
    legend.position = "none"
  ) +
  scale_fill_manual(values = cols_statsmodel_detailed) +
  scale_color_manual(values = cols_statsmodel_detailed) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.01, 25))


### Supp: Overall graph of operationalizations vs  definitions ---------------

# Prepping ata
def_op_plot <-
  components_all %>%
  distinct(
    ArticleID,
    component_cat_time,
    StatisticalModelFamily
  ) %>%
  separate_rows(
    StatisticalModelFamily,
    sep = ";"
  ) %>%
  mutate(
    StatisticalModelFamily = str_trim(StatisticalModelFamily)
  )


p_op_def <-
  ggplot(
    def_op_plot,
    aes(
      y = component_cat_time,
      fill = StatisticalModelFamily
    )
  ) +
  geom_bar() +
  scale_fill_manual(
    values = cols_statsmodel_detailed
  ) +
  labs(
    x = "Number of articles (per Definition Component Type)",
    fill = "Statistical model",
    y = NULL,
    title = ""
  ) +
  gglayer_theme +
  n_annotation(nrow(def_op_plot)) +
  coord_cartesian(clip = "off")


### Supp: Coherence between context defs vs operationalizations --------------
context_df <- components_all %>%
  filter(component_cat == "Context-related") %>%
  select(
    ArticleID,
    component,
    StatisticalModelFamily,
    WithinPersonChange_StatisticalModel,
    WithinPersonChange_HowModeled,
    WithinPersonChange_Correlate,
    WithinPersonChange_YN
  ) %>%
  mutate(
    ModelContext = case_when(
      !is.na(WithinPersonChange_StatisticalModel) ~
        WithinPersonChange_StatisticalModel,
      is.na(WithinPersonChange_StatisticalModel) ~ StatisticalModelFamily
    )
  )

p_context <-
  ggplot(
    context_df,
    aes(
      y = component,
      fill = ModelContext,
      pattern = WithinPersonChange_YN
    )
  ) +
  ggpattern::geom_bar_pattern(
    color = NA,
    pattern_fill = "black",
    pattern_colour = "black"
  ) +
  ggpattern::scale_pattern_manual(
    values = c(
      "No" = "none",
      "Yes" = "stripe"
    ),
    name = "Used term inertia?"
  ) +
  scale_fill_manual(values = cols_statsmodel_detailed) +
  labs(
    x = "Number of articles",
    y = NULL,
    fill = "Statistical model"
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(pattern = "none")),
    pattern = guide_legend(order = 2, override.aes = list(fill = "gray70"))
  ) +
  gglayer_theme


### Supp: Coherence between state vs trait defs & operationalizations --------
winchangedef_df <- components_all %>%
  filter(
    component_cat_time == "Trait" | component_cat_time == "Within-person change"
  ) %>%
  select(
    ArticleID,
    component,
    StatisticalModelFamily,
    WithinPersonChange_StatisticalModel,
    WithinPersonChange_YN
  )

p_winchangedef <-
  ggplot(
    winchangedef_df,
    aes(
      y = component,
      fill = WithinPersonChange_YN
    )
  ) +
  geom_bar() +
  scale_fill_manual(values = cols_os) +
  labs(
    x = "Number of articles",
    y = NULL,
    fill = "Within-person change assessed",
    title = ""
  ) +
  gglayer_theme +
  guides(fill = guide_legend(reverse = T))


### Supp: Individual diffs ---------------------------------------------------

# Getting data ready
model_long_inddiffs <- ScopingReview_clean %>%
  separate_rows(
    IndividualDifferencesHandled,
    sep = ";"
  ) %>%
  mutate(
    IndividualDifferencesHandled = str_trim(
      IndividualDifferencesHandled
    )
  ) %>%
  filter(
    !is.na(
      IndividualDifferencesHandled
    )
  ) %>%
  distinct(
    Title,
    IndividualDifferencesHandled,
    .keep_all = TRUE
  ) %>%
  # Getting names of methods in order I want for figure
  mutate(
    IndividualDifferencesHandled = factor(
      IndividualDifferencesHandled,
      levels = c(
        "Do not specify",
        "Not fully clear",
        "Single case analysis",
        "Individual then cluster",
        "Individual then group-level analysis",
        "Multilevel model"
      )
    )
  )


p_inddiffs_byar <- model_long_inddiffs %>%
  ggplot(
    aes(
      y = IndividualDifferencesHandled,
      fill = AR_model
    )
  ) +
  geom_bar() +
  labs(
    title = "A. Individual differences: Handling",
    x = NULL,
    y = NULL,
    fill = "Model type"
  ) +
  scale_fill_manual(
    values = cols_statsmodels_gen
  ) +
  gglayer_theme +
  n_annotation(nrow(model_long_inddiffs)) +
  coord_cartesian(clip = "off")

### Supp: Centering ----------------------------------------------------------

centering_plot <- ScopingReview_clean %>%
  mutate(
    CenterPredictors = factor(
      CenterPredictors,
      levels = c(
        "No",
        "Not clearly mentioned",
        "N/A (no multilevel techniques)",
        "No (latent centering)",
        "Yes"
      )
    ),
    # Making new variable for type of centering (person specific vs other)
    Center_Person = case_when(
      str_detect(
        CenterPredictors_Categorized,
        regex("Person", ignore_case = TRUE)
      ) ~
        "Person-mean",
      str_detect(
        CenterPredictors_Categorized,
        regex("Standardized", ignore_case = TRUE)
      ) ~ "Standardized",
      str_detect(
        CenterPredictors_Categorized,
        regex("Group|Grand", ignore_case = TRUE)
      ) ~ "Also group/grand-mean",
      is.na(CenterPredictors_Categorized) ~ "None"
    )
  ) %>%
  mutate(
    Center_Person = factor(
      Center_Person,
      levels = c(
        "None",
        "Also group/grand-mean",
        "Standardized",
        "Person-mean"
      )
    )
  )


p_center <- ggplot(
  centering_plot,
  aes(y = CenterPredictors, fill = Center_Person)
) +
  geom_bar() +
  labs(
    y = NULL,
    x = NULL,
    title = "B. Centering performed",
    fill = "Centering method"
  ) +
  # Setting order for legend
  scale_fill_manual(
    values = cols_centering,
    breaks = c('Person-mean', 'Standardized', 'Also group/grand-mean', 'None')
  ) +
  theme(plot.margin = margin(t = 20)) +
  coord_cartesian(xlim = c(0, 75)) +
  gglayer_theme +
  n_annotation(nrow(centering_plot)) +
  coord_cartesian(clip = "off")


### Supp: Estimation technique -----------------------------------------------

estimation_long <-
  ScopingReview_clean %>%
  select(
    StudyID,
    IndividualDifferencesHandled,
    EstimationTechnique,
    StatisticalModelFamily
  ) %>%
  separate_rows(
    EstimationTechnique,
    sep = ";"
  ) %>%
  mutate(
    ModelCategory = classify_model_category(StatisticalModelFamily),
    EstimationTechnique = factor(
      EstimationTechnique,
      levels = c(
        "Not clearly reported",
        "N/A",
        "N/A (Autocorrelation)",
        "Nonparametric smooth functions (GAM)",
        "Maximum likelihood",
        "Bayesian estimation"
      )
    )
  )

p_estimation <- ggplot(
  estimation_long,
  aes(y = EstimationTechnique, fill = ModelCategory)
) +
  geom_bar() +
  gglayer_theme +
  labs(
    y = NULL,
    x = "Number of studies",
    title = "C. Estimation method",
    fill = "Model category"
  ) +
  scale_fill_manual(values = cols_statsmodels) +
  coord_cartesian(xlim = c(0, 75)) +
  n_annotation(nrow(estimation_long)) +
  coord_cartesian(clip = "off")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7) WITHIN-PERSON CHANGE - INERTIA  -----------------------------------------------

## 8A: Fig about assessing w/in person change --------------------------------------

# Creating a dataframe for the labels (just based on WithinPersonChange_YN, not Term_Inertia)
winchange_labels <- ScopingReview_clean %>%
  count(WithinPersonChange_YN, name = "n") %>%
  mutate(
    pct = 100 * n / sum(n),
    fill_col = cols_os[as.character(WithinPersonChange_YN)],
    text_color = get_text_colour(fill_col),
    # Only have % showing if it's a large enough # (w/ enough space)
    label = case_when(
      pct < 2 ~ "",
      pct >= 6 ~ paste0(round(pct), "%"),
      TRUE ~ as.character(round(pct))
    )
  )


p_winchange <- ggplot(
  # Focusing on the full dataset, counting across all categories for the figure
  ScopingReview_clean %>% count(WithinPersonChange_YN, Term_Inertia),
  aes(
    x = n,
    y = "",
    fill = WithinPersonChange_YN,
    pattern = Term_Inertia #,
  )
) +
  ggpattern::geom_col_pattern(
    position = "stack",
    colour = "black",
    pattern_colour = "gray",
    pattern_fill = "gray",
    pattern_density = 0.08,
    pattern_spacing = 0.5,
    pattern_angle = 45,
    pattern_key_scale_factor = 0.6
  ) +
  ggpattern::scale_pattern_manual(
    values = c(
      "Yes" = "stripe",
      "No" = "none"
    ),
    name = "Used term inertia?"
  ) +
  geom_text(
    data = winchange_labels,
    aes(
      x = n,
      y = "",
      label = label,
      color = text_color,
      group = WithinPersonChange_YN
    ),
    inherit.aes = FALSE,
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_fill_manual(
    values = cols_os,
    breaks = intersect(
      names(cols_os),
      as.character(unique(ScopingReview_clean$WithinPersonChange_YN))
    ),
    name = "Examined within-person change"
  ) +
  labs(
    title = "A. Articles that examine within-person change"
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(pattern = "none"),
      reverse = TRUE
    ),
    pattern = guide_legend(
      order = 2,
      override.aes = list(
        fill = "gray30",
        pattern_spacing = .08,
        pattern_density = .25
      )
    )
  ) +
  gg_row +
  theme(
    legend.box = "horizontal",
    legend.spacing.x = grid::unit(24, "pt"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  coord_cartesian(clip = "off") +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", nrow(ScopingReview_clean)),
    hjust = 1.05,
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  )


## 8B: Specific statistical models for w/in person change --------------------------

# Prepping data
wp_models_long <- ScopingReview_clean %>%
  filter(
    WithinPersonChange_YN == "Yes"
  ) %>%
  separate_rows(
    WithinPersonChange_StatisticalModel,
    sep = ";"
  ) %>%
  mutate(
    WithinPersonChange_StatisticalModel = str_trim(
      WithinPersonChange_StatisticalModel
    )
  ) %>%
  filter(
    !is.na(
      WithinPersonChange_StatisticalModel
    )
  ) %>%
  distinct(
    Title,
    WithinPersonChange_StatisticalModel,
    WithinPersonChange_Evidence,
    WithinPersonChange_HowModeled
  ) %>%
  mutate(
    WithinPersonChange_StatisticalModel = WithinPersonChange_StatisticalModel |>
      replace_values(
        "Time-varying autoregressive model" ~ "Time-varying AR"
      ),
    StatMethod_Color = case_when(
      WithinPersonChange_StatisticalModel == "Time-varying AR" ~ "AR-based",
      WithinPersonChange_StatisticalModel ==
        "AR model with time-varying interaction term" ~ "AR-based",
      WithinPersonChange_StatisticalModel == "Moving window (AR)" ~ "AR-based",
      WithinPersonChange_StatisticalModel ==
        "AC computed each day" ~ "AR-based",
      WithinPersonChange_StatisticalModel == "Survival analysis" ~ "Other",
      WithinPersonChange_StatisticalModel ==
        "Detrended fluctuation analysis" ~ "Other",
      WithinPersonChange_StatisticalModel ==
        "Visual inspection of plots" ~ "Other",
      WithinPersonChange_StatisticalModel == "Not fully clear" ~ "Unknown"
    ),
    WithinPersonChange_StatisticalModel = factor(
      WithinPersonChange_StatisticalModel,
      levels = c(
        "Not fully clear",
        "Visual inspection of plots",
        "Detrended fluctuation analysis",
        "Survival analysis",
        "AC computed each day",
        "AR model with time-varying interaction term",
        "Time-varying AR",
        "Moving window (AR)"
      )
    ),
    Moderator_YN = case_when(
      WithinPersonChange_HowModeled == "Via a moderator" ~ "Yes",
      WithinPersonChange_HowModeled == "Via both" ~ "Yes",
      WithinPersonChange_HowModeled == "Model/method without moderator" ~ "No"
    )
  )

# Graph
p_winchange_models <-
  ggplot(
    wp_models_long,
    aes(
      y = WithinPersonChange_StatisticalModel,
      alpha = Moderator_YN,
      fill = StatMethod_Color
    )
  ) +
  geom_bar() +
  labs(
    x = "Number of articles",
    title = "B. Method to investigate within-person change",
    y = NULL,
    fill = "Model category",
    alpha = "Used a moderator"
  ) +
  scale_fill_manual(
    values = cols_statsmodels_gen
  ) +
  scale_alpha_discrete(range = c(.5, 1)) +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(reverse = TRUE)
  ) +
  gglayer_theme +
  n_annotation(nrow(wp_models_long)) +
  coord_cartesian(clip = "off")

## 8C: Evidence graph --------------------------------------------------------------

# Prepping data for moderator graph (9) & evidence
ScopingReview_within <- ScopingReview_clean %>%
  filter(WithinPersonChange_YN == "Yes") %>%
  select(
    StudyID,
    Authors,
    Term_Inertia,
    Definition_ComponentsUnique,
    Constructs_Coarse,
    StudyTopic,
    WithinPersonChange_Correlate,
    WithinPersonChange_HowModeled,
    WithinPersonChange_Moderator_Step,
    WithinPersonChange_Moderator_Form,
    WithinPersonChange_Moderator_ExactInfo,
    WithinPersonChange_Moderator_Type,
    WithinPersonChange_StatisticalModel,
    WithinPersonChange_Evidence,
    WithinPersonChange_Evidence_LinkedWithModerator,
    WithinPersonChange_Evidence_Moderator_Which,
    StatisticalModelFamily
  ) %>%
  mutate(
    Moderator_grouped = case_when(
      WithinPersonChange_HowModeled == "Via both" ~ "Via both",
      WithinPersonChange_HowModeled ==
        "Model/method without moderator" ~ "Model without moderator",
      WithinPersonChange_Moderator_Step == "in one step" ~ "Moderator (1 step)",
      WithinPersonChange_Moderator_Step ==
        "in a second analysis step" ~ "Moderator (2 steps)"
    ),
    Moderator_grouped = factor(
      Moderator_grouped,
      levels = c(
        "Via both",
        "Moderator (2 steps)",
        "Moderator (1 step)",

        "Model without moderator"
      )
    ),
    ModelCategory = classify_model_category(StatisticalModelFamily),
    # Updating factor levels for later graphs
    WithinPersonChange_Evidence = factor(
      WithinPersonChange_Evidence,
      levels = c("No", "Not mentioned", "Yes")
    )
  )

# Graph
p_evidence <- make_stacked_row_plot(
  data = ScopingReview_within,
  category = WithinPersonChange_Evidence,
  title = "C. Evidence of within-person change",
  colours = cols_os
)

## 9: Moderator graph ---------------------------------------------------------

p_moderator <- make_stacked_row_plot(
  data = ScopingReview_within,
  category = Moderator_grouped,
  title = "Moderator",
  colours = cols_moderator
)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Exporting ------------------------------------------------------------------

## For main manuscript ------------------------------------------------------

# DESCRIPTIVE SECTION OF MANUSCRIPT

### Fig 4: Sampling (composite) ------------------------------------------------------

mainmanu_os_sampling <-
  p_sampling_clear /
  free(p_sampling_perarticle, side = "l") /
  p_prereg /
  p_datasharing /
  p_codesharing /
  plot_layout(heights = c(.5, 2.5, .5, .5, .5))

ggsave(
  here::here("figures", "fig4_mainmanu_os_sampling.jpg"),
  mainmanu_os_sampling,
  width = 9,
  height = 9,
  units = "in"
)

### FIGURE 5 - Constructs ------------------------------------------------------
constructs_comp <- (p_constructs |
  p_constructs_peryear) /
  patchwork::wrap_elements(full = p_constructs_commonlegend) +
  plot_layout(heights = c(1, .04))

ggsave(
  here::here("figures", "Fig5_constructs_comp.jpg"),
  constructs_comp,
  width = 10,
  height = 4.5,
  units = "in"
)


### FIGURE 6 - Inertia term & usage ------------------------------------------------------

p_definitionfig <- p_term /
  p_clear /
  free(p_alpha_components, side = "r") +
  plot_layout(heights = c(1, 1, 16)) +
  theme(legend.justification = "left")

ggsave(
  here::here("figures", "fig6_definitions.jpg"),
  p_definitionfig,
  width = 10,
  height = 11,
  units = "in"
)


### FIGURE 7 - Statistical models ------------------------------------------------------
ggsave(
  here::here("figures", "fig7_statsmodel.jpg"),
  p_statsmodels,
  width = 9,
  height = 3.5,
  units = "in"
)

### FIGURE 8 - Within-person change ------------------------------------------------------
p_winchange_comp_other <- p_winchange /
  free(p_winchange_models, side = "r") /
  p_evidence +
  plot_layout(heights = c(1, 6, 1))

ggsave(
  here::here("figures", "fig8_winchange_comp.jpg"),
  p_winchange_comp_other,
  width = 10,
  height = 6,
  units = "in"
)

### FIGURE 9 - Within-person change ------------------------------------------------------
# note - the table included in the figure with article-specific info is created & added via powerpoint
ggsave(
  here::here("figures", "fig9_winchange_moderators.jpg"),
  p_moderator,
  width = 8,
  height = 1.5,
  units = "in"
)


# Exporting: SUPP MATERIALS ----------------------------------------------------

## DESCRIPTIVE relevant section ------------------------------------------------

# Sampling info with each design (not combining per article into "multiple types")
ggsave(
  here::here("figures", "supplementary", "fig_s6_Sampling_AllSampling.jpg"),
  p_sampling_bydesign,
  width = 7,
  height = 4,
  units = "in"
)

# How constructs were assessed : Single vs multiple items etc
ggsave(
  here::here("figures", "supplementary", "fig_s5_Items_Measured.jpg"),
  p_items,
  width = 6,
  height = 4,
  units = "in"
)

# Removing datapoints
p_removeddata_comp <- p_removeddata_yn /
  free(p_removeddatapoints_why, side = "l") +
  plot_layout(heights = c(1, 4))

ggsave(
  here::here("figures", "supplementary", "fig_s8_removeddata_comp.jpg"),
  p_removeddata_comp,
  width = 8,
  height = 6,
  units = "in"
)

# Model comparison
ggsave(
  here::here("figures", "supplementary", "fig_s15_modelcomparison.jpg"),
  # to make slightly thinner bar
  p_modelcomp + plot_layout(heights = .5),
  width = 8,
  height = 2,
  units = "in"
)

# Reporting of average N answered
ggsave(
  here::here(
    "figures",
    "supplementary",
    "fig_s7_averageBeepsAnswered_Reported.jpg"
  ),
  p_averageN_rep + plot_layout(heights = .5),
  width = 8,
  height = 2,
  units = "in"
)

# Map of first-author affiliations
ggsave(
  here::here("figures", "supplementary", "fig_s1_map.jpg"),
  p_map,
  width = 8,
  height = 4,
  units = "in"
)

## DEFINITION relevant section -------------------------------------------------
ggsave(
  here::here("figures", "supplementary", "fig_s10_term_peryear.jpg"),
  p_term_peryear,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  here::here("figures", "supplementary", "fig_s4_topic.jpg"),
  p_topic,
  width = 8,
  height = 5,
  units = "in"
)

# Recovery composite figure

p_recovery_comp <- p_recovery_yn /
  p_recovery_how /
  free(p_recovery_defs, side = "r") +
  plot_layout(heights = c(.5, .5, 7))

ggsave(
  here::here("figures", "supplementary", "fig_s13_recovery_composite.jpg"),
  p_recovery_comp,
  width = 11,
  height = 8,
  units = "in"
)

# Where definition components first appear
ggsave(
  here::here(
    "figures",
    "supplementary",
    "fig_s11_definitioncomponents_firstappearance.jpg"
  ),
  p_def_firstappears,
  width = 8,
  height = 4.5,
  units = "in"
)

# Definition components co-occurrences
ggsave(
  here::here("figures", "supplementary", "fig_s12a_defcom_cooccurrences.jpg"),
  defcomp_cooc,
  width = 10,
  height = 8,
  units = "in"
)

# Definition components appearing alone & overall prevalence
ggsave(
  here::here("figures", "supplementary", "gif_s12b_defcom_single.jpg"),
  defcomp_occur,
  width = 10,
  height = 5,
  units = "in"
)


# Time intervals
p_timeint_comp <- p_timeint_yn /
  p_timeint_construct /
  p_timeint_operationalization +
  plot_layout(heights = c(.5, 9, 7))

ggsave(
  here::here("figures", "supplementary", "fig_s2_timeinterval.jpg"),
  p_timeint_comp,
  width = 9,
  height = 10,
  units = "in"
)

# Constructs by general operationalization
ggsave(
  here::here(
    "figures",
    "supplementary",
    "fig_s3_constructs_operationalization.jpg"
  ),
  p_constructs_operationalization,
  width = 8,
  height = 5,
  units = "in"
)


## OPERATIONALIZATION relevant section -----------------------------------------

# Overall graph of operationalizations vs definitions
ggsave(
  here::here("figures", "supplementary", "fig_s14_component_bystatmodel.jpg"),
  p_op_def,
  width = 9,
  height = 5,
  units = "in"
)

# Context-specific graph of operationalizations vs  definitions
ggsave(
  here::here(
    "figures",
    "supplementary",
    "fig_s17_component_bystatmodel_context.jpg"
  ),
  p_context,
  width = 9,
  height = 4,
  units = "in"
)

# State vs trait definitions vs operationalizations
ggsave(
  here::here(
    "figures",
    "supplementary",
    "fig_s18_component_bystatmodel_winpersonchange.jpg"
  ),
  p_winchangedef,
  width = 8,
  height = 4,
  units = "in"
)

# Statistical details: Individual differences handles; Centering; Estimation

# Putting together
p_statsdetails <- p_inddiffs_byar /
  p_center +
  p_estimation +
  plot_layout(heights = c(6, 5, 6)) &
  theme(legend.justification = "left")

ggsave(
  here::here("figures", "supplementary", "gif_s16_statsdetails.jpg"),
  p_statsdetails,
  width = 10.5,
  height = 8,
  units = "in"
)


# List 'other' variables for supplementary materials (saving w/ all supp material figures)
writeLines(
  unique(ScopingReview_clean$StudyTopic_other),
  here::here("figures", "supplementary", "Topic_Other.txt")
)
