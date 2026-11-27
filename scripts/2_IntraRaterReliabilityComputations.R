# This is R code for the project “Lingering over time: How do
# authors define and model (emotional) inertia? A scoping review.”
# For information on this project, see: https://osf.io/uaw4n/

#This script focuses on:
# Computing inter-rater computations

# Packages
library(here)
library(tidyverse)
library(purrr) # for open responses & strings
library(stringr) # for open responses & strings
library(irr)
library(psych) # for ICC

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------

# Data processed in wrangling script
ScopingReview_LP_ir <- readr::read_rds(here::here(
  "data",
  "processed",
  "ForInterRaterReliability",
  "ScopingReview_LP_ir.RDS"
)) %>%
  # Getting both datasets in the same order
  arrange(Authors)
ScopingReview_MAB_ir <- readr::read_rds(here::here(
  "data",
  "processed",
  "ForInterRaterReliability",
  "ScopingReview_MAB_ir.RDS"
)) %>%
  arrange(Authors)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) IRR functions -----------------------------------------------------------

# Select-one ----------------------------------------------------------

# Function to compute IRR for factor variables (i.e., select-one)
IRR_factor <- function(x, y, var_name) {
  # Determine if a branching item (e.g., if NAs are present)
  # If yes keeping only relevant articles for that item
  # If either rater puts NA, the article doesn't get counted for computations,
  # so taking any article with any NA out
  keep <- !(is.na(x) | is.na(y))
  x <- x[keep]
  y <- y[keep]

  # Aligning factor levels
  # (AKA: making sure the levels of LP (from omly 14 articles) have all the same as MAB (which includes all response options, from the 100 articles))
  lvls <- union(
    levels(factor(x)),
    levels(factor(y))
  )

  x <- factor(x, levels = lvls)
  y <- factor(y, levels = lvls)

  # percent agreement
  agreement <- mean(
    as.character(x) == as.character(y),
    na.rm = TRUE
  )

  # kappa
  kap <- tryCatch(
    irr::kappa2(data.frame(x, y))$value,
    error = function(e) NA_real_
  )

  # kappa max
  tab <- table(x, y)

  kappa_max <- tryCatch(
    {
      n <- sum(tab)

      row_marg <- rowSums(tab) / n
      col_marg <- colSums(tab) / n

      pmax_obs <- sum(pmin(row_marg, col_marg))
      pe <- sum(row_marg * col_marg)

      (pmax_obs - pe) / (1 - pe)
    },
    error = function(e) NA_real_
  )

  # permutation test
  B <- 10000
  set.seed(45)

  perm_result <- tryCatch(
    {
      perm_kappa <- base::replicate(B, {
        irr::kappa2(
          data.frame(x, sample(y))
        )$value
      })

      (sum(perm_kappa >= kap) + 1) / (B + 1)
    },
    error = function(e) {
      perm_error <<- conditionMessage(e)
      NA_real_
    }
  )

  # Category frequencies pooled across raters
  freq <- prop.table(
    table(c(as.character(x), as.character(y)))
  )

  freq_string <- paste(
    paste0(
      names(freq),
      "=",
      round(freq, 2)
    ),
    collapse = "; "
  )

  # Number of relevant articles for each computation
  n_articles <- length(x)

  absent <- tryCatch(round(freq[[1]], 2), error = function(e) NA_real_) # Just for binary variables
  present <- tryCatch(round(freq[[2]], 2), error = function(e) NA_real_) # Just for binary variables

  tibble(
    variable = var_name,
    n_categories = length(lvls),
    agreement = round(agreement, 2),
    kappa = round(kap, 2),
    kappa_max = round(kappa_max, 2),
    permutation_p = round(perm_result, 2),
    category_frequencies = freq_string,
    absent = absent,
    present = present,
    n_articles = length(x)
  )
}


# Select-multiple variables ----------------------------------------------------------

# Making multi-select variables into binary ones:
# Making binary variables
make_binary_multiselect <- function(
  df1,
  df2,
  vars,
  id_vars = c("Title", "Authors"),
  sep = ";"
) {
  df1_bin <- df1 %>% select(all_of(id_vars))
  df2_bin <- df2 %>% select(all_of(id_vars))

  for (var in vars) {
    categories <- c(df1[[var]], df2[[var]]) %>%
      na.omit() %>%
      str_split(sep) %>%
      unlist() %>%
      str_trim() %>%
      unique() %>%
      sort()

    # Create one binary column per category

    for (cat in categories) {
      new_var <- paste0(
        var,
        "_",
        make.names(cat)
      )

      df1_bin[[new_var]] <- unname(
        sapply(df1[[var]], function(x) {
          if (is.na(x)) {
            return(NA_character_)
          }

          vals <- str_trim(
            unlist(str_split(x, sep))
          )

          if (cat %in% vals) {
            "Present"
          } else {
            "Absent"
          }
        })
      )

      df2_bin[[new_var]] <- unname(
        sapply(df2[[var]], function(x) {
          if (is.na(x)) {
            return(NA_character_)
          }

          vals <- str_trim(
            unlist(str_split(x, sep))
          )

          if (cat %in% vals) {
            "Present"
          } else {
            "Absent"
          }
        })
      )
    }
  }

  list(
    df1_bin = df1_bin,
    df2_bin = df2_bin
  )
}

# Numeric ----------------------------------------------------------

IRR_numeric <- function(
  df1,
  df2,
  vars
) {
  results <- lapply(vars, function(var) {
    x <- df1[[var]]
    y <- df2[[var]]

    # Getting agreement of NAs (before removing)
    na_agreement <- mean(is.na(x) == is.na(y))

    # Keep only observations available from both raters
    keep <- !(is.na(x) | is.na(y))

    x <- x[keep]
    y <- y[keep]

    n_articles <- length(x)

    icc_val <- tryCatch(
      {
        psych::ICC(
          data.frame(
            rater1 = x,
            rater2 = y
          )
        )$results["Single_random_raters", "ICC"]
      },
      error = function(e) NA_real_
    )

    # Getting NA agreements
    exact_agreement <- mean((x == y) | (is.na(x) & is.na(y)))

    tibble(
      variable = var,
      ICC = round(icc_val, 4),
      agreement = round(mean(x == y, na.rm = TRUE), 2),
      exact_agreement = round(exact_agreement, 2),
      na_agreement = round(na_agreement, 2),
      n_articles = n_articles,
      effective_articles = sum(!is.na(x) & !is.na(y))
    )
  })

  bind_rows(results)
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Preprocessing specific to the IRR computations --------------------------

# -- Numeric variables ################
# N participants
# Studies with 2 waves
ScopingReview_LP_ir$NParticipants[
  ScopingReview_LP_ir$NParticipants == "1031, 171"
] <- 1031
ScopingReview_MAB_ir$NParticipants[
  ScopingReview_MAB_ir$NParticipants == "1031, 171"
] <- 1031
# Changing "Not reported" to "NA" (so can treat as numeric)
ScopingReview_LP_ir$NParticipants[
  ScopingReview_LP_ir$NParticipants == "Not reported"
] <- NA

# AverageTimeInterval_Hours
ScopingReview_LP_ir$AverageTimeInterval_Hours[
  ScopingReview_LP_ir$AverageTimeInterval_Hours == "2.98 pre-ABM, 2.93 post-ABM"
] <- 2.96 # taking average across two time periods

# StudyDuration_Days
ScopingReview_LP_ir$StudyDuration_Days[
  ScopingReview_LP_ir$StudyDuration_Days == "4 months"
] <- 112
ScopingReview_MAB_ir$StudyDuration_Days[
  ScopingReview_MAB_ir$StudyDuration_Days == "4 months"
] <- 112

ScopingReview_LP_ir$StudyDuration_Days[
  ScopingReview_LP_ir$StudyDuration_Days ==
    "three consecutive days per week (ie, Thursday, Friday, Saturday) for eight weeks"
] <- 24

ScopingReview_MAB_ir$StudyDuration_Days[
  ScopingReview_MAB_ir$StudyDuration_Days == "3 weeks"
] <- 21
ScopingReview_MAB_ir$StudyDuration_Days[
  ScopingReview_MAB_ir$StudyDuration_Days == "2 weeks"
] <- 14

# ESMPrompts_perDay
ScopingReview_MAB_ir$ESMPrompts_perDay[
  ScopingReview_MAB_ir$ESMPrompts_perDay == "Minimum 8"
] <- 8
ScopingReview_LP_ir$ESMPrompts_perDay[
  ScopingReview_LP_ir$ESMPrompts_perDay == "8 + bursts"
] <- 8

#AverageNAnsweredESM
ScopingReview_LP_ir$AverageNAnsweredESM[
  ScopingReview_LP_ir$AverageNAnsweredESM == "37 pre-ABM, 35 post-ABM"
] <- 72 # Added together in full study, so putting sum

ScopingReview_MAB_ir$AverageNAnsweredESM[
  ScopingReview_MAB_ir$AverageNAnsweredESM == "25.8, 32.4"
] <- 32.4

ScopingReview_LP_ir$AverageNAnsweredESM[
  ScopingReview_LP_ir$AverageNAnsweredESM == "25.8, 32.4"
] <- 32.4

ScopingReview_MAB_ir$AverageNAnsweredESM[
  ScopingReview_MAB_ir$AverageNAnsweredESM == "Median: 545"
] <- 545

ScopingReview_MAB_ir$AverageNAnsweredESM[
  ScopingReview_MAB_ir$AverageNAnsweredESM ==
    "127.68 scheduled beeps; 18 episodes; 43.51 burst beeps"
] <- 43.51 # bursts

# MAB - Take NAs into account for some numeric variables; check questionnaire

# -- Checking formatting to make sure variables are in appropriate formats
glimpse(ScopingReview_MAB_ir)

# Altering variables as needed (for both dataframes, from MAB & LP)
ScopingReview_MAB_ir <- ScopingReview_MAB_ir %>%
  mutate(
    across(
      c(
        NParticipants,
        StudyDuration_Days,
        ESMPrompts_perDay,
        AverageTimeInterval_Hours,
        AverageNAnsweredESM
      ),
      as.integer
    ),
    across(
      c(
        #Distribution_Exact,
        EstimationTechnique,
        ResponseScales # for purposes of IRR, can treat as factor (only 1 variable selected each time)
      ),
      as.factor
    )
  )

ScopingReview_LP_ir <- ScopingReview_LP_ir %>%
  mutate(
    across(
      c(
        NParticipants,
        StudyDuration_Days,
        ESMPrompts_perDay,
        AverageTimeInterval_Hours,
        AverageNAnsweredESM
      ),
      as.integer
    ),
    across(
      c(
        EstimationTechnique,
        ResponseScales # for purposes of IRR, can treat as factor (only 1 variable selected each time)
      ),
      as.factor
    )
  )

# Checking levels of all factors (as these will be used to ID select-one vs select-mult vs numeric variables)
hardhat::get_levels(ScopingReview_MAB_ir)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Select-One Variables ----------------------------------------------------

# Choosing select-one variables (i.e., factors)
selectone_vars <- ScopingReview_MAB_ir %>% select_if(is.factor) %>% names()

# Check levels of factors are correct # MAB - does this matter ?? Can check function...
# Still has just 'no's for some variables etc.
for (v in selectone_vars) {
  cat("\n", v, "\n")

  print(levels(factor(ScopingReview_MAB_ir[[v]])))
  print(levels(factor(ScopingReview_LP_ir[[v]])))
}


# Running IRR functions for select-one variables
irr_selectone_results <- purrr::map_dfr(
  selectone_vars,
  function(v) {
    IRR_factor(
      x = ScopingReview_MAB_ir[[v]],
      y = ScopingReview_LP_ir[[v]],
      var_name = v
    )
  }
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5) Select-Multiple Variables -----------------------------------------------

# Choosing select-multiple variables (characters)
selectmult_vars <- ScopingReview_MAB_ir %>%
  select_if(is.character) %>%
  select(-c(Title, Authors)) %>%
  names()

# Making variables into binary response options
binary_responses <- make_binary_multiselect(
  df1 = ScopingReview_LP_ir,
  df2 = ScopingReview_MAB_ir,
  vars = selectmult_vars
)

LP_bin <- binary_responses$df1_bin
MAB_bin <- binary_responses$df2_bin

# Getting names of new variables (repsonse options)
binary_vars <- setdiff(
  names(LP_bin),
  c("Title", "Authors")
)

# Getting IRR indices for select-multiple binary response choices
irr_selectmult_binary <- purrr::map_dfr(
  binary_vars,
  ~ IRR_factor(
    x = factor(LP_bin[[.x]]),
    y = factor(MAB_bin[[.x]]),
    var_name = .x
  )
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6) Numeric Variables ----------------------------------------------------

# Selecting numeric variables
selectnumeric_vars <- ScopingReview_MAB_ir %>%
  select_if(is.integer) %>%
  names()

# Getting IRR indices (i.e., ICC) for numeric variables
irr_numericvars <- IRR_numeric(
  df1 = ScopingReview_LP_ir,
  df2 = ScopingReview_MAB_ir,
  vars = selectnumeric_vars
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7) Exporting results ------------------------------------------------------------

# Data for inter-rater computations
readr::write_csv(
  irr_selectone_results,
  here::here(
    "data",
    "interRaterReliability",
    "ir_reliability_selectone.csv"
  )
) # MAB - put appendix name & #


readr::write_csv(
  irr_selectmult_binary,
  here::here(
    "data",
    "interRaterReliability",
    "ir_reliability_selectmult.csv"
  )
)

readr::write_csv(
  irr_numericvars,
  here::here(
    "data",
    "interRaterReliability",
    "ir_reliability_numeric.csv"
  )
)

# Getting the two dataframes exported together to more easily compare
combined_forIRR <- ScopingReview_LP_ir %>%
  rename_with(
    ~ paste0(.x, "_LP"),
    -c(Title, Authors)
  ) %>%
  bind_cols(
    ScopingReview_MAB_ir %>%
      select(-Title, -Authors) %>%
      rename_with(~ paste0(.x, "_MAB"))
  ) %>%
  select(order(colnames(.)))


readr::write_csv(
  combined_forIRR,
  here::here(
    "data",
    "interRaterReliability",
    "directComparison2raters_forIRR.csv"
  )
)
