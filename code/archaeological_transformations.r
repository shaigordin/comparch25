#############################################################
# Archaeological Data Transformation and Analysis Script
# Created from the Quarto presentation: "Data Transformations in Archaeological Research"
# This script demonstrates various data transformation techniques
# used in archaeological research using the tidyverse package in R
#############################################################

# Part 1: Loading Required Packages
#############################################################
library(tidyverse)    # Main package for data manipulation
library(janitor)      # Package for cleaning data
library(car)          # For Box-Cox transformations
library(compositions) # For compositional data analysis
library(ggplot2)      # For visualization

# Part 2: Data Loading and Initial Exploration
#############################################################
# The code below assumes the data file has UTF-8 encoding with headers not in the first row
# Try-catch block to handle potential file loading issues
tryCatch({
  # First, explore the file structure without assuming header position
  raw_peek <- read_csv("https://raw.githubusercontent.com/shaigordin/comparch25/refs/heads/main/slide-decks/data/enc.csv", col_names = FALSE, n_max = 5,
                        locale = locale(encoding = "UTF-8"))
  print(raw_peek)

  # Now load the data properly, skipping the first row if headers are in second row
  archaeological_data <- read_csv("https://raw.githubusercontent.com/shaigordin/comparch25/refs/heads/main/slide-decks/data/enc.csv",
                                 skip = 1,             # Skip first row
                                 col_names = TRUE,     # Use first row as headers
                                 locale = locale(encoding = "UTF-8")) # Hebrew encoding

  # Check column names and structure
  cat("Column names in the dataset:\n")
  print(names(archaeological_data))
  glimpse(archaeological_data)
}, error = function(e) {
  # If file can't be loaded, create a sample dataset for demonstration
  cat("Note: Could not load 'enc.csv'. Creating sample archaeological dataset instead.\n")
  set.seed(123)

  # Create sample data with archaeological measurements
  archaeological_data <<- tibble(
    "קוד זיהוי" = 1:100,  # ID code
    "1" = runif(100, 0, 20),  # Length (cm)
    "2" = runif(100, 0, 15),  # Width (cm)
    "3" = runif(100, 0, 5),   # Thickness (cm)
    "4" = runif(100, 0, 500), # Weight (g)
    "5" = sample(1:10, 100, replace = TRUE), # Color code
    "סוג_כלי" = sample(c("קערה", "סיר", "פך", "קנקן", "נר"), 100, replace = TRUE), # Tool type
    "תקופה" = sample(c("ברונזה קדומה", "ברונזה תיכונה", "ברזל", "רומי", "ביזנטי"),
                     100, replace = TRUE) # Period
  )
})

# Part 3: Creating a Field Dictionary for Documentation
#############################################################
# Create a dictionary of fields to document their meaning
field_dictionary <- tribble(
  ~original_name, ~meaning, ~category,
  "מספר_אתר", "Unique site ID number", "Administrative info",
  "שם_אתר", "Site name as appears in encyclopedia", "Administrative info",
  "ערך_משנה", "Sub-entry in a larger encyclopedia entry", "Administrative info",
  "נקודת_ציון", "Coordinates in Israel grid", "Geographic info",
  "אזור", "General geographic area", "Geographic info",
  "תת_אזור", "More detailed geographic division", "Geographic info",
  "חופר", "Names of archaeologists who conducted the excavation", "Administrative info",
  "מחבר_הערך", "Author of the encyclopedia entry", "Bibliographic info",
  "תקופה_כללי", "General archaeological period", "Chronological info",
  "תקופה_מעבר", "Transitional period sites or layers", "Chronological info",
  "תקופה_מיוצגת", "Archaeological sub-period if available", "Chronological info",
  "תיארוך_ממצא", "More detailed dating information", "Chronological info",
  "סוג_ממצא", "Type of finding (installation/flint/tomb/ceramics/building)", "Archaeological info",
  "סוג_אתר", "Site classification (tel/khirba/cave/quarry etc.)", "Archaeological info",
  "אופי_אתר", "Settlement nature (rural/urban/tomb/agricultural remains)", "Settlement info",
  "גודל_אתר", "Estimated archaeological site size", "Settlement info",
  "גודל_חפירה", "Size of archaeological excavation if reported", "Administrative info",
  "הערות", "Additional data that did not fit any other field", "Bibliographic info",
  "הפניה_ביבליוגרפית", "Source of excavation report in encyclopedia", "Bibliographic info"
)

# Display the dictionary
print(field_dictionary)

# Part 4: Data Cleaning and Column Renaming
#############################################################
# Create a mapping for column names

column_name_map <- c(
  "פריט_מספר" = "item_id",
  "אורך" = "length",
  "רוחב" = "width",
  "עובי" = "thickness",
  "משקל" = "weight",
  "צבע" = "color_code",
  "חומר" = "material",
  "סוג_כלי" = "tool_type",
  "תקופה" = "period",
  "לוקוס" = "locus",
  "שכבה" = "stratum",
  # For numeric column names
  "1" = "length",
  "2" = "width",
  "3" = "thickness",
  "4" = "weight",
  "5" = "color_code",
  # Leave original names for columns with unknown meaning
  "X10" = "measure_10",
  "X11" = "measure_11"
)

# Apply the new names - handle both Hebrew and numeric column names
cleaned_data <- archaeological_data %>%
  # Rename columns using our mapping - only for columns that exist
  rename_with(~ column_name_map[.x], .cols = intersect(names(.), names(column_name_map))) %>%
  # Clean remaining column names
  clean_names()

# Check if we have an ID column, add one if not
if (!"item_id" %in% names(cleaned_data) && !"קוד זיהוי" %in% names(cleaned_data)) {
  cleaned_data <- cleaned_data %>%
    mutate(item_id = row_number(), .before = 1)
}

# Display the new column names
cat("Cleaned column names:\n")
print(names(cleaned_data))

# Part 5: Creating Variable Groups and Basic Analysis
#############################################################
# Identify the key measurement variables in our cleaned data
# This handles both explicitly named columns and numeric-named columns
has_measurement_cols <- any(c("length", "width", "thickness", "weight") %in% names(cleaned_data))

if (has_measurement_cols) {
  # Create variable groups by category using standard names
  measurement_vars <- intersect(c("length", "width", "thickness", "weight"), names(cleaned_data))
  typological_vars <- intersect(c("color_code", "material", "tool_type"), names(cleaned_data))
  context_vars <- intersect(c("locus", "stratum", "period"), names(cleaned_data))
} else {
  # Use numeric column names if standard names don't exist
  measurement_vars <- grep("^\\d+$", names(cleaned_data), value = TRUE)[1:4]
  if (length(measurement_vars) == 0) {
    # If no numeric columns, use the first 4 numeric columns
    measurement_vars <- names(cleaned_data)[sapply(cleaned_data, is.numeric)][1:4]
  }
  # Set context variables if they exist
  if ("סוג_כלי" %in% names(cleaned_data)) {
    typological_vars <- "סוג_כלי"
  } else {
    typological_vars <- character(0)
  }
  if ("תקופה" %in% names(cleaned_data)) {
    context_vars <- "תקופה"
  } else {
    context_vars <- character(0)
  }
}

# Basic descriptive analysis by variable group
if (length(measurement_vars) > 0) {
  cat("Variable Groups Created:\n")
  cat("Measurement variables:", paste(measurement_vars, collapse=", "), "\n")
  cat("Typological variables:", paste(typological_vars, collapse=", "), "\n")
  cat("Context variables:", paste(context_vars, collapse=", "), "\n\n")

  # Summarize measurement variables
  measurement_summary <- cleaned_data %>%
    select(all_of(measurement_vars)) %>%
    summary()

  # Display the summary
  cat("Summary of Measurement Variables:\n")
  print(measurement_summary)

  # Group analysis - mean measurements by period if period variable exists
  if ("period" %in% names(cleaned_data) || "תקופה" %in% names(cleaned_data)) {
    period_col <- if("period" %in% names(cleaned_data)) "period" else "תקופה"

    period_measurements <- cleaned_data %>%
      group_by(across(all_of(period_col))) %>%
      summarise(across(all_of(measurement_vars),
                      list(mean = ~mean(.x, na.rm = TRUE),
                           median = ~median(.x, na.rm = TRUE),
                           sd = ~sd(.x, na.rm = TRUE)))) %>%
      arrange(desc(1))  # Order by first summary statistic

    # Display results
    cat("\nMeasurements by Period:\n")
    print(head(period_measurements))
  }
} else {
  cat("No measurement variables could be identified in the dataset.\n")
}

# Part 6: Working with Categorical Variables
#############################################################
# Check if we have period and tool type variables
has_period <- any(c("period", "תקופה") %in% names(cleaned_data))
has_tool_type <- any(c("tool_type", "סוג_כלי") %in% names(cleaned_data))

if (has_period && has_tool_type) {
  period_col <- if("period" %in% names(cleaned_data)) "period" else "תקופה"
  tool_col <- if("tool_type" %in% names(cleaned_data)) "tool_type" else "סוג_כלי"

  # Summary of tools by period and type
  tool_period_summary <- cleaned_data %>%
    count(across(all_of(c(period_col, tool_col)))) %>%
    arrange(across(all_of(period_col)), desc(n))

  # Convert categorical variables to factors with logical order
  if (period_col == "period") {
    # For English periods
    period_levels <- c(
      "Pre-Pottery Neolithic", "Neolithic", "Chalcolithic",
      "Early Bronze", "Middle Bronze", "Late Bronze",
      "Iron Age", "Persian", "Hellenistic", "Roman", "Byzantine"
    )
  } else {
    # For Hebrew periods
    period_levels <- c(
      "קדם-קרמי", "ניאולית", "כלקולית",
      "ברונזה קדומה", "ברונזה תיכונה", "ברונזה מאוחרת",
      "ברזל", "פרסי", "הלניסטי", "רומי", "ביזנטי"
    )
  }

  # Only use levels that actually exist in the data
  existing_periods <- unique(cleaned_data[[period_col]])
  valid_period_levels <- period_levels[period_levels %in% existing_periods]

  cleaned_data <- cleaned_data %>%
    mutate(
      # Convert period to factor with chronological order
      across(all_of(period_col),
             ~factor(.x, levels = valid_period_levels)),

      # Convert tool type to factor
      across(all_of(tool_col), ~factor(.x))
    )

  # Summary by period
  period_counts <- cleaned_data %>%
    count(across(all_of(period_col))) %>%
    arrange(across(all_of(period_col)))

  # Display results
  cat("\nTools by Period and Type:\n")
  print(head(tool_period_summary))

  cat("\nPeriod Counts:\n")
  print(period_counts)
}

# Part 7: Creating Archaeological Indices
#############################################################
# Check if we have the necessary variables for indices
has_length_width <- all(c("length", "width") %in% names(cleaned_data))
has_thickness <- "thickness" %in% names(cleaned_data)
has_weight <- "weight" %in% names(cleaned_data)

if (has_length_width) {
  # Create archaeological indices
  archaeological_indices <- cleaned_data %>%
    mutate(
      # Shape index: length to width ratio
      elongation_index = length / width
    )

  # Add volume estimate if we have thickness
  if (has_thickness) {
    archaeological_indices <- archaeological_indices %>%
      mutate(
        # Volume estimate (rough)
        volume_estimate = length * width * thickness
      )

    # Add density estimate if we have weight
    if (has_weight) {
      archaeological_indices <- archaeological_indices %>%
        mutate(
          # Density estimate (rough)
          density_estimate = if_else(volume_estimate > 0,
                                    weight / volume_estimate,
                                    NA_real_)
        )
    }
  }

  # Add shape categorization based on elongation index
  archaeological_indices <- archaeological_indices %>%
    mutate(
      # Classification by shape
      shape_category = case_when(
        elongation_index > 3 ~ "very elongated",
        elongation_index > 2 ~ "elongated",
        elongation_index > 0.7 & elongation_index < 1.3 ~ "square",
        elongation_index < 0.5 ~ "very wide",
        TRUE ~ "regular"
      ),

      # Convert to ordered factor
      shape_category = factor(shape_category,
                             levels = c("very wide", "regular", "square",
                                        "elongated", "very elongated"))
    )

  # Distribution of shape categories
  shape_distribution <- archaeological_indices %>%
    count(shape_category) %>%
    mutate(percentage = n / sum(n, na.rm = TRUE) * 100)

  # Display results
  cat("\nShape Category Distribution:\n")
  print(shape_distribution)

  # Save the indices back to our main cleaned data
  cleaned_data <- archaeological_indices
} else {
  cat("Cannot create archaeological indices - missing required length/width columns.\n")
}

# Part 8: Basic Variable Transformations
#############################################################
# Select our first measurement variable for transformation
if (length(measurement_vars) > 0) {
  first_measure <- measurement_vars[1]
  cat("\nPerforming transformations on variable:", first_measure, "\n")

  # Create simple transformations
  working_data <- cleaned_data %>%
    mutate(
      # Get ID column
      id_col = if ("item_id" %in% names(.)) item_id else row_number(),

      # Original value
      original_value = .data[[first_measure]],

      # Normalized value (e.g., convert to cm)
      normalized_value = .data[[first_measure]] / 10,

      # Log transformation (adding 1 to prevent log(0))
      log_value = log10(.data[[first_measure]] + 1)
    ) %>%
    select(id_col, original_value, normalized_value, log_value)

  # Display results
  cat("Basic transformations:\n")
  print(head(working_data))
} else {
  cat("Cannot perform basic transformations - no measurement variables identified.\n")
}

# Part 9: Common Transformations
#############################################################
if (length(measurement_vars) > 0) {
  first_measure <- measurement_vars[1]

  # Create a dataset for transformations
  transformed_data <- cleaned_data %>%
    mutate(
      # Original value
      original = .data[[first_measure]],

      # Log transformations
      log10_transform = log10(.data[[first_measure]] + 1),      # Base 10 logarithm
      natural_log = log(.data[[first_measure]] + 1),            # Natural logarithm

      # Root transformations
      sqrt_transform = sqrt(.data[[first_measure]]),            # Square root
      cube_root = sign(.data[[first_measure]]) * abs(.data[[first_measure]])^(1/3),  # Cube root

      # Inverse transformation
      inverse_transform = 1 / (.data[[first_measure]] + 0.001)  # Inverse transform
    )

  # Display summary of transformations
  transformation_summary <- transformed_data %>%
    select(original, log10_transform, natural_log, sqrt_transform, cube_root, inverse_transform) %>%
    summary()

  cat("\nSummary of Common Transformations:\n")
  print(transformation_summary)

  # Optionally plot histograms to show effect of transformations
  # (These would be visible in an R environment)
  cat("\nIf running in an interactive R session, you could use these commands to visualize:\n")
  cat("par(mfrow = c(2, 3))\n")
  cat("hist(transformed_data$original, main = 'Original Distribution')\n")
  cat("hist(transformed_data$log10_transform, main = 'Log10 Transformed')\n")
  cat("hist(transformed_data$natural_log, main = 'Natural Log')\n")
  cat("hist(transformed_data$sqrt_transform, main = 'Square Root')\n")
  cat("hist(transformed_data$cube_root, main = 'Cube Root')\n")
  cat("hist(transformed_data$inverse_transform, main = 'Inverse')\n")
  cat("par(mfrow = c(1, 1))\n")
} else {
  cat("Cannot perform common transformations - no measurement variables identified.\n")
}

# Part 10: Z-scores (Standardization)
#############################################################
if (length(measurement_vars) > 0) {
  # Create standardized data
  standardized_data <- cleaned_data %>%
    mutate(across(all_of(measurement_vars),
                 ~scale(.x)[,1],
                 .names = "z_{.col}"))

  # Display results
  cat("\nStandardized Data (Z-scores):\n")
  print(head(standardized_data %>% select(matches("^z_"))))

  # Visualize original vs standardized for first measurement
  first_measure <- measurement_vars[1]
  first_z <- paste0("z_", first_measure)

  comparison_data <- standardized_data %>%
    select(all_of(c(first_measure, first_z))) %>%
    pivot_longer(cols = everything(),
                 names_to = "variable_type",
                 values_to = "value")

  cat("\nComparison of original vs. standardized data:\n")
  cat("Original data summary:\n")
  print(summary(standardized_data[[first_measure]]))
  cat("\nStandardized data summary:\n")
  print(summary(standardized_data[[first_z]]))
}

# Part 11: Box-Cox Transformations
#############################################################
if (length(measurement_vars) > 0 && requireNamespace("car", quietly = TRUE)) {
  first_measure <- measurement_vars[1]

  # Define lambda values to test
  lambda_values <- c(-2, -1, -0.5, 0, 0.5, 1, 2)

  # Extract positive values for Box-Cox transformation
  positive_values <- cleaned_data %>%
    filter(.data[[first_measure]] > 0) %>%
    pull(.data[[first_measure]])

  if (length(positive_values) > 0) {
    # Calculate Box-Cox transformations
    box_cox_transforms <- sapply(lambda_values, function(lambda) {
      if (lambda == 0) {
        return(log(positive_values))
      } else {
        return((positive_values^lambda - 1) / lambda)
      }
    })

    # View summary of transformations
    colnames(box_cox_transforms) <- paste0("lambda_", lambda_values)

    cat("\nBox-Cox Transformations with different lambda values:\n")
    print(summary(box_cox_transforms))

    cat("\nEffect of different lambda values (summary statistics):\n")
    print(apply(box_cox_transforms, 2, function(x) c(
      mean = mean(x),
      median = median(x),
      sd = sd(x),
      skewness = mean((x - mean(x))^3) / sd(x)^3
    )))
  } else {
    cat("\nCannot perform Box-Cox transformation - no positive values in the selected measure.\n")
  }
} else {
  if (length(measurement_vars) == 0) {
    cat("\nCannot perform Box-Cox transformation - no measurement variables identified.\n")
  } else {
    cat("\nCannot perform Box-Cox transformation - 'car' package not available.\n")
    cat("You can install it with: install.packages('car')\n")
  }
}

# Part 12: Ratio Transformations
#############################################################
if (length(measurement_vars) >= 2) {
  # Create ratios between measurements
  ratio_data <- cleaned_data %>%
    mutate(
      ratio_1_2 = .data[[measurement_vars[1]]] / .data[[measurement_vars[2]]]
    )

  if (length(measurement_vars) >= 3) {
    ratio_data <- ratio_data %>%
      mutate(
        ratio_1_3 = .data[[measurement_vars[1]]] / .data[[measurement_vars[3]]]
      )
  }

  # Display results
  cat("\nRatio Transformations:\n")
  print(head(ratio_data %>% select(starts_with("ratio_"))))

  cat("\nSummary of Ratios:\n")
  print(summary(ratio_data %>% select(starts_with("ratio_"))))
} else {
  cat("\nCannot perform ratio transformations - need at least 2 measurement variables.\n")
}

# Part 13: Geometric Mean Size Adjustment
#############################################################
if (length(measurement_vars) >= 3) {
  # Calculate geometric mean for each row
  size_adjusted_data <- cleaned_data %>%
    rowwise() %>%
    mutate(
      # Calculate geometric mean (add small value to prevent 0)
      geom_mean = exp(mean(log(c_across(all_of(measurement_vars)) + 0.001), na.rm = TRUE))
    ) %>%
    ungroup()

  # Create size-adjusted versions of each measurement
  for (var in measurement_vars) {
    size_adjusted_data <- size_adjusted_data %>%
      mutate(!!paste0(var, "_adj") := .data[[var]] / geom_mean)
  }

  # Display results
  cat("\nGeometric Mean Size Adjustment:\n")
  print(head(size_adjusted_data %>%
               select(geom_mean, ends_with("_adj"))))

  cat("\nSummary of Size-Adjusted Variables:\n")
  print(summary(size_adjusted_data %>%
                  select(ends_with("_adj"))))
} else {
  cat("\nCannot perform geometric mean size adjustment - need at least 3 measurement variables.\n")
}

# Part 14: Compositional Data - Percentages
#############################################################
if (length(measurement_vars) >= 3) {
  # Convert data to percentages (compositional data)
  percentage_data <- cleaned_data %>%
    rowwise() %>%
    mutate(
      row_sum = sum(c_across(all_of(measurement_vars)), na.rm = TRUE)
    ) %>%
    ungroup()

  # Only process rows with non-zero sums
  percentage_data <- percentage_data %>%
    filter(row_sum > 0)

  # Create percentage versions of each measurement
  for (var in measurement_vars) {
    percentage_data <- percentage_data %>%
      mutate(!!paste0(var, "_pct") := .data[[var]] / row_sum * 100)
  }

  # Display results
  cat("\nCompositional Data (Percentages):\n")
  print(head(percentage_data %>%
               select(ends_with("_pct"))))

  cat("\nSummary of Percentage Variables:\n")
  print(summary(percentage_data %>%
                  select(ends_with("_pct"))))

  # Demonstrate compositional constraint (sum = 100%)
  row_sums <- percentage_data %>%
    rowwise() %>%
    mutate(total = sum(c_across(ends_with("_pct")), na.rm = TRUE)) %>%
    pull(total)

  cat("\nVerification of compositional constraint (should be close to 100%):\n")
  print(summary(row_sums))
} else {
  cat("\nCannot convert to compositional data - need at least 3 measurement variables.\n")
}

# Part 15: Log-ratio Transformations
#############################################################
if (length(measurement_vars) >= 3 && requireNamespace("compositions", quietly = TRUE)) {
  # Deal with zero values by adding a small value
  compositional_data <- cleaned_data %>%
    select(all_of(measurement_vars)) %>%
    mutate(across(everything(), ~ifelse(.x == 0, 0.001, .x)))

  # Convert to compositional form (sum to 1)
  comp_data_matrix <- compositional_data %>%
    rowwise() %>%
    mutate(row_sum = sum(c_across(everything()))) %>%
    ungroup() %>%
    mutate(across(all_of(measurement_vars), ~.x/row_sum)) %>%
    select(all_of(measurement_vars)) %>%
    as.matrix()

  # Calculate additive log-ratio (alr)
  tryCatch({
    alr_result <- compositions::alr(comp_data_matrix)
    alr_df <- as.data.frame(alr_result)
    colnames(alr_df) <- paste0("alr_", measurement_vars[-length(measurement_vars)])

    cat("\nAdditive Log-Ratio (ALR) Transformation:\n")
    print(head(alr_df))

    # Calculate centered log-ratio (clr)
    clr_result <- compositions::clr(comp_data_matrix)
    clr_df <- as.data.frame(clr_result)
    colnames(clr_df) <- paste0("clr_", measurement_vars)

    cat("\nCentered Log-Ratio (CLR) Transformation:\n")
    print(head(clr_df))

    cat("\nVerification of CLR constraint (sum should be close to 0 for each row):\n")
    clr_row_sums <- rowSums(clr_result)
    print(summary(clr_row_sums))
  }, error = function(e) {
    cat("\nError in log-ratio transformation:", conditionMessage(e), "\n")
    cat("This may be due to problematic values in the compositional data.\n")
  })
} else {
  if (length(measurement_vars) < 3) {
    cat("\nCannot perform log-ratio transformations - need at least 3 measurement variables.\n")
  } else {
    cat("\nCannot perform log-ratio transformations - 'compositions' package not available.\n")
    cat("You can install it with: install.packages('compositions')\n")
  }
}

# Part 16: Comparison of Transformation Methods
#############################################################
if (length(measurement_vars) > 0) {
  first_measure <- measurement_vars[1]

  # Create a data frame for comparison
  comparison_framework <- cleaned_data %>%
    mutate(
      original = .data[[first_measure]],
      log10 = log10(.data[[first_measure]] + 1),
      sqrt = sqrt(.data[[first_measure]]),
      cube_root = sign(.data[[first_measure]]) * abs(.data[[first_measure]])^(1/3),
      z_score = scale(.data[[first_measure]])[,1]
    ) %>%
    select(original, log10, sqrt, cube_root, z_score) %>%
    pivot_longer(cols = everything(),
                 names_to = "transformation",
                 values_to = "value")

  # Display summary statistics by transformation
  comparison_summary <- comparison_framework %>%
    group_by(transformation) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      skewness = mean((value - mean(value, na.rm = TRUE))^3, na.rm = TRUE) /
                sd(value, na.rm = TRUE)^3
    )

  cat("\nComparison of Different Transformation Methods:\n")
  print(comparison_summary)

  cat("\nIn an interactive R session, you could visualize with:\n")
  cat("ggplot(comparison_framework, aes(x = value)) +\n")
  cat("  geom_density() +\n")
  cat("  facet_wrap(~ transformation, scales = 'free') +\n")
  cat("  theme_minimal() +\n")
  cat("  labs(title = 'Comparison of Different Transformation Methods')\n")
}

# Part 17: When to Use Each Transformation
#############################################################
cat("\n-----------------------------------------------------\n")
cat("TRANSFORMATION GUIDE: WHEN TO USE EACH TRANSFORMATION\n")
cat("-----------------------------------------------------\n\n")

cat("LOGARITHMIC TRANSFORMATION:\n")
cat("- Use for: Right-skewed distributions, exponential relationships\n")
cat("- Benefits: Reduces right skew, stabilizes variance, handles wide ranges\n")
cat("- Archaeological examples: Artifact dimensions, settlement sizes\n\n")

cat("SQUARE ROOT / CUBE ROOT:\n")
cat("- Use for: Count data, moderately right-skewed distributions\n")
cat("- Benefits: Milder transformation than logarithm, works with zero values\n")
cat("- Archaeological examples: Artifact counts, sherd densities\n\n")

cat("Z-SCORES:\n")
cat("- Use for: Comparing variables with different scales/units\n")
cat("- Benefits: Standardization for statistical models, removes size differences\n")
cat("- Archaeological examples: Multi-variable analyses, comparing assemblages\n\n")

cat("RATIOS:\n")
cat("- Use for: Shape analysis, standardization by size\n")
cat("- Benefits: Removes absolute size, focuses on proportional relationships\n")
cat("- Archaeological examples: Vessel proportions, stone tool morphology\n\n")

cat("GEOMETRIC MEAN SIZE ADJUSTMENT:\n")
cat("- Use for: Removing size effects while preserving shape information\n")
cat("- Benefits: Controls for isometric scaling\n")
cat("- Archaeological examples: Pottery typology, skeletal measurements\n\n")

cat("LOG-RATIO TRANSFORMATIONS:\n")
cat("- Use for: Compositional data (percentages, proportions)\n")
cat("- Benefits: Addresses closure constraints, removes spurious correlations\n")
cat("- Archaeological examples: Ceramic assemblages, faunal proportions\n\n")

# End of script
cat("\nScript execution completed.\n")
