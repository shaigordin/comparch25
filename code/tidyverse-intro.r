################################################################################
# ניתוח וטרנספורמציה של נתונים ארכיאולוגיים
# קובץ הדרכה לקורס מתודולוגיות מחקר חישוביות בארכיאולוגיה
################################################################################

# טעינת חבילות נדרשות
library(tidyverse)    # לניתוח וויזואליזציה
library(janitor)      # לניקוי נתונים
library(skimr)        # לסיכום סטטיסטי
library(knitr)        # להצגת טבלאות
library(compositions) # לניתוח נתונים קומפוזיציוניים
library(Cairo)        # לייצוא גרפים באיכות גבוהה

# הגדרת תיקיית פלט לשמירת הגרפים
output_dir <- "figures"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# פונקציה לשמירת גרפים ב-PNG
save_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
  ggsave(
    filename = file.path(output_dir, paste0(filename, ".png")),
    plot = plot,
    device = "png",
    width = width,
    height = height,
    dpi = dpi
  )
}

################################################################################
# חלק 1: טעינה וחקירה ראשונית של נתונים
################################################################################

# קריאה ראשונית בלי הנחות לגבי כותרות (רק לבדיקה)
raw_peek <- read_csv("https://raw.githubusercontent.com/shaigordin/comparch25/refs/heads/main/slide-decks/data/enc.csv", col_names = FALSE, n_max = 5)
print(raw_peek)

# קריאה נכונה של הנתונים - שמות השדות האמיתיים מופיעים בשורה השנייה!
archaeological_data <- read_csv("https://raw.githubusercontent.com/shaigordin/comparch25/refs/heads/main/slide-decks/data/enc.csv",
                               skip = 1,             # דילוג על שורה ראשונה
                               col_names = TRUE,     # השורה הראשונה שנקרא תהיה כותרות
                               locale = locale(encoding = "UTF-8")) # קידוד עברית

# בדיקה ראשונית של הנתונים
glimpse(archaeological_data)

# סיכום סטטיסטי של הנתונים
skim_result <- skim(archaeological_data)
print(skim_result)

# בדיקת ערכים חסרים
missing_values <- colSums(is.na(archaeological_data))
missing_values_df <- data.frame(
  variable = names(missing_values),
  missing_count = missing_values
) %>%
  arrange(desc(missing_count))

print(missing_values_df)

# ויזואליזציה של ערכים חסרים
missing_plot <- missing_values_df %>%
  mutate(variable = fct_reorder(variable, missing_count)) %>%
  ggplot(aes(x = variable, y = missing_count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "מספר ערכים חסרים לפי עמודה",
    x = "משתנה",
    y = "מספר ערכים חסרים"
  ) +
  theme(text = element_text(size = 12))

print(missing_plot)
save_plot(missing_plot, "missing_values")

################################################################################
# חלק 2: ניקוי וארגון הנתונים
################################################################################

# יצירת מפת שמות עבור עמודות (גרסה מפושטת לדוגמה - יש להתאים לשמות האמיתיים בקובץ)
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
  "שכבה" = "stratum"
)

# הקוד הבא יעבוד רק אם השמות במפה קיימים בנתונים האמיתיים
# יש להתאים את שמות העמודות בהתאם לשמות האמיתיים בנתונים

# תחילה, נמיר את השמות המקוריים לשמות באנגלית לפי המיפוי שקיים
cleaned_names <- names(archaeological_data)
for (i in seq_along(column_name_map)) {
  original_name <- names(column_name_map)[i]
  new_name <- column_name_map[i]

  # אם השם המקורי קיים, נחליף אותו
  if (original_name %in% names(archaeological_data)) {
    cleaned_names[cleaned_names == original_name] <- new_name
  }
}

# ניצור עותק עם השמות החדשים
cleaned_data <- archaeological_data
names(cleaned_data) <- cleaned_names

# ניקוי כללי נוסף של שמות העמודות
cleaned_data <- clean_names(cleaned_data)

# בדיקת השמות החדשים
print(names(cleaned_data))

# המרת עמודות מספריות לסוג הנכון
# נשתמש בגישה זהירה - ננסה להמיר רק עמודות מספריות מובהקות
# הקוד הבא הוא גנרי - יש להתאימו לעמודות המספריות האמיתיות בקובץ
numeric_columns <- c("length", "width", "thickness", "weight")
numeric_columns <- numeric_columns[numeric_columns %in% names(cleaned_data)]

if (length(numeric_columns) > 0) {
  cleaned_data <- cleaned_data %>%
    mutate(across(all_of(numeric_columns), ~as.numeric(as.character(.x))))
}

# בדיקת סוגי הנתונים לאחר ההמרה
str(cleaned_data)

# טיפול בערכים חסרים בעמודות מספריות
if (length(numeric_columns) > 0) {
  # חישוב סטטיסטיקה תיאורית לפני הטיפול בערכים חסרים
  numeric_summary_before <- cleaned_data %>%
    select(all_of(numeric_columns)) %>%
    summary()

  print("סיכום עמודות מספריות לפני טיפול בערכים חסרים:")
  print(numeric_summary_before)

  # טיפול בערכים חסרים - מילוי עם ממוצע
  for (col in numeric_columns) {
    if (sum(is.na(cleaned_data[[col]])) > 0) {
      col_mean <- mean(cleaned_data[[col]], na.rm = TRUE)
      cleaned_data[[col]] <- ifelse(is.na(cleaned_data[[col]]), col_mean, cleaned_data[[col]])
    }
  }

  # סיכום אחרי טיפול בערכים חסרים
  numeric_summary_after <- cleaned_data %>%
    select(all_of(numeric_columns)) %>%
    summary()

  print("סיכום עמודות מספריות אחרי טיפול בערכים חסרים:")
  print(numeric_summary_after)
}

################################################################################
# חלק 3: יצירת משתנים חדשים ומדדים מורכבים
################################################################################

# הקוד הבא יעבוד רק אם העמודות הרלוונטיות קיימות - יש להתאים לנתונים האמיתיים
if (all(c("length", "width", "thickness") %in% names(cleaned_data))) {
  enriched_data <- cleaned_data %>%
    mutate(
      # חישוב נפח (אם יש לנו גובה/עובי)
      volume = length * width * thickness,

      # חישוב יחס בין אורך לרוחב - מדד לצורה
      elongation_index = length / width,

      # קטגוריזציה לפי צורה
      shape_category = case_when(
        elongation_index > 3 ~ "מאורך מאוד",
        elongation_index > 2 ~ "מאורך",
        elongation_index > 0.7 & elongation_index < 1.3 ~ "מרובע",
        elongation_index < 0.5 ~ "רחב מאוד",
        TRUE ~ "רגיל"
      ),

      # המרה לפקטור מסודר
      shape_category = factor(
        shape_category,
        levels = c("רחב מאוד", "רגיל", "מרובע", "מאורך", "מאורך מאוד")
      )
    )
} else {
  # אם העמודות הרלוונטיות לא קיימות, נעתיק את הנתונים המנוקים כמו שהם
  enriched_data <- cleaned_data
  # ונוסיף הערה בקונסול
  message("לא ניתן ליצור מדדים מורכבים - חסרות עמודות אורך ורוחב")
}

# ויזואליזציה של התפלגות המשתנים המספריים
if (length(numeric_columns) > 0) {
  distribution_plot <- enriched_data %>%
    select(all_of(numeric_columns)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    facet_wrap(~ variable, scales = "free") +
    theme_minimal() +
    labs(title = "התפלגות המשתנים המספריים")

  print(distribution_plot)
  save_plot(distribution_plot, "variable_distributions")
}

# אם יצרנו משתנים מורכבים, נציג אותם גם
if ("volume" %in% names(enriched_data) && "elongation_index" %in% names(enriched_data)) {
  derived_plot <- enriched_data %>%
    select(volume, elongation_index) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
    facet_wrap(~ variable, scales = "free") +
    theme_minimal() +
    labs(title = "התפלגות המשתנים המורכבים")

  print(derived_plot)
  save_plot(derived_plot, "derived_variable_distributions")

  # פיזור המציג את היחס בין מדד ההארכה לנפח
  scatter_plot <- enriched_data %>%
    ggplot(aes(x = elongation_index, y = volume)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    scale_x_log10() +  # סקלה לוגריתמית לשיפור הקריאות
    scale_y_log10() +
    theme_minimal() +
    labs(
      title = "היחס בין מדד ההארכה לנפח הפריטים",
      x = "מדד הארכה (יחס אורך-רוחב)",
      y = "נפח (יחידות מעוקבות)"
    )

  print(scatter_plot)
  save_plot(scatter_plot, "elongation_volume_scatter")
}

# אם יש לנו קטגוריה של צורה, נציג את ההתפלגות שלה
if ("shape_category" %in% names(enriched_data)) {
  shape_plot <- enriched_data %>%
    count(shape_category) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ggplot(aes(x = shape_category, y = percentage)) +
    geom_col(fill = "steelblue") +
    theme_minimal() +
    labs(
      title = "התפלגות קטגוריות צורה",
      x = "קטגוריית צורה",
      y = "אחוז (%)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(shape_plot)
  save_plot(shape_plot, "shape_category_distribution")
}

################################################################################
# חלק 4: טרנספורמציות נתונים
################################################################################

# 1. טרנספורמציות לוגריתמיות
if (length(numeric_columns) > 0) {
  # יצירת משתנים עם טרנספורמציות שונות
  transformed_data <- enriched_data %>%
    mutate(across(all_of(numeric_columns),
                 list(
                   log10 = ~log10(.x + 1),
                   sqrt = ~sqrt(.x),
                   cube_root = ~sign(.x) * abs(.x)^(1/3)
                 )))

  # לכל משתנה מספרי, ניצור גרף המשווה את ההתפלגות המקורית לטרנספורמציות
  for (col in numeric_columns) {
    # יצירת מסגרת נתונים להשוואה
    comparison_df <- transformed_data %>%
      select(
        original = !!col,
        log10 = !!paste0(col, "_log10"),
        sqrt = !!paste0(col, "_sqrt"),
        cube_root = !!paste0(col, "_cube_root")
      ) %>%
      pivot_longer(everything(),
                  names_to = "transformation",
                  values_to = "value")

    # יצירת הגרף
    transform_plot <- ggplot(comparison_df, aes(x = value)) +
      geom_density(fill = "steelblue", alpha = 0.7) +
      facet_wrap(~ transformation, scales = "free") +
      theme_minimal() +
      labs(
        title = paste("השוואת טרנספורמציות עבור", col),
        x = "ערך לאחר טרנספורמציה",
        y = "צפיפות"
      )

    print(transform_plot)
    save_plot(transform_plot, paste0("transformations_", col))
  }
}

# 2. סטנדרטיזציה (Z-scores)
if (length(numeric_columns) > 0) {
  # חישוב Z-scores
  z_scores_data <- enriched_data %>%
    mutate(across(all_of(numeric_columns),
                 ~scale(.x)[,1],
                 .names = "z_{.col}"))

  # השוואה בין ערכים מקוריים ומתוקננים
  for (col in numeric_columns) {
    z_col <- paste0("z_", col)
    if (z_col %in% names(z_scores_data)) {
      # יצירת מסגרת נתונים להשוואה
      compare_z_df <- z_scores_data %>%
        select(!!col, !!z_col) %>%
        pivot_longer(everything(),
                    names_to = "variable_type",
                    values_to = "value")

      # יצירת הגרף
      z_compare_plot <- ggplot(compare_z_df, aes(x = value)) +
        geom_density(fill = "darkgreen", alpha = 0.7) +
        facet_wrap(~ variable_type, scales = "free") +
        theme_minimal() +
        labs(
          title = paste("השוואה בין ערכים מקוריים ומתוקננים -", col),
          x = "ערך",
          y = "צפיפות"
        )

      print(z_compare_plot)
      save_plot(z_compare_plot, paste0("z_scores_", col))
    }
  }
}

# 3. התאמה לגודל באמצעות ממוצע גיאומטרי
if (length(numeric_columns) >= 3) {  # לפחות 3 עמודות מספריות נדרשות
  # פונקציה לחישוב ממוצע גיאומטרי
  geometric_mean <- function(x, na.rm = TRUE) {
    if (na.rm) x <- x[!is.na(x)]
    if (any(x <= 0, na.rm = TRUE)) return(NA)
    exp(mean(log(x)))
  }

  # חישוב ממוצע גיאומטרי עבור כל שורה
  size_adjusted_data <- enriched_data %>%
    rowwise() %>%
    mutate(
      # חישוב ממוצע גיאומטרי
      geom_mean = geometric_mean(c_across(all_of(numeric_columns)))
    ) %>%
    ungroup()

  # יצירת עמודות מותאמות לגודל
  for (col in numeric_columns) {
    size_adjusted_data <- size_adjusted_data %>%
      mutate(!!paste0(col, "_adj") := !!sym(col) / geom_mean)
  }

  # בחירת זוג משתנים להשוואה (למשל, אורך ורוחב)
  if (all(c("length", "width") %in% numeric_columns)) {
    # השוואה של המשתנים המקוריים מול המותאמים לגודל
    original_scatter <- ggplot(size_adjusted_data, aes(x = length, y = width)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal() +
      labs(
        title = "משתנים מקוריים",
        x = "אורך",
        y = "רוחב"
      )

    adjusted_scatter <- ggplot(size_adjusted_data, aes(x = length_adj, y = width_adj)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal() +
      labs(
        title = "משתנים מותאמים לגודל",
        x = "אורך (מותאם)",
        y = "רוחב (מותאם)"
      )

    # הדפסת הגרפים
    print(original_scatter)
    print(adjusted_scatter)

    save_plot(original_scatter, "original_length_width")
    save_plot(adjusted_scatter, "adjusted_length_width")
  }
}

# 4. נתונים קומפוזיציוניים - אחוזים
if (length(numeric_columns) >= 3) {
  # יצירת נתונים באחוזים
  percentage_data <- enriched_data %>%
    rowwise() %>%
    mutate(
      row_sum = sum(c_across(all_of(numeric_columns)), na.rm = TRUE)
    ) %>%
    ungroup()

  # יצירת עמודות באחוזים
  for (col in numeric_columns) {
    percentage_data <- percentage_data %>%
      mutate(!!paste0(col, "_pct") := !!sym(col) / row_sum * 100)
  }

  # השוואת קורלציות בנתונים מקוריים ובאחוזים
  if (all(c("length", "width") %in% numeric_columns)) {
    # יצירת גרפי פיזור להשוואה
    original_cor <- cor(enriched_data$length, enriched_data$width, use = "complete.obs")
    pct_cor <- cor(percentage_data$length_pct, percentage_data$width_pct, use = "complete.obs")

    original_corr_plot <- ggplot(enriched_data, aes(x = length, y = width)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal() +
      labs(
        title = paste("נתונים מקוריים (קורלציה:", round(original_cor, 2), ")"),
        x = "אורך",
        y = "רוחב"
      )

    pct_corr_plot <- ggplot(percentage_data, aes(x = length_pct, y = width_pct)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal() +
      labs(
        title = paste("נתוני אחוזים (קורלציה:", round(pct_cor, 2), ")"),
        x = "אורך (%)",
        y = "רוחב (%)"
      )

    print(original_corr_plot)
    print(pct_corr_plot)

    save_plot(original_corr_plot, "original_correlation")
    save_plot(pct_corr_plot, "percentage_correlation")
  }
}

# 5. טרנספורמציות log-ratio
if (length(numeric_columns) >= 3) {
  # יצירת נתונים לטרנספורמציית log-ratio
  # ראשית, נטפל בערכי 0 על ידי הוספת ערך קטן
  comp_data <- enriched_data %>%
    select(all_of(numeric_columns)) %>%
    mutate(across(everything(), ~ifelse(.x <= 0, 0.001, .x)))

  # חישוב centered log-ratio (clr)
  if (requireNamespace("compositions", quietly = TRUE)) {
    tryCatch({
      clr_data <- as.data.frame(compositions::clr(as.matrix(comp_data)))

      # השוואת קורלציות בנתונים מקוריים ואחרי טרנספורמציית clr
      if (all(c("length", "width") %in% colnames(clr_data))) {
        # חישוב קורלציות
        original_cor <- cor(comp_data$length, comp_data$width)
        clr_cor <- cor(clr_data$length, clr_data$width)

        # יצירת גרפי פיזור להשוואה
        clr_plot <- data.frame(
          length = clr_data$length,
          width = clr_data$width
        ) %>%
          ggplot(aes(x = length, y = width)) +
          geom_point(alpha = 0.7) +
          geom_smooth(method = "lm", color = "red") +
          theme_minimal() +
          labs(
            title = paste("טרנספורמציית CLR (קורלציה:", round(clr_cor, 2), ")"),
            x = "אורך (CLR)",
            y = "רוחב (CLR)"
          )

        print(clr_plot)
        save_plot(clr_plot, "clr_correlation")
      }
    }, error = function(e) {
      message("שגיאה בחישוב טרנספורמציית CLR: ", e$message)
    })
  } else {
    message("חבילת 'compositions' אינה מותקנת. לא ניתן לבצע טרנספורמציות log-ratio.")
  }
}

################################################################################
# חלק 5: שמירת הנתונים המעובדים
################################################################################

# שמירת הנתונים המעובדים לקובץ CSV
write_csv(enriched_data, "clean_archaeological_data.csv")

# סיכום של תהליך העיבוד
cat("סיכום תהליך עיבוד הנתונים:\n")
cat("----------------------------------------\n")
cat("- מספר תצפיות במקור:", nrow(archaeological_data), "\n")
cat("- מספר תצפיות לאחר ניקוי:", nrow(enriched_data), "\n")
cat("- מספר משתנים במקור:", ncol(archaeological_data), "\n")
cat("- מספר משתנים לאחר עיבוד:", ncol(enriched_data), "\n")
cat("- משתנים מספריים שטופלו:", paste(numeric_columns, collapse = ", "), "\n")
cat("- גרפים שנוצרו נשמרו בתיקייה:", output_dir, "\n")
cat("----------------------------------------\n")

# הודעת סיום
cat("\nתהליך העיבוד הסתיים בהצלחה!\n")
