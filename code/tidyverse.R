# טעינת חבילות
library(tidyverse)
library(janitor)  # חבילה מעולה לניקוי נתונים

# טעינת הנתונים
archaeological_data <- read_csv("https://raw.githubusercontent.com/shaigordin/comparch25/refs/heads/main/slide-decks/data/enc.csv", locale = locale(encoding = "UTF-8"))

# הצצה בנתונים
glimpse(archaeological_data)

#############################################################
## צעד 1: זיהוי מבנה הנתונים וקריאת השדות הנכונים
#############################################################

# קריאה ראשונית בלי הנחות לגבי כותרות
raw_peek <- read_csv("https://raw.githubusercontent.com/shaigordin/comparch25/refs/heads/main/slide-decks/data/enc.csv", col_names = FALSE, n_max = 5)
print(raw_peek)

# ניתן לראות ששמות השדות האמיתיים מופיעים בשורה השנייה!
# נקרא שוב עם הגדרות מתאימות

archaeological_data <- read_csv("https://raw.githubusercontent.com/shaigordin/comparch25/refs/heads/main/slide-decks/data/enc.csv",
                                skip = 1,             # דילוג על שורה ראשונה
                                col_names = TRUE,     # השורה הראשונה שנקרא תהיה כותרות
                                locale = locale(encoding = "UTF-8")) # קידוד עברית

# נבדוק את שמות העמודות החדשים
names(archaeological_data)

# הצגת המבנה החדש
glimpse(archaeological_data)

#############################################################
## צעד 2: יצירת מילון מונחים למשתנים
#############################################################

# יצירת טבלת מילון מונחים
field_dictionary <- tribble(
  ~original_name, ~clean_name, ~meaning, ~category,
  "מס'", "site_id", "מספר ייחודי לאתר הארכיאולוגי", "מידע מנהלי",
  "שם אתר", "site_name", "שם האתר כפי שמופיע באנציקלופדיה", "מידע מנהלי",
  "ערך משנה", "sub_entry", "למקרים שבהם יש ערך משנה בתוך ערך גדול יותר באנציקלופדיה", "מידע מנהלי",
  "נ\"צ X", "coordinate_x", "קואורדינטות האתר ברשת ישראל", "מידע גיאוגרפי",
  "נ\"צ Y", "coordinate_y", "קואורדינטות האתר ברשת ישראל", "מידע גיאוגרפי",
  "אזור", "region", "האזור הגיאוגרפי הכללי בו נמצא האתר", "מידע גיאוגרפי",
  "תת אזור", "sub_region", "חלוקה מפורטת יותר של האזור הגיאוגרפי", "מידע גיאוגרפי",
  "חופר", "excavator", "שמות הארכיאולוגים שביצעו את החפירה", "מידע מנהלי",
  "מחבר הערך", "entry_author", "שם החוקר האחראי על הערך באנציקלופדיה", "מידע ביבליוגרפי",
  "תקופה כללי", "general_period", "התקופה הארכיאולוגית המיוצגת באתר", "מידע כרונולוגי",
  "תק' מעבר", "transition_period", "אתרים או שכבות שהוגדרו כתקופת מעבר", "מידע כרונולוגי",
  "תק' מיוצגת", "represented_period", "תת-תקופה ארכיאולוגית אם קיימת", "מידע כרונולוגי",
  "תארוך ממצא", "find_dating", "מידע מפורט יותר על תאריך הממצא", "מידע כרונולוגי",
  "סוג ממצא", "find_type", "סוג הממצא שהתגלה (מתקן/צור/קבר/קרמיקה/בנייה)", "מידע ארכיאולוגי",
  "סוג אתר", "site_type", "סיווג האתר (תל/ח׳ירבה/מערה/מחצבה וכו׳)", "מידע ארכיאולוגי",
  "אופי אתר", "site_nature", "אופי היישוב (כפרי/עירוני/קבר/שרידים חקלאיים)", "מידע יישובי",
  "גודל אתר", "site_size", "גודל האתר הארכיאולוגי המשוער", "מידע יישובי",
  "גודל חפירה", "excavation_size", "גודל החפירה הארכיאולוגית אם דווח", "מידע מנהלי",
  "הערות", "notes", "נתונים נוספים שלא התאימו לשום שדה אחר", "מידע ביבליוגרפי",
  "הפניה", "bibliographic_reference", "מקור הדיווח על החפירה באנציקלופדיה", "מידע ביבליוגרפי"
)

# הצגת המילון
view(field_dictionary)

#############################################################
## צעד 3: ניקוי וסטנדרטיזציה של שמות המשתנים
#############################################################

# Create mapping from original Hebrew names to English clean names
column_name_map <- setNames(
  field_dictionary$clean_name,
  field_dictionary$original_name
)

# Display original column names for reference
cat("Original column names:\n")
print(names(archaeological_data))

# Apply the mapping to rename columns
cleaned_data <- archaeological_data %>%
  # Rename columns using our mapping - only for columns that exist
  rename_with(~ column_name_map[.x],
              .cols = intersect(names(archaeological_data), names(column_name_map))) %>%
  # Clean any remaining column names
  janitor::clean_names()

# Display the standardized column names
cat("\nStandardized column names:\n")
print(names(cleaned_data))

view(cleaned_data)

#############################################################
## צעד 4: ניצול שמות המשתנים האמיתיים לקבוצות ניתוח
#############################################################

# זיהוי ויצירת קבוצות משתנים לפי קטגוריות
# משתנים מרחביים - מיקום גיאוגרפי ומרחבי
spatial_vars <- c("coordinate_x", "coordinate_y", "region", "sub_region")

# משתנים כרונולוגיים - מיצוב זמני ומחזוריות
chronological_vars <- c("general_period", "transition_period", "represented_period", "find_dating")

# משתנים טיפולוגיים - סיווג ופרשנות חומרית
typological_vars <- c("find_type", "site_type", "site_nature")

# משתנים כמותיים - ממדים מדידים לניתוח סטטיסטי
quantitative_vars <- c("site_size", "excavation_size")

# משתנים מנהליים - תיעוד וניהול חפירות ומחקר
administrative_vars <- c("site_id", "site_name", "sub_entry", "excavator",
                         "entry_author", "notes", "bibliographic_reference")

# רשימות משתנים מסוננות על סמך מערך הנתונים בפועל
existing_spatial_vars <- intersect(spatial_vars, names(cleaned_data))
existing_chronological_vars <- intersect(chronological_vars, names(cleaned_data))
existing_typological_vars <- intersect(typological_vars, names(cleaned_data))
existing_quantitative_vars <- intersect(quantitative_vars, names(cleaned_data))

# ניתוח כרונולוגי-טיפולוגי - התפלגות סוגי ממצא על פני תקופות
if(length(existing_chronological_vars) > 0 && length(existing_typological_vars) > 0) {
  # Select the most appropriate period variable
  period_var <- existing_chronological_vars[1]
  type_var <- existing_typological_vars[1]

  # טבלת התפלגות כרונולוגית-טיפולוגית
  period_type_distribution <- cleaned_data %>%
    count(.data[[period_var]], .data[[type_var]]) %>%
    spread(.data[[type_var]], n, fill = 0)

  # הצג התפלגות
  cat("\nCHRONOLOGICAL-TYPOLOGICAL DISTRIBUTION:\n")
  print(head(period_type_distribution))
}

## ניתוח גודל אתר לפי תקופה
# ראשית, בדוק את מבנה הנתונים כדי להבין את חוסר העקביות בסוג הנתונים
site_size_types <- cleaned_data %>%
  group_by(general_period) %>%
  summarise(
    n_rows = n(),
    data_type = first(class(site_size)),
    example_values = paste(head(site_size, 2), collapse = ", ")
  )

print("DATA TYPE ANALYSIS BY PERIOD:")
print(site_size_types)
view(site_size_types)

#############################################################
## צעד 5: טיפול במשתנים קטגוריאליים עם שמות אמיתיים
#############################################################

# יצירת מטריצת תפוצה טיפולוגית-כרונולוגית
# שימוש בשמות משתנים סטנדרטיים המבוססים על מיפוי הנתונים הקודם שלנו
find_period_summary <- cleaned_data %>%
  # Use standardized field names: represented_period for chronology, find_type for typology
  count(represented_period, find_type) %>%
  arrange(represented_period, desc(n))
view(find_period_summary)

# סידור כרונולוגי המבוסס על חלוקת תקופות ארכיאולוגיות
# The archaeological chronology must be adapted to match actual periods in dataset
cleaned_data <- cleaned_data %>%
  mutate(
    # Converting period to factor with archaeologically-informed chronological order
    # This sequence reflects standard Near Eastern archaeological periodization
    represented_period = factor(represented_period),
    # Converting typological classification to factor
    # Using standardized field name for find type
    find_type = factor(find_type)
  )
summary(cleaned_data$represented_period)
summary(cleaned_data$find_type)

# יצירת ניתוח התפלגות כרונולוגי
period_counts <- cleaned_data %>%
  count(represented_period) %>%
  arrange(represented_period)

# הצגת דפוסי התפלגות כרונולוגיים
print(period_counts)
view(period_counts)

plot(cleaned_data$find_type)

#############################################################
## צעד 6: טרנספורמציות בסיסיות עם dplyr
#############################################################

# יצירת עותק של הנתונים לעבודה
working_data <- period_type_distribution %>%
  # נבחר עמודות מספריות לדוגמה
  select(where(is.numeric)) %>%
  # ניצור טרנספורמציה פשוטה - המרה מיחידות אחת לאחרת
  mutate(
    normalized_value = .[[2]] / 10, # לדוגמה: המרה לס"מ
    log_value = log10(.[[2]] + 1)   # טרנספורמציה לוגריתמית עם הוספת 1 למניעת log(0)
  )

# הצגת התוצאות
head(working_data)


# טרנספורמציות נפוצות
transformed_data <- period_type_distribution %>%
  select(where(is.numeric)) %>%
  mutate(
    log10_transform = log10(.[[1]] + 1),          # לוגריתם בבסיס 10
    natural_log = log(.[[1]] + 1),                # לוגריתם טבעי
    sqrt_transform = sqrt(.[[1]]),                # שורש ריבועי
    inverse_transform = 1 / (.[[1]] + 0.001),     # טרנספורמציית הופכי
    cube_root = sign(.[[1]]) * abs(.[[1]])^(1/3)     # שורש שלישי
  )

# הצגת ההשפעה על ההתפלגות
hist_original <- hist(transformed_data$natural_log, main = "התפלגות מקורית")
hist_log <- hist(transformed_data$log10_transform, main = "התפלגות אחרי טרנספורמציה לוגריתמית")


# חישוב מתוקנן Z (Z-scores)
standardized_data <- period_type_distribution %>%
  select(`general_period`, where(is.numeric)) %>%
  mutate(across(where(is.numeric), ~scale(.x)[,1], .names = "z_{.col}"))

# הצגת התוצאות
head(standardized_data)

# ויזואליזציה של נתונים מקוריים מול מתוקננים
library(ggplot2)

# נשתמש בפונקציית pivot_longer מחבילת tidyr
comparison_data <- standardized_data %>%
  select(`general_period`,
         `ארכיטקטורה`
         , `z_ארכיטקטורה`) %>%
  pivot_longer(cols = c(`ארכיטקטורה`
                        , `z_ארכיטקטורה`),
               names_to = "variable_type",
               values_to = "value")

# נוצר גרף השוואה
comparison_plot <- ggplot(comparison_data, aes(x = value)) +
  geom_density() +
  facet_wrap(~ variable_type, scales = "free") +
  theme_minimal() +
  labs(title = "השוואה בין נתונים מקוריים למתוקננים")

ggsave("comparison_plot.png", comparison_plot, width = 8, height = 5)


# שימוש בחבילת car לטרנספורמציית Box-Cox
library(car)

# בחירת מספר ערכי למבדא לבדיקה
lambda_values <- c(-2, -1, -0.5, 0, 0.5, 1, 2)

# יצירת טרנספורמציות Box-Cox (רק למשתנים חיוביים)
positive_values <- period_type_distribution %>%
  filter(`ארכיטקטורה` > 0) %>%
  pull(`ארכיטקטורה`)

# חישוב טרנספורמציות Box-Cox שונות
box_cox_transforms <- sapply(lambda_values, function(lambda) {
  if (lambda == 0) {
    return(log(positive_values))
  } else {
    return((positive_values^lambda - 1) / lambda)
  }
})

plot(box_cox_transforms)
