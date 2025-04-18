
# יצירת וקטור של עומקי שכבות ארכיאולוגיות (במטרים)
depths <- c(0.5, 1.2, 1.8, 2.3, 3.0)

# יצירת וקטור של תיארוכים (לפני הספירה)
datings <- c(-1200, -900, -850, -720, -586)

# שמות התקופות המתאימות
periods <- c("ברזל 1", "ברזל 2א", "ברזל 2ב", "ברזל 2ג", "חורבן בית ראשון")

# יצירת טבלה של הנתונים
stratigraphy_data <- data.frame(
  depth = depths,
  dating = datings,
  period = periods
)

# הצגת הטבלה
stratigraphy_data

# יצירת וקטור המייצג מספר חרסים מתקופות שונות
pottery_count <- c(45, 78, 134, 89, 23, 56)
periods <- c("ברונזה מאוחרת", "ברזל 1", "ברזל 2א", "ברזל 2ב", "פרסית", "הלניסטית")

# חישוב סך כל החרסים
total_sherds <- sum(pottery_count)
total_sherds

# חישוב ממוצע החרסים לתקופה
average_sherds <- mean(pottery_count)
average_sherds

# איתור התקופה עם מספר החרסים הגבוה ביותר
max_period_index <- which.max(pottery_count)
most_abundant_period <- periods[max_period_index]
most_abundant_period

# נניח שיש לנו נתונים על אתרים ארכיאולוגיים
site_data <- data.frame(
  site_name = c("תל חצור", "תל לכיש", "תל מגידו", "תל דן", "תל באר שבע"),
  elevation = c(230, 270, 180, 200, 280),
  site_size = c(80, 30, 60, 20, 10),  # בדונמים
  has_fortifications = c(TRUE, TRUE, TRUE, FALSE, TRUE)
)

# מציאת אתרים בגובה מעל 200 מטר
high_elevation_sites <- site_data[site_data$elevation > 200, ]
high_elevation_sites$site_name

# מציאת אתרים גדולים (מעל 50 דונם) עם ביצורים
large_fortified_sites <- site_data[site_data$site_size > 50 &
                                     site_data$has_fortifications == TRUE, ]
large_fortified_sites$site_name


# פונקציה לחישוב צפיפות ממצאים לפי שטח החפירה
calc_artifact_density <- function(artifact_count, excavation_area) {
  density <- artifact_count / excavation_area
  return(density)
}

# שימוש בפונקציה
pottery_count <- 450
excavation_area <- 25  # במ"ר
pottery_density <- calc_artifact_density(pottery_count, excavation_area)
pottery_density  # חרסים למ"ר


# יצירת גרף מספר אתרים לפי תקופה
periods <- c("ברונזה תיכונה", "ברונזה מאוחרת", "ברזל 1", "ברזל 2", "פרסית", "הלניסטית")
sites_count <- c(65, 34, 78, 120, 60, 95)

# גרף עמודות בסיסי
barplot(sites_count, names.arg = periods,
        main = "מספר אתרים לפי תקופה",
        col = "lightblue", las = 2)

# טעינת החבילה
library(ggplot2)

# יצירת נתונים לגרף
settlement_data <- data.frame(
  period = periods,
  sites = sites_count
)

# יצירת גרף עמודות מתקדם
ggplot(settlement_data, aes(x = period, y = sites, fill = period)) +
  geom_col() +
  labs(title = "מספר אתרים לפי תקופה",
       x = "תקופה", y = "מספר אתרים") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
