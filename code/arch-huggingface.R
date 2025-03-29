# התקנת חבילות נדרשות (פעם אחת בלבד)
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")

# טעינת חבילות
library(tidyverse)
library(httr)
library(jsonlite)
library(plotly)

# יצירת נתוני דוגמה של ממצאים ארכיאולוגיים
set.seed(123)  # לשחזוריות התוצאות
artifacts <- tibble(
  x = runif(200, 0, 10),          # קואורדינטה מזרח-מערב
  y = runif(200, 0, 10),          # קואורדינטה צפון-דרום
  period = sample(c("תקופת הברונזה", "תקופת הברזל", "התקופה ההלניסטית"),
                  200, replace = TRUE)  # תקופה היסטורית
)

# הצגת מבנה הנתונים
head(artifacts)

# Create interactive plot with proper RTL text handling
plotly_visualization <- ggplotly(
  ggplot(artifacts, aes(x = x, y = y, color = period)) +
    geom_point(alpha = 0.7) +
    theme_minimal()
) %>%
  layout(
    title = list(text = "התפלגות ממצאים ארכיאולוגיים לפי תקופה"),
    xaxis = list(title = "קואורדינטה מזרח-מערב"),
    yaxis = list(title = "קואורדינטה צפון-דרום"),
    legend = list(title = list(text = "תקופה"))
  )

# Save as interactive HTML with proper bidirectional text support
htmlwidgets::saveWidget(plotly_visualization, "archaeological_distribution.html")

# הגדרת פונקציה לשימוש ב-Huggingface API
analyze_with_ai <- function(data_summary, prompt,
                            api_key = Sys.getenv("HF_API_TOKEN"),
                            model_id = "mistralai/Mistral-7B-Instruct-v0.2") {

  # בדיקה שיש מפתח API
  if (api_key == "") {
    stop("נא להגדיר מפתח API של Huggingface")
  }

  # הכנת הנתונים עבור ה-API
  artifacts_summary <- capture.output(summary(data_summary))
  artifacts_str <- paste(artifacts_summary, collapse = "\n")

  # יצירת הבקשה ל-API
  endpoint <- paste0("https://api-inference.huggingface.co/models/", model_id)
  # בניית הבקשה בפורמט המתאים למודל
  full_prompt <- paste0(
    "להלן נתונים על ממצאים ארכיאולוגיים:\n\n",
    artifacts_str,
    "\n\n",
    prompt
  )

  payload <- list(
    inputs = full_prompt,
    parameters = list(
      max_new_tokens = 500,
      temperature = 0.7,
      return_full_text = TRUE
    )
  )

  # שליחת הבקשה ל-API
  response <- POST(
    url = endpoint,
    body = toJSON(payload, auto_unbox = TRUE),
    add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    encode = "json"
  )
  # טיפול בתשובה
  if (http_status(response)$category == "Success") {
    content <- content(response, "parsed")
    if (is.list(content) && length(content) > 0 && "generated_text" %in% names(content[[1]])) {
      return(content[[1]]$generated_text)
    } else {
      return("התקבלה תשובה בפורמט לא צפוי")
    }
  } else {
    return(paste("שגיאה:", http_status(response)$message))
  }
}

# ניתוח כמותי בסיסי של הנתונים
period_counts <- artifacts %>% count(period)
print(period_counts)

# שאלה לניתוח ב-AI
analysis_prompt <- "אני חוקר/ת ארכיאולוג/ית. על סמך ההתפלגות של הממצאים:
1. האם ניתן לזהות דפוסים מעניינים בפיזור המרחבי?
2. מה המשמעות הארכיאולוגית האפשרית של ההתפלגות הנצפית?
3. אילו בדיקות נוספות היית ממליץ/ה לבצע?
אנא ספק/י ניתוח מקצועי מנקודת מבט ארכיאולוגית."

# הרצת הניתוח (הורידו את ה-# כאשר יש לכם מפתח API)
# Sys.setenv(HUGGINGFACE_API_KEY = "your_api_key_here")
ai_analysis <- analyze_with_ai(artifacts, analysis_prompt)
cat(ai_analysis)


# Initialize visualization framework with period-specific parameters
periods <- unique(artifacts$period)
plot_objects <- list()

# Implement period-specific analytical units with direct density estimation
for(i in seq_along(periods)) {
  period_data <- filter(artifacts, period == periods[i])

  # Construct individual period visualization with integrated density estimation
  p <- plot_ly(data = period_data, x = ~x, y = ~y, type = "scatter",
               mode = "markers", marker = list(opacity = 0.7),
               name = periods[i]) %>%
    layout(
      title = periods[i],
      xaxis = list(title = "קואורדינטות מזרח-מערב"),
      yaxis = list(title = "קואורדינטות צפון-דרום")
    )

  # Add direct density visualization without matrix transformation
  # Calculate kernel density estimate
  kd <- MASS::kde2d(period_data$x, period_data$y, n = 50)

  # Add contour visualization directly from density object
  p <- p %>% add_contour(
    x = kd$x,
    y = kd$y,
    z = kd$z,  # Direct matrix representation
    contours = list(coloring = "heatmap"),
    colorscale = "Viridis",
    opacity = 0.3,
    showlegend = FALSE
  )

  plot_objects[[i]] <- p
}

# Synthesize period-specific analytical units into comparative framework
composite_visualization <- subplot(
  plot_objects,
  nrows = 1,
  shareY = TRUE,
  titleX = TRUE,
  margin = 0.05
) %>%
  layout(
    title = list(
      text = "ניתוח צפיפות ממצאים ארכיאולוגיים לפי תקופה",
      font = list(size = 18)
    ),
    margin = list(l = 100, r = 50, t = 100, b = 80),
    annotations = list(
      list(x = 0.16, y = 1.05, text = periods[1], showarrow = FALSE, xref = "paper", yref = "paper"),
      list(x = 0.5, y = 1.05, text = periods[2], showarrow = FALSE, xref = "paper", yref = "paper"),
      list(x = 0.84, y = 1.05, text = periods[3], showarrow = FALSE, xref = "paper", yref = "paper")
    )
  ) %>%
  config(locale = "he")

# Materialize analytical framework as interactive document
htmlwidgets::saveWidget(composite_visualization, "archaeological_density_analysis.html", selfcontained = TRUE)
