## Setting Up Hugging Face API in R

# Go to huggingface.co and sign up for a free account
# Navigate to your profile → Settings → Access Tokens
# Create a new token (select "Read" role for free access)

# Install Required R Packages
install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

# Create .Renviron file with API token (NOT to be committed to git)
# Token is loaded from .Renviron automatically
# Add to .gitignore
# .Renviron
# .env

hf_inference <- function(model_id, inputs, task = "text-generation", parameters = NULL) {
  API_URL <- paste0("https://api-inference.huggingface.co/models/", model_id)

  body <- list(inputs = inputs)
  if (!is.null(parameters)) {
    body$parameters <- parameters
  }

  response <- POST(
    url = API_URL,
    add_headers(
      Authorization = paste("Bearer", Sys.getenv("HF_API_TOKEN")),
      "Content-Type" = "application/json"
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )

  content(response)
}

# Text generation with Mistral-7B-Instruct
result <- hf_inference(
  model_id = "mistralai/Mistral-7B-Instruct-v0.2",
  inputs = "<s>[INST] What is the capital of France? [/INST]",
  parameters = list(
    max_new_tokens = 100,
    temperature = 0.7,
    return_full_text = FALSE
  )
)

# Print result
cat(result[[1]]$generated_text)

result <- hf_inference(
  model_id = "deepset/roberta-base-squad2",
  inputs = list(
    question = "What is the capital of France?",
    context = "France is a country in Western Europe. Its capital is Paris."
  )
)

# Print answer
cat("Answer:", result$answer)
