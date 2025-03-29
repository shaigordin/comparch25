# Load packages
install.packages("png")

library(httr)
library(jsonlite)
library(base64enc)
library(png)

# Secure credential management as previously discussed
# Token loaded from .Renviron or keyring

# Function for image generation via Hugging Face API
hf_image_generation <- function(model_id, prompt, negative_prompt = NULL, parameters = NULL) {
  API_URL <- paste0("https://api-inference.huggingface.co/models/", model_id)

  # Construct payload with scholarly attention to parameter configuration
  body <- list(inputs = prompt)

  if (!is.null(negative_prompt)) {
    body$negative_prompt <- negative_prompt
  }

  if (!is.null(parameters)) {
    body <- c(body, parameters)
  }

  # Execute API request with appropriate authentication
  response <- POST(
    url = API_URL,
    add_headers(
      Authorization = paste("Bearer", Sys.getenv("HF_API_TOKEN")),
      "Content-Type" = "application/json"
    ),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )

  # Return binary image data
  content(response, "raw")
}

# Image processing and serialization function
save_generated_image <- function(image_data, file_path) {
  writeBin(image_data, file_path)
  cat("Image successfully preserved at:", file_path, "\n")
}

# Implementation case study: Mesopotamian artifact visualization
# Using Stable Diffusion XL model for high-fidelity representation
mesopotamian_visualization <- hf_image_generation(
  model_id = "stabilityai/stable-diffusion-xl-base-1.0",
  prompt = "Detailed Neo-Assyrian palace relief depicting royal lion hunt scene, Nineveh, 7th century BCE, archaeological artifact, museum quality",
  negative_prompt = "modern elements, inaccurate historical details, low quality",
  parameters = list(
    num_inference_steps = 50,
    guidance_scale = 7.5,
    width = 1024,
    height = 768
  )
)

# Preservation of digital artifact
save_generated_image(mesopotamian_visualization, "neo_assyrian_relief_reconstruction.png")
