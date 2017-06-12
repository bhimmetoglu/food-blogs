## Web Scraping Recipes from Online Blogs
## Author: B. Himmetoglu
## This sript scrapes recipes from Pinch of Yum
#
# Load libraries
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(rvest)
library(jsonlite)
library(lubridate)


# Function for connecting to a given page of the blog and getting all the links to all the recipes
get_recipe_links <- function(page_number){
  page <- read_html(paste0("http://pinchofyum.com/recipes?fwp_paged=", as.character(page_number)))
  links <- html_nodes(page, "a")
  
  # Get locations of recipe links
  loc <- which(str_detect(links, "<a class"))
  links <- links[loc]
  
  # Function for trimming each link
  trim_ <- function(link){
    temp1 <- str_split(link, " ")[[1]][3] %>%
      str_replace_all("\"", "") %>% # Remove \'s
      str_replace("href=", "") %>%
      str_replace(">", " ")
    temp1 <- str_split(temp1, " ")[[1]][1]
    
    temp1
  }
  
  # Trim the text to get proper links
  all_recipe_links <- map_chr(links, trim_)
  
  # Return
  all_recipe_links
}

# This function creates a (single entry) data frame from a link to a given recipe
get_recipe_data <- function(link_to_recipe){
  # Get the recipe page
  page <- read_html(link_to_recipe)
  
  # JSON data
  script_ <- html_nodes(page, "script")
  loc_json <- which(str_detect(script_, "application/ld"))
  if (length(loc_json) == 0){
    return(NULL) # If the link does not contain a recipe, just return null
  }
  
  # Load the data to JSON
  recipe_data <- fromJSON(html_text(script_[loc_json]))
  
  ## From JSON get relevant features
  # Ingredients
  if ("recipeIngredient" %in% names(recipe_data)){
    ingredients <- paste(recipe_data$recipeIngredient, collapse="\n")
  } else if ("ingredients" %in% names(recipe_data)){
    ingredients <- paste(recipe_data$ingredients, collapse="\n")
  }
  
  
  # Instructions
  instructions <- paste(recipe_data$recipeInstructions, collapse="\n")
  
  # Prep & Cook time
  prepTime <- ifelse("prepTime" %in% names(recipe_data), 
                     recipe_data$prepTime,
                     NA)
  cookTime <- ifelse("cookTime" %in% names(recipe_data), 
                     recipe_data$cookTime,
                     NA)
  
  # Description
  description <- ifelse("description" %in% names(recipe_data),
                        recipe_data$description, 
                        NA)
  
  # Reviews
  if ("aggregateRating" %in% names(recipe_data)){
    nReviews <- recipe_data$aggregateRating$reviewCount
    rating=recipe_data$aggregateRating$ratingValue
  } else {
    nReviews <- NA
    rating <- NA
  }

  # Nutritional data
  get_nutrition <- function(recipe_data){
    # Initiate empty df with NAs
    nutrition_df <- data.frame(servingSize = NA, calories = NA, sugarContent = NA, sodiumContent = NA,
                               fatContent = NA, saturatedFatContent = NA, transFatContent = NA, 
                               carbohydrateContent = NA, fiberContent = NA, proteinContent = NA,
                               cholesterolContent = NA, stringsAsFactors = FALSE)
    
    # Check of nurition is included in data
    if (!("nutrition" %in% names(recipe_data))){
      return(nutrition_df)
    }
    
    # Get nuirition features
    features <- names(nutrition_df)
    for (ii in 1:length(features)){
      value <- ifelse(features[ii] %in% names(recipe_data$nutrition),
                     recipe_data$nutrition[features[ii]],
                     "NA")[[1]]
      nutrition_df[features[ii]] <- ifelse(is.null(value), NA, value)
    }
    # Return 
    nutrition_df
  }
  
  # Nutritional values
  nutrition_df <- get_nutrition(recipe_data)
  
  # Return data frame (combined with nutrition information)
  df_1 <- data.frame(name=recipe_data$name, 
                     description=description,
                     ingredients=ingredients,
                     prepTime=prepTime,
                     cookTime=cookTime,
                     nReviews=nReviews,
                     rating=rating, stringsAsFactors = FALSE)
  
  bind_cols(df_1, nutrition_df)
}

# Function for getting the images
get_recipe_image <- function(link_to_recipe, img_folder="./images"){
  # Get the recipe page
  page <- read_html(link_to_recipe)
  
  # JSON data
  script_ <- html_nodes(page, "script")
  loc_json <- which(str_detect(script_, "application/ld"))
  if (length(loc_json) == 0){
    return(NULL) # If the link does not contain a recipe, just return null
  }
  
  # Load the data to JSON
  recipe_data <- fromJSON(html_text(script_[loc_json]))
  
  # Image data
  img_link <- html_nodes(page, "meta")
  img_link <- img_link[which(str_detect(img_link, "og:image"))][1]
  
  # Function for trimming each link
  trim_ <- function(link){
    temp1 <- str_split(link, " ")[[1]][3] %>%
      str_replace_all("\"", "") %>% # Remove \'s
      str_replace("content=", "") %>%
      str_replace(">", " ")
    temp1 <- str_split(temp1, " ")[[1]][1]
    
    temp1
  }

  # URL of image
  url <- trim_(img_link)
  
  # Download the file
  temp <- str_split(url, "/")[[1]]
  loc <- which(str_detect(temp, "(jpg|png|jpeg)"))
  image_name <- temp[loc]
  download.file(url, destfile = paste(img_folder, image_name, sep = "/"), quiet = TRUE)
  
  # Return as data frame for reference
  data_frame(name = recipe_data$name, imageURL = url)
}
