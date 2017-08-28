## Web Scraping Recipes from Online Blogs
## Author: B. Himmetoglu
## This sript scrapes recipes from The Full Helping: http://www.thefullhelping.com/
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
  page <- read_html(paste0("http://www.thefullhelping.com/recipes/?fwp_paged=", as.character(page_number)))
  links <- html_nodes(page, "a")
  
  # Get locations of recipe links
  loc <- which(str_detect(links, "<a class"))
  links <- links[loc]
  
  # Some links are not recipe related: filter them out
  mask <- map_lgl(links, str_detect, "entry-image-link")
  links <- links[mask]
  
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
  
  # Get the node "span" 
  span <- html_nodes(page, "span")
  
  # Get the node "time"
  time <- html_nodes(page, "time")
  
  # Get the node "li"
  li <- html_nodes(page, "li")
  
  # Get the node "div"
  div <- html_nodes(page, "div")
  
  # Get recipe name
  title <- html_nodes(page, "title")
  recipe_name <- title[1] %>% 
    str_replace_all("\"|/|<|>", " ") %>%
    str_replace_all("title", "") %>%
    str_split("\\|")
  recipe_name <- recipe_name[[1]][1] %>% str_trim()
  
  # Get recipe rating (an if statement is probably needed for recipes with no ratings)
  temp <- span[map_lgl(span,str_detect, "ratingValue")]
  if (length(temp) > 0){
    temp1 <- temp[2] %>% str_extract_all("[0-9]")
    rating_value <- as.numeric(paste0(temp1[[1]],collapse = "."))
  } else {
    rating_value <- NA
  }

  # Get rating count (again, if statement)
  temp <- span[map_lgl(span,str_detect, "ratingCount")]
  if (length(temp) > 0){
    temp1 <- temp[2] %>% str_extract_all("[0-9]")
    rating_count <- as.numeric(temp1[[1]])
  } else {
    rating_count <- NA
  }
  
  # Recipe category
  recipe_categories <- span[map_lgl(span, str_detect, "recipeCategory")] %>% 
    str_replace_all("\"|/|<|>", " ") %>% 
    str_replace_all("span|itemprop=|recipeCategory", "") %>%
    str_trim()
  if (length(recipe_categories) == 0){ recipe_categories <- NA }
  
  # Recipe cuisine
  recipe_cuisine <- span[map_lgl(span, str_detect, "recipeCuisine")] %>%
    str_replace_all("\"|/|<|>", " ") %>%
    str_replace_all("span|itemprop=|recipeCuisine", "") %>%
    str_trim()
  if (length(recipe_cuisine) == 0){ recipe_cuisine <- NA } 
  
  # Recipe author
  recipe_author <- span[map_lgl(span, str_detect, "itemprop=\"author\"")] %>%
    str_replace_all("\"|/|<|>", " ") %>%
    str_replace_all("span|itemprop=|author", "") %>%
    str_trim()
  if (length(recipe_author) == 0){ recipe_author <- NA }
  
  # Prep time
  prep_time <- time[map_lgl(time, str_detect, "itemprop=\"prepTime\"")] %>%
    str_extract("PT[:alnum:]*")
  if (length(prep_time) == 0){ prep_time <- NA }
  
  # Cook time
  cook_time <- time[map_lgl(time, str_detect, "itemprop=\"cookTime\"")] %>%
    str_extract("PT[:alnum:]*")
  if (length(cook_time) == 0){ cook_time <- NA }
  
  # Total time
  tot_time <- time[map_lgl(time, str_detect, "itemprop=\"totalTime\"")] %>%
    str_extract("PT[:alnum:]*")
  if (length(tot_time) == 0){ tot_time <- NA }
  
  # Recipe yield
  recipe_yield <- span[map_lgl(span, str_detect, "itemprop=\"recipeYield\"")] %>%
    str_replace_all("\"|/|<|>", " ") %>%
    str_replace_all("span|itemprop=|recipeYield", "") %>%
    str_trim()
  if (length(recipe_yield) == 0){ recipe_yield <- NA }
  
  # Ingredients
  ingredients <- li[map_lgl(li, str_detect, "class=\"ingredient\"")] %>%
    str_replace_all("\"|/|<|>", " ") %>%
    str_replace_all("li|class=|itemprop=|(ingredients*)", "") %>% 
    str_trim() %>%
    paste(collapse = "\n ")
  if ((ingredients == "")|(length(ingredients) == 0) ){ ingredients <- NA }
  
  # Instructions
  instructions <- li[map_lgl(li, str_detect, "class=\"instruction\"")] %>%
    str_replace_all("\"|/|<|>", " ") %>%
    str_replace_all("li|class=|itemprop=|recipeInstructions|instruction", "") %>% 
    str_trim() %>%
    paste(collapse = "\n ")
  if ((instructions == "")|(length(instructions) == 0)){ instructions <- NA }
  
  # Return data frame (combined with nutrition information)
  df <- data.frame(name=recipe_name, 
                   rating_value = rating_value,
                   rating_count = rating_count,
                   recipe_author = recipe_author,
                   recipe_cuisine = recipe_cuisine,
                   recipe_categories = recipe_categories,
                   prep_time = prep_time,
                   cook_time = cook_time,
                   tot_time = tot_time,
                   recipe_yield = recipe_yield,
                   ingredients = ingredients,
                   instructions = instructions,
                   stringsAsFactors = FALSE)
                   
  df
}

# Function for getting the images
get_recipe_image <- function(link_to_recipe, img_folder="./images"){
  # Get the recipe page
  page <- read_html(link_to_recipe)
  
  # Get the node "meta" 
  meta <- html_nodes(page, "meta")
  url <- meta[map_lgl(meta, str_detect, "property=\"og:image\"")] %>%
    str_extract("(http)(.*)(jpeg|jpg|png)")
  
  # Image name
  if (!is.na(url)){
    temp <- str_split(url, "/")[[1]]
    loc <- which(str_detect(temp, "(jpg|png|jpeg)"))
    image_name <- temp[loc]
    
    # Download file
    download.file(url, destfile = paste(img_folder, image_name, sep = "/"), quiet = TRUE)
  } else {
    image_name <- NA
  }

  # Return as data frame for reference
  data_frame(name = image_name, imageURL = url)
}
