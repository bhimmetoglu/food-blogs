## Web Scraping Recipes from Online Blogs
## Author: B. Himmetoglu
## Scrape Site: http://pinchofyum.com/
#
# Load libraries
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(rvest)
library(jsonlite)
library(lubridate)

# Load the functions in utilities
source("utilities.R")

## Get all the links (takes a while to run)
if(!file.exists("all_links.RData")){
  # Get all the recipe links
  all_links <- 1:50 %>% map(get_recipe_links) %>% unlist()
  
  # Save for future use
  save(all_links, file="all_links.RData")
} else {
  # If already done, just load them
  load(file="all_links.RData")
}

## Now that we have all the links, let's collect all the recipes
if (!file.exists("all_recipes.RData")){
  # Initiate
  all_recipes_df <- get_recipe_data(all_links[1]) # Initiation
  
  # I will use a for look just to see the progress (instead of map_df)
  for (ii in 2:length(all_links)){
    link_to_recipe <- all_links[ii]
    # If NULL is returned continue with next iteration
    if (is.null(get_recipe_data(link_to_recipe))){
      next
    }
    all_recipes_df <- rbind(all_recipes_df, get_recipe_data(link_to_recipe))
    cat("Scraped recipe: ", ii, " of ", length(all_links), "\n")
  }
  
  # Save for later use
  save(all_recipes_df, file="all_recipes.RData")
} else {
  load(file = "all_recipes.RData")
}

## Now that we have all the links, let's collect all the photos
if(!file.exists("all_photos.RData")){
  # Initiate
  all_photos_df <- get_recipe_image(all_links[1])
  
  # I will use a for look just to see the progress (instead of map_df)
  for (ii in 2:length(all_links)){
    link_to_recipe <- all_links[ii]
    # If NULL is returned continue with next iteration
    if (is.null(get_recipe_image(link_to_recipe))){
      next
    }
    all_photos_df <- rbind(all_photos_df, get_recipe_image(link_to_recipe))
    cat("Scraped recipe image: ", ii, " of ", length(all_links), "\n")
  }
  
  # Save for later use
  save(all_photos_df, file="all_photos.RData")
} else {
  load(file="all_photos.RData")
}

