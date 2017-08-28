## Web Scraping Recipes from Online Blogs
## Author: B. Himmetoglu
## Modelling
#
# Load libraries
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(rvest)
library(jsonlite)
library(lubridate)
library(tidytext)
library(tokenizers)
library(syuzhet)
library(ggplot2)

# Load the data.frame of all recipes
load(file="all_recipes.RData")

# NAs
all_recipes_df %>% map_dbl(~sum(is.na(.x)))

# Empty descriptions
all_recipes_df %>% filter(description =="") %>% count() # Only 6

# Let's assign and ID number
all_recipes_df <- all_recipes_df %>% mutate(ID = 1:nrow(all_recipes_df))

# Let's check the ingredient & rating relationship
df_ingrdt <- all_recipes_df %>% 
  select(ID, ingredients) %>%
  mutate(ingredients = str_replace(ingredients, "\n", " ") %>% str_replace("<.*?>", " ")) %>%
  unnest_tokens(word, ingredients)

# Store ratings and nReviews in a separate data frame
df_reviews <- all_recipes_df %>% 
  select(ID, name, rating, nReviews)

# Let's look at the top 10 most commonly used words
df_ingrdt %>% count(word, sort = TRUE) %>% slice(1:10)

# Remove stop words
data("stop_words")

# Also remove the following (which is not included in stopwords)
word_remove = c("cup", "cups", "teaspoon", "teaspoons", "tablespoon", "tablespoons", 
                "ounce", "ounces", "lb", "lbs", "tbs", "tsp", "oz", "handful", "handfull",
                "inch", "i", "can")

df_ingrdt <- df_ingrdt %>% 
  filter(!(word %in% stopwords())) %>%
  filter(!(word %in% word_remove)) %>%
  filter(!(str_detect(word, "[0-9]")))  # Remove numbers as well

# Now let's lokk at the top 10 most commonly used words
df_ingrdt %>% count(word, sort = TRUE) %>% slice(1:10)

# Get the most common 25 words in the ingredients to be used in modellling
top_words <- df_ingrdt %>% count(word, sort = TRUE) %>% slice(1:25)
df_ingrdt <-df_ingrdt %>%
  filter(word %in% top_words$word)

# Spread the word counts to columns
df_ingrdt <- df_ingrdt %>% 
  group_by(ID) %>%
  count(word) %>%
  spread(key = word, value = n, fill = 0) %>%
  ungroup()

# We actually do not want word couts per recipe, rather one-hot encoded word features
vars <- setdiff(names(df_ingrdt), "ID")
df_ingrdt <- df_ingrdt %>%
  mutate_at(vars, function(x) ifelse(x > 0, 1, 0))

# Now combine with df_reviews by ID
df_ingrdt_reviews <- df_reviews %>%
  inner_join(df_ingrdt, by = "ID")

# NAs
df_ingrdt_reviews %>% map_dbl(~sum(is.na(.x)))

# Let's just look at recipes with ratings
df_ingrdt_reviews <- df_ingrdt_reviews %>%
  mutate(rating = as.numeric(rating)) %>%
  mutate(nReviews = as.numeric(nReviews))


## Visualization

# Distribution of reviews
g0 <- ggplot(df_ingrdt_reviews, aes(rating)) + 
  geom_histogram(color = "black", fill = "slateblue1", binwidth = 0.1) + 
  geom_vline(xintercept = median(df_ingrdt_reviews$rating, na.rm = T), na.rm = TRUE, size = 0.6) + 
  ylab("Counts") + ggtitle("Distribution of Average Ratings")
g0

g1 <- ggplot(df_ingrdt_reviews %>% filter(nReviews < 200), aes(nReviews)) + 
  geom_histogram(color = "black", fill = "slateblue1", binwidth = 5) + 
  geom_vline(xintercept = median(df_ingrdt_reviews$nReviews, na.rm = T), na.rm = TRUE, size = 0.6) + 
  ylab("Counts") + ggtitle("Distribution of Number of Ratings")
g1 

## Principal components for ingredients
data <- df_ingrdt %>% select(-ID)
pc <- prcomp(data, scale = TRUE)

# Plot the first two principal components
biplot(pc, scale = FALSE, cex = c(0.2, 0.6) )

### Let's look at descriptions

# Non empty descriptions
df_descript <- all_recipes_df %>% filter(description != "") %>% filter(!is.na(description))

# Let's check the ingredient & rating relationship
df_descript <- df_descript %>% 
  select(ID, description) %>%
  mutate(description = str_replace(description, "\n", " ") %>% str_replace("<.*?>", " ")) %>%
  unnest_tokens(word, description)

# Let's look at the top 10 most commonly used words
df_descript %>% count(word, sort = TRUE) %>% slice(1:10)

# Remove unwanted words
df_descript <- df_descript %>% 
  filter(!(word %in% stopwords())) %>%
  filter(!(word %in% word_remove)) %>%
  filter(!(str_detect(word, "[0-9]")))  # Remove numbers as well

df_descript %>% count(word, sort = TRUE) %>% slice(1:10)

# Get the most common 25 words in the ingredients to be used in modellling
top_words <- df_descript %>% count(word, sort = TRUE) %>% slice(1:25)
df_descript <-df_descript %>%
  filter(word %in% top_words$word)

# Spread the word counts to columns
df_descript <- df_descript %>% 
  group_by(ID) %>%
  count(word) %>%
  spread(key = word, value = n, fill = 0) %>%
  ungroup()

# Let's keep the word counts in this case
# Principal components
data <- df_descript %>% select(-ID)
pc <- prcomp(data, scale = TRUE)

# Plot the first two principal components
biplot(pc, scale = FALSE, cex = c(0.2, 0.6) )

# # We actually do not want word couts per recipe, rather one-hot encoded word features
# vars <- setdiff(names(df_ingrdt), "ID")
# df_ingrdt <- df_ingrdt %>%
#   mutate_at(vars, function(x) ifelse(x > 0, 1, 0))


# ## Models
# library(pls)
# set.seed(111)
# data <- df_ingrdt_reviews %>%
#   select(-c(ID,name,nReviews)) %>%
#   filter(!is.na(rating)) %>%
#   mutate(rating = log(rating))
# pcr.fit <- pcr(rating~., data = data, scale=TRUE, validation = "CV")
# summary(pcr.fit)
#  
# # Get only the first principal component
# pc <- prcomp(data %>% select(-rating), scale = TRUE)
# df_pc <- data.frame(PC1=pc$x[,1], rating = data$rating)
# g3 <- ggplot(df_pc, aes(PC1, rating)) + geom_smooth(method = "lm")
# g3

