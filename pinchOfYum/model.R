## Web Scraping Recipes from Online Blogs
## Author: B. Himmetoglu
## Modelling: How names of recipes are correlated with number of ratings?
#
# Load libraries
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(tidytext)
library(tokenizers)
library(syuzhet)
library(ggplot2)
library(SnowballC)

# Load the data.frame of all recipes
load(file="all_recipes.RData")

# NAs
all_recipes_df %>% map_dbl(~sum(is.na(.x)))

# Let's assign an ID number to each recipe
all_recipes_df <- all_recipes_df %>% mutate(ID = 1:nrow(all_recipes_df))

# Unnest all the tokens
df_name <- all_recipes_df %>%
  select(ID, name) %>%
  unnest_tokens(word, name)

# Store ratings and nReviews in a separate data frame
df_reviews <- all_recipes_df %>% 
  select(ID, name, rating, nReviews) %>%
  mutate(rating = as.numeric(rating)) %>%
  mutate(nReviews = as.integer(nReviews))

# Replace NA's in nReviews with 0
df_reviews <- df_reviews %>%
  mutate(nReviews = ifelse(is.na(nReviews),0,nReviews))

# Create a new column: large/small nReviews
med_nRev <- median(df_reviews$nReviews)
df_reviews <- df_reviews %>% 
  mutate(highViews = ifelse(nReviews >= med_nRev, 1, 0))

# Combine with raings
# Remove stop words and numbers
data("stop_words")
df_name <- df_name %>% 
  filter(!(word %in% stopwords())) %>%
  filter(!(str_detect(word, "[0-9]")))

# Get word stems
df_name <- df_name %>%
  mutate(word = wordStem(word))

# Now let's look at the top 10 most commonly used words
df_name %>% count(word, sort = TRUE) %>% slice(1:10)

# Combine df_reviews with df_name
model_df <- left_join(df_reviews %>% select(ID, rating, highViews), 
                      df_name, by = "ID")

# Get the most common 25 words in the ingredients for visualization
top_words <- model_df %>% count(word, sort = TRUE) %>% slice(1:25)

# Visualize how words in title correlate with highviews
df <- model_df %>% 
  filter(word %in% top_words$word) %>%
  group_by(highViews) %>%
  count(word) %>%
  ungroup(highViews) %>%
  mutate(highViews = as.factor(highViews))

gg <- ggplot(df, aes(word,n)) +
  geom_bar(stat="identity", aes(fill=highViews), position = "dodge") + 
  coord_flip()
gg

### Data frame for Modelling

# Spread the word counts to columns
df <- model_df %>% 
  group_by(ID) %>%
  count(word) %>%
  spread(key = word, value = n, fill = 0) %>%
  ungroup()

# Only top words (choose 50 of them)
top_words <- model_df %>% count(word, sort = TRUE) %>% slice(1:50)
df <- df %>% select_(.dots = c("ID",top_words$word))
model_df <- left_join(df, 
                 model_df %>% select(ID, rating, highViews) %>% distinct(),
                 by="ID")

# One-hot encode instead of counts
vars <- setdiff(names(model_df), c("ID","rating","highViews"))
model_df <- model_df %>%
  mutate_at(vars, function(x) ifelse(x > 0, 1, 0))

# Train a model (e.g. randomForest)
library(randomForest)
library(caret)
dat <- model_df %>% 
  select(-c(ID,rating))

ctrl <- trainControl(method="cv", number=10, verboseIter = TRUE)
rf_grid <- expand.grid(mtry = c(2,5,10,15,20))

mod <- train(x = select(dat, -highViews), y = ifelse(dat$highViews == 1, "y","n"),
             method = "rf", trControl = ctrl, tuneGrid = rf_grid)

# Print results
mod$results

