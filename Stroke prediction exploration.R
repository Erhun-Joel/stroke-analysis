# Loading required library
library(tidyverse)

# Loading data to be used. Replace the empty string in the next line with the location of the file
stroke.data <- read.csv("https://raw.githubusercontent.com/Erhun-Joel/stroke-analysis/refs/heads/main/Data%20and%20objects/healthcare-dataset-stroke-data.csv") %>%
  as_tibble()
stroke.data

# Creating function to get NA counts and class types by columns
count.na <- function(data){

  # Create in-function variables
  x <- data
  y <- c()
  z <- c()

  # Create for-loop attaching class type and NA counts to declared vectors
  for(i in colnames(x)){

    y <- y %>% append(sum(is.na(x[i])))
    z <- z %>% append(class(x[[i]]))

  }

  # Combine fully formed vectors into a tibble
  a <- tibble(
    column = colnames(x),
    type = z,
    counts = y
  )

  # Create list containing data dimensions and tibble
  b <- list(
    nas = dim(x),
    data = a
  )

  # Instruct function to return the list
  return(b)

}
count.na(stroke.data)
# No NA values exist

# Just discovered that NA characters are in string formats in the bmi column
# Change NA strings into actual NA values
stroke.data <- stroke.data %>%
  mutate(bmi = case_when(
    bmi == "N/A" ~ NA,
    TRUE ~ bmi
  ))
count.na(stroke.data)

# Checking overall distribution of bmi values via a histogram
stroke.data %>%
  filter(!is.na(bmi)) %>%
  mutate(bmi = as.numeric(bmi)) %>%
  ggplot(aes(bmi)) +
  geom_histogram() +
  labs(
    x = "Body Mass Index",
    y = NULL
  ) +
  ggtitle("Distribution of BMI Values") +
  theme_minimal()

# There are substantial outliers in the bmi distribution. To counter this lets use a median value as replacement for NA values


# Calculating the median value of the bmi distribution
median.bmi <-
stroke.data %>%
  filter(!is.na(bmi)) %>%
  mutate(bmi = as.numeric(bmi)) %>%
  select(bmi) %>%
  as_vector() %>%
  unname() %>%
  median() %>%
  as.character()
median.bmi

# Inputing median BMI into NA instances
stroke.data <-
stroke.data %>%
  mutate(
    bmi = if_else(is.na(bmi), median.bmi, bmi),
    bmi = as.numeric(bmi)
  )
stroke.data

# Question: Is heart disease related to stroke?
# Creating a confusion matrix of heart disease and stroke occurances
stroke.data %>%
  select(heart_disease, stroke) %>%
  table()

# Lets also take the proportion of instances of stroke in relation to heart diseases
stroke.data %>%
  select(heart_disease, stroke) %>%
  group_by(heart_disease) %>%
  count(stroke) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  filter(stroke != 0)
# People who had heart diseases are way more likely to have strokes

# Question: Is gender related to stroke?
stroke.data %>%
  select(gender, stroke) %>%
  group_by(gender) %>%
  count(stroke) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  filter(stroke != 0)
# Slight difference

# Question: What about work type
stroke.data %>%
  ggplot(aes(y = str_replace(work_type, "_", " "), fill = work_type)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~if_else(stroke == 0, "No Stroke", "Stroke"), scales = "free_x") +
  labs(
    x = "Count of Occurance",
    y = "Type of work"
  ) +
  theme_minimal()  
# Here we see distinctions seemly worthy of modeling:
# People who raise children hardly have stroke
# Self-employed people have more stroke probability, etc

# Question: Does smoking affect stroke probability?
stroke.data %>%
  select(smoking_status, stroke) %>%
  ggplot(aes(y = smoking_status, fill = smoking_status)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~if_else(stroke == 0, "No Stroke", "Stroke"), scales = "free_x") +
    labs(
      x = "Count of Occurance",
      y = "Smoking status"
    ) +
    theme_minimal()  
# People who formally smoked have higher stroke probability than others

# Question: Lets see if married status affects the stroke distribution
stroke.data %>%
  select(ever_married, stroke) %>%
  table()

# Lets see by proportions
stroke.data %>%
  group_by(ever_married) %>%
  count(stroke) %>%
  mutate(prop = n/sum(n))
# If married or ever being married, the probability of stroke occurring increases slightly

# Perhaps this is partly a function of age. Lets check if married people are older
stroke.data %>%
  ggplot(aes(y = age, x = ever_married, fill = ever_married)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    x = "Have you ever being married?",
    y = "Age"
  ) +
  theme_minimal()
# Yes, people who have been married tend to be older. Still, this doesn't nullify the effect of marriage on its own

# Question: Does age correlate with stroke occurrence
stroke.data %>%
  ggplot(aes(age, fill = as.factor(if_else(stroke == 0, "No Stroke", "Stroke")))) +
  geom_histogram(alpha = 0.6, position = "identity") +
  labs(
    x = "Age",
    y = "Count of Occurance",
    fill = NULL
  ) +
  theme_minimal() +
  ggtitle("Frequency of the Occurance of Age Segmented by Stroke")
# Yes, age correlates with stroke occurrence

# Finally, lets look at the class imbalance of stroke
stroke.data %>%
  count(stroke)
# Stroke is extremely imbalanced. We will deal with this in the modeling stage







