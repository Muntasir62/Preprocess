data <- read.csv("D:/Data Science/Mid Dataset.csv",header=TRUE,sep=",", na.strings = c("", " "))
data


install.packages("ggplot2")
install.packages("naniar")
library(dplyr)
library(ggplot2)
library(naniar)

vis_miss(data) 
summary(data)

is.na(data)
colSums(is.na(data))
which(is.na(data$person_age))
data_discarded <- na.omit(data)
data_discarded

get_mode <- function(v) {
  uv <- unique(v)
  uv[which.max(tabulate(match(v,uv)))]
}
data_freq <- data
mode_gender <- get_mode(na.omit(data_freq$person_gender))
mode_education <- get_mode(na.omit(data_freq$person_education))
mode_loan_status <- get_mode(na.omit(data_freq$loan_status))
data_freq$person_gender[is.na(data_freq$person_gender)] <- mode_gender
data_freq$person_education[is.na(data_freq$person_education)] <- mode_education
data_freq$loan_status[is.na(data_freq$loan_status)] <- mode_loan_status
data_freq

data_mean <- data
mean_age <- mean(data_mean$person_age, na.rm = TRUE)
mean_income <- mean(data_mean$person_income, na.rm = TRUE)

data_mean$person_age[is.na(data_mean$person_age)] <- mean_age
data_mean$person_income[is.na(data_mean$person_income)] <- mean_income
data_mean

num_cols <- names(data)[sapply(data, is.numeric)]
for (col in num_cols) {
  iqr_val <- IQR(data[[col]], na.rm = TRUE)
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  cat("Column :", col, "| IQR :", iqr_val, "\n")
  lower <- Q1 - 1.5 * iqr_val
  upper <- Q3 + 1.5 * iqr_val
  outliers <- data[[col]] < lower | data[[col]] > upper
  cat("Potential outliers:", sum(outliers, na.rm = TRUE), "\n")
  
  data[[col]][data[[col]] < lower] <- lower
  data[[col]][data[[col]] > upper] <- upper
}
data
head(data)  

data_loan_status_update <- ifelse(data$loan_status == 1, "Yes", "No")
print(data.frame(loan_status_original = data$loan_status,
                 loan_status_updated = data_loan_status_update),)


data$gender_numeric <- ifelse(tolower(trimws(data$person_gender)) == "male", 1, 0)
data$gender_numeric[is.na(data$person_gender)] <- NA
print(data.frame(gender_original = data$person_gender,
                 gender_numeric = data$gender_numeric),)

colnames(data)
min_income <- min(data$person_income, na.rm = TRUE)
max_income <- max(data$person_income, na.rm = TRUE)
data$person_income_normalized <- (data$person_income - min_income) / (max_income - min_income)
print(data.frame(
  person_income = data$person_income,
  person_income_normalized = data$person_income_normalized
))

data_without_duplicates <- d istinct(data, 
                                    person_income, 
                                    loan_amnt,
                                    loan_int_rate,  
                                    loan_percent_income,
                                    credit_score,
                                    .keep_all = TRUE)
select(data_without_duplicates, person_income, loan_amnt, loan_int_rate, loan_percent_income, credit_score)

filter(data, loan_status == 1)
filter(data, loan_amnt > 10000)
filter(data, person_age == 25, person_gender == "male")


install.packages("caret")
install.packages("tidyr")

library(caret)
library(tidyr)



print(unique(data$person_home_ownership))
data_clean_person_home_ownership <- recode(data$person_home_ownership, 
                                           "RENTT" = "RENT",
                                           "OOWN" = "OWN",
                                           .default = NA_character_)
print(unique(data_clean_person_home_ownership))
print(unique(data$person_age))
data_clean_person_age <- filter(data, person_age >= 0 | is.na(person_age))
print(unique(data_clean_person_age))
print(unique(data$person_gender))
data_clean_person_gender <- recode(data$person_gender,
                                   "malee" = "male",
                                   "feemale" = "female",
                                   default = NA_character_)
print(unique(data_clean_person_gender))

install.packages("ROSE")
library(ROSE)
print(table(data$loan_status))
data$person_age[is.na(data$person_age)] <- mean(data$person_age, na.rm = TRUE)
data$person_income[is.na(data$person_income)] <- mean(data$person_income, na.rm = TRUE)
data$loan_int_rate[is.na(data$loan_int_rate)] <- mean(data$loan_int_rate, na.rm = TRUE)
data$person_emp_exp[is.na(data$person_emp_exp)] <- mean(data$person_emp_exp, na.rm = TRUE)
data$loan_amnt[is.na(data$loan_amnt)] <- mean(data$loan_amnt, na.rm = TRUE)
data$loan_percent_income[is.na(data$loan_percent_income)] <- mean(data$loan_percent_income, na.rm = TRUE)
data$cb_credit_score[is.na(data$credit_score)] <- mean(data$credit_score, na.rm = TRUE)
data$cb_person_cred_hist_length[is.na(data$cb_person_cred_hist_length)] <- mean(data$cb_person_cred_hist_length, na.rm = TRUE)
data_clean <- data[!is.na(data$loan_status), ]

data_clean <- data_clean %>%
  mutate_if(is.character, as.factor)

majority_class <- data_clean %>% filter(loan_status == 1)
minority_class <- data_clean %>% filter(loan_status == 0)
oversampled_minority <- sample_n(minority_class,
                                 size = nrow(majority_class),
                                 replace = TRUE)
balanced_data <- bind_rows(majority_class, oversampled_minority)
print(table(balanced_data$loan_status))

balanced_data <- balanced_data %>%
  select(-loan_status_factor,-loan_status_update, -cb_credit_score)
balanced_data$loan_status


data$person_age[is.na(data$person_age)] <- mean(data$person_age, na.rm = TRUE)
data$person_income[is.na(data$person_income)] <- mean(data$person_income, na.rm = TRUE)
data$loan_int_rate[is.na(data$loan_int_rate)] <- mean(data$loan_int_rate, na.rm = TRUE)
data$person_emp_exp[is.na(data$person_emp_exp)] <- mean(data$person_emp_exp, na.rm = TRUE)
data$loan_amnt[is.na(data$loan_amnt)] <- mean(data$loan_amnt, na.rm = TRUE)
data$loan_percent_income[is.na(data$loan_percent_income)] <- mean(data$loan_percent_income, na.rm = TRUE)
data$cb_credit_score[is.na(data$credit_score)] <- mean(data$credit_score, na.rm = TRUE)
data$cb_person_cred_hist_length[is.na(data$cb_person_cred_hist_length)] <- mean(data$cb_person_cred_hist_length, na.rm = TRUE)
data_clean <- data[!is.na(data$loan_status), ]

data_clean <- data_clean %>%
  mutate_if(is.character, as.factor)

majority_class <- data_clean %>% filter(loan_status == 1)
minority_class <- data_clean %>% filter(loan_status == 0)
oversampled_minority <- sample_n(minority_class,
                                 size = nrow(majority_class),
                                 replace = TRUE)
balanced_data <- bind_rows(majority_class, oversampled_minority)


train_index <- createDataPartition(balanced_data$loan_status, p = 0.8, list = FALSE)
training_set <- balanced_data[train_index, ]
testing_set <- balanced_data[-train_index, ]
print(dim(training_set))
print(dim(testing_set))
print(table(training_set$loan_status))
print(table(testing_set$loan_status))

desc_stats <- balanced_data %>%
  group_by(loan_status) %>%
  summarise(
    age_mean = mean(person_age, na.rm = TRUE),
    age_median = median(person_age, na.rm = TRUE),
    age_sd = sd(person_age, na.rm = TRUE),
    age_iqr = IQR(person_age, na.rm = TRUE),
    age_min = min(person_age, na.rm = TRUE),
    age_max = max(person_age, na.rm = TRUE),
    
    income_mean = mean(person_income, na.rm = TRUE),
    income_median = median(person_income, na.rm = TRUE),
    income_sd = sd(person_income, na.rm = TRUE),
    income_iqr = IQR(person_income, na.rm = TRUE),
    income_min = min(person_income, na.rm = TRUE),
    income_max = max(person_income, na.rm = TRUE)
  )
print(desc_stats)

credit_score_comparison <- balanced_data %>%
  group_by(loan_status) %>%
  summarise(
    avg_credit_score = mean(credit_score, na.rm = TRUE),
    median_credit_score = median(credit_score, na.rm = TRUE),
    sd_credit_score = sd(credit_score, na.rm = TRUE),
    min_credit_score = min(credit_score, na.rm = TRUE),
    max_credit_score = max(credit_score, na.rm = TRUE),
    n_customers = n()
  )
print(credit_score_comparison)

emp_exp_by_education <- balanced_data %>%
  group_by(person_education) %>%
  summarise(
    count = n(),
    mean_exp = mean(person_emp_exp, na.rm = TRUE),
    median_exp = median(person_emp_exp, na.rm = TRUE),
    iqr_exp = IQR(person_emp_exp, na.rm = TRUE),
    min_exp = min(person_emp_exp, na.rm = TRUE),
    max_exp = max(person_emp_exp, na.rm = TRUE),
    sd_exp = sd(person_emp_exp, na.rm = TRUE),
    range_exp = diff(range(person_emp_exp, na.rm = TRUE))
    
  ) %>%
  arrange(desc(median_exp))
print(emp_exp_by_education)
