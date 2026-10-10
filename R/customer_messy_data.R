# clean workspace --------------------------------------------------------------
rm(list = ls())

# attach required packages -----------------------------------------------------
pacman::p_load(tidyverse, janitor, skimr)

messy_data <- read_csv("data/messy_customer_data.csv")

glimpse(messy_data)
summary(messy_data)
colSums(is.na(messy_data))
sum(duplicated((messy_data)))

# Dataset assessment scorecard  ------------------------------------------------
invalid_counts <- tibble(
  column = c("CustomerID", "Gender", "Age", "Signup_Date", 
             "Last_purchase_date", "purchase_amount", "email", 
             "Phone_number", "Country"),
  missing = c(
    sum(is.na(messy_data$CustomerID)), 
    sum(is.na(messy_data$Gender)),
    sum(is.na(messy_data$Age)),
    sum(is.na(messy_data$Signup_Date)),
    sum(is.na(messy_data$Last_purchase_date)),
    sum(is.na(messy_data$purchase_amount)),
    # sum(is.na(messy_data$feedback_score)),
    sum(is.na(messy_data$email)),
    sum(is.na(messy_data$Phone_number)),
    sum(is.na(messy_data$Country))
  ),
  invalid = c(
    0,
    sum(!is.na(messy_data$Gender) & !(messy_data %in% c("Male","Female"))),
    sum(messy_data$Age < 0 | messy_data$Age > 100, na.rm = TRUE),
    sum(is.na(messy_data$Signup_Date) & is.na(suppressWarnings(dmy(messy_data$Signup_Date)))),
    sum(is.na(messy_data$Last_purchase_date) & is.na(suppressWarnings(dmy(messy_data$Last_purchase_date)))),
    sum(messy_data$purchase_amount %in% c("-999", "N/A")),
    sum(messy_data$feedback_score == -1, na.rm = TRUE),
    sum(!is.na(messy_data$email) & !str_detect(messy_data$email, "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$")),
    sum(!is.na(messy_data$Phone_number) & !str_detect(messy_data$Phone_number, "^\\d{10}$"))
  )
)

# Cleaning -------------------------------------
clean <- messy_data |> 
  mutate(across(where(is.character), str_squish)) |> 
  mutate(
    customer_id = parse_number(CustomerID),
    gender = case_when(
      str_to_lower(Gender) %in% c("f", "female") ~ "Female",
      str_to_lower(Gender) %in% c("m", "male") ~ "Male",
      TRUE ~ NA_character_
    ),
    age = if_else(Age >= 0 & Age <= 100, Age, NA_real_),
    signup_date = suppressWarnings(dmy(Signup_Date)),
    last_purchase_date = suppressWarnings(dmy(Last_purchase_date)),
    purchase_amount_clean = if_else(purchase_amount > 0, as.numeric(purchase_amount), NA_real_),
    feedback_score = if_else(feedback_score >= 0, feedback_score, NA_real_),
    email_valid = str_detect(email, "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"),
    email = if_else(email_valid, str_to_lower(email), NA_character_),
    phone_valid = str_detect(Phone_number, "^\\d{10}$"),
    phone = if_else(phone_valid, Phone_number, NA_character_)
  ) |> 
  select(customer_id, name = Name, gender, age, city = City, signup_date, 
         last_purchase_date, purchase_amount_clean, feedback_score, email, phone, country = Country)

clean_2 <- messy_data |> 
  mutate(
    # pull the number from customerID and append "C"
    customer_id = if_else(is.na(CustomerID), NA_character_, paste0("C",parse_number(CustomerID))),
    
    # Title capitalisation of names and the title
    customer_name = str_to_title(Name),
    
    # standardize all gender into two Male and Female
    gender = case_when(
      Gender %in% c("Female", "female", "F") ~ "Female",
      Gender %in% c("M", "m", "Male", "Male") ~ "Male",
      TRUE ~ NA_character_
    ),
    
    # Age to be between 0 and 200
    age = if_else(Age < 0 | Age > 100, NA_real_, Age),
    
    # Title capitalization to city
    city = str_to_title(City), 
    
    # Convert signup date and last purchase data as proper dates format
    signup_date = dmy(Signup_Date, quiet = T),
    last_purchase_date = dmy(Last_purchase_date, quiet = T),
    
    # purchase amount to numeric, change N/A and -999 as missing values
    purchase_amount_clean = parse_number(if_else(purchase_amount %in% c("N/A", "-999"), NA_character_, purchase_amount)),
    
    # change -1 to NA
    feedback_score = if_else(feedback_score < 0, NA_integer_, feedback_score),
    
    # email standardization
    email_cleaned = if_else(is.na(email),NA_character_, paste0(str_sub(email, start = 1, end = str_length(email) - 8), "@mail.com")),
    
    # stanadardize length of phone numbers
    phone_number = if_else(str_length(Phone_number) == 10, Phone_number, NA_character_),
    
    # Standardize country
    country = str_to_title(Country)
  ) |> 
  
  # retain only clean columns
  select(customer_id, customer_name, gender, age, city, signup_date, last_purchase_date, purchase_amount, feedback_score, email_cleaned, phone_number, country) 
  
