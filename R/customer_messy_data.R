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
      Gender %in% c("female", "f") ~ "Female",
      Gender %in% c("male", "m") ~ "Male",
      TRUE ~ NA_character_
    ),
    
    # Age to be between 0 and 100
    age = if_else(Age < 0 | Age > 100, NA_real_, Age),
    
    # Title capitalization to city
    city = str_to_title(City), 
    
    # Convert signup date and last purchase data as proper dates format
    signup_date = dmy(Signup_Date, quiet = T),
    last_purchase_date = dmy(Last_purchase_date, quiet = T),
    
    # purchase amount to numeric, change N/A and -999 as missing values
    purchase_amount_clean = parse_number(if_else(purchase_amount %in% c("N/A", "-999"), NA_character_, purchase_amount)),
    
    # change -1 to NA
    feedback_score = if_else(feedback_score < 0, NA_real_, feedback_score),
    
    # email standardization
    email_cleaned = case_when(
      is.na(email) ~ NA_character_,
      str_detect(email, "@") ~ email,                              # already valid, leave alone
      str_detect(email, "mail\\.com$") ~ str_replace(email, "mail\\.com$", "@mail.com"),  # insert the missing @
      TRUE ~ NA_character_
    ),
    
    # stanadardize length of phone numbers
    phone_number = if_else(str_detect(Phone_number, "^\\d{10}$"), Phone_number, NA_character_),
    
    # Standardize country
    country = str_squish(Country)
  ) |> 
  
  # retain only clean columns
  select(customer_id, customer_name, gender, age, city, signup_date, last_purchase_date, purchase_amount_clean, feedback_score, email_cleaned, phone_number, country) |> 
  rename(purchase_amount = purchase_amount_clean)


# ---- helpers -------------------------------------------------------------
html_table <- function(df) {
  header <- paste0("<th>", names(df), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) paste0("<td>", r, "</td>", collapse = ""))
  paste0("<table><tr>", header, "</tr>",
         paste0("<tr>", rows, "</tr>", collapse = ""), "</table>")
}

html_check <- function(title, expected, result_html, pass = NA) {
  badge <- case_when(
    is.na(pass) ~ "<span class='badge info'>INFO</span>",
    pass        ~ "<span class='badge pass'>PASS</span>",
    TRUE        ~ "<span class='badge fail'>FAIL</span>"
  )
  paste0("<div class='check'><h3>", badge, " ", title, "</h3>",
         "<p class='expected'>Expected: ", expected, "</p>",
         result_html, "</div>")
}

# ---- run the checks ------------------------------------------------------
rows_match <- nrow(clean_2) == nrow(messy_data)

double_at <- clean_2 |>
  filter(str_count(email_cleaned, "@") > 1) |> nrow()

country_counts <- clean_2 |> count(country, sort = TRUE) |>
  mutate(country = replace_na(country, "(missing)"))

amount_summary <- summary(clean_2$purchase_amount)
amount_df <- tibble(statistic = names(amount_summary),
                    value = round(as.numeric(amount_summary), 2))

date_check <- clean_2 |>
  filter(!is.na(signup_date), !is.na(last_purchase_date)) |>
  summarise(testable_rows = n(),
            purchase_before_signup = sum(last_purchase_date < signup_date),
            pct = round(100 * purchase_before_signup / testable_rows, 1))

# ---- assemble the HTML ---------------------------------------------------
checks <- c(
  html_check("Row count preserved",
             paste0(nrow(messy_data), " rows in, same rows out"),
             paste0("<p>messy_data: ", nrow(messy_data),
                    " | clean_2: ", nrow(clean_2), "</p>"),
             pass = rows_match),
  
  html_check("No double @ in emails", "0 rows",
             paste0("<p>", double_at, " rows found</p>"),
             pass = double_at == 0),
  
  html_check("Country values", "USA and UK still uppercase",
             html_table(country_counts),
             pass = all(c("USA", "UK") %in% country_counts$country)),
  
  html_check("purchase_amount is numeric, no -999",
             "class numeric, minimum above 0",
             paste0("<p>class: ", class(clean_2$purchase_amount)[1], "</p>",
                    html_table(amount_df)),
             pass = is.numeric(clean_2$purchase_amount) &&
               all(clean_2$purchase_amount > 0, na.rm = TRUE)),
  
  html_check("Purchase dated before signup",
             "a data finding, not a cleaning error (flagged, rows kept)",
             html_table(date_check))
)

css <- "
body{font-family:system-ui,sans-serif;max-width:800px;margin:2rem auto;padding:0 1rem;color:#222}
.check{border:1px solid #ddd;border-radius:8px;padding:1rem;margin:1rem 0}
h3{margin:0 0 .5rem}
.expected{color:#666;margin:.25rem 0 .75rem}
table{border-collapse:collapse}
th,td{border:1px solid #ccc;padding:4px 10px;text-align:left}
th{background:#f4f4f4}
.badge{padding:2px 8px;border-radius:4px;color:#fff;font-size:.8rem}
.pass{background:#2e7d32}.fail{background:#c62828}.info{background:#1565c0}
"

out_file <- "validation_report.html"

cat(
  "<!DOCTYPE html><html><head><meta charset='utf-8'>",
  "<title>Data cleaning validation</title>",
  "<style>", css, "</style></head><body>",
  "<h1>Data cleaning validation report</h1>",
  "<p>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "</p>",
  checks,
  "</body></html>",
  file = out_file, sep = "\n"
)

browseURL(out_file)


# Check 

count_status <- function(x, is_valid) {
  is_miss  <- is.na(x)
  is_ok    <- !is_miss & coalesce(is_valid, FALSE)
  is_bad   <- !is_miss & !is_ok
  tibble(valid = sum(is_ok), missing = sum(is_miss), invalid = sum(is_bad))
}

email_ok <- "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"

before <- bind_rows(
  count_status(messy_data$CustomerID, str_detect(messy_data$CustomerID, "^C\\d+$")) |> mutate(column = "customer_id"),
  count_status(messy_data$Gender, messy_data$Gender %in% c("Male", "Female")) |> mutate(column = "gender"),
  count_status(messy_data$Age, messy_data$Age >= 0 & messy_data$Age <= 100) |> mutate(column = "age"),
  count_status(messy_data$Signup_Date, !is.na(dmy(messy_data$Signup_Date, quiet = TRUE))) |> mutate(column = "signup_date"),
  count_status(messy_data$Last_purchase_date, !is.na(dmy(messy_data$Last_purchase_date, quiet = TRUE))) |> mutate(column = "last_purchase_date"),
  count_status(messy_data$purchase_amount, !messy_data$purchase_amount %in% c("-999", "N/A")) |> mutate(column = "purchase_amount"),
  count_status(messy_data$feedback_score, messy_data$feedback_score >= 0) |> mutate(column = "feedback_score"),
  count_status(messy_data$email, str_detect(messy_data$email, email_ok)) |> mutate(column = "email"),
  count_status(messy_data$Phone_number, str_detect(messy_data$Phone_number, "^\\d{10}$")) |> mutate(column = "phone")
) |> mutate(stage = "Before")

after <- bind_rows(
  count_status(clean_2$customer_id, str_detect(clean_2$customer_id, "^C\\d+$")) |> mutate(column = "customer_id"),
  count_status(clean_2$gender, clean_2$gender %in% c("Male", "Female")) |> mutate(column = "gender"),
  count_status(clean_2$age, clean_2$age >= 0 & clean_2$age <= 100) |> mutate(column = "age"),
  count_status(clean_2$signup_date, TRUE) |> mutate(column = "signup_date"),
  count_status(clean_2$last_purchase_date, TRUE) |> mutate(column = "last_purchase_date"),
  count_status(clean_2$purchase_amount, clean_2$purchase_amount > 0) |> mutate(column = "purchase_amount"),
  count_status(clean_2$feedback_score, clean_2$feedback_score >= 0) |> mutate(column = "feedback_score"),
  count_status(clean_2$email_cleaned, str_detect(clean_2$email_cleaned, email_ok)) |> mutate(column = "email"),
  count_status(clean_2$phone_number, str_detect(clean_2$phone_number, "^\\d{10}$")) |> mutate(column = "phone")
) |> mutate(stage = "After")

scorecard <- bind_rows(before, after) |>
  pivot_longer(c(valid, missing, invalid), names_to = "status", values_to = "n") |>
  mutate(status = factor(status, levels = c("invalid", "missing", "valid")),
         stage  = factor(stage, levels = c("Before", "After")))



p <- scorecard |>
  ggplot(aes(stage, n, fill = status)) +
  geom_col() +
  geom_text(
    aes(label = if_else(n > 0, scales::comma(n), "")),
    position = position_stack(vjust = 0.5),
    size = 3, colour = "white"
  ) +
  facet_wrap(~column, ncol = 3) +
  scale_fill_manual(values = c(valid = "#2e7d32", missing = "#757575", invalid = "#c62828")) +
  labs(title = "Data quality before vs after cleaning", x = NULL, y = "Rows", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("before_after.png", p, width = 10, height = 8, dpi = 150)
p
