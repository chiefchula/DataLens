# clean workspace --------------------------------------------------------------
rm(list = ls())

# attach required packages -----------------------------------------------------
pacman::p_load(tidyverse, janitor)

messy_data <- read_csv("data/messy_customer_data.csv")

