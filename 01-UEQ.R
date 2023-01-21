# ================ AttrakDiff Script for data analysis ================
# Create a reproductible Script that enables the analysis of the AttrakDiff Methodology

# CONTENTS
#   1. Reading the Excel/CSV Data
#   2. Treating Excel Data
#   3. Making the Tables and the Graphics
#   4. Introduce the results in Rmarkdown table.


# KEY PACKAGES
## Install those now if you do not have the latest versions. ----
#install.packages("tidyverse")  # Tidyverse approach for Data science
#install.packages("readxl")


## Load the packages ----
library(tidyverse) # Data Science Tools
library(readxl)  # Read a Excel File


#  1. Reading the CSV Data ----
UEQ <- read_csv('data/2023/UEQ/F.csv')

## Quantity of participants
total <- nrow(UEQ)

## Obtaining the names of the columns
names(UEQ)




















