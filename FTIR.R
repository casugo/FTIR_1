# Data Analysis of FTIR
## Date: 07/12/2022

# Load Libraries
library(tidyverse)
library(here)
library(kableExtra)
library(ggpubr)
library (dbplyr)
library(haven)



# Data Loading ----
## Loading the paths
files <- 
  here::here("Data") %>%
  dir( recursive=TRUE, full.names=TRUE, pattern="\\.dat$")


## Creating a function to transform the data 
data_trans <- function(path_file, material ){
  
  table <- read_delim(file = path_file, skip = 2, delim = " ")
  x <-table[1] 
  y <- names(table)[-1]
  data <- tibble( var_x = x , var_y = y ) %>% 
    set_names("var_x", "var_y") %>% 
    mutate(Material = material)
  
  return(data)
}

## Reading teach dataframe -----
BB <- data_trans(path_file = files[1], "BB" )
HDPE <- data_trans(path_file = files[3], "HDPE" )
PET <- data_trans(path_file = files[3], "PET" )


## Joining together the data------
data <- rbind(BB, HDPE, PET) %>% set_names("Var_x", "Var_Y", "Material" )

