#Load packages
library(tidyverse)

rm(list = ls()) #clean variables out of environment

ds <- read_csv('data_example_4/vocab.csv')
#let's add some 'fixed' columns
ds <- ds %>% mutate(
  id = "Jonah",
  dob = as.Date("2017-08-30"),
  .before = "12"
)
print(ds)

#Perfect example of "columns as values"
#Age should be a variable, words should be a variable
#Each age-word pair is an observation that should appear as a row

#Wide to long with pivot_longer
#cols = which columns should be stacked
#names_to = what to call the variable that will contain our former column names
#values_to = what to call the variable that will contain the values
ds_long <- pivot_longer(ds, cols = -(id:dob), names_to = "age", values_to = "word")

#Can drop NA values when pivoting
ds_long <- pivot_longer(ds, cols = -(id:dob), names_to = "age", values_to = "word", values_drop_na = T)

