library(readr)
library(tidyverse)

flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv",
                       col_types = cols(
                         code = col_character(),
                         area = col_character(),
                         year = col_character(),
                         total_incidents = col_number(),
                         total_action_taken = col_number(),
                         warning_letters = col_number(),
                         fixed_penalty_notices = col_number(),
                         statutory_notices = col_number(),
                         formal_cautions = col_number(),
                         injunctions = col_number(),
                         prosecutions = col_number()
                       ))

View(flytipping)

flytipping_long <- flytipping1 %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )

# view the data
view(flytipping_long)

#pivot the tidy tibble into one that is suitable for mapping
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )

view(flytipping_wide)

#an alternative which just pulls everything out into a single table
flytipping2 <- flytipping1[,1:4]

widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)
