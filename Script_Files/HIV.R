
#Loading the Libraries 
#| message: FALSE 
library(tidyverse)
library(ggthemes)
library(plotly)
library(rstanarm)
library(shiny)
library(ggthemr)
ggthemr("fresh")
darken_swatch(amount = 0.1)


#Reading the data_set and assigning it to x 
x <- read_csv("Infectious Disease 2001-2014.csv", show_col_type = FALSE)

#Filtering the data set x, selecting the necessary columns, and adding a column called total_sex
#Total_sex is the sum of all of the infections, per sex, for each year 
my_data <- x|>
  drop_na() |>
  select(Disease, Year, Sex, Count, County, Population) |>
  filter(Sex != "Total" & Disease ==  "HIV" & Year != 2014, .by = Year) |>
  mutate(total_sex = sum(Count), .by = c(Sex, Year))


#Plotting my_data to create a line graph with limits of 2001 and 2014 
my_graph <- my_data |>
  ggplot(aes(x = Year, y = total_sex, color = Sex)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(2001, 2013), breaks = seq(from=2001, to=2013, by=1)) +
  labs(
    title = "HIV Infected Males vs. Females over 2001-2013",
    subtitle = "Males seem to contract HIV at a larger rate",
    y = "Infected",
    x = "Years"
  )

#Assigning my_graph to an rds file to make it easier for the respective qmd files to run 
write_rds(my_graph, "HIV_FM.rds")

