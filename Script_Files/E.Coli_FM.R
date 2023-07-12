
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
  filter(Sex != "Total" & Disease ==  "E. coli O157", .by = Year) |>
  mutate(total_sex = sum(Count), .by = c(Sex, Year))


#Plotting my_data to create a line graph with limits of 2001 and 2014 
my_graph <-my_data |>
  ggplot(aes(x = Year, y = total_sex, color = Sex)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(2001, 2014), breaks = seq(from=2001, to=2014, by=1)) +
  labs(
    title = "E. Coli Infected Males vs. Females over 2001-2014",
    subtitle = "Males and females contract E.coli simarily, but Females contracted more in the years 2011 - 2013",
    y = "Infected",
    x = "Years"
  )

#Assigning my_graph to an rds file to make it easier for the respective qmd files to run 
write_rds(my_graph, "E.Coli_FM.rds")

