library(tidyverse)
library(ggthemes)
library(plotly)
library(rstanarm)
library(ggplot2)
library(ggthemr)
ggthemr("fresh")
lighten_swatch(amount = .1)

x <- read_csv("Infectious Disease 2001-2014.csv", show_col_types = FALSE)

options(warn = -1)
x <- read_csv("Infectious Disease 2001-2014.csv", show_col_types = FALSE)

my_data <- x |>
  drop_na() |>
  select(Disease, Year, Sex, Count, County) |>
  filter(Sex != "Total" & Disease == c("E. coli O157", "Malaria", "HIV"), .by = Year)

model_plot <- my_data |>
  ggplot(aes(x = Sex, y = Count, fill = Sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Disease, labeller = labeller(Disease = c(`E. coli O157` = "E. Coli", HIV = "HIV", Malaria = "Malaria"))) +
  labs(
    title = "Total Infection Count from 2001-2014 in the United States",
    x = "Sex",
    y = "Infected"
  )

write_rds(model_plot, "Model1.rds")
