#Loading in Libaries 
library(tidyverse)
library(ggthemes)
library(plotly)
library(rstanarm)
library(gtsummary)
library(ggthemr)
ggthemr("fresh")
lighten_swatch(amount = .1)

#Reading the Data set
x <- read_csv("Infectious Disease 2001-2014.csv", show_col_types = FALSE)

#Filtering the data set, selecting necessary columns, and assigning to my_data
my_data <- x |>
  drop_na() |>
  select(Disease, Year, Sex, Count, County) |>
  filter(Sex != "Total" & Disease == "E. coli O157", .by = Year)

#Using the stan_glm function to create a fitted model and assigning it to fit_obj
fit_obj <- stan_glm(data = my_data, 
                    formula = Count ~ Sex - 1, 
                    family = gaussian, 
                    refresh = 0,
                    seed = 9)

#creating a professional looking table table of fit_ovj and assigning it to t1
t1 <- tbl_regression(fit_obj, exponentiate = TRUE)

#creating an empty tibble named Sex with rows male and female
new_obs <- tibble(Sex = c("Male", "Female"))

#using the posterior_predict function on fit_)obj and assigning the output to new_obs
pe <- posterior_predict(fit_obj, new_data = new_obs) |>
  as_tibble() 

#Plotting the pe tibble using a histogram with the x axis being the num of infected and the y axis being the probability 
model_plot <- pe |>
  pivot_longer(cols = `1`:`2`,
               names_to = "Sex",
               values_to = "Count") |>
  ggplot(aes(x = abs(Count), fill = Sex)) +
  geom_histogram(aes(y = after_stat(count/sum(count))), bins = 100) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Posterior for E. Coli Infected Males vs Infected Females",
    subtitle = "21% probability that more than 5 Males are infected with E. Coli in 2023 \n17% probability that more than 5 Females are infected with E. Coli in 2023",
    x = "Infected",
    y = "Probability",
  )+
  scale_fill_discrete(labels = c("Male", "Female"))

#Assigning t1 and model_plot to an rds file to make it easier for the respective qmd files to run 
write_rds(model_plot, "Model.rds")
write_rds(t1, "Table.rds")
