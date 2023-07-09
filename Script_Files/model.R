library(tidyverse)
library(ggthemes)
library(plotly)
library(rstanarm)

x <- read_csv("Infectious Disease 2001-2014.csv", show_col_types = FALSE)

my_data <- x |>
  drop_na() |>
  select(Disease, Year, Sex, Count, County) |>
  filter(Sex != "Total" & Disease == "E. coli O157", .by = Year)

fit_obj <- stan_glm(data = my_data, 
                    formula = Count ~ Sex - 1, 
                    family = gaussian, 
                    refresh = 0,
                    seed = 9)

new_obs <- tibble(Sex = "Male")

pe <- posterior_predict(fit_obj, new_data = new_obs) |>
  as_tibble() 

model_plot <- pe |>
  ggplot(aes(x = abs(`1`))) +
  geom_histogram(aes(y = after_stat(count/sum(count))), bins = 100) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  labs(
    title = "Posterior for E. Coli 0185 infected Males ",
    subtitle = "21% probability that 5 males are infected with E. Coli 0185",
    x = "Male Count",
    y = "Probability"
  )

write_rds(model_plot, "Model.rds")
