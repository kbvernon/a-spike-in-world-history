library(patchwork)
library(tidyverse)

theme_set(theme_classic())

theme_update(
  axis.text = element_text(size = rel(0.7), color = "gray60"),
  axis.ticks = element_line(linewidth = 0.3, color = "gray60"),
  axis.title = element_text(size = rel(0.8)),
  legend.text = element_text(size = rel(0.8)),
  legend.title = element_text(size = rel(0.8)),
  plot.margin = margin(5, 15, 5, 5)
)

sauce <- file.path(
  "https://raw.githubusercontent.com", 
  "gagemweston", 
  "PWI-Population-Projections", 
  "main"
)

human_history <- file.path(sauce, "population_history.csv") |> read_csv()

e <- list()

e$births <- with(human_history, approx(year, births, xout = -10000:2024)$y)
e$population <- with(human_history, approx(year, population, xout = -10000:2024)$y)

human_history <- human_history |> 
  complete(year = -10000:2024) |> 
  mutate(
    births = e$births,
    population = e$population
  )

remove(e)

human_future <- file.path(sauce, "main_output.csv") |> read_csv()

human_future <- human_future |> 
  filter(
    age == "all",
    tfr_scenario %in% c("1.8", "1.66", "1.2")
  ) |> 
  select(tfr_scenario, year, births, population) |> 
  rename("tfr" = tfr_scenario)

make_plot <- function(x, history = human_history, future = human_future, gtitle){
  
  ggplot() +
    geom_line(
      data = history,
      aes(year, {{x}}),
      linewidth = 0.8
    ) +
    geom_line(
      data = future,
      aes(year, {{x}}, color = tfr, linetype = tfr, linewidth = tfr)
    ) +
    scale_linetype_manual(
      name = "Fertility Rate\nScenario", 
      values = c("dashed", "solid", "dashed")
    ) +
    scale_linewidth_manual(
      name = "Fertility Rate\nScenario", 
      values = c(0.4, 0.8, 0.4)
    ) +
    scale_color_manual(
      name = "Fertility Rate\nScenario", 
      values = c("gray65", "darkred", "gray65")
    ) +
    geom_point(
      data = history |> filter(year == 2023),
      aes(year, {{x}})
    ) +
    geom_segment(
      data = history |> filter(year == 2023),
      aes(year, {{x}}, xend = year - 1000, yend = {{x}})
    ) +
    geom_text(
      data = history |> filter(year == 2023),
      aes(year - 1000, {{x}}, label = year),
      hjust = 1,
      nudge_x = -100,
      size = 11/.pt
    ) +
    scale_x_continuous(
      breaks = c(-10000, -5000, 1, 4000),
      labels = c("10,000 BCE", "5,000 BCE", "1 CE", "4,000 CE"),
      limits = c(-10000, 4000),
      expand = expansion(0.03)
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = gtitle
    )
  
}

population <- make_plot(population, gtitle = "Global Population") +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2) * 1e9,
    labels = paste(seq(0, 10, by = 2), "B"),
    limits = c(0, 10.3e9),
    expand = expansion(0.03)
  )

births <- make_plot(births, gtitle = "Global Births") +
  scale_y_continuous(
    breaks = seq(0, 150, by = 50) * 1e6,
    labels = paste(seq(0, 150, by = 50), "M"),
    limits = c(0, 150e6),
    expand = expansion(0.03)
  )

# appears to be some figure hacking from the paper
# global births per year nearly triples according to their model
# but y-axis is restricted to range 0 to 150 M
suppressWarnings({ population + births + plot_layout(guide = "collect") })

ggsave(
  filename = "spike.png",
  width = 7.5,
  height = 3.5,
  dpi = 600
)
