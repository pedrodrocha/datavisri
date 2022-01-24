library(tidyverse)
library(readxl)


dat <- read_xlsx('08_nuclear_latency/data/prep.xlsx')

dat %>%
  filter(!is.na(nuclear_start)) %>%
  pivot_longer(cols = c(nuclear_start,nuclear_end),names_to = 'type',values_to = "year") -> nuclear
nuclear
dat %>%
  pivot_longer(cols = c(min_start,max_end),names_to = 'type',values_to = 'year') %>%
  ggplot() +
  geom_line(
    aes(
      x = year,
      y = country_me,
      group = country_me
    )
  ) +
  geom_line(
    data = nuclear,
    aes(
      x = year,
      y = country_me,
      group = country_me
    ), color = 'red'
  ) +
  geom_point(
    aes(
      x = year,
      y = country_me
    ),
  ) +
  scale_x_continuous(breaks = seq(1940,2012,5)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) -> plot

plot
ggsave(
  plot = plot,
  height = 11.249,
  width = 11.249,
  filename = '08_nuclear_latency/output/plot.svg',
  dpi = 400
)
