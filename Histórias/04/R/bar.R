library(tidyverse)
library(colorspace)
library(readxl)
library(janitor)
library(ggtext)
library(showtext)
library(pdftools)
library(patchwork)
library(RColorBrewer)

font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'PT Sans',family = 'ptsans')
showtext_auto()

dat <- read_xlsx('04_refugee_ven/data/destino.xlsx',na = '-') %>%
  clean_names()
dat %>%
  filter(year == 2021,country_of_origin_iso == 'VEN') %>%
  select(country_of_asylum,refugees_under_unhc_rs_mandate) %>%
  arrange(desc(refugees_under_unhc_rs_mandate)) %>%
  head(5) -> dat_bar


dat_bar %>%
  mutate(
    n = case_when(
      country_of_asylum == "Spain" ~ "84 mil",
      country_of_asylum == "Brazil" ~ "48 mil",
      country_of_asylum == "United States of America" ~ "18 mil",
      country_of_asylum == "Mexico" ~ "14 mil",
      country_of_asylum == "Canada" ~ "3 mil",
    ),
    country_of_asylum = case_when(
      country_of_asylum == "Spain" ~ "Espanha",
      country_of_asylum == "Brazil" ~ "Brasil",
      country_of_asylum == "United States of America" ~ "Estados Unidos",
      country_of_asylum == "Mexico" ~ "México",
      country_of_asylum == "Canada" ~ "Canadá",
    )
  ) -> dat_bar

dat_bar %>%
  ggplot() +
  geom_col(
    aes(
      y = reorder(country_of_asylum,refugees_under_unhc_rs_mandate),
      x =refugees_under_unhc_rs_mandate
    ),
    width = .3,
    fill = case_when(
      dat_bar$country_of_asylum == 'Espanha' ~ "#704D9E",
      TRUE ~ "#444444"
    ),
  ) +
  geom_richtext(
    aes(
      x = 0, y = country_of_asylum,
      label = glue::glue('**{country_of_asylum}**')
    ),
    fill = NA,label.color = NA,
    size = 8, # Twitter
    family = 'ptsans',
    color = case_when(
      dat_bar$country_of_asylum == 'Espanha' ~ "#704D9E",
      TRUE ~ "#444444"
    ),
    hjust = -.0001,
    vjust = -.9
  ) +
  geom_richtext(
    aes(
      x = refugees_under_unhc_rs_mandate, y = country_of_asylum,
      label = glue::glue('**{n}**')
    ),
    fill = NA,label.color = NA,
    size = 8, # Twitter
    family = 'ptsans',
    color = case_when(
      dat_bar$country_of_asylum == 'Espanha' ~ "#704D9E",
      TRUE ~ "#444444"
    ),
    hjust = -.1
  ) +
  labs(
    title = '<span>**Em 2021, a <span style="color:#444444;">Espanha</span> foi o país que mais recebeu refugiados da Venezuela sob mandato do ACNUR**</span>',
    caption =  "<span style = 'color:#444444;'>Pedro D. Rocha | VisRI #04b | @datavisri | Dados: ACNUR </span>"
  ) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(
    plot.margin = margin(30,60,30,30),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.direction="horizontal",
    plot.title = element_textbox_simple(
      size = 30,
      margin = margin(20, 60, 30, 30),
      color = "#444444",
      family = 'montserrat',linewidth = .2
    ),
    plot.caption = element_textbox_simple(
      family = 'montserrat', size = 14,color = "#44444460",
      margin = margin(30,60,0,30)
    )
  ) -> bar



ggsave(
  plot = bar,
  height = 11.249,
  width = 11.249,
  filename = '04_refugee_ven/output/plot3.pdf',
  dpi = 400
)


pdf_convert(pdf = '04_refugee_ven/output/plot3.pdf', filename = '04_refugee_ven/output/plot3.png',
            format = "png", dpi = 400)




