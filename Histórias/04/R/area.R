library(tidyverse)
library(colorspace)
library(readxl)
library(janitor)
library(ggtext)
library(showtext)
library(pdftools)


font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'PT Sans',family = 'ptsans')
showtext_auto()

dat <- read_xlsx('04_refugee_ven/data/population.xlsx',na = '-') %>%
  clean_names()
dat %>%
  select(year, country_of_origin,country_of_origin_iso,refugees_under_unhc_rs_mandate) -> refugees

dat %>%
  select(year, country_of_origin,country_of_origin_iso,asylum_seekers) -> asylum_seekers

refugees  %>%
  filter(country_of_origin_iso == 'VEN') -> refugees_ven

refugees_ven %>%
  filter(year >= 2017)  -> ven_2017

refugees_ven %>%
  group_by(year) %>%
  summarise(n = sum(refugees_under_unhc_rs_mandate)) %>%
  ggplot() +
  geom_area(
    aes(x = year, y = n),
    fill = '#704D9E', alpha = .4
  ) +
  geom_area(
    data = ven_2017,
    aes(x = year, y = refugees_under_unhc_rs_mandate),
    fill = '#704D9E'
  ) +
  geom_richtext(
    aes(x = 2021, y = 190000, label = "**186 mil +**"),
    fill = NA,label.color = NA,
    family = 'ptsans',
    color = "#444444",
    size = 7
  ) +
  geom_richtext(
    aes(x = 2021, y = 0, label = "**0**"),
    fill = NA,label.color = NA,
    family = 'ptsans',
    color = "#444444",
    size = 7,
    vjust = 1.3
  ) +
  geom_richtext(
    aes(x = 2017 - .6, y = 9272, label = "**9 mil +**"),
    fill = NA,label.color = NA,
    family = 'ptsans',
    color = "#444444",
    size = 7,
    vjust = 0
  ) +
  labs(
    title = '<span>**A Venezuela vive hoje a maior crise de**</span><p>**<span style="color:#444444;">refugiados</span> da América Latina**</p>',
    caption =  "<span style = 'color:#444444;'>Pedro D. Rocha | VisRI #04a | @datavisri | Dados: ACNUR </span>"
  ) +
  geom_richtext(
    aes(
      x = 2012 - .8,
      y = 130000,
      label = '<span style="color:#704D9E00;">,,</span><span><span> </span><span> </span><span> </span>Em 2021, já são quase <span style="color:#704D9E;">**190  mil venezuelanos**</span></span><p><span style="color:#704D9E00;">,</span><span style="color:#704D9E;">**em refúgio**</span> no mundo todo. É a crise mais intensa</p><p><span> </span><span> </span><span style="color:#704D9E00;">_,,</span>fora de contexto de guerra da história recente</p>',
    ),
    fill = NA,label.color = NA,
    size = 8, # Twitter
    family = 'ptsans',
    color = "#444444"
  ) +
  scale_y_continuous(limits = c(0,191000),position = 'right') +
  scale_x_continuous(limits = c(2001,2021), breaks = seq(2001,2021,5)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(
    plot.margin = margin(20,40,20,40),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = 'dashed', color = "#44444430"),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 40, color = "#444444",family = 'ptsans', margin = margin(t = 20)),
    axis.title.x = element_blank(),
    # axis.line.x = element_line(size = 2, color = "#444444"),
    plot.title = element_textbox_simple(
      size = 35, lineheight = 1.6,
      margin = margin(20, 0, 0, 0),
      color = "#444444",
      family = 'montserrat',linewidth = .2
    ),
    plot.caption = element_textbox_simple(
      family = 'montserrat', size = 14,color = "#44444460", margin = margin(30,0,0,-9)
    )
  ) -> plot



ggsave(
  plot = plot,
  height = 11.249,
  width = 11.249,
  filename = '04_refugee_ven/output/plot1_roxo.pdf',
  dpi = 400
)


pdf_convert(pdf = '04_refugee_ven/output/plot1_roxo.pdf', filename = '04_refugee_ven/output/plot1_roxo.png',
            format = "png", dpi = 400)



