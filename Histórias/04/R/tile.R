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
  filter(country_of_origin_iso == 'VEN', year == 2021)  %>%
  select(country_of_asylum,country_of_asylum_iso,refugees_under_unhc_rs_mandate) %>%
  rename("alpha.3" = country_of_asylum_iso)-> ven2021

dat %>%
  filter(country_of_origin_iso == 'VEN', country_of_asylum == "Spain")  %>%
  select(country_of_asylum,country_of_asylum_iso,refugees_under_unhc_rs_mandate,year)

# Source Tile Grid: https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv
# Example Tile: # https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html
tilegrid <- read_csv('04_refugee_ven/data/worldtilegrid.csv')

tilegrid  %>%
  filter(region == 'Americas') %>%
  select(name, alpha.2,alpha.3,region,x,y) %>%
  left_join(.,ven2021, by = 'alpha.3') %>%
  mutate(refugees_under_unhc_rs_mandate = replace_na(refugees_under_unhc_rs_mandate,0)) -> plot_dat

sequential_hcl(n = 5,"Sunset") -> my_pallete


plot_dat %>%
  mutate(
    alpha.3 = case_when(
      alpha.3 == 'USA' ~ 'EUA',
      alpha.3 == 'GTM' ~ 'GUA',
      alpha.3 == 'SLV' ~ 'ELS',
      alpha.3 == 'HND' ~ 'HON',
      alpha.3 == 'CRI' ~ 'COS',
      alpha.3 == 'PRY' ~ 'PAR',
      alpha.3 == 'URY' ~ 'URU',
      alpha.3 == 'BHS' ~ 'BHA',
      alpha.3 == 'HTI' ~ 'HAI',
      alpha.3 == 'ATG' ~ 'ANT',
      alpha.3 == 'KNA' ~ 'STK',
      alpha.3 == 'LCA' ~ 'STL',
      alpha.3 == 'VCT' ~ 'STV',
      alpha.3 == 'BRB' ~ 'BAR',
      alpha.3 == 'DMA' ~ 'DMI',
      alpha.3 == 'GRD' ~ 'GRN',
      alpha.3 == 'TRO' ~ 'TRI',
      TRUE ~ alpha.3
    )
  )  -> plot_dat

plot_dat %>%
  filter(
    refugees_under_unhc_rs_mandate > 0, alpha.3 != "VEN"
  ) %>%
  ggplot(
    aes(
      xmin = x, ymin = y,
      xmax = x + 1, ymax = y + 1
    )
  ) +
  geom_rect(aes(fill = refugees_under_unhc_rs_mandate), color = 'white') +
  geom_richtext(
    aes(x = x, y = y, label = glue::glue('**{alpha.3}**')),
    fill = NA,label.color = NA,
    nudge_x = 0.5,
    nudge_y = -0.5,
    size = 4,
    family = 'ptsans',
    color = "white"
  ) +
  scale_fill_stepsn(
    colours = rev(my_pallete),
    breaks = c(1000,5000,15000,30000,40000),
    labels = c('1 mil','5 mil','15 mil','30 mil','40 mil')
  ) +
  # scale_fill_steps(
  #   low = '#E0C241',
  #   high = '#704D9E',
  #   breaks = c(1000,5000,15000,30000,40000),
  #   labels = c('1 mil','5 mil','15 mil','30 mil','40 mil')
  # ) +
  guides(
    fill = guide_colorsteps(
      title = '',
      barwidth = 12,
      barheight = 1,
      label.theme = element_text(family = 'ptsans', color = '#444444',face = 'bold.italic',size = 9),
      frame.colour = 'white',
      frame.linewidth = 2
    )
  ) +
  geom_richtext(
    aes(
      x = 1.35, y = 13.3,
      label = "**Legenda**"
    ),
    fill = NA,label.color = NA,
    family = 'ptsans', color = "#444444", size = 5

  ) +
  geom_rect(
    data = plot_dat %>% filter(refugees_under_unhc_rs_mandate == 0, alpha.3 != "VEN"),
    color = 'white',
    fill = '#44444430'
  ) +
  geom_richtext(
    data = plot_dat %>% filter(refugees_under_unhc_rs_mandate == 0, alpha.3 != "VEN"),
    aes(x = x, y = y, label = glue::glue('**{alpha.3}**')),
    fill = NA,label.color = NA,
    nudge_x = 0.5,
    nudge_y = -0.5,
    size = 4,
    family = 'ptsans',
    color = "white"
  ) +
  labs(
    # title = "<span>**Em 2021, o Brasil foi o país das Américas que mais recebeu refugiados da Venezuela sob mandato do ACNUR**</span>",
    title = "<span>**Nas Américas, o Brasil foi o país que mais recebeu refugiados registrados pelo ACNUR**</span>",
    caption =  "<span style = 'color:#444444;'>Pedro D. Rocha | VisRI #04c | @datavisri | Dados: ACNUR </span>"

    ) +
  # geom_richtext(
  #   aes(
  #     x = 2.8, y = 11,
  #     label = "<span>Dos principais países receptores,</span><p>somente a **Espanha** não está nas</p><p>Américas. O país recebeu 84 mil</p><p>venezuelanos em 2021.</p>"
  #   ),
  #   fill = NA,label.color = NA,
  #   family = 'ptsans', color = "#444444", size = 7
  # ) +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    plot.margin = margin(40,30,30,30),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position =  c(.16, .145),
    legend.direction="horizontal",
    plot.title = element_textbox_simple(
      size = 30,
      margin = margin(20, 30, 25, 30),
      color = "#444444",
      family = 'montserrat',linewidth = .2
    ),
    plot.caption = element_textbox_simple(
      family = 'montserrat',
      size = 14,
      color = "#44444460",
      margin = margin(0,30,0,30)
    )
  ) -> plot2



ggsave(
  plot = plot2,
  height = 11.249,
  width = 11.249,
  filename = '04_refugee_ven/output/plot2.pdf',
  dpi = 400
)


pdf_convert(pdf = '04_refugee_ven/output/plot2.pdf', filename = '04_refugee_ven/output/plot2.png',
            format = "png", dpi = 400)




