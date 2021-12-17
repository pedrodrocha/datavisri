library(tidyverse)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(showtext)
library(pdftools)



font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'Roboto',family = 'roboto')
font_add_google(name = 'PT Sans',family = 'ptsans')
showtext_auto()

dat <- read_xlsx('03/data/pivot.xlsx')

dat %>%
  filter(calendar_year != 2009) %>%
  group_by(government_donor,calendar_year) %>%
  summarise(sum = sum(amount)) -> yearly

yearly %>%
  group_by(calendar_year) %>%
  summarise(total = sum(sum))  %>%
  ungroup() -> tot_year

yearly %>%
  left_join(.,tot_year) %>%
  mutate(prop = sum/total * 100, calendar_year = as.numeric(calendar_year)) %>%
  filter(prop > 0) -> dat_ready

dat_ready %>%
  filter(government_donor == 'Brazil') -> br


dat_ready %>%
  filter(calendar_year == 2018) %>%
  arrange(desc(prop)) %>%
  head(20)

data = dat_ready %>%
  filter(government_donor %in% c("United States of America (the)","Germany","United Kingdom","Japan","China","Brazil"), calendar_year %in% c(2010,2020)) -> year201020

year201020 %>% filter(calendar_year == 2020) -> year2020
dat_ready %>%
  ggplot() +
  geom_line(
    aes(
    x = calendar_year, y = prop, group = government_donor
    ),
    color = "grey",
  ) +
  # Brasil
  geom_line(
    data = dat_ready %>%
      filter(government_donor == 'Brazil'),
    aes(
      x = calendar_year, y = prop
    ),
    color = '#e99301',
    size = 1.4
  ) +
  # Estados Unidos
  geom_line(
    data = dat_ready %>%
      filter(str_detect(government_donor,"United States of")),
    aes(
      x = calendar_year, y = prop
    ), color = '#909DC7', size = 1.4
  ) +
  # Alemanha
  geom_line(
    data = dat_ready %>%
      filter(str_detect(government_donor,"Germany")),
    aes(
      x = calendar_year, y = prop
    ), color = '#463C3D', size = 1.4
  ) +
  # Reino Unido
  geom_line(
    data = dat_ready %>%
      filter(str_detect(government_donor,"United Kingdom")),
    aes(
      x = calendar_year, y = prop
    ), color = '#674254', size = 1.4
  ) +
  # Japão
  geom_line(
    data = dat_ready %>%
      filter(str_detect(government_donor,"Japan")),
    aes(
      x = calendar_year, y = prop
    ), color = '#CB937C', size = 1.4
  ) +
  # China
  geom_line(
    data = dat_ready %>%
      filter(government_donor == 'China'),
    aes(
      x = calendar_year, y = prop
    ), color = '#C04347', size = 1.4
  ) +
  labs(
    # title = "<span>**Os <span style = 'color:#909DC7;'>Estados Unidos</span> são o país que mais contribui com o orçamento das Nações Unidas. Em 2020, o <span style = 'color:#e99301;'>Brasil</span> contribuiu com menos de 1% do total**</span>",
    title = "<span>**Em 2020, o <span style = 'color:#e99301;'>Brasil</span> contribuiu com menos de 1% do orçamento total das Nações Unidas. Os <span style = 'color:#909DC7;'>Estados Unidos</span>, com 27.5% do total, é o país que mais contribuiu**</span>",

    subtitle = "(% do total de contribuições obrigatórias e voluntárias)",
    x = "",
    y = "",
    caption =  "<span style = 'color:#444444;'>Pedro D. Rocha | VisRI #02a | @datavisri | Dados: UN CEB</span>"
    ) +
  geom_richtext(
    data = dat_ready %>%
      filter(government_donor %in% c("United States of America (the)","Germany","United Kingdom","Japan","China","Brazil"),calendar_year == 2020) %>%
      mutate(government_donor = case_when(
        government_donor == "United States of America (the)" ~ "EUA",
        government_donor == "Germany" ~ "Alemanha",
        government_donor == "United Kingdom" ~ "Reino Unido",
        government_donor == "Japan" ~ "Japão",
        government_donor == "China" ~ "China",
        government_donor == "Brazil" ~ "Brasil",
      )),
    aes(
      x = 2020, y = prop, label = glue::glue("**{government_donor} ({as.character(round(prop,1))}%)**")
    ),
    fill = NA, label.color = NA, hjust = -.05, size = 5, family = 'ptsans',
    color = case_when(
      year2020$government_donor == "United States of America (the)" ~ "#909DC7",
      year2020$government_donor == "Germany" ~ "#463C3D",
      year2020$government_donor == "United Kingdom" ~ "#674254",
      year2020$government_donor == "Japan" ~ "#CB937C",
      year2020$government_donor == "China" ~ "#C04347",
      year2020$government_donor == "Brazil" ~ "#e99301",
    )
  ) +
  geom_point(
    data =year2020,
    aes(
      x = calendar_year, y = prop
    ),
    color = case_when(
      year2020$government_donor == "United States of America (the)" ~ "#909DC7",
      year2020$government_donor == "Germany" ~ "#463C3D",
      year2020$government_donor == "United Kingdom" ~ "#674254",
      year2020$government_donor == "Japan" ~ "#CB937C",
      year2020$government_donor == "China" ~ "#C04347",
      year2020$government_donor == "Brazil" ~ "#e99301",
    ), size = 3

  ) +
  # geom_richtext(
  #   data =year2020,
  #   aes(
  #     x = calendar_year, y = prop, label = glue::glue("**{as.character(round(prop,1))}%**")
  #   ),
  #   fill = NA, label.color = NA, family = 'ptsans', size = 4, vjust = -.3,
  #   color = case_when(
  #     year2020$government_donor == "United States of America (the)" ~ "#909DC7",
  #     year2020$government_donor == "Germany" ~ "#463C3D",
  #     year2020$government_donor == "United Kingdom" ~ "#674254",
  #     year2020$government_donor == "Japan" ~ "#CB937C",
  #     year2020$government_donor == "China" ~ "#C04347",
  #     year2020$government_donor == "Brazil" ~ "#e99301",
  #   )
  # ) +
  scale_x_continuous(breaks = c(2010,2020)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(
    plot.margin = margin(30,110,30,30),
    panel.grid = element_blank(),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    legend.position = 'none',
    plot.title = element_textbox_simple(
      size = 30,
      margin = margin(20, 0, 10, 0),
      color = "#444444",
      family = 'montserrat',linewidth = .2
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(15, 0, 50, 0),
      family = 'montserrat',
      size = 18,
      color = "#444444"),
    plot.caption = element_textbox_simple(
      family = 'montserrat', size = 14,color = "#44444460", margin = margin(30,0,0,0)
    ),
    axis.text.x = element_text(size = 30, color = "#444444",family = 'ptsans', margin = margin(t = 20) ),
    axis.line.x = element_line(size = 1.3, color = "#444444"),
    axis.text.y = element_blank()

  ) -> plot



ggsave(
  plot = plot,
  height = 11.249,
  width = 11.249,
  filename = '03/output/line.pdf',
  dpi = 400
)

pdf_convert(pdf = '03/output/line.pdf', filename = '03/output/line.png',
            format = "png", dpi = 400)



dat_ready %>%
  filter(calendar_year %in% c(2010,2020), government_donor == 'Brazil') %>%
  arrange(desc(prop))
dat_ready %>%
  filter(calendar_year %in% c(2020)) %>%
  arrange(desc(prop))
