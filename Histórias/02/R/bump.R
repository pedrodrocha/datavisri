library(tidyverse)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(showtext)
library(pdftools)
library(ggbump)


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
  select(government_donor,calendar_year, prop) %>%
  group_by(calendar_year) %>%
  arrange(desc(prop)) %>%
  slice(1:20) %>%   mutate(rank = rank(-prop)) %>%
  ungroup() %>%
  mutate(is_brazil = if_else(government_donor == 'Brazil',TRUE,FALSE)) -> bump_ready
bump_ready %>%
  filter(government_donor %in% c("United States of America (the)","Germany","United Kingdom","Japan","China","Brazil"),calendar_year == 2020) -> year_2020
bump_ready %>%
  filter(
    government_donor != "Brazil",
    government_donor != "China",
    government_donor != "Japan",
    government_donor != "United Kingdom",
    government_donor != "Germany",
    !str_detect(government_donor,"United States of"),
    calendar_year == 2020) -> year2020_notfoco

bump_ready %>%
  ggplot() +
  geom_line(
    data = bump_ready %>%
      filter(
        government_donor != "Brazil",
        government_donor != "China",
        government_donor != "Japan",
        government_donor != "United Kingdom",
        government_donor != "Germany",
        !str_detect(government_donor,"United States of")
      ),
    aes(
      x = calendar_year, y = rank, group = government_donor
    ), color = "grey90", size = 1
  ) +
  geom_point(
    data = bump_ready %>%
      filter(
        government_donor != "Brazil",
        government_donor != "China",
        government_donor != "Japan",
        government_donor != "United Kingdom",
        government_donor != "Germany",
        !str_detect(government_donor,"United States of")
      ),
    aes(
      x = calendar_year, y = rank,
    ), color = "grey90", size = 3
  ) +
  # Brasil
  geom_line(
    data = bump_ready %>% filter(government_donor == "Brazil"),
    aes(x = calendar_year, y = rank, group = government_donor),
    size = 1.4, color ='#e99301'
  ) +
  geom_point(
    data = bump_ready %>% filter(government_donor == "Brazil"),
    aes(x = calendar_year, y = rank, group = government_donor),
    size = 3, color ='#e99301'
  ) +
  # Estados Unidos
  geom_line(
    data = bump_ready %>%
      filter(str_detect(government_donor,"United States of")),
    aes(
      x = calendar_year, y = rank
    ), color = '#909DC7', size = 1.4
  ) +
  geom_point(
    data = bump_ready %>% filter(str_detect(government_donor,"United States of")),
    aes(x = calendar_year, y = rank, group = government_donor),
    size = 3, color ='#909DC7'
  ) +
  # Alemanha
  geom_line(
    data = bump_ready %>%
      filter(str_detect(government_donor,"Germany")),
    aes(
      x = calendar_year, y = rank
    ), color = '#463C3D', size = 1.4
  ) +
  geom_point(
    data = bump_ready %>% filter(government_donor == "Germany"),
    aes(x = calendar_year, y = rank, group = government_donor),
    size = 3, color ='#463C3D'
  ) +
  # Reino Unido
  geom_line(
    data = bump_ready %>%
      filter(str_detect(government_donor,"United Kingdom")),
    aes(
      x = calendar_year, y = rank
    ), color = '#674254', size = 1.4
  ) +
  geom_point(
    data = bump_ready %>% filter(government_donor == "United Kingdom"),
    aes(x = calendar_year, y = rank, group = government_donor),
    size = 3, color ='#674254'
  ) +
  # Japão
  geom_line(
    data = bump_ready %>%
      filter(str_detect(government_donor,"Japan")),
    aes(
      x = calendar_year, y = rank
    ), color = '#CB937C', size = 1.4
  ) +
  geom_point(
    data = bump_ready %>% filter(government_donor == "Japan"),
    aes(x = calendar_year, y = rank, group = government_donor),
    size = 3, color ='#CB937C'
  ) +
  # China
  geom_line(
    data = bump_ready %>%
      filter(str_detect(government_donor,"China"), calendar_year > 2012),
    aes(
      x = calendar_year, y = rank
    ), color = '#C04347', size = 1.4
  ) +
  geom_line(
    data = bump_ready %>%
      filter(str_detect(government_donor,"China"), calendar_year %in% c(2011,2012,2013)),
    aes(
      x = calendar_year, y = rank
    ), color = '#C04347', size = 1.4,linetype = 'dotted'
  ) +
  geom_point(
    data = bump_ready %>% filter(government_donor == "China"),
    aes(x = calendar_year, y = rank, group = government_donor),
    size = 3, color ='#C04347'
  ) +
  geom_richtext(
    data = year_2020 %>%
      mutate(government_donor = case_when(
        government_donor == "United States of America (the)" ~ "EUA",
        government_donor == "Germany" ~ "Alemanha",
        government_donor == "United Kingdom" ~ "Reino Unido",
        government_donor == "Japan" ~ "Japão",
        government_donor == "China" ~ "China",
        government_donor == "Brazil" ~ "Brasil",
      )),
    aes(
      x = 2020, y = rank, label = glue::glue("**{rank}º {government_donor}**")
    ),
    fill = NA, label.color = NA, hjust = -.05, size = 5, family = 'ptsans',
    color = case_when(
      year_2020$government_donor == "United States of America (the)" ~ "#909DC7",
      year_2020$government_donor == "Germany" ~ "#463C3D",
      year_2020$government_donor == "United Kingdom" ~ "#674254",
      year_2020$government_donor == "Japan" ~ "#CB937C",
      year_2020$government_donor == "China" ~ "#C04347",
      year_2020$government_donor == "Brazil" ~ "#e99301",
    )
  ) +

  geom_richtext(
    data = year2020_notfoco %>%
      mutate(government_donor = case_when(
        government_donor == "Sweden" ~ "Suécia",
        government_donor == "France" ~ "França",
        government_donor == "Canada" ~ "Canadá",
        government_donor == "Norway" ~ "Noruega",
        government_donor == "Italy" ~ "Itália",
        str_detect(government_donor,"Netherlands") ~ "Holanda",
        str_detect(government_donor,"Korea") ~ "Coréia",
        government_donor == "Saudi Arabia" ~ "Arábia Saudita",
        government_donor == "Australia" ~ "Austrália",
        government_donor == "Switzerland" ~ "Suíça",
        government_donor == "Denmark" ~ "Dinamarca",
        government_donor == "Russian Federation (the)" ~ "Rússia",
        government_donor == "Spain" ~ "Espanha",
        government_donor == "Belgium" ~ "Bélgica"
      )),
    aes(
      x = 2020, y = rank, label = glue::glue("**{rank}º {government_donor}**")
    ),
    fill = NA, label.color = NA, hjust = -.05, size = 5, family = 'ptsans',color = 'grey80'

  ) +
  geom_richtext(
    aes(
      x = 2016,
      y = 18,
      label = '<span>Para além do <span style="color:#e99301">**Brasil**</span> ter caído no ranking a partir</span><p>de 2019, chama a atenção o crescimento da <span style="color:#C04347">**China**</span></p><p>durante a década: **se em 2010 e 2012 o país não**</p><p>**estava entre os 20 primeiros, em 2020 já está em**</p><p>**5º lugar**</p>'
    ),
    fill = NA, label.color = NA, # remove background and outline,
    family = 'ptsans', size = 6, colour = '#444444',hjust = .62
  ) +
  labs(
    title = '**Com Bolsonaro, o Brasil está em sua pior posição (20º) no ranking de contribuição financeira para o Sistema ONU desde 2010**',
    subtitle = "<span>(_ranking relativo a % do total de contribuições obrigatórias e voluntárias_)</span><br>",
    # caption =  "<span style='font-size:18pt; color:#444444'>* **Como ler o gráfico?** O gráfico mostra a evolução dos países em ranking de contribuição financeira para o Sistema ONU entre 2010 e 2020. Quanto melhor a posição de um país, mais acima ele está na imagem em deter-</span><p style='font-size:18pt; color:#444444'>-minado ano.</p><br><p style = 'font-size:10pt;color:#444444;'>Pedro D. Rocha | VisRI #03b | @datavisri | Dados: UN CEB</p>"
    caption =  "<p style = 'font-size:14pt;color:#444444;'>Pedro D. Rocha | VisRI #02b | @datavisri | Dados: UN CEB</p>"

  ) +
  coord_cartesian(clip = 'off') +
  scale_y_reverse(limits = c(20,1), breaks = seq(20,1,-1)) +
  scale_x_continuous(limits = c(2010,2020), breaks = c(2010,2020)) +
  theme_void() +
  theme(
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    plot.margin = margin(30,110,30,30),
    axis.text.x = element_markdown(size = 30, color = "#444444",family = 'ptsans', margin = margin(t = 20)),
    axis.line.x = element_line(size = 1.3, color = "#444444"),
    plot.title = element_textbox_simple(
      size = 30,
      margin = margin(15, 0, 0, 0),
      color = "#444444",
      family = 'montserrat',linewidth = .2
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(10, 0, 10, 0),
      family = 'montserrat',
      size = 18,
      color = "#444444"),
    plot.caption = element_textbox_simple(
      family = 'montserrat', margin = margin(30,0,0,0)
    ),
    )   -> bump

ggsave(
  plot = bump,
  height = 11.249,
  width = 11.249,
  filename = '03/output/bump.pdf',
  dpi = 400
)

pdf_convert(pdf = '03/output/bump.pdf', filename = '03/output/bump.png',
            format = "png", dpi = 400)

