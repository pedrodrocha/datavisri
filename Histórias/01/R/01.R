library(tidyverse)
library(vroom)
library(ggtext)
library(showtext)
library(patchwork)
library(pdftools)


font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'PT Sans',family = 'ptsans')
showtext_auto()


mid <- vroom('01_singer1970/data/MID 5 Data and Supporting Materials/MIDA 5.0.csv')
igo <- vroom('01_singer1970/data/igo_year_formatv3/igo_year_formatv3.csv')

igo %>%
  # filter(year > 1964) %>%
  group_by(year) %>%
  count() %>%
  rename('n_igos' = n) -> igo
mid %>%
 # filter(styear > 1964) %>%
  group_by(styear) %>%
  count() %>%
  rename('year' = styear, 'n_mid' = n) %>%
  left_join(.,igo) -> dat

# write_csv(dat,'01_singer1970/data/viz_dat.csv')

dat %>%
  filter(year %in% c(1816,1940,1987,2014)) -> dat_limit


dat %>%
  filter(year > 1945) %>%
  ggplot(aes(y = n_mid, x= n_igos, label = year)) +
  geom_point(
    color = '#444444',
    size = 6, alpha = .8
  )  +
  # 1948, 1ª Guerra árabe-israelense
  geom_point(
    data = dat %>% filter(year %in% c(1948)),color = '#e09f3e', size = 6
  ) +
  geom_richtext(
    aes(x = 110,y = 1,label = '**1948**: 1ª Guerra Árabe-Israelense'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 66, xend = 110, y = 0, yend = 0, colour = "#e09f3e", size=1) +
  annotate("segment", x = 66, xend = 66, y = 0, yend = 8, colour = "#e09f3e", size=1) +
  # 1950-53, Guerra da Coreia
  geom_point(
    data = dat %>% filter(year %in% c(1950)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 111.5,y = 7,label = '**1950**: Guerra da Coreia'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 79, xend = 111.5, y = 6, yend = 6, colour = "#e09f3e", size=1) +
  annotate("segment", x = 79, xend = 79, y = 6, yend = 16, colour = "#e09f3e", size=1) +
  # # 1950-53, Canal de Suez
  # geom_point(
  #   data = dat %>% filter(year %in% c(1956)),color = '#540b0e', size = 6) +
  # 1965, Guerra do Vietnã - Fase, 2
  geom_point(
    data = dat %>% filter(year %in% c(1965)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 90,y = 35,label = '**1965**: Guerra do Vietnã'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 90, xend = 122.5, y = 34, yend = 34, colour = "#e09f3e", size=1) +
  annotate("segment", x = 122.5, xend = 148, y = 34, yend = 24, colour = "#e09f3e", size=1) +
  # 1967, Guerra dos 6 Dias
  geom_point(
    data = dat %>% filter(year %in% c(1967)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 95,y = 40,label = '**1967**: Guerra dos 6 dias'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 95, xend = 127.5, y = 39, yend = 39, colour = "#e09f3e", size=1) +
  annotate("segment", x = 127.5, xend = 149, y = 39, yend = 25, colour = "#e09f3e", size=1) +
  # 1973,Guerra do Yom Kippur
  geom_point(
    data = dat %>% filter(year %in% c(1973)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 105,y = 45,label = '**1973**: Guerra do Yom Kippur'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 105, xend = 142.5, y = 44, yend = 44, colour = "#e09f3e", size=1) +
  annotate("segment", x = 142.5, xend = 183, y = 44, yend = 20, colour = "#e09f3e", size=1) +
  # 1979, Guerra do Afeganistão
  geom_point(
    data = dat %>% filter(year %in% c(1979)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 125,y = 50,label = '**1979**: Guerra do Afeganistão'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 125, xend = 160.5, y = 49, yend = 49, colour = "#e09f3e", size=1) +
  annotate("segment", x = 160.5, xend = 226, y = 49, yend = 23, colour = "#e09f3e", size=1) +
  # 1980, Iran-Iraq War
  geom_point(
    data = dat %>% filter(year %in% c(1980)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 135,y = 55,label = '**1980**: Guerra Irã-Iraque'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 135, xend = 167.5, y = 54, yend = 54, colour = "#e09f3e", size=1) +
  annotate("segment", x = 167.5, xend = 230, y = 54, yend = 27, colour = "#e09f3e", size=1) +
  # 1982, Guerra das Malvinas
  geom_point(
    data = dat %>% filter(year %in% c(1982)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 145,y = 60,label = '**1982**: Guerra das Malvinas'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 145, xend = 180, y = 59, yend = 59, colour = "#e09f3e", size=1) +
  annotate("segment", x = 180, xend = 238, y = 59, yend = 31  , colour = "#e09f3e", size=1) +
  # 1991, Guerra do Golfo
  geom_point(
    data = dat %>% filter(year %in% c(1991)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 220,y = 14,label = '**1991**: Guerra do Golfo'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 220, xend = 252.5, y = 13, yend = 13, colour = "#e09f3e", size=1) +
  annotate("segment", x = 252.5, xend = 269, y = 13, yend = 19  , colour = "#e09f3e", size=1) +
  # 1998, Guerra do Badme
  geom_point(
    data = dat %>% filter(year %in% c(1998)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 240,y = 7,label = '**1998**: Guerra do Badme'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 240, xend = 272.5, y = 6, yend = 6, colour = "#e09f3e", size=1) +
  annotate("segment", x = 272.5, xend = 285, y = 6, yend = 21, colour = "#e09f3e", size=1) +
  # 1999, Guerra de Cargil
  geom_point(
    data = dat %>% filter(year %in% c(1999)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 243,y = 55,label = '**1999**: Guerra de Cargil'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 243, xend = 275.5, y = 54, yend = 54, colour = "#e09f3e", size=1) +
  annotate("segment", x = 275.5, xend = 281, y = 54, yend = 34, colour = "#e09f3e", size=1) +
  # 2003, Invasão do Iraque
  geom_point(
    data = dat %>% filter(year %in% c(2003)),color = '#e09f3e', size = 6) +
  geom_richtext(
    aes(x = 238,y = 50,label = '**2003**: Invasão do Iraque'),
    family = 'ptsans', color = "#e09f3e",
    fill = NA, label.color = NA,size = 5
  ) +
  annotate("segment", x = 238, xend = 270.5, y = 49, yend = 49, colour = "#e09f3e", size=1) +
  annotate("segment", x = 270.5, xend = 278, y = 49, yend = 35, colour = "#e09f3e", size=1) +
  labs(
    x = '',
    y = '',
    # title = "<span style = 'font-size:30pt;'>**Organizações Internacionais levam à paz?**</span><br>
    # <p style = 'font-size:15pt;'> Para Singer e Wallace (1970), **não necessariamente**. Os autores analisaram a relação entre o número de OIs no sistema e o número de guerras em um mesmo ano e demonstraram que o aumento no número de IOs a partir de 1945 não explica a diminuição do número de conflitos armados a nível sistêmico no mesmo período</p>"
    title = "**MAIS ORGANIZAÇÕES INTERNACIONAIS, MAIOR A PAZ ?**",
    subtitle = "**Não necessariamente**. Para Wallace e Singer (1970), o número total de Organizações Internacionais (OIs) ativas não tem efeito aparente a nível sistêmico no número total de conflitos armados iniciados por Estados. <br><p>O gráfico abaixo demonstra essa relação. Cada círculo representa um ano entre 1946 e 2014, e sua posição depende do número total de OIs e do número total de disputas militarizadas (ou MIDs*) contabilizadas no período. Em <span style = 'color:#e09f3e;'><b>amarelo</b></span> estão demarcados anos em que se iniciaram algumas das principais guerras desde 1945.</p>",
    caption =  "<span style = 'color:#444444;'>* **O que são MIDs, ou disputas militarizadas?** 'São casos em que um Estado ameaça, demonstra ou usa a força explicitamente tendo como foco o governo, representantes oficiais, as forças armadas, a propriedade ou o território de outros Estados' (Jones, Bremer e Singer, 1996, p.163)</span><br><p style = 'color:#44444460;'>Pedro D. Rocha | VisRI #01 | Dados: MIDs - Palmer et al (2020). OIs - Pevenhouse, Nordstrom e McManus (2020) e Wallace e Singer (1970)</p>"
) +
  annotate("segment", x = 58, xend = 58, y = 50, yend = 65, colour = "#444444", size=1,alpha = .9, arrow=arrow()) +
  annotate("segment", x = 270, xend = 310, y = 3, yend = 3, colour = "#444444", size=1,alpha = .9, arrow=arrow()) +
  geom_richtext(
    aes(x = 50,y = 45,label = 'Número total de **MIDs** em um ano'),
    family = 'ptsans', color = "#444444", angle = 90,
    fill = NA, label.color = NA,size = 5
  ) +
  geom_richtext(
    aes(x = 263,y = 0,label = 'Número total de **OIs** em um ano'),
    family = 'ptsans', color = "#444444",
    fill = NA, label.color = NA,size = 5
  ) +
  scale_x_continuous(limits = c(50,310), breaks = seq(50,300,50)) +
  scale_y_continuous(limits = c(0,70)) +
  theme_void() +
  theme(
    plot.margin = margin(30,30,30,30),
    axis.text.x = element_text(size = 20, family = 'ptsans'),
    axis.text.y = element_text(size = 20, family = 'ptsans'),
    panel.grid.major = element_line(linetype = 'dotted', color = "#444444"),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    # panel.grid = element_line(linetype = 'dotted',color = '#44444440'),
    panel.grid = element_blank(),
    # plot.title = element_textbox_simple(margin = margin(b = 30),family = 'montserrat', color = '#444444'),
    plot.title = element_textbox_simple(
      size = 24,
      margin = margin(15, 0, 20, 0),
      color = "#444444",
      family = 'montserrat'
    ),
    plot.subtitle = element_textbox_simple(margin = margin(15, 0, 50, 0),family = 'montserrat', size = 13.5,color = "#444444"),
    axis.title.x = element_text(color = '#444444',family = 'montserrat'),
    axis.title.y = element_text(color = '#444444',family = 'montserrat'),
    plot.caption = element_textbox_simple(
      family = 'montserrat', size = 10,color = "#44444460", margin = margin(30,0,0,0)
    )
  ) -> plot1


ggsave(
  plot = plot1,
  height = 11.249,
  width = 11.249,
  filename = '01_singer1970/output/plot1.pdf',
  dpi = 400
)

pdf_convert(pdf = '01_singer1970/output/plot1.pdf', filename = '01_singer1970/output/plot1.png',
            format = "png", dpi = 400)

