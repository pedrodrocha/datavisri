library(tidyverse)
library(colorspace)
library(readxl)
library(janitor)
library(ggtext)
library(showtext)
library(pdftools)


font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'PT Sans',family = 'ptsans')
font_add_google(name = 'Nanum Gothic', family = 'nanumgothic')
showtext_auto()


dat <- read_xlsx('05/data/presidentes_la.xlsx')


dat %>%
  mutate(
    idade_inicio = inicio - ano_de_nascimento,
    idade_final = final - ano_de_nascimento,
    tempo_poder = final - inicio
  ) %>%
  filter(tempo_poder > 0, inicio > 1899) -> presidente_dat

presidente_dat_longer <- presidente_dat %>%
  pivot_longer(
    col = starts_with("idade_"),
    values_to = 'idade',
    names_to = 'tipo_idade'
  ) %>%
  mutate(
    position = if_else(tipo_idade == 'idade_inicio',3,9)
  )
presidente_dat_longer %>%
  filter(tipo_idade == 'idade_inicio') -> idade_init_longer

idade_init_longer %>%
  ggplot() +
  # geom_segment(
  #   aes(
  #     x = 36, xend = 36,
  #     y = 3.02 ,yend = 3.33
  #   ), color = '#C96E00', size = 1.1, alpha = .8
  # ) +
  # geom_segment(
  #   aes(
  #     x = 35.6, xend = 36.4,
  #     y = 3.33 ,yend = 3.33
  #   ), color = '#C96E00', size = 1.1, alpha = .8
  # ) +
  geom_point(
    aes(
      x = idade, y = position
    ),
    size = case_when(
      idade_init_longer$presidente == 'Gabriel Boric Font' ~ 6,
      idade_init_longer$presidente == "Germán Busch Becerra" ~ 6,
      TRUE ~ 5

    ),
    alpha = case_when(
      idade_init_longer$presidente == 'Gabriel Boric Font' ~ 1,
      idade_init_longer$presidente == "Germán Busch Becerra" ~ 1,
      TRUE ~ .3

    ),
    position = position_jitter(seed = 123,width = 0,height = .2),
    color = case_when(
      idade_init_longer$presidente == 'Gabriel Boric Font' ~ '#C96E00',
      # idade_init_longer$presidente %in% c(
      #   'Juan Domingo Perón 02', "Pedro Pablo Kuczynski",
      #   "Victor Paz Estenssoro 03","Rafael Caldera 02"
      #
      # ) ~ '#004534',
      TRUE ~ '#444444'

    )
  ) +
  # geom_richtext(
  #   aes(x =50, y = 3.4,
  #       label = "<span><span style = 'color:#C96E00'>**Gabriel Boric**</span> completará 36 anos em 11 de fev.</span><p>de 2022. Será um dos mais novos da história e</p><p>o mais novo em 3 décadas</p>"
  #   ),
  #   fill = NA,label.color = NA,
  #   family = 'ptsans',
  #   color = "#444444",
  #   size = 5.5,
  # ) +
  # geom_richtext(
  #   aes(x =49, y = 2.6,
  #       label = "<span><span style='color: #444444;'>**Germán Busch Becerra**</span> é o presidente mais novo</span><p>a ter assumido o poder na América do Sul.Em 1937, ano</p><p> em que assumiu a presidência da Bolívia, completou **33 anos**</p><p>de idade</p>"
  #   ),
  #   fill = NA,label.color = NA,
  #   family = 'ptsans',
  #   color = "#444444",
  #   size = 5.5,
  # ) +
  scale_x_continuous(limits = c(32,79)) +
  scale_y_continuous(limits = c(2.5,3.5)) +
  coord_cartesian(clip = 'off') +
  labs(
    # title = '<span>**<span style = "color: #C96E00;">Gabriel Boric</span> é o político mais novo a**</span><p>**assumir o poder na América do Sul?**</p><p>**Não!**</p>',
    title = " ",
    # subtitle = "<span>_(idade completa no primeiro ano de mandato)_</span>",
    subtitle = ' ',
    # caption =  "<span style = 'color:#444444;'>Pedro D. Rocha | VisRI #05 | @datavisri | Dados: Compilados do Wikipedia </span>",
    # caption =  "<span style = 'color:#444444;font-size:18pt;'>* **Como ler o gráfico?** No gráfico, a posição de cada círculo representa a idade de um político no ano em que alçou ao poder em um país sul-americano. </span> <br><p style = 'color:#444444;'>Pedro D. Rocha | VisRI #05 | @datavisri | Dados: Compilados do Wikipedia</p>",
    caption = " ",
    y = '  '
    ) +
  theme_void() +
  theme(
    plot.margin = margin(30,60,30,60),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    axis.title.y = element_markdown(),
    axis.text.x = element_markdown(
      family = 'nanumgothic', color = '#444444', size = 35
    ),
    panel.grid.major.x = element_line(linetype = 'dashed', color = "#44444430"),
    # plot.caption = element_textbox_simple(
    #   family = 'montserrat',
    #   size = 14,
    #   color = "#44444460",
    #   margin = margin(30,0,0,-14)
    # ),
    plot.title = element_textbox_simple(
      # size = 35, lineheight = 1.6,
      margin = margin(50, -20, 80, -10),
      # color = "#444444",
      # family = 'montserrat',linewidth = .2
    ),
    # plot.subtitle = element_textbox_simple(
    #   size = 20, lineheight = 1.6,
    #   margin = margin(10, -20, 0, -10),
    #   color = "#444444",
    #   family = 'montserrat',linewidth = .2
    # )
    plot.caption = element_textbox_simple(
      margin = margin(t = 60, b = 30)
    )
  ) -> jitter



ggsave(
  plot = jitter,
  height = 11.249,
  width = 11.249,
  filename = '05/output/jitter.svg',
  dpi = 400
)


pdf_convert(pdf = '05/output/jitter.pdf', filename = '05/output/jitter.png',
            format = "png", dpi = 400)




presidente_dat_longer  %>%
  filter(pais == 'Chile', tipo_idade == 'idade_inicio') %>%
  arrange(idade)


presidente_dat_longer  %>%
  filter(tipo_idade == 'idade_inicio') %>%
  arrange(idade)
