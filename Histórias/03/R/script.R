library(tidyverse)
library(readxl)
library(ggtext)
library(showtext)
library(patchwork)
library(pdftools)



font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'PT Sans',family = 'ptsans')
showtext_auto()



dat <- read_xlsx('02_allison2017/data/data.xlsx')
dat$mark <- 0
dat %>%
  pivot_longer(cols = starts_with('period_'),names_to = 'anos',values_to = 'anos_valores') -> dat_line


dat %>%
  mutate(position = ID * (95/17)) -> dat

dat %>%
  mutate(
    rulling_power = case_when(
      rulling_power == "Portugal" ~ "POR",
      rulling_power == "França" ~ "FRA",
      rulling_power == "Império Habsburgo" ~ "Imp. HAB",
      rulling_power == "Holanda" ~ "HOL",
      rulling_power == "Reino Unido" ~ "GB",
      rulling_power == "França e Reino Unido" ~ "FRA e GB",
      rulling_power == "China e Rússia" ~ "CHN e RUS",
      rulling_power == "União Soviética, França e Reino Unido" ~ "URSS, FRA e GB",
      rulling_power == "Estados Unidos" ~ "EUA",
      rulling_power == "Reino Unido e França" ~ "GB e FRA"
    ),
    rising_power = case_when(
      rising_power == "Espanha"  ~ "ESP",
      rising_power == "Império Habsburgo" ~ "Imp. HAB",
      rising_power == "Império Otomano"  ~ "Imp. OTO",
      rising_power == "Suécia"  ~ "SUE",
      rising_power == "Reino Unido"  ~ "GB",
      rising_power == "França"  ~ "FRA",
      rising_power == "Império Russo"  ~ "Imp. RUS",
      rising_power == "Alemanha"  ~ "ALE",
      rising_power == "Japão"  ~ "JPN",
      rising_power == "Estados Unidos"  ~ "EUA",
      rising_power == "União Soviética"  ~ "URSS",
      rising_power == "China"  ~ "CHN",
    )
  ) -> dat
dat_line
dat %>%
  ggplot() +
  # geom_line(
  #   data = dat_line,
  #   aes(x = anos_valores,y = mark),
  #   color = '#444444',
  #   size = 2
  # ) +
  # ID: 1
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[1], yend = dat$position[1], colour = "#44444420", size=10) +
  annotate("segment", x = dat$period_start[1], xend = dat$period_end[1], y = dat$position[1], yend = dat$position[1], colour = "#302F51", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_end[1] - 47,
      y = position[1] - .5,
      label = paste0("**",rulling_power[1]," vs. ",rising_power[1],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#302F51'
  ) +
  # ID: 2
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[2], yend = dat$position[2], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[2], xend = dat$period_end[2], y = dat$position[2], yend = dat$position[2], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_end[2] - 95,
      y = position[2] - .5,
      label = paste0("**",rulling_power[2]," vs. ",rising_power[2],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 3
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[3], yend = dat$position[3], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[3], xend = dat$period_end[3], y = dat$position[3], yend = dat$position[3], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_end[3] - 245,
      y = position[3] - .5,
      label = paste0("**",rulling_power[3]," vs. ",rising_power[3],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 4
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[4], yend = dat$position[4], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[4], xend = dat$period_end[4], y = dat$position[4], yend = dat$position[4], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[4] - 36,
      y = position[4] - .5,
      label = paste0("**",rulling_power[4]," vs. ",rising_power[4],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 5
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[5], yend = dat$position[5], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[5], xend = dat$period_end[5], y = dat$position[5], yend = dat$position[5], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[5] - 25,
      y = position[5] - .5,
      label = paste0("**",rulling_power[5]," vs. ",rising_power[5],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 6
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[6], yend = dat$position[6], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[6], xend = dat$period_end[6], y = dat$position[6], yend = dat$position[6], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[6] - 24.5,
      y = position[6] - .5,
      label = paste0("**",rulling_power[6]," vs. ",rising_power[6],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 7
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[7], yend = dat$position[7], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[7], xend = dat$period_end[7], y = dat$position[7], yend = dat$position[7], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[7] - 24.5,
      y = position[7] - .5,
      label = paste0("**",rulling_power[7]," vs. ",rising_power[7],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 8
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[8], yend = dat$position[8], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[8], xend = dat$period_end[8], y = dat$position[8], yend = dat$position[8], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[8] - 44.5,
      y = position[8] - .5,
      label = paste0("**",rulling_power[8]," vs. ",rising_power[8],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 9
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[9], yend = dat$position[9], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[9], xend = dat$period_end[9], y = dat$position[9], yend = dat$position[9], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[9] - 27,
      y = position[9] - .5,
      label = paste0("**",rulling_power[9]," vs. ",rising_power[9],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 10
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[10], yend = dat$position[10], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[10], xend = dat$period_end[10], y = dat$position[10], yend = dat$position[10], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[10] - 38,
      y = position[10] - .5,
      label = paste0("**",rulling_power[10]," vs. ",rising_power[10],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 11
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[11], yend = dat$position[11], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[11], xend = dat$period_end[11], y = dat$position[11], yend = dat$position[11], colour = "#302F51", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[11] - 24,
      y = position[11] - .5,
      label = paste0("**",rulling_power[11]," vs. ",rising_power[11],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#302F51'
  ) +
  # ID: 12
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[12], yend = dat$position[12], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[12], xend = dat$period_end[12], y = dat$position[12], yend = dat$position[12], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[12] - 24.5,
      y = position[12] - .5,
      label = paste0("**",rulling_power[12]," vs. ",rising_power[12],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 13
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[13], yend = dat$position[13], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[13], xend = dat$period_end[13], y = dat$position[13], yend = dat$position[13], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[13] - 46.5,
      y = position[13] - .5,
      label = paste0("**",rulling_power[13]," vs. ",rising_power[13],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 14
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[14], yend = dat$position[14], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[14], xend = dat$period_end[14], y = dat$position[14], yend = dat$position[14], colour = "#C16337", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[14] - 26,
      y = position[14] - .5,
      label = paste0("**",rulling_power[14]," vs. ",rising_power[14],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#C16337'
  ) +
  # ID: 15
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[15], yend = dat$position[15], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[15], xend = dat$period_end[15], y = dat$position[15], yend = dat$position[15], colour = "#302F51", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[15] - 29,
      y = position[15] - .5,
      label = paste0("**",rulling_power[15]," vs. ",rising_power[15],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#302F51'
  ) +
  # ID: 16
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[16], yend = dat$position[16], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[16], xend = dat$period_end[16], y = dat$position[16], yend = dat$position[16], colour = "#302F51", size=9, alpha = .8, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[16] - 35,
      y = position[16] - .5,
      label = paste0("**",rulling_power[16]," vs. ",rising_power[16],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#302F51'
  ) +
  # ID: 17
  # annotate("segment", x = 1480, xend = 2021, y = dat$position[17], yend = dat$position[17], colour = "#44444420", size=9) +
  annotate("segment", x = dat$period_start[17], xend = dat$period_end[17], y = dat$position[17], yend = dat$position[17], colour = "#754149", size=9, lineend = 'round') +
  geom_richtext(
    aes(
      x = period_start[17] - 27,
      y = position[17] - .5,
      label = paste0("**",rulling_power[17]," vs. ",rising_power[17],"**")
    ),
    fill = NA, label.color = NA, size = 7,family = 'ptsans', colour = '#754149'
  ) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(breaks = c(1500,1600,1700,1800,1900,2000)) +
  coord_cartesian(clip = 'off') +
  labs(
    # title = 'A Armadilha de Tucídides: estariam **Estados Unidos** e **China** destinados à Guerra?',
    # title = 'A Armadilha de Tucídides: estariam **Estados Unidos** e **China** destinados à Guerra?',
    # title = 'A Armadilha de Tucídides',

    # subtitle = '<span>A Armadilha de Tucídides descreve a aparente tendência inevitável à guerra em períodos de transição de poder.</span><p>Na história, Allison(2017) identifica somente 4 casos de <span style = "color:#302F51;">**transição pacífica**</span>. Em todos outros 12 a <span style = "color:#C16337;">**guerra**</span> não pode</p></span><p>ser evitada. Seria este também o destino de <span style = "color:#754149;">**Estados Unidos e China**</span> no Século XXI?</p>',
    caption =  "<span style = 'color:#444444;font-size:23.5pt;'>* **Como ler o gráfico?** O gráfico é uma linha do tempo. As barras demonstram o período em que uma transição de poder es-</span> <p style = 'color:#444444;font-size:23.5pt;'>-teve ativa. Em <span style = 'color:#C16337;'>**laranja**</span> estão àquelas em que houve guerra. Em <span style = 'color:#302F51;'>**azul**</span>, estão as transições pacíficas. Em <span style = 'color:#754149;'>**marrom**</span>, está a atual</p><p style = 'color:#444444;font-size:23.5pt;'>transição entre Estados Unidos e China.</p><br><p style = 'color:#444444;'>Pedro D. Rocha | VisRI #03 | @datavisri | Dados: Allison (2017)</p>"
  ) +
  geom_richtext(
    aes(
      # x = 1690 - 115, # Twitter
      # y = 91, # Twitter,
      x = 1690 - 138, # Instagram
      y = 90, # instagram
      label = '<span>**A Armadilha de Tucídides**</span>',
    ),
    fill = NA, label.color = NA,
    # size = 20, # Twitter
    size = 17, # Instagram
    family = 'montserrat',
    color = "#444444"
  ) +
  geom_richtext(
    aes(
      # x = 1690 - 113, # Twitter
      # y = 74, # Twitter
      x = 1690 - 132, # instagram
      y = 71, # Instagram
      label = '<span>A Armadilha de Tucídides descreve a aparente tendência inevitável à guerra em</span><p>períodos de transição de poder. Na história, Allison(2017) identifica somente 4 </p></span><p>casos de <span style = "color:#302F51;">**transição pacífica**</span>. Em todos outros 12 a <span style = "color:#C16337;">**guerra**</span> não pode ser evitada.</p><br><p>**Seria este também o destino de <span style = "color:#754149;">Estados Unidos e China</span> no Século XXI?**</p>',
    ),
    fill = NA, label.color = NA,
    # size = 8, # Twitter
    size = 7, # instagram
    family = 'ptsans', color = "#444444"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    plot.margin = margin(30,30,30,80),
    plot.title = element_textbox_simple(
      margin = margin(25, 0, 30, 130),
      color = "#444444",
      # family = 'amatic',
      # size = 60,
      family = 'montserrat',
      # size = 35,
      size = 50,
      face = 'bold',
      # margin = margin(25, 0, 0, 55),
    ),
    plot.subtitle = element_textbox_simple(
      size = 26,
      margin = margin(25, 0, 20, 70),
      color = "#444444",
      family = 'montserrat',
      linewidth = 1
    ),
    plot.caption = element_textbox_simple(
      family = 'montserrat', size = 16, margin = margin(35,10,0,-20),lineheight = 1.5
    ),
    axis.text.x = element_text(size = 50, color = "#444444",family = 'ptsans'),
    panel.grid.major.x = element_line(linetype = 'dashed', color = "#44444470")
  ) -> plot2



ggsave(
  plot = plot2,
  height = 11.249,
  width = 22.498,
  filename = '02_allison2017/output/instagram.pdf',
  dpi = 400
)

pdf_convert(pdf = '02_allison2017/output/instagram.pdf', filename = '02_allison2017/output/instagram.png',
            format = "png", dpi = 400)



