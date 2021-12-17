library(tidyverse)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(showtext)
library(pdftools)
library(ggforce)

font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'Roboto',family = 'roboto')
font_add_google(name = 'PT Sans',family = 'ptsans')
showtext_auto()

dat <- read_xlsx('03/data/pivot.xlsx')

dat %>%
  filter(
    government_donor == "Brazil", calendar_year == 2020
  )

dat %>%
  filter(
    government_donor == "Brazil"
  )  %>%
  mutate(
    tipo = if_else(str_detect(rev_type,"non-core"),"**Voluntária**","**Obrigatória**")
  ) %>%
  group_by(tipo,calendar_year) %>%
  summarise(sum = sum(amount))  %>%
  ungroup() %>%
  mutate(
    calendar_year = as.numeric(calendar_year),
    sum = sum/1000000
  ) -> data_prep

for_labs <- data_prep %>% filter(calendar_year %in% c(2010,2018,2019,2020))


data_prep %>%
  ggplot() +
  annotate(xmin = 2018, xmax = 2020,
           ymin = 0, ymax = 1000,
           geom = "rect", alpha = .05, color = "grey") +
  geom_line(
    aes(x = calendar_year, y = sum, group = tipo, color = tipo), size = 1.5,
    alpha = case_when(
      data_prep$calendar_year < 2018 ~ .4,
      TRUE ~ 1
    )
  ) +
  geom_point(
    aes(x = calendar_year, y = sum, group = tipo, color = tipo), size = 2.8,
    alpha = case_when(
      data_prep$calendar_year < 2018 & data_prep$calendar_year != 2010 ~ .4,
      TRUE ~ 1
    )
  ) +
  geom_richtext(
    data = data_prep %>% filter(calendar_year == 2020),
    aes(x = calendar_year, y = sum, label = tipo, color = tipo),

    fill = NA, label.color = NA, size = 6, family = 'ptsans',hjust = -.08
  ) +
  geom_richtext(
    dat =for_labs ,
    aes(x = calendar_year, y = sum, label = glue::glue("**{round(sum)}**"),color = tipo),
    fill = NA, label.color = NA, size = 6, family = 'ptsans',
    # vjust = -.3,
    vjust = case_when(
      round(for_labs$sum) == 101 ~ 1.3,
      TRUE ~ -.3
    ),
    alpha = case_when(
      for_labs$calendar_year < 2018 & for_labs$calendar_year != 2010 ~ .4,
      TRUE ~ 1
    )
  ) +
  geom_richtext(
    aes(
      x = 2018,
      y = 850, label = '<span>Em **2019**, pela primeira vez em uma década</span><p>a contribuição voluntária do Brasil foi menor</p><p>do que a obrigatória. Isso contribuiu para di-</p><p>-minuir a parcela total de recursos do país</p><p>no orçamento do Sistema ONU</p>'
    ),
    fill = NA, label.color = NA, # remove background and outline,
    family = 'ptsans', size = 6, color = '#444444', hjust = .62
  ) +
  labs(
    title = '**No governo Bolsonaro, a contribuição <span style = "color:#CB937C;">voluntária</span> do Brasil para o Sistema ONU diminuiu drasticamente**',
    subtitle = '<span>(_valores em milhões de dólares_)</span>',
    caption =  "<span style = 'color:#444444;'>Pedro D. Rocha | VisRI #02c | @datavisri | Dados: UN CEB</span>"
  ) +
  scale_x_continuous(limits = c(2010, 2020),breaks = c(2010,2020)) +
  scale_y_continuous(limits = c(0,1000)) +
  scale_color_manual(values = c('#674254',"#CB937C")) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    panel.grid = element_blank(),
    plot.margin = margin(20,90,20,30),
    legend.position = 'none',
    axis.title = element_blank(),
    plot.title = element_textbox_simple(
      size = 30,
      margin = margin(20, 0, 10, 20),
      color = "#444444",
      family = 'montserrat',linewidth = .2
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(4, 0, 50, 20),
      family = 'montserrat',
      size = 18,
      color = "#444444"),
    plot.caption = element_textbox_simple(
      family = 'montserrat', size = 14,color = "#44444460", margin = margin(30,0,0,0)
    ),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 30, color = "#444444",family = 'ptsans', margin = margin(t = 20)),
    axis.line.x = element_line(size = 1.3, color = "#444444"),
    panel.grid.major.x = element_line(color = '#44444450', linetype = 'dotted')
  ) -> plot

ggsave(
  plot = plot,
  height = 11.249,
  width = 11.249,
  filename = '03/output/corenoncore.pdf',
  dpi = 400
)

pdf_convert(pdf = '03/output/corenoncore.pdf', filename = '03/output/corenoncore.png',
            format = "png", dpi = 400)


