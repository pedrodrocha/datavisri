library(tidyverse)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(showtext)
library(pdftools)
library(ggsankey)
library(treemapify)

font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'Roboto',family = 'roboto')
font_add_google(name = 'PT Sans',family = 'ptsans')
showtext_auto()

dat <- read_xlsx('03/data/pivot.xlsx')


dat %>%
  filter(
    government_donor == "Brazil", calendar_year == 2020
  ) %>%
  select(government_donor, entity, amount) %>%
  group_by(entity) %>%
  mutate(amount = sum(amount)) %>%
  distinct() %>%
  ungroup() %>%
  mutate(
    hierarchy = case_when(
      entity %in% c(
        "UNDP",
        "UNEP",
        "UNFPA",
        "UNICEF",
        "WFP",
        "ITC",
        "UNHCR",
        "UNRWA",
        "UNWOMEN",
        "UNOPS",
        "IAEA",
        "WTO",
        "UN",
        "ICC",
        "CTBTO",
        "IOM",
        "OPCW",
        "ITLOS"
      ) ~ 'Assembléia Geral',
      entity %in% c(
        "DPKO",
        'UNODC'

      ) ~ "Secretariado",
      entity %in% c(
        "FAO",
        "ICAO",
        "ILO",
        "IMF",
        "IMO",
        "ITU",
        "UNESCO",
        "WHO",
        "WIPO",
        "WMO",
        "UNAIDS",
        "UNIDO",
        "UPU",
        "UNWTO"
      ) ~ "ECOSOC",
      TRUE ~ "Outros"
    )
  ) %>%
  filter(entity != "UNFPA")  %>%
  mutate(entity = case_when(
    entity == "UN" ~ "UN (Orçamento Regular)",
    TRUE ~ entity
  )) -> br_2020
br_2020
br_2020 %>%
  ggplot(aes(area = amount,fill = hierarchy, subgroup = hierarchy,label = entity)) +
  geom_treemap(
    colour = '#f0f0f0', size = 3

  ) +
  geom_treemap_text(family = 'ptsans', colour = "#f0f0f0", place = "centre") +
  geom_treemap_subgroup_border(color = '#f0f0f0', size = 4) +
  labs(
    title = '<span>**Como a contribuição brasileira se distribuiu**</span> <p>**entre os órgãos do Sistema ONU em 2020?**</p>',
    subtitle = "<span>(_valores incluem o total de contribuições obrigatórias e voluntárias_)</span><br><p>* **Como ler o gráfico?** O tamanho das formas representa a quantidade de recursos atribuídos ao órgão. A  cor representa a vinculação hierárquica do órgão na burocracia da ONU. Em <span style = 'color:#909DC7;'>**azul**</span> estão àqueles vinculados a Assembléia Geral, em <span style = 'color:#CB937C;'>**rosa**</span> ao Secretariado, em <span style = 'color:#E59500;'>**amarelo**</span> ao ECOSOC, e em <span style = 'color:#463C3D;'>**marrom**</span> a outros. </span></p>",
    caption =  "<span style = 'color:#444444;font-size:14pt;'><br><p style = 'color:#444444;font-size:14pt;'>Pedro D. Rocha | VisRI #02d | @datavisri | Dados: UN CEB</p>"
  ) +
  scale_fill_manual(values= c(
    '#909DC7',# Assembléia Geral
    '#E59500', # Ecosoc
    "#463C3D", # Outros
    "#CB937C") # Secretariado
  ) +
  theme_void() +
  theme(
    legend.position = 'none',
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    plot.title = element_textbox_simple(
      color = "#444444",
      family = 'montserrat',
      size = 30,
      face = 'bold',
      margin = margin(25, 0, 0, 0),
      lineheight = 1.2
    ),
    plot.subtitle = element_textbox_simple(
      size = 15,
      margin = margin(15, 0, 20, 0),
      color = "#444444",
      family = 'montserrat',
      linewidth = 1
    ),
    plot.caption = element_textbox_simple(
      family = 'montserrat', margin = margin(25,0,0,0)
    )
  ) -> plot

ggsave(
  plot = plot,
  height = 11.249,
  width = 11.249,
  filename = '03/output/treemap.pdf',
  dpi = 400
)

pdf_convert(pdf = '03/output/treemap.pdf', filename = '03/output/treemap.png',
            format = "png", dpi = 400)

