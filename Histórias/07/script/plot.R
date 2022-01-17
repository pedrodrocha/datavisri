library(tidyverse)
library(colorspace)
library(readxl)
library(janitor)
library(ggtext)
library(showtext)
library(pdftools)
library(ggforce)

normalize <- function(current,min,max){
  (current - min) / (max - min)

}

r <- function(area){
  sqrt(area/pi)
}

dat <- read_xlsx('06/data/data.xlsx') %>%
  clean_names() %>%
  select(years_h,country,most_recent_year,regional_group)  %>%
  mutate(country = str_trim(country)) %>%
  filter(most_recent_year >= 2022)


dat %>%
  mutate(
    area = normalize(years_h,min(dat$years_h),max(dat$years_h)) * 60,
    r = r(area) + 2
  )  -> dat_area


dat_area

tibble::tibble(
  x0 = c(
    c(8,22,36),c(16.5,30.5), # P5
    c(8,16), # Latin America
    40, # E. Europe
    c(57,67), # W. Europe & Others
    c(82,96), # Asia-Pacífic
    90, # Africa or Asia
    c(75,82) # Africa
),
  y0 = c(
    rep(80,3),c(70,68), # P5
    rep(22,2), # Latin America & Caribe
    35, # E. Europe
    rep(50,2),# W. Europe & Others
    rep(80,2), # Asia-Pacífic
    60, # Africa or Asia
    rep(15,2) # Africa
  )
) -> dat_circle

dat_area %>%
  bind_cols(.,dat_circle) -> dat_circle

ggplot() +
  geom_circle(
    aes(x0 = x0, y0 = y0, r = r), fill = '#444444', color = '#444444',
    data = dat_circle
  ) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,5)) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,5)) +
  coord_cartesian(clip = 'off') +
  theme_void() +
  theme(plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0')) -> plot

plot
ggsave(
  plot = plot,
  height = 11.249,
  width = 11.249,
  filename = '06/output/plot.svg',
  dpi = 400
)

dat

pdf_convert(pdf = '06/output/plot.pdf', filename = '06/output/plot.png',
            format = "png", dpi = 400)

