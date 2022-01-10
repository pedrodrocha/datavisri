library(tidyverse)
library(ggraph)
library(igraph)
library(tidyverse)

dat <- read_csv('07/data/acled_la.csv')
sh_file <- st_read('07/data/shapefile/Americas.shp')

dat %>%
  mutate(
    day = str_extract(event_date,'[0-9]{2}|[0-9]{2}'),
    month = str_remove(event_date,'[0-9]{2}|[0-9]{2}') %>%
      str_remove('[0-9]{4}') %>%
      str_trim() %>%
      match(month.name),
    event_date = glue::glue('{year}-{month}-{day}') %>% as.Date()
  ) -> dat


dat %>%
  filter(region != 'North America') -> dat_no_na
dat %>%
  filter(iso3 == 'MEX') %>%
  bind_rows(dat_no_na,.) -> dat_la

dat_la %>%
  filter(event_type == 'Battles', year == 2021, month > 6) %>%
  rename('POINT_X' = longitude,'POINT_Y' = latitude) %>%
  st_as_sf(coords = c("POINT_X", "POINT_Y")) -> dat_spatial
st_crs(dat_spatial) <- 4326




ggplot(sh_file) +
  geom_sf() +
  geom_sf(data = dat_spatial,aes(size = fatalities), color = 'peru', alpha = .4) +
  coord_sf() + theme_void() -> plot

ggsave(
  plot = plot,
  height = 11.249,
  width = 11.249,
  filename = '07/output/plot.svg',
  dpi = 400
)


dat_spatial %>%
  group_by(iso3) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(
    prop_total = n/2567 * 100
  )


dat_spatial %>%
  group_by(iso3) %>%
  summarise(sum_fat = sum(fatalities)) %>%
  arrange(desc(sum_fat))
