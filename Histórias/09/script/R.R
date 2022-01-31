library(tidyverse)
library(haven)
library(janitor)


dat <- read_sav('09_apology_age/data/JPR_Zoodsma & Schaafsma_Raw datafile_200421.sav')

dat %>%
  select(Year_1,Year_2, Year_Cat, Description,CheckCountry_S,Count_S_ISO_Alpha, Region_S_UN, Region_R_OECD, Name_Send,Role_Send,Group_Rec_1,Context_Cat_1,Context_Cat_2,Context_Cat_3, HRV_1,HRV_2,HRV_3, HRV_Date_Start,HRV_Date_End,Apol_Set,Apol_Med) -> dat

dat %>%
  mutate(
    Context_Cat_1 = str_to_lower(Context_Cat_1),
    Context_Cat_2 = str_to_lower(Context_Cat_2),
    Context_Cat_3 = str_to_lower(Context_Cat_3),
  ) %>%
  filter(str_detect(Context_Cat_1,'war') | str_detect(Context_Cat_2,'war')) %>%
  group_by(Year_2) %>%
  count() %>%
  ungroup() %>%
  summarise(count = sum(n))

dat %>%
  filter(Year_2 != 10) %>%
  nrow(.)

177/359

dat %>%
  ggplot() +
  geom_dotplot(
    aes(x = Year_2), dotsize = .25, stackratio = 1.25
  ) +
  labs(x = "",y = "") +
  theme_void() -> plot


ggsave(
  plot = plot,
  height = 11.249,
  width = 11.249,
  filename = '09_apology_age/output/base.svg',
  dpi = 400
)
