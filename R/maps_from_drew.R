

# setting up

library(pacman)
p_load(tidyverse, maps, mapproj, ggthemes, viridis)
setwd("C:/Users/cyrus/OneDrive/EC 419/Crime-Project/data")


clos <- read.csv("school_closure_data1.csv")
clos <- clos%>%
  rename("fips" = "countyfips3")
piss <- get("county.fips")



map1 <- map_data("county")
map1 <- map1%>%
  mutate(polyname = paste(region, subregion, sep = ","))
map1 <- left_join(map1, piss, by = "polyname")
mob <- left_join(map1, clos, by = "fips")
state_df <- map_data("state")



mob%>%
  filter(year==2020)%>%
  filter(month==2)%>%
  ggplot(aes(x = long, y = lat, group = group, fill = share_all_closed_25)) +
  geom_polygon(color = "white", size = 0.01) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_viridis(option = "plasma", na.value = "white", limits = c(0, 1)) +
  labs(title = "Share of Schools Experiencing at least 25% closure by County for February 2020",
       fill = NULL) +
  geom_polygon(data = state_df, color = "white", fill = NA, size = 0.65) +
  theme_map()



mob2 <- mob%>%
  filter(year==2020)%>%
  filter(month==4)%>%
  ggplot(aes(x = long, y = lat, group = group, fill = share_all_closed_25)) +
  geom_polygon(color = "white", size = 0.01) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_viridis(option = "plasma", na.value = "white", limits = c(0, 1)) +
  labs(title = "Share of Schools Experiencing at least 25% closure by County For April 2020",
       fill = NULL) +
  geom_polygon(data = state_df, color = "white", fill = NA, size = 0.65) +
  theme_map()
mob2



mob3 <- mob%>%
  filter(year==2020)%>%
  filter(month==10)%>%
  ggplot(aes(x = long, y = lat, group = group, fill = share_all_closed_50)) +
  geom_polygon(color = "white", size = 0.01) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_viridis(option = "plasma", na.value = "white", limits = c(0, 1)) +
  labs(title = "Share of Schools Experiencing at least 50% closure by County for October 2020",
       fill = NULL) +
  geom_polygon(data = state_df, color = "white", fill = NA, size = 0.65) +
  theme_map()
mob3



mob4 <- mob%>%
  filter(year==2021)%>%
  filter(month==3)%>%
  ggplot(aes(x = long, y = lat, group = group, fill = share_all_closed_25)) +
  geom_polygon(color = "white", size = 0.01) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_viridis(option = "plasma", na.value = "white", limits = c(0, 1)) +
  labs(title = "Share of Schools Experiencing at least 25% closure by County for March 2021",
       fill = NULL) +
  geom_polygon(data = state_df, color = "white", fill = NA, size = 0.65) +
  theme_map()
mob4
beepr::beep(sound = 3)
