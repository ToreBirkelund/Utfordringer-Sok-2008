# Nødvendige pakker. 
library(readr)
library(ggplot2) 
library(tidyverse)
library(RCurl)
library(maps)

# Henter data.
url <- getURL("https://raw.githubusercontent.com/Kenneth-Benonisen/sok-2008/main/utfordring%202/union_unempl.csv")
union <- read.csv(text = url)

# Endrer navnet på observasjon tilknyttett England. 
union$country <- gsub("United Kingdom", "UK", union$country)

# Endrer navnet på variabel slik at man kan merge dataset.
names(union)[names(union) == "country"] <- "region"

# Beregner overskudsdekning. 
union$excess_coverage <- union$coverage - union$density

# Henter kart data. 
mapdata <- map_data("world")

# Merger union og kart data basert på region. 
df <- left_join(mapdata, union, by= "region")

# Fjerner unødvendige regioner. 
df <- df %>% 
  filter(!is.na(df$mean_unempl2015_2019))

# Oppretter kolonne hvor vi kan sortere lønnskordinasjon på ulike numeriske nivå.
df <- df %>% 
  mutate(coord_level = case_when(
    coord == "1. Fragmented wage bargaining" ~ 1,
    coord == "2. Some coordination" ~ 2,
    coord == "3. Procedural negotiation guidelines" ~ 3,
    coord == "4. Non-binding national norms" ~ 4,
    coord == "5. Binding national norms" ~ 5,
  ))


# Plot for arbeidsledighet.
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=unempl), color = "black") +
  scale_fill_gradient2(name = "% Arbeidsledighet", low = "green", mid = "white", high = "red", na.value = "grey50") +
  labs(title = "Arbeidsledighet i Europa \n 2019") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )


# Plot for fagforeningsdensitet.
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=density), color = "black") +
  scale_fill_gradient2(name = "Fagforeningsdensitet", low = "green", mid = "white", high = "red", na.value = "grey50") +
  labs(title = "Fagforeningsdensitet i Europa \n 2019") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )


# Plot for overskudsdekning.
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=excess_coverage), color = "black") +
  scale_fill_gradient2(name = "Excess coverage", low = "green", mid = "white", high = "red", na.value = "grey50") +
  labs(title = "Excess coverage i Europa \n 2019") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )


# Plot for koordinasjon av lønnsfastsettelse.
df %>%
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=coord_level), color = "black") +
  scale_fill_gradient2(name = "Nivå av koordinering",
                       low = "green", mid = "white", high = "red", na.value = "grey50", 
                       label = c("Fragmenterte lønnsforhandlinger",
                                 "Litt koordinering",
                                 "Prosedyremessige forhandlingsretningslinjer",
                                 "Ikke-bindende nasjonale normer",
                                 "Bindende nasjonale normer")) +
  labs(title = "Lønnskoordinering i Europa \n 2019") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )
