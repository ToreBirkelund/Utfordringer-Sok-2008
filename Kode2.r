library(gglorenz)
library(PxWebApiData)
library(janitor)
library(dplyr)
library(tidyr)
# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")
#Hvilke variabler som finnes i tabellen
variables <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                     returnMetaFrames = TRUE)
names(variables)

#hvilke verdier har ulike variablene
values <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                  returnMetaData = TRUE)
#Kommunekoder
values[[1]]$values
#Inntekt før/etter skatt
values[[2]]$values # 00 = Samlet inntekt, 00S=Inntekt etter skatt
#Desiler
values[[3]]$values
#Statistikkvariabel
values[[4]]$values
#År
values[[5]]$values


data<-ApiData("https://data.ssb.no/api/v0/en/table/12558/",
              Tid =c("2005","2020"), # Velg årene 2005 og 2020
              Desiler=c("01", "02", "03" ,"04", "05", "06" ,"07", "08" ,"09", "10"), #Vi velger alle desiler
              InntektSkatt="00", #Vi velger samlet inntekt
              ContentsCode="VerdiDesil", #Velger den høyeste verdien i desilen
              Region=c("5401","1902"))#Tromsø endret kommunenummer i 2020


#Gjør om til dataframe samt rydder litt opp.
Tempdf <- as.data.frame(data)
#Tempdf <- Tempdf %>% 
  #clean_names()
Tempdf <- Tempdf %>% 
  subset(select = -dataset_n_astatus)

#Pynter på brura!
names(Tempdf)[names(Tempdf)=="dataset_tid"] <- "Årstall"

#Lager Lorenz-kurvene.
Tempdf %>%
  ggplot(aes(x=dataset_value,colour=Årstall)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  labs(x = "Andel av befolkning",
       y = "Andel av inntektene",
       title = "Lorenz-kurver for inntektsfodelingen i Tromsø kommune")
