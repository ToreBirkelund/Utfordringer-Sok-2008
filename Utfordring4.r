# Nødvendige pakker
library(tidyverse)
library(gridExtra)
library(grid)
# Kode for a kunne bruke norske bokstaver.
Sys.setlocale(locale="no_NO")

# Henter datasettet.
women <- read.csv2("https://uit-sok-2008-h22.github.io/Assets/women.csv")

# Endrer character til numeric.
women$tot_full_rate<-as.numeric(women$tot_full_rate)
women$fem_emp_rate_0_2<-as.numeric(women$fem_emp_rate_0_2)
women$fem_emp_rate_6_14<-as.numeric(women$fem_emp_rate_6_14)


# Graf1
kids_0_2 <- women %>%
  ggplot(aes(x=tot_full_rate,y=fem_emp_rate_0_2))+
  geom_point()+
  ylim(0, 100)+
  labs(x ="Uker med 100% støtte.", y = "Yrkesdeltakelse blant mødre hvis yngste barn er mellom 0-2 år.") +
  theme_classic() +
  geom_smooth(method=lm, se=FALSE)
# Graf2
kids_6_14 <- women %>%
  ggplot(aes(x=tot_full_rate,y=fem_emp_rate_6_14))+
  geom_point()+
  ylim(0, 100)+
  labs(x ="Uker med 100% støtte.", y = "Yrkesdeltakelse blant mødre hvis yngste barn er mellom 6-14 år.") +
  theme_classic() +
  geom_smooth(method=lm, se=FALSE)

# Illustrere plottene i et grid system. 
grid.arrange(kids_0_2,kids_6_14, nrow = 1,  top = textGrob("Korrelasjon, lengde på foreldrepermisjon og mødres yrkesdeltakelse etter yngste barns alder.",gp=gpar(fontsize=20,font=3)))
