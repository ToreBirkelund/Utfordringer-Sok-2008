# Pakker
library(rjstat)
library(httr)
library(PxWebApiData)
library(tidyverse)
library(janitor)




# Kode for a kunne bruke norske bokstaver.
Sys.setlocale(locale="no_NO")


# Henter data fra SSB via JSON.
# Tabell 12441 - sykefravær for lønnstakere (prosent).
url <- "https://data.ssb.no/api/v0/no/table/12441/"


# Tabell 05111 - arbeidsledige (prosent).
url_2 <- "https://data.ssb.no/api/v0/no/table/05111/" 


# Legger inne query for hva vi er ute etter. ######
# Tabell 12411
data <- '
{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "00-99"
        ]
      }
    },
    {
      "code": "Sykefraver2",
      "selection": {
        "filter": "item",
        "values": [
          "Alt"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'


# Tabell 05111
data_2 <- '
{
  "query": [
    {
      "code": "ArbStyrkStatus",
      "selection": {
        "filter": "item",
        "values": [
          "2"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Prosent"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'


#####


# Datafra SSB. 

# Tabell 12441.
d.tmp <- POST(url, body = data, encode = "json", verbose())

# Tabell 05111.
d.tmp_2 <- POST(url_2, body = data_2, encode = "json", verbose())



# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat.
# For tabell 12411.
df_syk <- fromJSONstat(content(d.tmp, "text"))

# For tabell 05111.
df_arb_ledig <- fromJSONstat(content(d.tmp_2, "text"))


# Clean names.
df_syk <- df_syk %>% 
  clean_names()

df_arb_ledig <- df_arb_ledig %>% 
  clean_names()

# Endrer variabelnavn.
names(df_syk)[names(df_syk) == "naering_sn2007"] <- "næring"
names(df_syk)[names(df_syk) == "statistikkvariabel"] <- "variabel_syk"
names(df_syk)[names(df_syk) == "value"] <- "value_syk"
names(df_arb_ledig)[names(df_arb_ledig) == "value"] <- "value_arb_ledig"


# Setter sammen dataset. 
df_combined <- left_join(df_syk, df_arb_ledig, by=c("kjonn", "ar")) %>% 
  select(kjonn, ar, everything())


farger <- c("Arbeidsledig" = "red", "Sykefravær" = "blue")

# Plot for kvinner.
p1<-ggplot(df_combined%>% filter(kjonn == "Kvinner"), aes(x = ar, group=kjonn)) + 
  geom_line(aes(y= value_syk, color = "Sykefravær")) + 
  geom_line(aes(y= value_arb_ledig, color = "Arbeidsledig"))+
  scale_y_continuous(name = "Sykefraværprosent", 
                     sec.axis = sec_axis(~ . * 1, name = "Arbeidsledighet"))+
  scale_color_manual(values = farger)+
  labs(title="Kvinner",
       color="Status")+
  theme_minimal()


# Plot for menn.
p2<-ggplot(df_combined%>% filter(kjonn == "Menn"), aes(x = ar, group=kjonn)) + 
  geom_line(aes(y= value_syk, color = "Sykefravær")) + 
  geom_line(aes(y= value_arb_ledig, color = "Arbeidsledig"))+
  scale_y_continuous(name = "Sykefraværprosent", 
                     sec.axis = sec_axis(~ . * 1, name = "Arbeidsledighet"))+
  scale_color_manual(values = farger)+
  labs(title="Menn",
       color="Status")+
  theme_minimal()

# Multiplott funksjon.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  
  if (is.null(layout)) {
    
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    
    for (i in 1:numPlots) {
      
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Sluttplott. 
multiplot(p1,p2, cols=1)
