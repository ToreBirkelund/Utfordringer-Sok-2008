# Pakker..
library(rjstat)
library(httr)
library(PxWebApiData)
library(tidyverse)
library(ggrepel)
library(janitor)

# Norske bokstaver.
Sys.setlocale(locale="no_NO")
# Henter data fra SSB via JSON.
url <- "https://data.ssb.no/api/v0/no/table/05185/"
# Legger inne query for hva vi er ute etter. ######
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
      "code": "Landbakgrunn",
      "selection": {
        "filter": "agg:Verdensdel2",
        "values": [
          "b11",
          "b12",
          "b13",
          "b14",
          "b2",
          "b3",
          "b4",
          "b5",
          "b6"
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
          "2019",
          "2020",
          "2021",
          "2022"
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
# Henter selve dataen fra SSB 
d.tmp <- POST(url, body = data, encode = "json", verbose())
# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
df <- fromJSONstat(content(d.tmp, "text"))
# Summerer value fra begge kjønn til ett, og filtrer bort det ene kjønne for å unngå dublikering av labels. 
df <- df %>%
  group_by(landbakgrunn, år) %>% 
  mutate(total_innvandring = sum(value)) %>% 
  filter(kjønn == "Menn")
# Oppretter labels til hver inndeling. 
df_label <- df %>%
  group_by(landbakgrunn) %>%
  top_n(1, år)
# Figur1. 
df %>%
  ggplot(aes(år, total_innvandring, group = landbakgrunn, col = landbakgrunn)) +
  geom_line(aes(group = landbakgrunn),size = 1) +
  labs(x = "År",
       y = "Antall innvandrere") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0,300000, 20000)) +
  theme_classic() +
  theme(legend.position="right")
# Henter data fra SSB via JSON.
url2 <- "https://data.ssb.no/api/v0/no/table/13215/"
# Legger inn query, for oppsøkt data. ######
data2 <- '
{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "B"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "item",
        "values": [
          "00",
          "194",
          "015a",
          "100c",
          "694c",
          "400",
          "200b",
          "794a"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "agg:NACE260InnvGrupp2",
        "values": [
          "SNI-00-99",
          "SNI-01-03",
          "SNI-05-09",
          "SNI-10-33",
          "SNI-35-39",
          "SNI-41-43",
          "SNI-45-47",
          "SNI-49-53",
          "SNI-49.3",
          "SNI-55",
          "SNI-56",
          "SNI-58-63",
          "SNI-64-66",
          "SNI-68-75",
          "SNI-77-82",
          "SNI-78.2",
          "SNI-81.2",
          "SNI-84",
          "SNI-85",
          "SNI-86-88",
          "SNI-90-99",
          "SNI-00"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2021"
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
# Henter selve dataen fra SSB.
d.tmp2 <- POST(url2, body = data2, encode = "json", verbose())
# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat.
df2 <- fromJSONstat(content(d.tmp2, "text"))
# Korrigerer kolonnene slik at det er enklere å benytte. 
df2 <- df2 %>% 
  clean_names()
# Oppretter ny kolonne, hvor vi rydder i navnene som plotter y-aksen i grafen.
df2 <- df2 %>% 
  mutate(Næringer = case_when(
    naering_sn2007 == "00-99 ... Alle næringer" ~ "Alle næringer",
    naering_sn2007 == "00 ... Uoppgitt" ~ "Uoppgitt",
    naering_sn2007 == "01-03 ... Jordbruk, skogbruk og fiske" ~ "Jordbruk, skogbruk og fiske",
    naering_sn2007 == "05-09 ... Bergverksdrift og utvinning" ~ "Bergverksdrift og utvinning",
    naering_sn2007 == "10-33 ... Industri" ~ "Industri",
    naering_sn2007 == "35-39 ... Elektrisitet, vann og renovasjon" ~ "Elektrisitet, vann og renovasjon",
    naering_sn2007 == "41-43 ... Bygge- og anleggsvirksomhet" ~ "Bygge- og anleggsvirksomhet",
    naering_sn2007 == "45-47 ... Varehandel, motorvognreparasjoner" ~ "Varehandel, motorvognreparasjoner",
    naering_sn2007 == "49-53 ... Transport og lagring" ~ "Transport og lagring",
    naering_sn2007 == "49.3 ... Annen landtransport med passasjerer" ~ "Annen landtransport med passasjerer",
    naering_sn2007 == "55 ... Overnattingsvirksomhet" ~ "Overnattingsvirksomhet",
    naering_sn2007 == "56 ... Serveringsvirksomhet" ~ "Serveringsvirksomhet",
    naering_sn2007 == "58-63 ... Informasjon og kommunikasjon" ~ "Informasjon og kommunikasjon",
    naering_sn2007 == "64-66 ... Finansiering og forsikring" ~ "Finansiering og forsikring",
    naering_sn2007 == "68-75 ... Teknisk tjenesteyting, eiendomsdrift" ~ "Teknisk tjenesteyting, eiendomsdrift",
    naering_sn2007 == "77-82 ... Forretningsmessig tjenesteyting" ~ "Forretningsmessig tjenesteyting",
    naering_sn2007 == "78.2 ... Utleie av arbeidskraft" ~ "Utleie av arbeidskraft",
    naering_sn2007 == "81.2 ... Rengjøringsvirksomhet" ~ "Rengjøringsvirksomhet",
    naering_sn2007 == "84 ... Offentlig administrasjon, forsvar, sosialforsikring" ~ "Offentlig administrasjon, forsvar, sosialforsikring",
    naering_sn2007 == "85 ... Undervisning" ~ "Undervisning",
    naering_sn2007 == "86-88 ... Helse- og sosialtjenester" ~ "Helse- og sosialtjenester",
    naering_sn2007 == "90-99 ... Personlig tjenesteyting" ~ "Personlig tjenesteyting"
  ))
# Figur2.
df2 %>%
  mutate("Innvandring" = value) %>%
  filter(Næringer != "Alle næringer") %>% 
  ggplot(aes(Innvandring, Næringer, fill = landbakgrunn)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma, breaks = seq(0,40000, 5000)) +
  theme_classic() +
  facet_wrap(~landbakgrunn) +
  theme(legend.position="none")
