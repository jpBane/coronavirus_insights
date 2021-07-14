# -----------------
# Pakete laden
# -----------------
library(readxl)
library(httr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(stringr)


# -----------------
# Daten einlesen
# -----------------
url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile"
GET(url = url, write_disk("RKI_Fallzahlen_Kum_Tab.xlsx", 
                          overwrite = T))

excel_sheets("RKI_Fallzahlen_Kum_Tab.xlsx")
landkreise_7tage_inzidenz <- read_excel("RKI_Fallzahlen_Kum_Tab.xlsx", 
                                        sheet = "LK_7-Tage-Inzidenz (fixiert)", 
                                        skip = 4)


# ------------------
# Daten vorbereiten
# ------------------
landkreise_tidy <- landkreise_7tage_inzidenz %>% 
  gather(-NR, -LK, -LKNR, key = "Datum", value = "Inzidenz_7_Tage")

#landkreise_tidy$Datum <- as.Date(landkreise_tidy$Datum, format = "%d.%m.%Y")
# Das Datenformat ändert sich ab dem 16.04.2021 plötzlich von Standard auf Datum -> wie gehe ich damit um?

landkreise_tidy <- landkreise_tidy %>% 
  mutate(Datum_korr = as.Date(ifelse(is.na(as.numeric(landkreise_tidy$Datum)),
                             as.Date(landkreise_tidy$Datum, format = "%d.%m.%Y"), 
                             as.Date(as.numeric(landkreise_tidy$Datum), origin = "1899-12-30")),
                             origin = "1970-01-01")
  )

# --------------------------
# Explorative Datenanalyse
# --------------------------
landkreise_tidy %>% 
  summary()

plot_OG <- landkreise_tidy %>% 
  filter(LK == "LK Ortenaukreis") %>% 
  ggplot(aes(x = Datum, y = Inzidenz_7_Tage)) +
    geom_point() +
    geom_line()

ggplotly(plot_OG)

landkreise_tidy %>% 
  filter(str_detect(landkreise_tidy$LK, "München"))

plot_relation <- landkreise_tidy %>% 
  filter(LK %in% c("LK Ortenaukreis", "SK Leverkusen", "SK Freiburg i.Breisgau", "LK Rastatt", "SK München")) %>% 
  ggplot(aes(x = Datum_korr, y = Inzidenz_7_Tage, color = LK)) +
    geom_point() +
    geom_line() +
    labs(title = "7 Tage Inzidenz in ausgewählten Gebieten Deutschlands",
         color = NULL)

ggplotly(plot_relation)
