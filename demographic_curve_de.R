# Pakete laden
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Daten einlesen
pyramide <- read_csv2("14_bevoelkerungsvorausberechnung_daten.csv")

# Daten analysieren
demographic_2018 <- pyramide %>% 
  filter(Simulationsjahr == 2018) %>% 
  gather(-c(Variante, Simulationsjahr, mw, Bev), key = "Alter", value = "Anzahl")

demographic_2018 <- demographic_2018 %>% 
  mutate(Alter_neu = str_replace_all(Alter, "_", "A"),
         Alter_neu = str_sub(Alter_neu, 4), # trim the "Bev"
         Alter_neu = str_replace_all(Alter_neu, "A", "-A"), # insert the "-"
         Alter_neu = str_sub(Alter_neu, 2), # trim the first "-"
         Alter_neu = str_replace(Alter_neu, "A0-A1", "A00-A01"),
         Alter_neu = str_replace(Alter_neu, "A1-A2", "A01-A02"),
         Alter_neu = str_replace(Alter_neu, "A2-A3", "A02-A03"),
         Alter_neu = str_replace(Alter_neu, "A3-A4", "A03-A04"),
         Alter_neu = str_replace(Alter_neu, "A4-A5", "A04-A05"),
         Alter_neu = str_replace(Alter_neu, "A5-A6", "A05-A06"),
         Alter_neu = str_replace(Alter_neu, "A6-A7", "A06-A07"),
         Alter_neu = str_replace(Alter_neu, "A7-A8", "A07-A08"),
         Alter_neu = str_replace(Alter_neu, "A8-A9", "A08-A09"),
         Alter_neu = str_replace(Alter_neu, "A9-A10", "A09-A10")
  )

demographic_2018 %>% 
  ggplot(aes(x = Alter_neu, y = Anzahl)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~mw, nrow = 1) +
    theme_classic()

# Population pyramid
demographic_2018 %>% 
  ggplot(aes(x = ifelse(test = mw == "m", yes = -Anzahl, no = Anzahl),
             y = Alter_neu,
             fill = mw)) +
    geom_col() +
    scale_x_continuous() +
    theme_classic() +
    labs(x = "Bevölkerung (in Tausend)",
         y = "Altersgruppe")

# Now the RKI age classes
demographic_2018 %>% 
  head()

Altersgruppe <- c(replicate(10, "A00-A04"),
                 replicate(20, "A05-A14"),
                 replicate(40, "A15-A34"),
                 replicate(50, "A35-A59"),
                 replicate(40, "A60-A79"),
                 replicate(40, "A80+"))

demographic_2018_classes <-  demographic_2018 %>% 
  mutate(Altersgruppe = Altersgruppe)

demographic_de_2018 <- demographic_2018_classes %>% 
  group_by(Altersgruppe, mw) %>% 
  summarise(Bevölkerung = sum(Anzahl))

colnames(demographic_de_2018)[2] <- "Geschlecht"

demographic_de_2018$Geschlecht <- factor(str_to_upper(demographic_de_2018$Geschlecht))

write_csv(demographic_de_2018, "demographic_de_2018.csv")
