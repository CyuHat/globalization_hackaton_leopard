# Libraries ----
library(tidyverse)
library(arrow)
library(patchwork)

# Data ----
df0 <- read_rds("MyData/df0.rds")
dt <- arrow::open_csv_dataset("../corpus-112_images_2024-04-30_1357.csv")

# Code 1 ----
gg0_base <- 
  dt %>%
  mutate(Date = floor_date(Date, unit = "year")) %>% 
  count(Date, Country) %>% 
  collect() %>% 
  filter(Country %in% c("France", "United States of America"),
         Date > date("1850-01-01"))

gg_base <- 
  gg0_base %>% 
  ggplot(aes(Date, n, fill = Country)) +
  geom_col() +
  labs(y = NULL,
       title = "Nombre d'image dans le temps par pays",
       subtitle = "Top 10") +
  theme(legend.position = "top")

gg_country / gg_base

# Code 2 ----
top10_countries <- 
  df0 %>% 
  count(Country) %>% 
  slice_max(n, n = 10) %>% 
  pull(Country)

gg0_country <- 
  df0 %>% 
  filter(Country %in% top10_countries,
         Date > date("1850-01-01")) %>% 
  mutate(Date = floor_date(Date, unit = "year")) %>% 
  count(Date, Country)

gg_country <- 
  gg0_country %>% 
  ggplot(aes(Date, n, fill = Country)) +
  geom_col() +
  labs(y = NULL,
       title = "Nombre d'image dans le temps par pays",
       subtitle = "Top 10") +
  theme(legend.position = "top")

gg0_country

# Code 3 ----
d1 <- 
  gg0_country %>% 
  filter(Country %in% c("France", "United States of America")) %>% 
  mutate(n = scale(n),
         categorie = "Leopard")

d2 <-
  gg0_base %>% 
  filter(Country %in% c("France", "United States of America")) %>% 
  mutate(n = scale(n),
         categorie = "Base")

bind_rows(d1, d2) %>% 
  ggplot(aes(Date, n, color = categorie)) +
  geom_line() +
  facet_wrap(~Country, ncol = 1)

# Test ----
unique(df0$`Journal Type`)

df0 %>% 
  mutate(case_match(`Journal Type`,
                    "Vogue (Paris)" ~ "Fashion",
                    "Le Jour (1933)" ~ "News",
                    "Midinette" ~ "Woman",
                    "Cinémagazine" ~ "Cinema",
                    "La Bourgogne républicaine (Dijon)" ~ "Regional News",
                    "Cinévie (Paris)" ~ "Culture, Leisure",
                    "Séduction (Paris)" ~ "Leisure",
                    "Claudine" ~ "Woman",
                    "Grazia : un’amica al vostro fianco" ~ "Woman",
                    "La Mode du jour" ~ "Fashion",
                    "Dimanche illustré (1923)" ~ "News",
                    "Paris-presse, L'Intransigeant" ~ "News",
                    "Corriere dei piccoli : supplemento illustrato del Corriere della sera" ~ "Magazine",
                    "L'Aurore (Paris. 1943)" ~ "Military",
                    "L'Effort (Clermont-Ferrand)" ~ "Regional News",
                    "La Revue de la femme" ~ "La Revue de la femme",
                    "La settimana Incom illustrata : settimanale di politica attualità e cultura" ~ "News",
                    "Les Dernières nouvelles illustrées" ~ "News",
                    "Marie-Claire (Paris)" ~ "Woman",
                    "Beaux-arts (1923)" ~ "Culture, Leisure",
                    "Candide (Paris. 1924)" ~ "Religion",
                    "Ciné-mondial" ~ "Cinema",
                    "Cinéa (1921)" ~ "Cinema",
                    "Cinéa (1930)" ~ "Cinema",
                    "France-soir (Paris. 1944)" ~ "News",
                    "Je suis partout" ~ "News",
                    "L'Aube (Paris. 1932)" ~ "Regional News",
                    "L'Italie nouvelle (Paris)" ~ "News",
                    "La Griffe cinématographique" ~ "Cinema",
                    "La Revue des revues (Paris. 1890)" ~ "Culture, Leisure",
                    "Le Bulletin de l'art ancien et moderne" ~ "Art",
                    "Le Cinéma chez soi (Paris. 1926)" ~ "Cinema",
                    "Le Courrier des cinémas" ~ "Cinema",
                    "Le Monde illustré, Miroir du monde" ~ "News",
                    "Le Petit journal illustré" ~ "News",
                    "Redbook 1953-11: Vol 102 Iss 1" ~ "Woman",
                    "The Red Book 1908-01: Vol 10 Iss 3" ~ "Woman",
                    "The Redbook Magazine 1913-07: Vol 21" ~ "Woman",
                    "The Redbook Magazine 1913-09: Vol 21" ~ "Woman",
                    "The Redbook Magazine 1921-01: Vol 36 Iss 3" ~ "Woman",
                    "The Redbook Magazine 1929-09: Vol 53 Iss 5" ~ "Woman",
                    "The Redbook Magazine 1930-10: Vol 55 Iss 6" ~ "Woman",
                    "Resimli ay" ~ "News",
                    "Dernière heure (Alger. 1946)" ~ "News"
  ))

df0 %>% 
  filter(is.na(`Journal Type`)) %>% 
  count(Title, sort = TRUE) %>% 
  pull(Title)

# Manquant:
# Apsi
# Le Golf (Paris.1925)
# Togo Cameroun (Paris)
# Ève (Paris. 1920)
# Amica : Rivista mensile illustrata per la donna e la casa
# L'Européen (Paris. 1929)
# La Vie aérienne
# Le Film complet (1922)
# Le Monde colonial illustré
# Point de vue (1945)
# Vénus (Paris.1935)
# Au bord de l'eau (Paris)
# Bravo (Paris. 1929)
# Crimen : documentario settimanale di criminologia
# Etre belle
# France-aviation
# L'Art ménager (Paris)
# La Femme chez elle
# Le Risque (Paris)
# Les Annales coloniales (Éd. illustrée)
# Les Dimanches de la femme
# Paris-photographe
# Pierrot (Paris. 1925)
# Voici la mode
# "al- balāġ al-usbūʻī"                                                        
# "al- ǧāmiʻa"                                                                 
# "Šehbāl"  

# Save ----
# write_rds(df0, file = "MyData/df0.rds")
