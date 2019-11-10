library(shiny)
library(readr)
library(leaflet)
library(DT)
library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)
library(ggseas)
library(ggthemes)
library(scales)
library(ggplot2)
library(knitr)
library(caTools)
library(RColorBrewer)
library(eurostat)

#setwd("C:/Users/Marco/Google Drive/Code/R/R Projects/Progetto Istat/Shiny App")
fl <- read.csv(list.files(pattern = "FORZL"),stringsAsFactors = FALSE)
dis <- read.csv(list.files(pattern = "DISOC"),stringsAsFactors = FALSE)
ina <- read.csv(list.files(pattern = "INAT"),stringsAsFactors = FALSE)
ore <- read.csv(list.files(pattern = "ORELAVMED"),stringsAsFactors = FALSE)

names(ore)
######################## functions to format group data ###########################

data_group_dis <- function(df, var) {
  
  var <- enquo(var)
  
    df %>% 
    filter(
      ifelse(!!var == ETA1, ETA1 %in% c("Y15-24","Y25-34","Y35-44","Y45-54","Y55-64"),
      ifelse(!!var == Cittadinanza, ETA1 == "Y_GE15", ETA1 =="Y15-64")),
      ifelse(!!var == Sesso, Sesso != "totale", Sesso == "totale"),
      ifelse(!!var == Territorio, !(Territorio %in% c("Italia","Nord")) , Territorio == "Italia"),
      ifelse(!!var == Titolo.di.studio, Titolo.di.studio != "totale", Titolo.di.studio == "totale"),
      ifelse(!!var == Cittadinanza, Cittadinanza != "totale", Cittadinanza == "totale"),
      CONDIZIONE_PROF == 99,
      DURATA == "TOTAL"
    ) %>% 
    mutate(Periodo = as.Date(as.yearqtr(TIME, format = "%Y-Q%q"))
           ) %>% 
    rename(Disoccupati = "Value") %>% 
    select(
      Periodo,
      !!var,
      Disoccupati
    ) %>%
    arrange(!!var,Periodo,Disoccupati)
}


#QA
# data_group_dis(dis,ETA1) %>% group_by(Periodo) %>% summarise(Disoccupati = sum(Disoccupati))
# data_group_dis(dis,Territorio) %>% group_by(Periodo) %>% summarise(Disoccupati = sum(Disoccupati))
# data_group_dis(dis,Titolo.di.studio) %>% group_by(Periodo) %>% summarise(Disoccupati = sum(Disoccupati))
# data_group_dis(dis,Sesso) %>% group_by(Periodo) %>% summarise(Disoccupati = sum(Disoccupati))

data_group_fl <- function(df, var) {
  
  var <- enquo(var)
  
  df %>% 
    filter(
      ifelse(!!var == ETA1, ETA1 %in% c("Y15-24","Y25-34","Y35-44","Y45-54","Y55-64"),
             ifelse(!!var == Cittadinanza, ETA1 == "Y_GE15", ETA1 =="Y15-64")),
      ifelse(!!var == Sesso, Sesso != "totale", Sesso == "totale"),
      ifelse(!!var == Territorio, !(Territorio %in% c("Italia","Nord")) , Territorio == "Italia"),
      ifelse(!!var == Titolo.di.studio, Titolo.di.studio != "totale", Titolo.di.studio == "totale"),
      ifelse(!!var == Cittadinanza, Cittadinanza != "totale", Cittadinanza == "totale")
    ) %>% 
    mutate(Periodo = as.Date(as.yearqtr(TIME, format = "%Y-Q%q"))
    ) %>% 
    rename(Forza_Lavoro = "Value") %>% 
    select(
      Periodo,
      !!var,
      Forza_Lavoro
    ) %>% 
    arrange(!!var,Periodo,Forza_Lavoro)
}


data_group_ina <- function(df, var) {
  
  var <- enquo(var)
  
  df %>% 
    filter(
      ifelse(!!var == ETA1, ETA1 %in% c("Y15-24","Y25-34","Y35-44","Y45-54","Y55-64"),
             ifelse(!!var == Cittadinanza, ETA1 == "Y_GE15", ETA1 =="Y15-64")),
      ifelse(!!var == Sesso, Sesso != "totale", Sesso == "totale"),
      ifelse(!!var == Territorio, !(Territorio %in% c("Italia","Nord")) , Territorio == "Italia"),
      ifelse(!!var == Titolo.di.studio, Titolo.di.studio != "totale", Titolo.di.studio == "totale"),
      ifelse(!!var == Cittadinanza, Cittadinanza != "totale", Cittadinanza == "totale"),
      CONDIZIONE_PROF == 99,
      MOTIVOPS1 == "ALL",
      CONDIZIONE_DICH == 99
    ) %>% 
    mutate(Periodo = as.Date(as.yearqtr(TIME, format = "%Y-Q%q"))) %>% 
    rename(Inattivi = "Value") %>% 
    select(Periodo,
           !!var,
           Inattivi) %>% 
    arrange(!!var,Periodo,Inattivi)
}

#data_group_dis(dis,ETA1)
######################## function to merge data for a group ###########################

data_group <- function(var) {
  
  var <- enquo(var)
  var_name <- as_label(var) 
  
 data_group_fl(fl, !!var) %>% 
  left_join(data_group_dis(dis, !!var)) %>%
  left_join(data_group_ina(ina, !!var)) %>% 
  mutate(Gruppo = !!var_name,
         Categoria = !!var,
         Inattivi = round(Inattivi/1000,2),
         Forza_Lavoro = round(Forza_Lavoro/1000,2),
         Disoccupati = round(Disoccupati/1000,2),
         Occupati = round((Forza_Lavoro - Disoccupati),2),
         Popolazione = Forza_Lavoro + Inattivi,
         Tasso_Disoccupazione = round(Disoccupati / Forza_Lavoro,4),
         Tasso_Occupazione = round(Occupati / Popolazione,4),
         Tasso_Inattivita = round(Inattivi / Popolazione,4)
  ) %>% 
  select(
    Periodo,
    Gruppo,
    Categoria,
    Popolazione,
    Inattivi,
    Occupati,
    Disoccupati,
    Tasso_Disoccupazione,
    Tasso_Occupazione,
    Tasso_Inattivita
  ) %>% 
  gather(Metrica, Valore, Popolazione:Tasso_Inattivita)
}


########################## Dati totale #############################
fl_subset <- fl %>% 
  select(ETA1,
         Territorio,
         Sesso,
         Titolo.di.studio,
         Cittadinanza,
         TIME,
         Value)  %>% 
  filter(ETA1 == "Y15-64",
         Territorio == "Italia",
         Sesso == "totale",
         Titolo.di.studio == "totale",
         Cittadinanza == "totale",
         str_detect(TIME,"Q")) %>% 
  select(TIME,
         Value) %>% 
  rename(Periodo = TIME,
         Forza_Lavoro = Value)  %>%
  mutate(Periodo = as.Date(as.yearqtr(Periodo, format = "%Y-Q%q"))
  )  %>%
  arrange(Periodo)

dis_subset <- dis %>% 
  select(ETA1,
         Territorio,
         Sesso,
         Titolo.di.studio,
         Cittadinanza,
         TIME,
         Value)  %>% 
  filter(ETA1 == "Y15-64",
         Territorio == "Italia",
         Sesso == "totale",
         Titolo.di.studio == "totale",
         Cittadinanza == "totale",
         str_detect(TIME,"Q")) %>% 
  select(TIME,
         Value) %>% 
  rename(Periodo = TIME,
         Disoccupati = Value)  %>%
  mutate(Periodo = as.Date(as.yearqtr(Periodo, format = "%Y-Q%q"))
  )  %>%
  arrange(Periodo)

ina_subset <- ina %>% 
  select(ETA1,
         Territorio,
         Sesso,
         Titolo.di.studio,
         Cittadinanza,
         CONDIZIONE_PROF,
         MOTIVOPS1,
         CONDIZIONE_DICH,
         TIME,
         Value)  %>% 
  filter(ETA1 == "Y15-64",
         Territorio == "Italia",
         Sesso == "totale",
         Titolo.di.studio == "totale",
         Cittadinanza == "totale",
         CONDIZIONE_PROF == 99,
         MOTIVOPS1 == "ALL",
         CONDIZIONE_DICH == 99,
         str_detect(TIME,"Q")) %>% 
  select(TIME,
         Value) %>% 
  rename(Periodo = TIME,
         Inattivi = Value)  %>%
  mutate(Periodo = as.Date(as.yearqtr(Periodo, format = "%Y-Q%q"))
  )  %>%
  arrange(Periodo)


########################## Merge Dati totale #############################
dati_tot <- fl_subset %>% 
  left_join(dis_subset) %>%
  left_join(ina_subset) %>% 
  mutate(Periodo = Periodo,
         Gruppo = "Totale",
         Categoria = "Totale",
         Inattivi = round(Inattivi/1000,2),
         Forza_Lavoro = round(Forza_Lavoro/1000,2),
         Disoccupati = round(Disoccupati/1000,2),
         Occupati = round((Forza_Lavoro - Disoccupati),2),
         Popolazione = Forza_Lavoro + Inattivi,
         Tasso_Disoccupazione = round(Disoccupati / Forza_Lavoro,4),
         Tasso_Occupazione = round(Occupati / Popolazione,4),
         Tasso_Inattivita = round(Inattivi / Popolazione,4)
  ) %>% 
  select(
  Periodo,
  Gruppo,
  Categoria,
  Popolazione,
  Inattivi,
  Occupati,
  Disoccupati,
  Tasso_Disoccupazione,
  Tasso_Occupazione,
  Tasso_Inattivita
  ) %>% 
  gather(Metrica, Valore, Popolazione:Tasso_Inattivita)

######################## combined grouped data ###########################
dati <- bind_rows(
  list(
    dati_tot,
    Territorio =data_group(Territorio),
    Sesso = data_group(Sesso),
    Eta = data_group(ETA1),
    Studio = data_group(Titolo.di.studio),
    Cittadinanza = data_group(Cittadinanza)
  )
) %>% 
mutate(Categoria = case_when(grepl("elementare", Categoria) ~ "elementari/niente",
                             grepl("scuola media", Categoria) ~ "medie",
                             grepl("laurea", Categoria) ~ "laurea o piu",
                             TRUE ~ Categoria
                             )
       ,Periodo_text = factor(case_when(month(Periodo) == 1 ~ "Gen-Mar",
                                month(Periodo) == 4 ~ "Apr-Giu",
                                month(Periodo) == 7 ~ "Lug-Set",
                                TRUE ~ "Ott-Dic"
                                ), levels = c("Gen-Mar", "Apr-Giu", "Lug-Set", "Ott-Dic"),
                              ordered = TRUE)
       ,Governo = factor(case_when(
                            Periodo < "2001-07-01" ~ "Amato II",
                            Periodo < "2005-07-01" ~ "Berlusconi II",
                            Periodo < "2006-04-01" ~ "Berlusconi III",
                            Periodo < "2008-04-01" ~ "Prodi II",
                            Periodo < "2011-10-01" ~ "Berlusconi IV",
                            Periodo < "2013-04-01" ~ "Monti",
                            Periodo < "2014-01-01" ~ "Letta",
                            Periodo < "2016-10-01" ~ "Renzi",
                            Periodo < "2018-07-01" ~ "Gentiloni",
                            Periodo < "2019-10-01" ~ "Conte I",
                            TRUE ~ "Conte II"), 
                           levels = c("Amato II", "Berlusconi II", "Berlusconi-III", "Prodi II", "Berlusconi IV",
                                      "Monti", "Letta", "Renzi", "Gentiloni", "Conte I","Conte-II"),
                           ordered = TRUE)
       ) %>% 
  group_by(Gruppo,Categoria,Metrica) %>% 
  arrange(Gruppo,Categoria,Metrica, Periodo) %>% 
  mutate(Cambio = ifelse(Governo == lag(Governo,1),NA,Valore)) %>% 
  ungroup() %>% 
  mutate_if(is.character, as.factor)

#QA
#filter(dati, Metrica == "Disoccupati") %>% arrange(Gruppo, Categoria, Periodo) %>% View()

######################## function for plot ###########################

trendLine <- function(df, group, metric, period){
  
   plotdf <- df %>% 
    filter(Gruppo == group, Metrica == metric, Periodo_text %in% period) %>% 
    droplevels() %>% 
    arrange(Periodo, Categoria, Valore)
  
  plot_ly(plotdf,
          x= ~Periodo, 
          y= ~Valore,
          hoverinfo = "text",
          text = ~paste("Periodo: ", paste0(year(Periodo)," ",Periodo_text),
                        "<br> Governo: ", Governo,
                        "<br> Classe: ", Categoria,
                        "<br> Valore: ", round(Valore, 2))
          )  %>% 
    # add_markers(y = ~Cambio, showlegend = FALSE,
    #             symbols = "cross"
    #             ) %>%
    add_lines(color = ~Categoria, colors = "Paired") %>%
    layout(
      title = "Trend Temporale",
      yaxis = list(title = "Valore (in Milioni o %)"),
      xaxis = list(title = "Periodo")
    )
  
}

#trendLine(dati,"ETA1","Disoccupati", c("Gen-Mar","Apr-Giu","Lug-Set", "Oct-Dec")) 

 


