#  visualiseeri temperatuur läbi minu elu
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggrepel)
 
load("data/temp.Rdata")

# x-teljele aastaaegade nimede asukohad
aastaaegade_asukoht <- temp %>%
    group_by(aastaaeg) %>%
    summarise(label = mean(paev_aastas)) %>%
    .$label

# min ja max temp 
min_max_temp <- temp %>%
    arrange(desc(mean_temp)) %>%
    tail(1) %>%
    bind_rows(temp %>%
                  arrange(desc(mean_temp)) %>%
                  head(1)) %>%
    .$kp %>%
    as.character(.)

# tekstina lisamiseks olulisemad kuupäevad ja sündmused
tahtpaevad_kp <- as.Date(c("1984-07-29", "2014-09-05", "2015-02-22", min_max_temp), 
                      format = "%Y-%m-%d")

tahtpaevad_label <- c("29.07.1984 - minu sünnipäev",
                      "05.09.2014 - abiellusin Jaanikaga",
                      "22.02.2015 - sündis minu poeg Lukas",
                      "10.01.1987 - minu elu kõige külmem päev",
                      "13.07.2010 - minu elu kõige kuumem päev")

# pane tähtpäeva kuupäev ja tekst kokku
tahtpaevad <- data_frame(tahtpaevad_kp, tahtpaevad_label) %>%
    inner_join(temp, by = c("tahtpaevad_kp" = "kp")) %>%
    mutate(tahtpaevad_label = str_c(tahtpaevad_label, " (", mean_temp,
                                    "°C)"))

# joonista graafik
temp %>%
    ggplot(aes(x = paev_aastas, y = mean_temp)) +
    # punasega üle 0 kraadise temp punktid
    geom_point(data = temp %>% filter(mean_temp >= 0), aes(x = paev_aastas, 
                                                           y = mean_temp),
               size = 0.2, alpha = 0.4, shape = 16, colour = "#f03b20") +
    # sinisega alla 0 kraadise temp punktid
    geom_point(data = temp %>% filter(mean_temp < 0), aes(x = paev_aastas, 
                                                           y = mean_temp),
               size = 0.2, alpha = 0.4, shape = 16, colour = "#2b8cbe") +
    # tähtpäevad
    geom_point(data = tahtpaevad, aes(x = paev_aastas, y = mean_temp),
    size = 1.4, color = "black") +
    # aastaaegade vahejooned
    geom_vline(xintercept = c(91, 182, 273), alpha = 0.2) +
    geom_smooth(colour = "#e34a33", se = FALSE) +
    scale_x_continuous(breaks = aastaaegade_asukoht, labels = c("..kevad..", "..suvi..", 
                                                     "..sügis..", "..talv..")) +
    labs(title = "Temperatuur minu elu jooksul",
         subtitle = "Iga punkt tähistab ühe päeva keskmist temperatuuri Tallinnas perioodil 29.07.1984 - 31.12.2015",
         y = "temp") +
    # olulisemad sündumused tekstina
    geom_label_repel(data = tahtpaevad, aes(x = paev_aastas, y = mean_temp, 
                                            label = tahtpaevad_label), 
                     fill = "grey",
                     size = 3,
                     # fontface = "bold",
                     color = "black",
                     box.padding = unit(0.5, "lines"),
                     point.padding = unit(0.5, "lines"),
                     arrow = arrow(length = unit(0.01, 'npc')),
                     force = 2,
                     alpha = 0.7) +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none",
          axis.ticks = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank())