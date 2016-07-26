# lae ilma andmed perioodi 29.07.1984 - 31.12.2015 kohta
library(stationaRy)
library(dplyr)
library(lubridate)
library(stringr)

# lae algandmed
temp_raw <- get_isd_stations() %>%
    select_isd_station(name = "tallinn") %>%
    get_isd_station_data(startyear = 1984, endyear = 2016)

# kuupäevast aastaaeg
# funktsioon aadressilt: http://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
getSeason <- function(DATES) {
    talv <- as.Date("2012-12-20", format = "%Y-%m-%d") # Winter Solstice
    kevad <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Spring Equinox
    suvi <- as.Date("2012-6-20",  format = "%Y-%m-%d") # Summer Solstice
    sygis <- as.Date("2012-9-20",  format = "%Y-%m-%d") # Fall Equinox
    
    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    
    ifelse (d >= talv | d < kevad, "talv",
            ifelse (d >= kevad & d < suvi, "kevad",
                    ifelse (d >= suvi & d < sygis, "suvi", "sügis")))
}

# leia iga päeva keskmine, min ja max temp
temp <- temp_raw %>%
    select(time, temp) %>%
    mutate(kp = as.Date(time),
           kuu_paev = str_c(day(kp), month(kp), sep = "-"),
           aastaaeg = getSeason(kp),
           aastaaeg = factor(aastaaeg, levels = c("kevad", "suvi", "sügis", "talv")),
           aasta = year(kp)) %>%
    # välista liigaastate 29. veebruarid
    filter(kuu_paev != "29-2") %>%
    group_by(kp, aasta, aastaaeg) %>%
    summarise(min_temp = round(min(temp, na.rm = TRUE), 1),
              max_temp = round(max(temp, na.rm = TRUE), 1),
              mean_temp = round(mean(temp, na.rm = TRUE), 1)) %>%
    ungroup() %>%
    arrange(aastaaeg) %>%
    group_by(aasta) %>%
    # graafikul kuvamiseks päeva number aastas (algusega kevadel)
    mutate(paev_aastas = row_number()) %>%
    filter(kp >= as.Date("29.07.1984", "%d.%m.%Y"),
           aasta < 2016)

# salvesta andmed
save(temp, file = "data/temp.Rdata")
