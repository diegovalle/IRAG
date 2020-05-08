

base_url <- 'https://www.gob.mx'

page <- read_html("https://www.gob.mx/salud/documentos/informes-semanales-para-la-vigilancia-epidemiologica-de-influenza-2020")
links <- page %>% html_nodes("a") %>% html_attr('href')

casos <- data.frame()
for (url in links[which(str_detect(links, "/cms/uploads/attachment/file/"))]) {
  # Extract the table
  print(url)
  out <- extract_tables(paste0(base_url, url))
  for (i in 1:length(out)) {
    if (str_detect(out[[i]][,1][1], "Información de la temporada de influenza estacional"))
      break
  }
  if (url == "/cms/uploads/attachment/file/522950/INFLUENZA_SE01_2020.pdf") {
    i <- 6
  }
  states <- out[i][[1]][,1]
  s <- which(str_detect(states, "AGUASCA"))
  e <- which(str_detect(states, "MICHOA"))
  first_half <- 
    str_replace_all(str_extract(states[s:e], "[[:alpha:][:space:]]+[[:digit:],]+"), 
                    ",", "")
  sec_half <- str_replace_all(
    str_extract(states[s:e], "[:digit:] [[:alpha:][:space:]]+[[:digit:],]+"), 
    ",", "")
  sec_half <- str_replace_all(sec_half, "^[:digit:] ", "")
  
  df <- data.frame(states = c(first_half, sec_half))
  df <- separate(df, states, c("states", "ETI_IRAG"), sep = "\\s+(?=\\S*$)")
  df$ETI_IRAG <- as.numeric(df$ETI_IRAG)
  df$week <- as.integer(str_extract(url, "(?!.*SE)[:digit:]+"))
  df$season <- "2019-2020"
  casos <- rbind(casos, df)
}

casos <- casos %>%
  arrange(week, states) %>%
  group_by(states) %>%
  mutate(diff = diff(c(NA, ETI_IRAG)))

casos_19_20 <- rbind(casos, casos19_3, casos_18_1, casos_17_1)
casos_19_20$date <- as.Date(paste(str_sub(casos_19_20$season, 6), 
                                  casos_19_20$week - 1, 1, sep = "-"), 
                            "%Y-%U-%u")

pop <- read_csv("data/pop.csv")
pop$date <- NULL
casos_19_20 <- left_join(casos_19_20, pop, by = c("states" = "state_name"))
casos_19_20$rate <- casos_19_20$diff / casos_19_20$population * 10^5

casos_19_20 <- casos_19_20  %>%
  arrange(season, week, states) %>%
  group_by(states) %>%
  mutate(order = rate[length(rate)] - rate[length(rate) - 1]) %>%
  filter(week <= 19)


total <- casos_19_20 %>%
  group_by(week, season) %>%
  summarise(total = sum(diff))

total$covid_seas <- "20162018"
total$covid_seas[total$season == "2019-2020"] <- "20192020"

total <- total %>%
  group_by(week, covid_seas) %>%
  summarise(total = mean(total, na.rm = TRUE))

total$size <- 1
total$size[total$covid_seas == "20192020"] <- 1.01

total$ymax <- total$ymin <- total$total
total$ymax[total$covid_seas == "20192020" & total$week >= 15] <- 
  total$total[total$covid_seas == "20192020" & total$week >= 15]
total$ymin[total$covid_seas == "20192020" & total$week >= 15] <- 
  total$total[total$covid_seas == "20162018" & total$week >= 15]

excess <- pivot_wider(filter(total, week >= 15), 
                      week, names_from = covid_seas, values_from = total)
excess <- round(sum(excess$`20192020` - excess$`20162018`))


total$covid_seas <- factor(total$covid_seas, levels = rev(unique(total$covid_seas)))
ggplot(total, 
       aes( group = covid_seas)) +
  geom_ribbon(data = total, 
              aes(week, total, ymin = ymin, ymax = ymax),
              fill = "red", color = "transparent", alpha = .3) +
  geom_line(aes(week, total, linetype = covid_seas, 
                lwd  = size, color = covid_seas)) +
  annotate("text", x = max(total$week) - 1.5, y = 5000, 
           label = paste0(comma(excess), "\nmás casos\nque el promedio\ndesde la\nsemana 15"),
           color = "black", size = 5) +
  scale_size( range = c(.5, 1.2), guide = FALSE) +
  expand_limits(y = 0) +
  xlab("semana epidemiológica") + 
  scale_color_manual("temporada", 
                     values = rev(c("#2171b5", "#f03b20")),
                     breaks = c("20192020", "20162018"),
                     labels = c("2019-2020", "promedio\nde las\ntemporadas\n2016-2017 a\n2018-2019")) + 
  scale_linetype_manual(values = c("solid", rep("solid", 3)), guide = FALSE) +
  ylab("casos reportados") +
  scale_x_continuous(breaks = c(5, 10, 18),
                     labels = c("5\n(en el 2020 va de\nene 26 a feb 1)", 
                                "10\n(en el 2020 va de\nmar 1 a mar 7)",
                                "19\n(en el 2020 va de\nmay 3 a may 7)")) +
  labs(title = str_c("Casos de Enfermedad tipo influenza (ETI) e ", 
                     "Infección respiratoria aguda grave (IRAG) en México"),
       subtitle = str_c("La información proviene de 475 Unidades de Salud Monitoras de Influenza (USMI)\n",
                        "Incluye datos con fecha de corte al 7 de mayo del 2020: semana epidemiológica 17. ",
                        "Los datos están ordenados por fecha de registro.\n",
                        "La mayoría de los casos ocurren en la semanas anteriores a la que fueron ",
                        "registrados, pero no todos. Los datos son preliminares\ne incompletos con fecha de acceso al 07/05/2020."),
       caption = "Fuente: Informes Semanales para la Vigilancia Epidemiológica de Influenza (https://bit.ly/3bVZfBM)") +
  theme_ipsum() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1.2, "cm"))
ggsave("graphs/eti_irag_national.png", width = 14, height = 8.5, dpi = 100)



# casos_19_20$t <- FALSE 
# casos_19_20[which(casos_19_20$season %in% c("2016-2017",
#                                 "2017-2018",
#                                 "2018-2019")), ]$t <- TRUE
# casos_19_20 <- casos_19_20 %>%
#   na.omit() %>%
#   group_by(t, week, states) %>%
#   summarize(max = max(diff), min = min(diff) ) %>%
#   mutate(season = if_else(t, "2016 a 2019", "2019-2020"))

casos_19_20$states <- reorder(casos_19_20$states, 
                              -casos_19_20$rate, 
                              function(x) min(tail(x, 1), 
                                              na.rm = TRUE))

casos_19_20$size <- 1
casos_19_20$size[casos_19_20$season == "2019-2020"] <- 1.01
casos_19_20$season <- factor(casos_19_20$season, 
                             levels = rev(unique(casos_19_20$season)))

ggplot(casos_19_20, 
       aes(week, rate, group = season, color = season))  +
  geom_line(aes(linetype = season, lwd  = size)) +
  scale_size( range = c(.5, .8), guide = FALSE) +
  facet_wrap(~states) +
  expand_limits(y = 0) +
  xlab("semana epidemiológica") +
  ylab("tasa por 100,000") +
  scale_color_manual("temporada", values = rev(c("#6baed6",
                                             "#4292c6",
                                             "#2171b5", "#f03b20"))) + 
  scale_linetype_manual(values = c("solid", rep("dashed", 3)), guide = FALSE) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = str_c("Tasas de Enfermedad tipo influenza (ETI) e ", 
                     "Infección respiratoria aguda grave (IRAG) en México, por estado"),
       subtitle = str_c("Incluye datos con fecha de corte al 7 de mayo del 2020: semana epidemiológica 19.\n",
                        "Los datos están ordenados por fecha de registro. ",
                        "La mayoría de los casos ocurren en la semanas anteriores\na la que fueron ",
                        "registrados, pero no todos. Los datos son preliminares e incompletos con fecha de acceso al 07/05/2020."),
       caption = "Fuente: Informes Semanales para la Vigilancia Epidemiológica de Influenza (https://bit.ly/3bVZfBM)") +
  theme_ipsum() 
ggsave("graphs/eti_irag_states.png", width = 16.5, height = 18, dpi = 100)

casos_19_20 <- filter(casos_19_20, states %in% c("CIUDAD DE MÉXICO",
                                                        "NUEVO LEÓN",
                                                        "JALISCO",
                                                        "ESTADO DE MÉXICO",
                                                        "PUEBLA",
                                                        "YUCATÁN",
                                                        "QUINTANA ROO",
                                                        "COLIMA"))
                                                 #"MICHOACÁN"))
casos_19_20$states <- reorder(casos_19_20$states, -casos_19_20$order)


ggplot(casos_19_20, 
       aes(week, diff, group = season, color = season)) +
  geom_line(aes(linetype = season)) +
  facet_wrap(~states) +
  expand_limits(y = 0) +
  xlab("semana epidemiológica") +
  ylab("casos reportados") +
  scale_color_manual("temporada", values = c("#6baed6",
                                             "#4292c6",
                                             "#2171b5", "#f03b20")) + 
  scale_linetype_manual(values = c(rep("dashed", 3), "solid"), guide = FALSE) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = str_c("Casos de Enfermedad tipo influenza (ETI) e\nInfección ", 
                     "respiratoria aguda grave (IRAG) en México"),
       subtitle = str_c("Incluye datos con fecha de corte del 9 de abril de 2020: semana epidemiológica 15.\n",
                        "Por la forma en que se registran los datos la mayoría de los casos de ",
                        "la semana epidemiológica 15\n",
                        "corresponden a los ocurridos del 29 de marzo al 4 de abril, pero incluyen ",
                        "algunos del 5 al 9 de abril\ny semanas anteriores\n",
                        "Los datos están ordenados por fecha de registro, no de ocurrencia. ",
                        "La mayoría de los casos ocurren\nen la semana anterior a la que fueron ",
                        "registrados, pero no todos."),
       caption = "Fuente: Informes Semanales para la Vigilancia Epidemiológica de Influenza") +
  theme_ft_rc()

ggsave("graphs/eti_irag_states_filter.png", width = 16.5, height = 8, dpi = 100)

