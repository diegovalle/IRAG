

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

casos_19_20 <- rbind(casos, casos19_3)
casos_19_20$date <- as.Date(paste(str_sub(casos_19_20$season, 6), 
                                  casos_19_20$week - 1, 1, sep = "-"), 
                            "%Y-%U-%u")

casos_19_20 <- casos_19_20  %>%
  arrange(season, week, states) %>%
  group_by(states) %>%
  mutate(order = diff[length(diff)] - diff[length(diff) - 1]) %>%
  filter(week <= 12)

casos_19_20$states <- reorder(casos_19_20$states, -casos_19_20$order)

ggplot(casos_19_20, aes(week, diff, group = season, color = season)) +
  geom_line(aes(linetype = season)) +
  scale_linetype_manual(values = c("twodash", "solid"), guide  =FALSE) +
  facet_wrap(~states) +
  expand_limits(y = 0) +
  xlab("semana epidemiológica") +
  ylab("casos reportados") +
  scale_color_manual("temporada", values = c("#00AFBB", "#FC4E07")) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = str_c("Casos de Enfermedad tipo influenza (ETI) e Infección ", 
                     "respiratoria aguda grave (IRAG) en México¸ por estado"),
       subtitle = str_c("Incluye datos con fecha de corte del 19 marzo de 2020: semana epidemiológica 12.\n",
                        "Por la forma en que se registran los datos la mayoría de los casos de ",
                        "la semana epidemiológica 12\n",
                        "corresponden a los ocurridos del 8 al 14 de marzo, pero incluyen algunos del 15 al 19 de marzo",
                        "\nLos datos están ordenados por fecha de registro, no de ocurrencia. ",
                        "La mayoría de los casos ocurren\nen la semana anterior a la que fueron ",
                        "registrados, pero no todos."),
       caption = "Fuente: Informes Semanales para la Vigilancia Epidemiológica de Influenza") +
  theme_ft_rc()
ggsave("graphs/eti_irag_states.png", width = 16.5, height = 12, dpi = 100)

total <- casos_19_20 %>%
  group_by(week, season) %>%
  summarise(total = sum(diff))


ggplot(total, aes(week, total, group = season, color = season)) +
  geom_line(aes(linetype = season)) +
  scale_linetype_manual(values = c("twodash", "solid"), guide  =FALSE) +
  expand_limits(y = 0) +
  xlab("semana epidemiológica") + 
  scale_color_manual("temporada", values = c("#00AFBB", "#FC4E07")) +
  ylab("casos reportados") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = str_c("Casos de Enfermedad tipo influenza (ETI) e\nInfección ", 
                     "respiratoria aguda grave (IRAG) en México"),
       subtitle = str_c("Incluye datos con fecha de corte del 19 marzo de 2020: semana epidemiológica 12.\n",
       "Por la forma en que se registran los datos la mayoría de los casos de ",
       "la semana epidemiológica 12\n",
       "corresponden a los ocurridos del 8 al 14 de marzo, pero incluyen",
       "algunos del 15 al 19 de marzo.\n",
       "Los datos están ordenados por fecha de registro, no de ocurrencia. ",
       "La mayoría de los casos ocurren\nen la semana anterior a la que fueron ",
       "registrados, pero no todos."),
       caption = "Fuente: Informes Semanales para la Vigilancia Epidemiológica de Influenza") +
  theme_ft_rc()
ggsave("graphs/eti_irag_national.png", width = 12, height = 8, dpi = 100)
