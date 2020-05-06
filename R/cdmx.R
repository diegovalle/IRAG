#page <- read_html("http://sersalud.cdmx.gob.mx/sspcdmx/direccion_epi_preven.php")

cdmx <- read.csv("data/cdmx2020.csv")
cdmx$week <- rep(c(1:9, 11:18), 1, each = 16)
cdmx$season <- "2019-2020"
cdmx <- cdmx  %>%
  arrange(season, week, alcaldía) %>%
  group_by(alcaldía) %>% 
  mutate(diff = diff(c(NA, n))) %>%
  mutate(order = diff[length(diff)]) 

cdmx$alcaldía <- reorder(cdmx$alcaldía, -cdmx$order)
cdmx <- subset(cdmx, !week %in% 10:11)

ggplot(cdmx, aes(week, diff)) +
  geom_bar(stat = "identity", fill = "#e31a1c", color = "#e31a1c") +
  #geom_point(color = "#e31a1c") +
  facet_wrap(~alcaldía) +
  expand_limits(y = 0) +
  xlab("semana epidemiológica") +
  ylab("casos reportados") +
  scale_x_continuous(breaks = c(4, 17),
                     labels = c("4\n(16 ene. a 25 ene.)", 
                                #"10\n(Mar 1 - Mar 7)",
                                "17\n(19 abr. - 25 abr.)")) +
  labs(title = str_c("Casos de Enfermedad tipo influenza (ETI) e Infección ", 
                     "respiratoria aguda grave (IRAG) en CDMX¸ por alcaldía"),
       subtitle = str_c("Incluye datos con fecha de corte de la semana epidemiólogica 17 y la información es preliminar.",
                        " Por la forma en que se registran los datos la mayoría de los casos de ",
                        "la semana epidemiológica 16\n",
                        "corresponden a los ocurridos de las semanas 12 a la 15. La semana 17 va del 19  al 25 de abril.",
                        "\nLos datos están ordenados por fecha de registro, no de ocurrencia. ",
                        "La mayoría de los casos ocurren en la semanas anteriores a la que fueron ",
                        "registrados, pero no todos.\nNo se pudieron obtener datos de las semanas 10 y 11."),
       caption = "Fuente: La información se obtiene de 39 Unidades de Salud Monitoras de Influenza (USMI) (http://sersalud.cdmx.gob.mx/sspcdmx/direccion_epi_preven.php)") +
  theme_ipsum()
ggsave("graphs/eti_irag_cdmx.png", width = 16.5, height = 12, dpi = 100)


cdmx %>%
  group_by(week) %>%
  summarise(n = sum(diff))  %>%
  ggplot(aes(week, n)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  xlab("semana epidemiológica") +
  ylab("casos reportados") +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_ipsum()
