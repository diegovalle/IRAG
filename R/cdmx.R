#page <- read_html("http://sersalud.cdmx.gob.mx/sspcdmx/direccion_epi_preven.php")

cdmx <- read.csv("data/cdmx2020.csv")
cdmx$week <- rep(c(1:9, 11:15), 1, each = 16)
cdmx$season <- "2019-2020"
cdmx <- cdmx  %>%
  arrange(season, week, alcaldía) %>%
  group_by(alcaldía) %>% 
  mutate(diff = diff(c(NA, n))) %>%
  mutate(order = diff[length(diff)] - diff[length(diff) - 1]) 

cdmx$alcaldía <- reorder(cdmx$alcaldía, -cdmx$order)
cdmx <- subset(cdmx, !week %in% 10:11)

ggplot(cdmx, aes(week, diff)) +
  geom_line() +
  geom_point() +
  facet_wrap(~alcaldía) +
  expand_limits(y = 0) +
  xlab("semana epidemiológica") +
  ylab("casos reportados") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = str_c("Casos de Enfermedad tipo influenza (ETI) e Infección ", 
                     "respiratoria aguda grave (IRAG) en CDMX¸ por alcaldía"),
       subtitle = str_c("Incluye datos con fecha de corte del 19 marzo de 2020: semana epidemiológica 12.\n",
                        "Por la forma en que se registran los datos la mayoría de los casos de ",
                        "la semana epidemiológica 12\n",
                        "corresponden a los ocurridos del 8 al 14 de marzo, pero incluyen algunos del 15 al 19 de marzo",
                        "\nLos datos están ordenados por fecha de registro, no de ocurrencia. ",
                        "La mayoría de los casos ocurren\nen la semana anterior a la que fueron ",
                        "registrados, pero no todos."),
       caption = "Fuente: Informes Semanales para la Vigilancia Epidemiológica de Influenza") +
  theme_ft_rc()
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
  theme_ft_rc()
