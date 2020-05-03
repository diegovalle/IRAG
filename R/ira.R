b <- read_csv("data/boletin.csv",
              col_types = cols(
                year = col_double(),
                week = col_double(),
                value = col_double(),
                size = col_double()
              ))

b$week <- as.numeric(b$week)
b$year <- as.factor(b$year)
b$size = 1
b$size[b$year == 2020] <- 2
ggplot(b, aes(week, value, group = year, color = year)) +
  geom_line(aes(size = size)) +
  scale_size(range = c(.6, 1.2), guide = FALSE) +
  expand_limits(y = 0) +
  scale_y_comma() +
  xlab("semana epidemilógica") +
  ylab("casos") +
  labs(title = "Infecciones Respiratorias Agudas a nivel nacional (2010-2020)",
       caption = "Fuente:Boletín Epidemiológico Sistema Nacional de Vigilancia Epidemiológica") +
  scale_color_manual("año", 
                     breaks = c(2020, 2010),
                         values = c(rep("#969696", 10), "#0570b0"),
                         labels = rev(c("2010-2019", "2020"))) +
  theme_ipsum(base_size = 16)
ggsave("graphs/boletin_ira.png", width = 10, height = 6, dpi = 100)

                