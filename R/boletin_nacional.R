bn <- read_csv("data/boletin_nacional.csv",
               col_types = cols(
                  tipo = col_character(),
                  year = col_double(),
                  `17` = col_character(),
                  `16` = col_character(),
                  `15` = col_character(),
                  `14` = col_character(),
                  `13` = col_character(),
                  `12` = col_character(),
                  `11` = col_character(),
                  `10` = col_character(),
                  `9` = col_character(),
                  `8` = col_character(),
                  `7` = col_character(),
                  `6` = col_character(),
                  `5` = col_character(),
                  `4` = col_character(),
                  `3` = col_character(),
                  `2` = col_character(),
                  `1` = col_character()
               ))

bn <- pivot_longer(bn, c(-tipo, -year), names_to = "week")
bn$value <- as.numeric(str_remove_all(bn$value, " "))
bn$week <- as.numeric(bn$week)
bn$year <- as.factor(bn$year)

bn$group <- "2017-2019"
bn$group[bn$year == 2020] <- "2020"
bn <- bn %>%
   na.omit() %>%
   group_by(tipo, group, week) %>%
   mutate(min = min(value)) %>%
   ungroup() %>%
   group_by(tipo) %>%
   mutate(order = max(min[12:16]) - min(min[12:16]))
bn$tipo <- reorder(bn$tipo, -bn$order)

ggplot(bn, aes(week, value, group = year, color = year)) +
   geom_line() +
   facet_wrap(~ tipo, scale = "free_y") +
   expand_limits(y = 0) +
   scale_y_comma() +
   scale_color_manual("año", 
                      breaks = c(2020, 2017),
                      values = c(rep("#969696", 3), "#0570b0"),
                      labels = rev(c("2017-2019", "2020"))) +
   labs(title = "Casos de Enfermedades Selectas a Nivel Nacional (2017-2020)",
         subtitle = "",
         caption = "Fuente: Boletín Epidemiológico Sistema Nacional de Vigilancia Epidemiológica") +
   theme_ipsum(base_size = 16) +
   xlab("semana epidemiológica") +
   ylab("número de casos")
ggsave("graphs/boletin_nacional.png", width = 14, height = 8, dpi = 100)
