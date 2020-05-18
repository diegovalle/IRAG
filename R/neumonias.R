if (!file.exists("cache/neumonias_nacional.csv")) {
  states <- c("Aguascalientes", "Baja California", "Baja California Sur",
              "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
              "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
              "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos",
              "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
              "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
              "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán",
              "Zacatecas", "TOTAL")
  
  ll <- list()
  l <- 1
  for (file in list.files("cache/boletines_nacional/2020/")) {
    out <- extract_tables(paste0("cache/boletines_nacional/2020/", file),
                          method = "stream", output = "data.frame")
    n <- data.frame()
    regex <- "Bonrocnoc|nuemumonías|Bronconeu|Nuemun|Neumon|Bonroc|Bornoc"
    if (file == "sem04.pdf")
      regex = "onías$"
    if (file == "sem06.pdf")
      regex = "nías$"
    for (i in 1:length(out)) {
      if (any(str_detect(names(out[[i]]), regex))) {
        n <- out[[i]][, which(str_detect(names(out[[i]]), regex) == TRUE)]
        n <- str_extract(n, "^[0-9]*[ ]{0,1}[0-9]*  ")
        n <- as.numeric(str_replace_all(n, " ", ""))
        n <- n[(length(n) - 32):length(n)]
        break()
      }
      else if (any(str_detect(out[[i]][1,], regex))) {
        n <- out[[i]][, which(str_detect(out[[i]][1,], regex) == TRUE)]
        n <- str_extract(n, "^[0-9]*[ ]{0,1}[0-9]*  ")
        n <- as.numeric(str_replace_all(n, " ", ""))
        n <- n[(length(n) - 32):length(n)]
        break()
      }
    }
    if (length(n) == 0)
      stop("empty vector")
    ll[[l]] <- n
    l <- l + 1
  }
  save(ll, file = "cache/ll.RData")
  #load("ll.RData")
  df <- t(as.data.frame(do.call(rbind, ll)))
  df <- as.data.frame(df)
  df$states <- states
  
  ll19 <- list()
  l <- 1
  for (file in list.files("cache/boletines_nacional/2019/")[1:19]) {
    out <- extract_tables(paste0("cache/boletines_nacional/2019/", file),
                          method = "stream", output = "data.frame")
    n <- data.frame()
    regex <- "coneumonías|NeNumeuomnoí|Bonrocnoc|nuemumonías|Bronconeu|Nuemun|Neumon|Bonroc|Bornoc"
    if (file == "sem04.pdf")
      regex = "onías$"
    if (file == "sem06.pdf")
      regex = "nías$"
    if (file == "sem17.pdf")
      regex = "nías$"
    if (file == "sem18.pdf")
      regex = "nías$"
    if (file == "sem19.pdf")
      regex = "coneumonías$"
    for (i in 1:length(out)) {
      if (any(str_detect(names(out[[i]]), regex))) {
        n <- out[[i]][, which(str_detect(names(out[[i]]), regex) == TRUE)]
        n <- str_extract(n, "^[0-9]*[ ]{0,1}[0-9]*  ")
        n <- as.numeric(str_replace_all(n, " ", ""))
        n <- n[(length(n) - 32):length(n)]
        break()
      }
      else if (any(str_detect(out[[i]][1,], regex))) {
        n <- out[[i]][, which(str_detect(out[[i]][1,], regex) == TRUE)]
        n <- str_extract(n, "^[0-9]*[ ]{0,1}[0-9]*  ")
        n <- as.numeric(str_replace_all(n, " ", ""))
        n <- n[(length(n) - 32):length(n)]
        break()
      }
    }
    if (length(n) == 0)
      stop("empty vector")
    ll19[[l]] <- n
    l <- l + 1
  }
  save(ll19, file = "cache/ll19.RData")
  df19 <- t(as.data.frame(do.call(rbind, ll19)))
  df19 <- as.data.frame(df19)
  df19$states <- states
  
  ll18 <- list()
  l <- 1
  for (file in list.files("cache/boletines_nacional/2018/")[1:19]) {
    out <- extract_tables(paste0("cache/boletines_nacional/2018/", file),
                          method = "stream", output = "data.frame")
    n <- data.frame()
    regex <- "NeNumeuomnoí|Bonrocnoc|nuemumonías|Bronconeu|Nuemun|Neumon|Bonroc|Bornoc"
    if (file == "sem04.pdf")
      regex = "onías$"
    if (file == "sem06.pdf")
      regex = "nías$"
    if (file == "sem17.pdf")
      regex = "nías$"
    if (file == "sem18.pdf")
      regex = "nías$"
    for (i in 1:length(out)) {
      if (any(str_detect(names(out[[i]]), regex))) {
        n <- out[[i]][, which(str_detect(names(out[[i]]), regex) == TRUE)]
        n <- str_extract(n, "^[0-9]*[ ]{0,1}[0-9]*  ")
        n <- as.numeric(str_replace_all(n, " ", ""))
        n <- n[(length(n) - 32):length(n)]
        break()
      }
      else if (any(str_detect(out[[i]][1,], regex))) {
        n <- out[[i]][, which(str_detect(out[[i]][1,], regex) == TRUE)]
        n <- str_extract(n, "^[0-9]*[ ]{0,1}[0-9]*  ")
        n <- as.numeric(str_replace_all(n, " ", ""))
        n <- n[(length(n) - 32):length(n)]
        break()
      }
    }
    if (length(n) == 0)
      stop("empty vector")
    ll18[[l]] <- n
    l <- l + 1
  }
  save(ll18, file = "cache/ll18.RData")
  df18 <- t(as.data.frame(do.call(rbind, ll18)))
  df18 <- as.data.frame(df18)
  df18$states <- states
  
  df$year <- 2020
  df19$year <- 2019
  df18$year <- 2018
  df <- rbind(df, df19, df18)
  
  df <- pivot_longer(df, c(-states, -year)) %>%
    mutate(week = as.numeric(str_replace(name, "V", "")) - 1)  %>%
    filter(week != 0)
    
  
  write.csv(df, "cache/neumonias_nacional.csv", row.names = FALSE)
} else {
  df <- read.csv("cache/neumonias_nacional.csv")
}
df$size <- 1
df$size[df$year == 2020] <- 1.1
df <- df %>% 
  group_by(states) %>%
  mutate(order = value[18] - value[11])
df$color <- "#bababa"
df$color[df$year == 2020] <- "#0571b0" 
df$color[df$states %in% c("Ciudad de México", "Baja California", "Tlaxcala",
                        "México", "Puebla", "Guerrero") &
           df$year == 2020] <- "#ca0020"

df$states <- reorder(df$states, -df$order)
df %>%
  ggplot(aes(week, value, group = year, color = color)) +
  geom_line(aes(size = size)) + 
  facet_wrap(~ states, scales = "free_y") +
  scale_y_comma() +
  scale_size(range = c(.5, 1.1), guide = FALSE) +
  scale_color_identity("año", 
                       guide = "legend",
                       breaks = c("#ca0020", "#0571b0", "#bababa"),
                       labels = c("2020\n(incremento)", "2020", "2018-2019")) +
  expand_limits(y = 0) +
  labs(title = "Casos de Neumonías y Bronconeumonías en México, por Estado",
       subtitle = "La última semana epidemiológica disponible es la 17 que va del 19 de abril 25 de abril\nIncluye CIE-10ª REV. J12-J18 excepto J18.2, J13 y J14",
       caption = "Fuente: Boletín epidemiológico Sistema Nacional de Vigilancia Epidemiológica") +
  xlab("semana epidemiológica") +
  ylab("casos reportados") +
  theme_ipsum(base_size = 18)
ggsave("graphs/neumonias_estados.png", width = 16.5, height = 14, dpi = 100)


df %>%
  filter(states %in% c("Ciudad de México", "Baja California", "Tlaxcala",
                       "México", "Puebla", "Guerrero")) %>%
  ggplot(aes(week, value, group = year, color = as.factor(year))) +
  geom_line(aes(size = size)) + 
  facet_wrap(~ states, scales = "free_y") +
  scale_y_comma() +
  scale_size(range = c(.5, 1.1), guide = FALSE) +
  scale_color_manual("año",
                     breaks = rev(c("2018", "2020")),
                     values = c("#969696", "#969696", "#ca0020"),
                     labels = rev(c("2018-2019", "2020"))) +
  expand_limits(y = 0) +
  labs(title = "Casos de Neumonías y Bronconeumonías en México, por Estado",
       subtitle = "La última semana epidemiológica disponible es la 17 que va del 19 de abril 25 de abril\nIncluye CIE-10ª REV. J12-J18 excepto J18.2, J13 y J14",
       caption = "Fuente: Boletín epidemiológico Sistema Nacional de Vigilancia Epidemiológica") +
  xlab("semana epidemiológica") +
  ylab("casos reportados") +
  theme_ipsum(base_size = 18)
ggsave("graphs/neumonias_estados_sub.png", width = 16.5, height = 8, dpi = 100)
