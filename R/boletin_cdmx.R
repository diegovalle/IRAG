clean_bol <- function(ll, s) {
  #if (!str_detect(ll[[1]], str_check))
  #  stop()
  splits <- strsplit(ll[length(ll)], " ")[[1]]
  data.frame(tipo = if (ll[[1]][1] != "") ll[[1]][1] else ll[[2]],
             semana = s,
             n = str_replace(splits[1], ",", ""),
             acumH = str_replace(splits[2], ",", ""),
             acumM = str_replace(splits[3], ",", ""),
             acum19 = str_replace(splits[4], ",", "")
             )
}

extract_table <- function(out, name, i) {
  print(name)
  for (c in 4:length(out)) {
    for (t in 1:ncol(out[[c]])) {
      if (str_detect(out[[c]][, t][[1]], regex(name, 
                                               ignore_case = TRUE)) |
          str_detect(out[[c]][, t][[2]], regex(name, 
                                               ignore_case = TRUE))) {
        print(out[[c]][, t][[1]])
        chapter <- c
        table <- t
      }
    }
  }
  df <- out[[chapter]][, table]
  clean_bol(df, i)
}

base_url <- "http://sersalud.cdmx.gob.mx/sspcdmx/Documentos/direccion/demp/boletin/Bolet%C3%ADn%20semanal%20de%20la%20Ciudad%20de%20M%C3%A9xico%20n%C3%BAmero%20"
last_boletin <- 17

df <- data.frame()
for (i in 1:last_boletin) {
  print(i)
  out <- extract_tables(paste0(base_url, i, ".pdf"))
  #ll <- mapply(extract_table, out = out, name = enfermedades, i = i)
  #df <- do.call("rbind", ll)
  #df <- extract_table(out, "cerebrovascular", i)
  #peatones <- rbind(peatones, df)
  df <- rbind(df, 
              extract_table(out, "cerebrovascular", i), 
              extract_table(out, "Peatón", i), 
              extract_table(out, "Herida por Arma de Fuego", i), 
              extract_table(out, "Neumonías", i), 
              extract_table(out, "Vulvovaginitis", i), 
              extract_table(out, "Infecciones respiratorias", i), 
              extract_table(out, "Accidentes de transporte", i), 
              extract_table(out, "Insuficiencia venosa", i), 
              extract_table(out, "Violencia intrafamiliar", i), 
              extract_table(out, "Asma", i), 
              extract_table(out, "Diabetes mellitus no", i), 
              extract_table(out, "Gingivitis", i), 
              #extract_table(out, "Quemaduras", i), 
              extract_table(out, "Hipertensión", i), 
              extract_table(out, "Otitis", i),  
              #extract_table(out, "Varicela", i), 
              extract_table(out, "Úlceras", i),
              extract_table(out, "Edema, proteinuria", i),
              extract_table(out, "Diabetes mellitus que se", i),
              extract_table(out, "Hiperplasia de la Próstata", i),
              extract_table(out, "Tumor maligno de mama", i),
              extract_table(out, "Displasia cervical leve", i),
              extract_table(out, "Intoxicación aguda por", i),
              extract_table(out, "Mordeduras por perro", i),
              extract_table(out, "Enfermedad isquémica", i),
              extract_table(out, "Infecciones intestinales por", i)
  )
}
df2 <- read_csv("data/obesidad_depre_varicela.csv")
df <- rbind(df, df2)
df[, 2:6] <- apply(df[, 2:6], 2, as.numeric)
df[which(df$semana == 11 & df$n == 3778),]$n <- 780

df$tipo <- str_replace_all(df$tipo, "Neumonías y$", 
                           "Neumonías y bronconeumonías")
df$tipo <- str_replace_all(df$tipo, "Infecciones respiratorias$", 
                "Infecciones respiratorias agudas")
df$tipo <- str_replace_all(df$tipo, "Peatón lesionado en$", 
                "Peatón lesionado en accidente de transporte")
df$tipo <- str_replace_all(df$tipo, "Insuficiencia venosa$", 
                           "Insuficiencia venosa periférica")
df$tipo <- str_replace_all(df$tipo, "Peatón lesionado en accidente$", 
                           "Peatón lesionado en accidente de transporte")
df$tipo <- str_replace_all(df$tipo, "Infecciones intestinales por otros$", 
                           "Infecciones intestinales")
df$tipo <- str_replace_all(df$tipo, "Infecciones intestinales por$", 
                           "Infecciones intestinales")
df$tipo <- str_replace_all(df$tipo, "Diabetes mellitus no$", 
                           "Diabetes (Tipo II)")
df$tipo <- str_replace_all(df$tipo, "Herida por Arma de Fuego y$", 
                           "Herida por Arma de Fuego y Punzocortantes")
df$tipo <- str_replace_all(df$tipo, "Enfermedad isquémica del$", 
                           "Enfermedad isquémica del corazón")
df$tipo <- str_replace_all(df$tipo, "Displasia cervical leve y$", 
                           "Displasia cervical leve y moderada")
df$tipo <- str_replace_all(df$tipo, "Edema, proteinuria y$", 
                           "Edema, proteinuria y transtornos")
df$tipo <- str_replace_all(df$tipo, "Diabetes mellitus que se$", 
                           "Diabetes (embarazo)")
df$tipo <- str_replace_all(df$tipo, "Diabetes mellitus que se origina$", 
                           "Diabetes (embarazo)")
df$tipo <- str_replace_all(df$tipo, "Intoxicación aguda por$", 
                           "Intoxicación aguda por alcohol")

df$acum19[which(df$tipo == "Hipertensión arterial" &
                  df$semana == 11)] <- 9320
df$acumH[which(df$tipo == "Hipertensión arterial" &
                  df$semana == 11)] <- 3778
df$acumM[which(df$tipo == "Hipertensión arterial" &
                  df$semana == 11)] <- 4895

df <- df %>%
  group_by(tipo) %>%
  mutate(acumH = if_else(is.na(acumH), 0, acumH),
         acumM = if_else(is.na(acumM), 0, acumM)) %>%
  mutate(y19 = c(acum19[1], diff(acum19))) %>%
  mutate(y20 = c(acumH[1] + acumM[1], diff(acumH + acumM))) 
df$y20[which(df$semana == 5 )] <- df$y20[which(df$semana == 6 )] -
  df$n[which(df$semana == 6 )] 
df$y20[which(df$semana == 6 )] <- df$n[which(df$semana == 6 )] 

df <- df %>%
  group_by(tipo) %>%
  mutate(y20p = y20 / y20[10]) %>%
  mutate(y19p = y19 / y20[10])

df <- df %>%
  group_by(tipo) %>%
  mutate(order = y19[16] - y20[16]) 
df$tipo <- reorder(df$tipo, -df$order)

ggplot(df, aes(semana, y20)) +
  geom_line(aes(color = "2020"), lwd = .9) +
  geom_line(aes(semana, y19, color = "2019"), lwd = .9) +
  scale_y_comma() +
  scale_color_manual("año",
                     breaks = rev(c("2019", "2020")),
                     values = c("#0570b0", "#969696"),
                     labels = rev(c("2019", "2020"))) +
  expand_limits(y = 0) +
  labs(title = "Casos de Enfermedades Selectas en la Ciudad de México",
       subtitle = "La última semana epidemiológica disponible es la 17 que va del 19 de abril 25 de abril",
       caption = "Fuente: Boletín epidemiológico semanal de la CDMX (http://sersalud.cdmx.gob.mx/sspcdmx/direccion_epi_preven.php)") +
  xlab("semana epidemiológica") +
  ylab("casos reportados") +
  facet_wrap(~ tipo, scales = "free_y") +
  theme_ipsum(base_size = 16)
ggsave("graphs/boletin_cdmx.png", width = 16.5, height = 12, dpi = 100)
# 
# 
# ggplot(df, aes(semana, y20p)) +
#   geom_line(aes(color = "2020"), lwd = .9) +
#   geom_line(aes(semana, y19p, color = "2019"), lwd = .9) +
#   geom_point(data = data.frame(semana = 11, y20p = 1), size = 2) +
#   scale_y_percent() +
#   scale_color_manual("año",
#                      breaks = rev(c("2019", "2020")),
#                      values = c("#0570b0", "#969696"),
#                      labels = rev(c("2019", "2020"))) +
#   expand_limits(y = 0) +
#   labs(title = "Casos Nuevos de Enfermedades Selectas en la Ciudad de México",
#        subtitle = "La última semana epidemiológica disponible es la 15 que va del 5 de abril 11 de abril",
#        caption = "Fuente: Boletín epidemiológico semanal de la CDMX (http://sersalud.cdmx.gob.mx/sspcdmx/direccion_epi_preven.php)") +
#   xlab("semana epidemiológica") +
#   ylab("casos reportados") +
#   facet_wrap(~ tipo) +
#   theme_ipsum(base_size = 16)
# ggsave("graphs/boletin_cdmx_per.png", width = 16.5, height = 12, dpi = 100)
