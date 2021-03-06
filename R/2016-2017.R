#Extract data from the 2016-2017 flu reports 


table_sep <- function(lk) {
  base_url <- 'https://www.gob.mx'
  print(lk)
  page <- read_html(lk)
  link_page <- page %>% html_nodes("a") %>% html_attr('href')
  
  out <- extract_tables(paste0(base_url, 
                               link_page[which(str_detect(link_page, "pdf"))]))
  
  
  
  states <- out[4][[1]]
  
  df <- as.data.frame(states)[6:21, c("V1", "V2", "V6", "V7")]
  
  df <- rbind(data.frame(states = df$V1, ETI_IRAG = df$V2), 
              data.frame(states = df$V6, ETI_IRAG = df$V7))
  df$ETI_IRAG <- as.numeric(str_replace_all(df$ETI_IRAG, ",", ""))
  df$week <- as.integer(str_extract(link_page[which(str_detect(link_page, 
                                                               "INFLUENZA"))],
                                    "(?!.*SE)[:digit:]+"))
  df$season <- "2016-2017"
  return(df)
}

table_mess <- function(lk) {
  print(lk)
  base_url <- 'https://www.gob.mx'
  page <- read_html(lk)
  link_page <- page %>% html_nodes("a") %>% html_attr('href')
  
  out <- extract_tables(paste0(base_url, 
                               link_page[which(str_detect(link_page, "pdf"))]))
  for (i in 1:length(out)) {
    if (str_detect(out[[i]][,1][1], "Tabla 4|^Información de la temporada de influenza (inter)?(inter )?estacional"))
      break
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
  df$week <- as.integer(str_extract(lk, "(?!.*-)[:digit:]+"))
  df$season <- "2016-2017"
  return(df)
}

base_link <- "http://www.gob.mx/salud/documentos/direccion-general-de-epidemiologia-informes-semanales-para-la-vigilancia-epidemiologica-de-influenza-2017-semana-epidemiologica-"
links <- paste0(base_link, c(paste0("0", 1:9), 10:17))

if (!file.exists("cache/casos17.csv")) {
  casos_17_1 <- rbind(
    table_sep(links[1]),
    table_mess(links[2]),
    table_mess(links[3]),
    table_mess(links[4]),
    table_mess(links[5]),
    table_mess(links[6]),
    table_mess(links[7]),
    table_mess(links[8]),
    table_mess(links[9]),
    table_mess(links[10]),
    table_mess(links[11]),
    table_mess(links[12]),
    table_mess(links[13]),
    table_mess(links[14]),
    table_mess(links[15]),
    table_mess(links[16]),
    table_mess("https://www.gob.mx/salud/documentos/informes-semanales-para-la-vigilancia-epidemiologica-de-influenza-2017-semana-epidemiologica-17"),
    table_mess("http://www.gob.mx/salud/documentos/informes-semanales-para-la-vigilancia-epidemiologica-de-influenza-2017-semana-epidemiologica-18"),
    table_mess("http://www.gob.mx/salud/documentos/informes-semanales-para-la-vigilancia-epidemiologica-de-influenza-2017-semana-epidemiologica-19"),
    table_mess("https://www.gob.mx/salud/documentos/informes-semanales-para-la-vigilancia-epidemiologica-de-influenza-2017-semana-epidemiologica-20")
  )
  write.csv(casos_17_1, "cache/casos17.csv", row.names = FALSE)
} else
  casos_17_1 <- read.csv("cache/casos17.csv")



casos_17_1 <- casos_17_1 %>%
  arrange(week, states) %>%
  group_by(states) %>%
  mutate(diff = diff(c(NA, ETI_IRAG)))
