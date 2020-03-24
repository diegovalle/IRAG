#Extract data from the 2018-2019 flu reports 


base_url <- 'https://www.gob.mx'
page <- read_html("https://www.gob.mx/salud/documentos/informes-semanales-para-la-vigilancia-epidemiologica-de-influenza-2019-186331")
links <- page %>% html_nodes("a") %>% html_attr('href')

#extract data from weeks 52-20
if (!file.exists("cache/casos19.csv")) {
  casos19 <- data.frame()
  for (url in links[which(str_detect(links, "/cms/uploads/attachment/file/"))]) {
    # Extract the table
    if ( as.integer(str_extract(url, "(?!.*SE)[:digit:]+")) <= 19)
      break
    out <- extract_tables(paste0(base_url, url))
    for (i in 1:length(out)) {
      if (str_detect(out[[i]][,1][1], "Tabla 4|^Información de la temporada de influenza (inter)?(inter )?estacional"))
        break
    }
    if (url == "/cms/uploads/attachment/file/467905/INFLUENZA_SE23_2019.pdf") {
      i <- 6
    }
    if (url == "/cms/uploads/attachment/file/464412/INFLUENZA_SE21_2019.pdf") {
      i <- 5
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
    df$season <- "2018-2019"
    casos19 <- rbind(casos19, df)
  }
  write.csv(casos19, "cache/casos19.csv", row.names = FALSE)
} else
  casos19 <- read.csv("cache/casos19.csv")


#extract data from weeks 19-17(different format)
if (!file.exists("cache/casos19_2.csv")) {
  casos19_2 <- data.frame()
  for (url in links[which(str_detect(links, "/cms/uploads/attachment/file/"))]) {
    print(url)
    # Extract the table
    if ( as.integer(str_extract(url, "(?!.*SE)[:digit:]+")) > 19)
      next
    if ( as.integer(str_extract(url, "(?!.*SE)[:digit:]+")) <= 17)
      next
    out <- extract_tables(paste0(base_url, url))
    
    states <- out[5][[1]]
    
    df <- as.data.frame(states)[6:21, c("V1", "V2", "V6", "V7")]
    
    df <- rbind(data.frame(states = df$V1, ETI_IRAG = df$V2), 
                data.frame(states = df$V6, ETI_IRAG = df$V7))
    df$ETI_IRAG <- as.numeric(str_replace_all(df$ETI_IRAG, ",", ""))
    df$week <- as.integer(str_extract(url, "(?!.*SE)[:digit:]+"))
    df$season <- "2018-2019"
    
    casos19_2 <- rbind(casos19_2, df)
  }
  write.csv(casos19_2, "cache/casos19_2.csv", row.names = FALSE)
}else
  casos19_2 <- read.csv("cache/casos19_2.csv")


#extract data from weeks 1-17 (yet another format)
if (!file.exists("cache/casos19_3.csv")) {
  casos19_3 <- data.frame()
  for (url in links[which(str_detect(links, "/cms/uploads/attachment/file/"))]) {
    # Extract the table
    print(url)
    if ( as.integer(str_extract(url, "(?!.*SE)[:digit:]+")) > 17)
      next
    out <- extract_tables(paste0(base_url, url))
    for (i in 1:length(out)) {
      if (str_detect(out[[i]][,1][1], "Tabla 4|^Información de la temporada de influenza (inter)?(inter )?estacional"))
        break
    }
    if (url == "/cms/uploads/attachment/file/467905/INFLUENZA_SE23_2019.pdf") {
      i <- 6
    }
    if (url == "/cms/uploads/attachment/file/464412/INFLUENZA_SE21_2019.pdf") {
      i <- 5
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
    df$season <- "2018-2019"
    casos19_3 <- rbind(casos19_3, df)
  }
  write.csv(casos19_3, "cache/casos19_3.csv", row.names = FALSE)
} else
  casos19_3 <- read.csv("cache/casos19_3.csv")


casos19_3 <- casos19_3 %>%
  arrange(week, states) %>%
  group_by(states) %>%
  mutate(diff = diff(c(NA, ETI_IRAG)))
