get_IFGF <- function(year = NULL, city = NULL, uf = NULL, region = NULL, indicator = NULL, ranking = FALSE) {
  
  options(encoding = "UTF-8")
  
  check.packages <- function(pkg) {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  pacotes <- c("rvest", "dplyr", "openxlsx", "stringr", "tidyr")
  check.packages(pacotes)
  
  url <- "https://www.firjan.com.br/ifgf/analises-e-rankings/"
  
  href <- read_html(url) %>%
    html_nodes(xpath = '//*[@id="Form_4028818B3E0E2559013E0EAEE37D2A9D"]/section/div/div/div[2]/div[1]/a') %>%
    html_attr("href")
  
  url_file <- paste0("https://www.firjan.com.br", substr(href, 6, nchar(href)))
  
  last_edition <- str_extract(href, "(?<=_)[^_]+(?=\\.xlsx)")
  
  file_name <- paste0("data ifgf ", last_edition, ".rds")
  
  if (!file.exists(file.path(tempdir(), file_name))) {
    
    destfile <- file.path(tempdir(), paste0("ifgf_", last_edition, ".xlsx"))
    curl_download(url_file, destfile = destfile)    
    
    sheets <- getSheetNames(destfile)
    
    list_of_dfs <- list()
    
    for (i in 1:length(sheets)) {
      
      file_ifgf <- read.xlsx(destfile, sheet = sheets[i])
      
      file_ifgf <- file_ifgf %>%
        pivot_longer(
          cols = 4:ncol(file_ifgf),
          names_to = "Variavel",
          values_to = "Valor"
        ) %>%
        mutate(Ano = as.numeric(str_extract(Variavel, ".{4}$")),
               Valor = suppressWarnings(as.numeric(Valor)),
               Variavel = case_when(
                 str_detect(tolower(Variavel), "estadual") ~ "Ranking Estadual",
                 str_detect(tolower(Variavel), "ranking.ifgf") ~ "Ranking Geral",
                 TRUE ~ "Índice"),
        Indicador = sheets[i],
        Sigla_Indicador = case_when(
          str_detect(tolower(Indicador), "gasto") ~ "GP",
          str_detect(tolower(Indicador), "auto") ~ "AT",
          str_detect(tolower(Indicador), "auto") ~ "IN",
          str_detect(tolower(Indicador), "auto") ~ "LI",
          TRUE ~ "IG")
        )
      
      list_of_dfs[[paste0(i)]] <- file_ifgf
    }
    
    reg <- data.frame(
      UF = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RN", "RS", 
             "RJ", "RO", "RR", "SC", "SP", "SE", "TO"),
      Região = c("N", "NE", "N", "N", "NE", "NE", "CO", "SE", "CO", "NE", 
                 "CO", "CO", "SE", "N", "NE", "S", "NE", "NE", "NE", "S", 
                 "SE", "N", "N", "S", "SE", "NE", "N")
    )
    
    data <- do.call(rbind, list_of_dfs) %>%
      left_join(reg, by = "UF")
    
    saveRDS(data, file.path(tempdir(), file_name))
  } else {
    data <- readRDS(file.path(tempdir(), file_name))
  }
  
  # Condições para filtros
  if (is.null(year)) { 
    year <- unique(data$Ano) 
  }
  if (is.null(region)) { 
    region <- unique(data$Região) 
  }
  if (is.null(city)) { 
    city <- unique(data$Município) 
  }
  if (is.null(indicator)) { 
    indicator <- unique(data$Sigla_Indicador) 
  }
  if (is.null(uf)) { 
    uf <- unique(data$UF) 
  }
  
if (ranking == FALSE){
  
  data <- data %>%
    filter(Ano %in% year, 
           Município %in% city, 
           Região %in% region, 
           UF %in% uf, 
           Sigla_Indicador %in% indicator,
           !(Variavel %in% c("Ranking Estadual", "Ranking Geral"))) %>%
    select(Ano, Código, Região, UF, Município, Indicador, Valor)

} else {
  
  data <- data %>%
    filter(Ano %in% year, 
           Município %in% city, 
           Região %in% region, 
           UF %in% uf, 
           Sigla_Indicador %in% indicator) %>%
    pivot_wider(names_from = "Variavel",
                 values_from = "Valor") %>%
    select(Ano, Código, Região, UF, Município, Indicador,`Ranking Estadual`, `Ranking Geral`, Índice) %>%
    rename(Valor = Índice)
  
}
  
  return(data)

}


dados <- get_IFGF(indicator = c("GP", "AT"))




