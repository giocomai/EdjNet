library("tidyverse")

allFilmsDF <- readRDS(file = "allFilmsDF.rds")
EuropeanCountries <- allFilmsDF %>% filter(Market!="EUR EU", Market!="EUR OBS(36)", Market!="US", Market!="QC") %>% select(Market) %>% distinct() %>% arrange(Market) %>% pull(Market)

temp <- allFilmsDF %>%
  filter(ProductionYear>2011) %>% 
  filter(str_detect(Year, "Total")) %>% 
  filter(str_detect(string = Market, pattern = stringr::regex(pattern = paste(EuropeanCountries, collapse = "|"))))

byMarketByType <- temp %>% mutate(Type = case_when(
  str_detect(string = ProducingCountry, pattern = Market) ~ "Domestic films",
  str_detect(string = ProducingCountry, pattern = "US")&str_detect(string = ProducingCountry, pattern = paste(EuropeanCountries, collapse = "|")) ~ "US/EU co-productions",
  str_detect(string = ProducingCountry, pattern = "US") ~ "American films", 
  str_detect(string = ProducingCountry, pattern = stringr::regex(pattern = paste(EuropeanCountries, collapse = "|"))) ~ "European films", 
  TRUE ~ "Other"
)) %>% 
  group_by(Market, Type) %>% 
  summarise(TotalAdmissions=sum(as.numeric(Admissions), na.rm = TRUE)) %>% 
  mutate(Share=TotalAdmissions/sum(TotalAdmissions)*100)

## with labels
#https://bl.ocks.org/vasturiano/12da9071095fbd4df434e60d52d2d58d

ZoomList <- vector("list", length = length(EuropeanCountries))

for (i in seq_along(EuropeanCountries)) {
  ZoomList[[i]]$name <- EuropeanCountries[i]
  ZoomList[[i]]$children <- list(list(name="Domestic films", size= byMarketByType$TotalAdmissions[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="Domestic films"]),
                                 list(name="European films", size = byMarketByType$TotalAdmissions[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="European films"]),
                                 list(name="US/EU co-productions", size = byMarketByType$TotalAdmissions[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="US/EU co-productions"]),
                                 list(name="American films", size = byMarketByType$TotalAdmissions[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="American films"]),
                                 list(name="Other", size = byMarketByType$TotalAdmissions[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="Other"]))
}


ZoomList


write(jsonlite::toJSON(x = list(name = "All 5.6 billion cinema tickets sold in Europe (2011-2016)", children = ZoomList),
                       pretty = TRUE, auto_unbox = TRUE), file = file.path("ZoomLabels", "data.json"))



#### proportional ####

dir.create(path = "ZoomLabelsShare", showWarnings = FALSE)


allFilmsDF %>%
  filter(ProductionYear>2011) %>% 
  filter(str_detect(Year, "Total")) %>% 
  filter(str_detect(string = Market, pattern = stringr::regex(pattern = paste(EuropeanCountries, collapse = "|"))))


ZoomList <- vector("list", length = length(EuropeanCountries))

for (i in seq_along(EuropeanCountries)) {
  ZoomList[[i]]$name <- EuropeanCountries[i]
  ZoomList[[i]]$children <- list(list(name=paste0("Domestic (", round(byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="Domestic films"]), "%)"), size= byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="Domestic films"]),
                                 list(name=paste0("European (", round(byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="European films"]), "%)"), size = byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="European films"]),
                                 list(name=paste0("US/EU (", round(byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="US/EU co-productions"]), "%)"), size = byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="US/EU co-productions"]),
                                 list(name=paste0("American (", round(byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="American films"]), "%)"), size = byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="American films"]),
                                 list(name=paste0("Other (", round(byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="Other"]), "%)"), size = byMarketByType$Share[byMarketByType$Market==EuropeanCountries[i]&byMarketByType$Type=="Other"]))
}


ZoomList


write(jsonlite::toJSON(x = list(name = "100% of cinema tickets sold in Europe (2011-2016)", children = ZoomList),
                       pretty = TRUE, auto_unbox = TRUE), file = file.path("ZoomLabelsShare", "data.json"))



#### detailed ####

byFilmByType <- allFilmsDF %>%
  filter(ProductionYear==2016) %>% 
  filter(str_detect(Year, "Total")) %>% 
  filter(str_detect(string = Market, pattern = stringr::regex(pattern = paste(EuropeanCountries, collapse = "|")))) %>% select(Title, Market, ProducingCountry, Admissions) %>% 
  mutate(Type = case_when(
    str_detect(string = ProducingCountry, pattern = Market) ~ "Domestic films",
    str_detect(string = ProducingCountry, pattern = "US")&str_detect(string = ProducingCountry, pattern = paste(EuropeanCountries, collapse = "|")) ~ "US/EU co-productions",
    str_detect(string = ProducingCountry, pattern = "US") ~ "American films", 
    str_detect(string = ProducingCountry, pattern = stringr::regex(pattern = paste(EuropeanCountries, collapse = "|"))) ~ "European films", 
    TRUE ~ "Other"
  )) 

isoDF <- readRDS(file = file.path("isoDF.rds"))

EuropeanCountriesFullName <- data_frame(ISO.code = allFilmsDF %>% filter(Market!="EUR EU", Market!="EUR OBS(36)", Market!="US", Market!="QC") %>% select(Market) %>% distinct() %>% arrange(Market) %>% pull(Market)) %>% left_join(isoDF, by = "ISO.code") %>% pull(Country) %>% str_remove(pattern = "Former Yugoslav Republic of ") %>% str_remove(pattern = fixed(" (from June 2006)"))

ZoomList <- vector("list", length = length(EuropeanCountries))

for (i in seq_along(EuropeanCountries)) {
  ZoomList[[i]]$name <- EuropeanCountriesFullName[i]
  ZoomList[[i]]$children <- list(list(name="Domestic"),
                                 list(name="European"),
                                 list(name="US/EU"),
                                 list(name="American"),
                                 list(name="Other"))

  filmTypes <- c("Domestic films", "European films", "US/EU co-productions", "American films", "Other")
  
  for (j in seq_along(filmTypes)) {
    MarketSubset <- byFilmByType %>%
      filter(Market == EuropeanCountries[i], Type == filmTypes[j]) %>% 
      arrange(desc(Admissions))
    
    MarketSubsetList <- vector("list", length = length(MarketSubset$Title))
    for (k in seq_along(MarketSubset$Title)) {
      MarketSubsetList[[k]] <- list(name = MarketSubset$Title[k],
                                    size = MarketSubset$Admissions[k])
    }
    
    ZoomList[[i]]$children[[j]] <- list(name = filmTypes[j], 
                                        children = MarketSubsetList)
     
  }
  
}


ZoomList


write(jsonlite::toJSON(x = list(name = "One billion cinema tickets sold in Europe in 2016", children = ZoomList),
                       pretty = TRUE, auto_unbox = TRUE), file = file.path("barchart", "readme.json"))


#sum(byFilmByType$Admissions)
