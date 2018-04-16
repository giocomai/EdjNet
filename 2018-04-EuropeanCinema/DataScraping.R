if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
pacman::p_load("rvest")
devtools::install_github(repo = "giocomai/castarter")
library("castarter")

SetCastarter(project = "film", website = "lumiere")
CreateFolders()

links <- c(paste0("http://lumiere.obs.coe.int/web/films/index.php?letter=", LETTERS), "http://lumiere.obs.coe.int/web/films/index.php?letter=[0-9]")

filenames <- c(paste0(LETTERS, ".html"), "numbers.html")

dir.create("filmlist", showWarnings = FALSE)
for (i in seq_along(links)) {
  if (file.exists(file.path("filmlist", filenames[i]))==FALSE) {
    download.file(url = links[i], destfile = file.path("filmlist", filenames[i]))
  }
}

if (file.exists("filmLinks.rds") == FALSE) {
  #RestoreArchives(html = TRUE, indexHtml = TRUE, overwrite = TRUE)
  filmLinks <- ExtractLinks(htmlLocation = "filmlist", partOfLink = "film_info", domain = "http://lumiere.obs.coe.int/")
  saveRDS(filmLinks, "filmLinks.rds")
  #ArchiveFolders(removeArchivedFolders = TRUE)
} else {
  filmLinks <- readRDS(file = "filmLinks.rds")
}


DownloadContents(links = filmLinks)
DownloadContents(links = filmLinks, missingPages = FALSE)


htmlLocation <- file.path("film", "lumiere", "Html") 

HtmlFiles <- list.files(path = htmlLocation, full.names = TRUE)
HtmlFiles <- HtmlFiles[stringr::str_extract(string = HtmlFiles, 
                                            pattern = "[[:digit:]]+[[:punct:]]html") %>% 
                         stringr::str_sub(start = 1L, end = -6L) %>% 
                         as.integer() %>% order()]

                         
## Extract data

allFilms <- as.list(names(filmLinks))

if (file.exists("allFilms.rds")==FALSE) {
  for (i in seq_along(allFilms)) {
    temp <- tryCatch(expr = read_html(x = HtmlFiles[i]) %>% html_nodes("table") %>% .[6] %>% html_table(fill = TRUE) %>% .[[1]]
                              , error = function(e) {
                                warning(paste("Could not read", HtmlFiles[i]))
                                NA
                              })
    
    if (is.data.frame(temp)==TRUE) {
          temp2 <- tryCatch(expr = read_html(x = HtmlFiles[i]) %>% html_nodes("table") %>% .[4] %>% html_table(fill = TRUE) %>% .[[1]]
                              , error = function(e) {
                                warning(paste("Could not read", HtmlFiles[i]))
                                NA
                              })
          
          if (stringr::str_count(string = temp2[1,ncol(temp2)], pattern = stringr::fixed("[IMDb Info]"))>1) {
                        tempDirector <- stringr::str_replace(string = stringr::str_replace_all(string = temp2[1,ncol(temp2)], pattern = stringr::fixed("  [IMDb Info]"), replacement = ", "), pattern = fixed("Directors : "), replacement = "")
          } else {
            tempDirector <- stringr::str_replace(string = stringr::str_replace(string = temp2[1,ncol(temp2)], pattern = stringr::fixed("  [IMDb Info]"), replacement = ""), pattern = fixed("Directors : "), replacement = "")
          }
          
            
            
          tempProducingCountry <- stringr::str_replace(string =temp2[which(stringr::str_detect(string = temp2[,1], pattern = stringr::fixed("Producing or Co-producing country : "))),1], pattern = fixed(pattern = "Producing or Co-producing country : "), replacement = "")
            
            
          
          
          tempProductionYear <- stringr::str_replace(string =temp2[nrow(temp2),1], pattern = fixed(pattern = "Production year : "), replacement = "")
            
          
         temp <- temp %>% mutate(Director =  tempDirector, 
                                     ProducingCountry = tempProducingCountry, 
                                     ProductionYear = tempProductionYear)
      
    }
    allFilms[[i]] <- temp
  }
  saveRDS(object = allFilms, file = "allFilms.rds")
} else {
  allFilms <- readRDS(file = "allFilms.rds")
}

## Convert to data frame

if (file.exists("allFilmsLong.rds")==FALSE) {
  allFilmsLong <- as.list(names(filmLinks))
for (i in seq_along(allFilms)[is.na(allFilms)==FALSE]) {
  if (is.element(el = "Distributor", set = colnames(allFilms[[i]]))==TRUE&is.element(el = "Release date", set = colnames(allFilms[[i]]))==TRUE) {
    allFilmsLong[[i]] <- allFilms[[i]] %>% gather(Year, Admissions, 4:(ncol(allFilms[[i]])-3)) %>% mutate(ID = i, Title = names(filmLinks)[i], Admissions = as.character(Admissions), Link = filmLinks[i]) %>% select(ID, Title, Market, Distributor, `Release date`, Director, ProducingCountry, ProductionYear, Year, Admissions, Link)
  } else if (is.element(el = "Distributor", set = colnames(allFilms[[i]]))==TRUE) {
    allFilmsLong[[i]] <- allFilms[[i]] %>% gather(Year, Admissions, 3:(ncol(allFilms[[i]])-3)) %>% mutate(ID = i, Title = names(filmLinks)[i], `Release date` = NA, Admissions = as.character(Admissions), Link = filmLinks[i]) %>% select(ID, Title, Market, Distributor, `Release date`, Director, ProducingCountry, ProductionYear, Year, Admissions, Link)
  } else {
    allFilmsLong[[i]] <- allFilms[[i]] %>% gather(Year, Admissions, 2:(ncol(allFilms[[i]])-3)) %>% mutate(ID = i, Title = names(filmLinks)[i]) %>% mutate(Distributor = NA, `Release date` = NA, Admissions = as.character(Admissions), Link = filmLinks[i]) %>% select(ID, Title, Market, Distributor, `Release date`, Director, ProducingCountry, ProductionYear, Year, Admissions, Link)
  }
}
  names(allFilmsLong) <- names(filmLinks)
  saveRDS(object = allFilmsLong, file = "allFilmsLong.rds")
} else {
  allFilmsLong <- readRDS(file = "allFilmsLong.rds")
}



if (file.exists("allFilmsDF.rds")==FALSE) {
  allFilmsDF <- bind_rows(x = allFilmsLong[is.na(allFilms)==FALSE]) %>%
    mutate(Admissions = as.integer(gsub(pattern = " ", replacement = "", x = Admissions)))
  
  saveRDS(object = allFilmsDF, file = "allFilmsDF.rds")
  write_csv(x = allFilmsDF, path = "allFilmsDF.csv")
} else {
  allFilmsDF <- readRDS(file = "allFilmsDF.rds")
}