#******************************************************************Script - 1**************************************************************************************************************
#Downloaded all the records from the CTRI registry

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
new_function <- function(a) {
  
  if (length(a) == 0) {
    a <- "NA"
  } else if (a == "") {
    a <- "NA"
  } else {
    return(a)
  }
}

#-- Step1: Downloading all the CTRI Webpages in HTML.
ids = c(1:160000)
for (i in seq_along(ids)) {
  myurl = paste0("http://ctri.nic.in/Clinicaltrials/pmaindet2.php?trialid=", ids[i]) 
  url = url(paste0("http://ctri.nic.in/Clinicaltrials/pmaindet2.php?trialid=",ids[i]))
  ctri_page = read_html(url)
  keyword = ctri_page %>% html_nodes("td") %>% html_text() %>% str_extract("Invalid Request")
  keyword = toString(keyword)
  ##This is done because there are many records which have their particular links, but no content is present in those records. Instead it is displayed as "Invalid Request".
  if (keyword != "Invalid Request") { 
    myfile = paste0("ctri_page",ids[i],".html")
    download.file(myurl, destfile = myfile, quiet = TRUE)
    time_of_download = as.character(timestamp())
    reg_num <- ctri_page %>% html_nodes("td tr:nth-child(1) td+ td > b") %>% html_text() %>% str_squish() %>% str_trim()
    reg_num <- toString(reg_num)
    reg_num <- new_function(reg_num)   
    time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                            downloaded_time = time_of_download,
                            URL = as.character(myurl),reg_num)
    
    write.table(time_stamp, "time_stamp_ctri.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_ctri.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
  }
  else {
    
    file <- data.frame(Trial_ID=as.character(ids[i]),URL=as.character(myurl))
    write.table(file, "time_stamp_ctri_invalid.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_ctri_invalid.csv"), append = T)
    
    print(paste("Count=",counter,"ID = ",ids[i],"but page is invalid one"))
  }
}



#**************************************************************************Script - 2*******************************************************************************************
#Web-scraped all the downloaded records for the field 'Type of study' and 'Type of trial'

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:84639)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("ctri_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    ctri_page <- read_html(myfile)
    
    if(is_empty(ctri_page)) {
      
      next 
    }
    
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "NA"
      } else if (a == "") {
        a <- "NA"
      } else {
        return(a)
      }
    }
    
    
    Trial_ID <- ids[i]
    Registration_number <- ctri_page %>% html_nodes("td tr:nth-child(1) td+ td > b") %>% html_text() %>% str_remove_all('\"') %>% toString() %>% str_trim() %>% str_squish()
    Registration_number <- new_function(Registration_number)
    Date_of_registration <- ctri_page %>% html_nodes("td+ td") %>% .[[1]] %>% html_text() %>% str_remove_all('\"') %>% toString() %>% str_trim() %>% str_squish()
    Date_of_registration <- new_function(Date_of_registration)
    Last_modified_on <- ctri_page %>% html_nodes("td+ td") %>% .[[2]] %>%  html_text() %>% str_remove_all('\"')  %>% toString() %>% str_trim() %>% str_squish()
    Last_modified_on <- new_function(Last_modified_on)
    Public_title_of_study <- ctri_page %>% html_nodes("td+ td") %>% .[[7]] %>% html_text() %>% str_remove_all('\"') %>% toString() %>% str_trim() %>% str_squish()
    Public_title_of_study <- new_function(Public_title_of_study)
    Scientific_title_of_study <- ctri_page %>% html_nodes("td+ td") %>% .[[8]] %>% html_text() %>% str_remove_all('\"') %>% toString() %>% str_trim() %>% str_squish()
    Scientific_title_of_study <- new_function(Scientific_title_of_study)
    Type_of_trial <- ctri_page %>% html_nodes("td+ td") %>% .[[4]] %>%  html_text() %>% str_remove_all('\"') %>% toString() %>% str_trim() %>% str_squish()
    Type_of_trial <- new_function(Type_of_trial)
    Type_of_study <- ctri_page %>% html_nodes("td+ td") %>% .[[5]] %>%  html_text() %>% str_remove_all('\"') %>% toString() %>% str_trim() %>% str_squish()
    Type_of_study <- new_function(Type_of_study)
 
    
    
    file <- data.frame(Trial_ID,Registration_number,Date_of_registration,Last_modified_on,Public_title_of_study,Scientific_title_of_study,Type_of_trial,Type_of_study)
    
    write.table(file,"RB_scrape_type_of_trial_type_study.csv", sep = ",",row.names = FALSE, col.names = !file.exists("RB_scrape_type_of_trial_type_study.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}



#**************************************************************************Script - 3****************************************************************************************
#Web-scraped for the field 'Post Graduate Thesis' and 'Countries of recruitment' from the list of filtered records.

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- data.frame()
ids <- read.csv("18830_ids.csv")
colnames(ids) <- "ids"
counter=0

for (row in 1:nrow(ids)) {
  i <- ids[row,1]
  myfile = paste0("ctri_page",i,".html")
  
  if (file.exists(myfile)) {
    
    ctri_page <- read_html(myfile)
    
    if(is_empty(ctri_page)) {
      
      next 
    } 
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "NA"
      } else if (a == "") {
        a <- "NA"
      } else {
        return(a)
      }
    }
    
    
    
    Trial_ID <- i
    Registration_number <- ctri_page %>% html_nodes("td tr:nth-child(1) td+ td > b") %>% html_text() %>% str_remove_all('\"') %>% toString() %>% str_trim() %>% str_squish()
    Registration_number <- new_function(Registration_number)
    Post_graduate_thesis <- ctri_page %>% html_nodes("td+ td") %>% .[[3]] %>%  html_text() %>% str_remove_all('\"')  %>% toString() %>% str_trim() %>% str_squish()
    Post_graduate_thesis <- new_function(Post_graduate_thesis)
    for (num in 1:5186){ 
      COR_label1 <- ctri_page %>% html_nodes("td") %>% .[[num]] %>%  html_text() %>% str_remove_all('\"') %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% toString() %>% str_trim() %>% str_squish()
      if ((COR_label1 == "Countries of Recruitment") || (COR_label1 == "Countries of Recruitment Modification(s)"))
      {
        Countries_of_recruitment <- ctri_page %>% html_nodes("td") %>% .[[num+1]] %>%  html_text() %>% str_remove_all('\"') %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString() %>% str_trim() %>% str_squish()
        COR_label <- ctri_page %>% html_nodes("td") %>% .[[num]] %>%  html_text() %>% str_remove_all('\"') %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% toString() %>% str_trim() %>% str_squish()
        selector_number <- num
        break
      } else {
        Countries_of_recruitment <- "Not a country name"
        COR_label <- "Not a country label"
        selector_number <- "Not within limits"
      }
      
      
    }
    
    
    file <- data.frame(Trial_ID,Registration_number,Post_graduate_thesis,Countries_of_recruitment,COR_label,selector_number)
    
    write.table(file,"RB_scrape_PG_COUNTRIES.csv", sep = ",",row.names = FALSE, col.names = !file.exists("RB_scrape_PG_COUNTRIES.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",i))
     }
}
#***********************************************************************Script - 4**********************************************************************************************************
#Web scraped for the field 'Details of Principal Investigator or overall Trial Coordinator (multi-center study)' and ‘Sites of study’ from the list of filtered records

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- data.frame()
ids <- read.csv("7516_ids.csv")
colnames(ids) <- "ids"
counter=0

for (row in 1:nrow(ids)) {
  i <- ids[row,1]
  myfile = paste0("ctri_page",i,".html")
  
  if (file.exists(myfile)) {
    
    ctri_page <- read_html(myfile)
    
    if(is_empty(ctri_page)) {
      
      next 
    } 
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "NA"
      } else if (a == "") {
        a <- "NA"
      } else {
        return(a)
      }
    }
    
    
    var <- "delimiter"
    Trial_ID <- i
    Registration_number <- ctri_page %>% html_nodes("td tr:nth-child(1) td+ td > b") %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
    Registration_number <- new_function(Registration_number)
    PI_Name <- ctri_page %>% html_nodes("tr:nth-child(11) tr:nth-child(1) td+ td") %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
    PI_Name <- new_function(PI_Name)
    PI_Designation <- ctri_page %>% html_nodes("tr:nth-child(11) tr:nth-child(2) td+ td") %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
    PI_Designation <- new_function(PI_Designation)
    PI_Affiliation <- ctri_page %>% html_nodes("tr:nth-child(11) tr:nth-child(3) td+ td") %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
    PI_Affiliation <- new_function(PI_Affiliation)
    PI_Address <- ctri_page %>% html_nodes("tr:nth-child(11) tr:nth-child(4) td+ td") %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
    PI_Address <- new_function(PI_Address)
    PI_Phone <- ctri_page %>% html_nodes("tr:nth-child(11) tr:nth-child(5) td+ td") %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
    PI_Phone <- new_function(PI_Phone)
    PI_Phone <- paste0(var,PI_Phone)
    PI_Fax <- ctri_page %>% html_nodes("tr:nth-child(11) tr:nth-child(6) td+ td") %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
    PI_Fax <- new_function(PI_Fax)
    PI_Fax <- paste0(var,PI_Fax)
    PI_Email <- ctri_page %>% html_nodes("tr:nth-child(11) tr:nth-child(7) td+ td") %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
    PI_Email <- new_function(PI_Email)
    num2=1
    for (num1 in 3:96){
      Site_PI_Name <- ctri_page %>% html_nodes(paste0("tr:nth-child(18) tr:nth-child(",num1,") td:nth-child(",num2,")")) %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
      Site_PI_Name <- new_function(Site_PI_Name)
      Site_Name <- ctri_page %>% html_nodes(paste0("tr:nth-child(18) tr:nth-child(",num1,") td:nth-child(",num2+1,")")) %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
      Site_Name <- new_function(Site_Name)
      Site_Address <- ctri_page %>% html_nodes(paste0("tr:nth-child(18) tr:nth-child(",num1,") td:nth-child(",num2+2,")")) %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
      Site_Address <- new_function(Site_Address)
      Phone_tax_email <- ctri_page %>% html_nodes(paste0("tr:nth-child(18) tr:nth-child(",num1,") td:nth-child(",num2+3,")")) %>% html_text() %>% str_remove_all('\"') %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% toString() %>% str_trim() %>% str_squish()
      Phone_tax_email <- new_function(Phone_tax_email)
      
      
      file <- data.frame(Trial_ID,Registration_number,PI_Name, PI_Designation, PI_Affiliation,PI_Address, PI_Phone, PI_Fax, PI_Email,Site_PI_Name,Site_Name,Site_Address,Phone_tax_email)
      
      write.table(file,"RB_scrape_PI_Site_information.csv", sep = ",",row.names = FALSE, col.names = !file.exists("RB_scrape_PI_Site_information.csv"), append = T)
      
      counter = counter + 1
      print(paste("Count = ", counter,"ID = ",i))
      
    }
    
    
  }
}





