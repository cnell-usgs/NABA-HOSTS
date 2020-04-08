
#### pull records from HOSTS database

library(tidyverse)
library(rvest)
library(beepr)

set_here('/Users/collnell/Dropbox/Projects/NABA')
here()

## NABA species list
naba.sp<-read.csv('/Users/collnell/Dropbox/Projects/NABA/data/NABA.butterflyspecies.csv')

# access HOSTS ------------------------------------------------------------

## input columns names
host.cols<-c('LEP_family','LEP_species','HOST_family','HOST_species','HOST_country')

## searching a genus - build address for genus + species query
http.genus<-'https://www.nhm.ac.uk/our-science/data/hostplants/search/list.dsml?searchPageURL=index.dsml&Familyqtype=starts+with&Family=&PFamilyqtype=starts+with&PFamily=&Genusqtype=equals&Genus='
http.genus.sp<-'&PGenusqtype=starts+with&PGenus=&Speciesqtype=starts+with&Species='
http.sp.end<-'&PSpeciesqtype=starts+with&PSpecies=&Country=&sort=Family'

naba<-naba.sp%>%
  mutate(http = paste0(http.genus, Genus, http.genus.sp, Species, http.sp.end))

host.records<-read_html(naba[3,'http'])%>%
  html_nodes('.dataTable_ms td')%>%
  html_text()%>%
  matrix(ncol = 5, byrow = TRUE)%>%
  data.frame()%>%
  setNames(host.cols)
host.records

## if the data frame is 16 rows, there are more pages..deal with later


## do this to all species, if the search returns nothing then move on

## the net step follows this procedure to apply to all species

# get_HOSTS function ------------------------------------------------------

## function to detect whether xml nodeset is empty or not
is_empty <- function(x) if(length(x) == 0) TRUE else FALSE

get_HOSTS<-function(http){
  host.records<-read_html(http)%>%
    html_nodes('.dataTable_ms td')
  
  str.a<-'https://www.nhm.ac.uk/our-science/data/hostplants/search/list.dsml\\?searchPageURL=index.dsml&Familyqtype=starts\\+with&Family=&PFamilyqtype=starts\\+with&PFamily=&Genusqtype=equals&Genus='
  str.b<-'&PGenusqtype=starts\\+with&PGenus=&Speciesqtype=starts\\+with&Species='
  str.c<-'&PSpeciesqtype=starts\\+with&PSpecies=&Country=&sort=Family'
  name.genus<-str_match(http, paste0(str.a, '(.*?)', str.b))[,2] ## extracts genus from between two strings
  name.sp<-str_match(http, paste0(str.b, '(.*?)', str.c))[,2]
  
  if (is_empty(host.records) == TRUE){
    
    # return empty row with species name
    return(data.frame(LEP_family = NA, LEP_species = paste(name.genus, name.sp), HOST_family = NA, HOST_species = NA, HOST_country = NA))
    
  }else{
    
    host.records<-read_html(http)%>%
      html_nodes('.dataTable_ms td')%>%
      html_text()%>%
      matrix(ncol = 5, byrow = TRUE)%>%
      data.frame()%>%
      setNames(host.cols);beep()
    return(host.records)
  }
}

## test her out

get_HOSTS(naba[1,'http'])
get_HOSTS(naba[3,'http'])

## works for species that are in and not


# search HOSTS of NABA species --------------------------------------------


host.output<-lapply(naba$http, get_HOSTS)
str(host.output)

## what to return if there are several pages??

host.df<-host.output%>%
  bind_rows()
str(host.df)  #4957 rows

write.csv(host.df, )

host.df%>%
  group_by(LEP_species)%>%
  summarize(rows = length(HOST_species) - 1)%>%
  filter(rows==0)
## 259 without any host records
# but could be due to differences in names? these might be worth searching further


