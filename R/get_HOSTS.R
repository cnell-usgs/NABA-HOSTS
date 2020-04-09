
#### pull records from HOSTS database

library(tidyverse)
library(rvest)
library(beepr)

## NABA species list
naba.sp<-read.csv('/Users/collnell/Dropbox/Projects/NABA/NABA-HOSTS/data/NABA.butterflyspecies.csv')

# access HOSTS ------------------------------------------------------------

## searching a genus - build address for genus + species query
http.genus<-'https://www.nhm.ac.uk/our-science/data/hostplants/search/list.dsml?searchPageURL=index.dsml&Familyqtype=starts+with&Family=&PFamilyqtype=starts+with&PFamily=&Genusqtype=equals&Genus='
http.genus.sp<-'&PGenusqtype=starts+with&PGenus=&Speciesqtype=starts+with&Species='
http.sp.end<-'&PSpeciesqtype=starts+with&PSpecies=&Country=&sort=Family'

naba<-naba.sp%>%
  mutate(http = paste0(http.genus, Genus, http.genus.sp, Species, http.sp.end))

## input columns names for dataframe output
host.cols<-c('LEP_family','LEP_species','HOST_family','HOST_species','HOST_country')

host.records<-read_html(naba[3,'http'])%>%
  html_nodes('.dataTable_ms td')%>%
  html_text()%>%
  matrix(ncol = 5, byrow = TRUE)%>%
  data.frame()%>%
  setNames(host.cols)
host.records

## determine total number of records for each species
count.recs<-read_html(naba[3,'http'])%>%
  html_nodes('.RecSetLoc')%>%
  html_text()%>%
  str_extract('Records 1 - (.+) of (.+)')%>%
  word(6)
count.recs
## do this to all species, if the search returns nothing then move on

## the net step follows this procedure to apply to all species

# get_HOSTS function ------------------------------------------------------

## function to detect whether xml nodeset is empty or not
is_empty <- function(x) if(length(x) == 0) TRUE else FALSE

read_html(http)%>%
  html_nodes('td')%>%
  html_text()
host.records

## process htmml for pages 2+
## recs is the number starting at it
html_to_data<-function(name.genus, name.sp, recs){
  page.return<-page.3%>%
    str_replace_all('60', recs)%>%
    str_replace_all('Agraulis', name.genus)%>%
    str_replace_all('vanillae',name.sp)%>%
    read_html()%>%
    html_nodes('.dataTable_ms td')%>%
    html_text()%>%
    matrix(ncol = 5, byrow = TRUE)%>%
    data.frame()%>%
    setNames(host.cols)
  return(page.return)

}

html_to_data('Papilio','glaucus', '120')

get_HOSTS<-function(http, name.genus, name.sp){
  
  host.records<-read_html(http)%>%
    html_nodes('.dataTable_ms td')
  
  ## how many records total?
  count.recs<-read_html(http)%>%
    html_nodes('.RecSetLoc')%>%
    html_text()%>%
    str_extract('Records 1 - (.+) of (.+)')%>%
    word(6)%>%as.numeric()
  
  
  if (is_empty(host.records) == TRUE){  ## what to do if there are no records for the species
    
    # return empty row with species name
    return(data.frame(LEP_family = NA, LEP_species = paste(name.genus, name.sp), HOST_family = NA, HOST_species = NA, HOST_country = NA))
    
  }else{
    
    ## pull records from first page
    host.records<-http%>%
      read_html()%>%
      html_nodes('.dataTable_ms td')%>%
      html_text()%>%
      matrix(ncol = 5, byrow = TRUE)%>%
      data.frame()%>%
      setNames(host.cols);beep()
    
    if (count.recs > 30){ ## what to do if there are multiple pages of records
      
      ## replace genus and species names and read in html
      host.page.2<-html_to_data(name.genus, name.sp, recs='30')
      host.records<-rbind(host.records, host.page.2)
  
      ## now jsut repeating this for multiple pages
      # would be better to iterate as many times for a given species as needed 
      
      if(count.recs > 60){
        host.page.3<-html_to_data(name.genus, name.sp, recs='60')
        host.records<-rbind(host.records, host.page.3)
        
        if(count.recs >90){
          host.page.4<-html_to_data(name.genus, name.sp, recs='90')
          host.records<-rbind(host.records, host.page.4)
          
          if(count.recs > 120){
            host.page.5<-html_to_data(name.genus, name.sp, recs='120')
            host.records<-rbind(host.records, host.page.5)
          }
        }
      }
      return(host.records)
    }else { ## what to do to read in multiple pages
      return(host.records)
    }
  }
}

## test her out

get_HOSTS(http=naba[444,'http'], name.genus='Papilio',name.sp='glaucus') # has 122 records


## works for species that are in and not


# search HOSTS of NABA species --------------------------------------------
str(naba)

host.output<-lapply(1:length(naba$http), function(x)get_HOSTS(http=naba[x, 'http'], name.genus=naba[x, 'Genus'], name.sp=naba[x, 'Species']))
str(host.output)

## what to return if there are several pages??

host.df<-host.output%>%
  bind_rows()
str(host.df)  #4957 rows

write.csv(host.df, )

# explore data ------------------------------------------------------------

# number of host records per species
record.df<-host.df%>%
  mutate(group = ifelse(is.na(HOST_species), 'no data', 'hosts'))%>%
  group_by(LEP_species, group)%>%
  summarize(rows = length(HOST_species))%>%
  mutate(rows = ifelse(group == 'no data', 0, rows))

record.df%>%filter(rows == 0)
## 210 without any host records
# but could some be due name changes? these might be worth searching further in ITIS

record.df%>%filter(rows == 30)%>%view
## 2 species that have more than 30 rows. ie. went onto 2 pages, need to search second page

## each page had up to 30 rows

## sample addresses from species
name.genus<-'Ascia'
name.sp<-'monuste'
page.1<-'https://www.nhm.ac.uk/our-science/data/hostplants/search/list.dsml?searchPageURL=index.dsml&Familyqtype=starts+with&Family=&PFamilyqtype=starts+with&PFamily=&Genusqtype=equals&Genus=Ascia&PGenusqtype=starts+with&PGenus=&Speciesqtype=equals&Species=monuste&PSpeciesqtype=starts+with&PSpecies=&Country=&sort=Family'
page.3<-'https://www.nhm.ac.uk/our-science/data/hostplants/search/list.dsml?PSpeciesqtype=starts+with&PFamilyqtype=starts+with&Species=vanillae&sort=Family&Familyqtype=starts+with&beginIndex=60&Speciesqtype=contains&Genus=Agraulis&Genusqtype=starts+with&PGenusqtype=starts+with&searchPageURL=index%2edsml%3fPSpeciesqtype%3dstarts%2bwith%26PFamilyqtype%3dstarts%2bwith%26Species%3dvanillae%26sort%3dFamily%26Familyqtype%3dstarts%2bwith%26Speciesqtype%3dcontains%26Genus%3dAgraulis%26Genusqtype%3dstarts%2bwith%26PGenusqtype%3dstarts%2bwith'

## replace model species with search species name
name.genus.new<-'Agraulis'
name.sp.new<-'vanillae'

## replace genus and species names and read in html
host.page.2<-read_html(str_replace_all(str_replace_all(page.2, 'Ascia', name.genus.new),'monuste', name.sp.new))%>%
  html_nodes('.dataTable_ms td')%>%
  html_text()%>%
  matrix(ncol = 5, byrow = TRUE)%>%
  data.frame()%>%
  setNames(host.cols)
host.page.2

host.page.3<-page.3%>%
  str_replace_all('Agraulis',name.genus)%>%
  str_replace_all('vanillae',name.sp)%>%
  read_html()%>%
  html_nodes('.dataTable_ms td')%>%
  html_text()%>%
  matrix(ncol = 5, byrow = TRUE)%>%
  data.frame()%>%
  setNames(host.cols)


#read_html(str_replace_all(str_replace_all))

host.page.4<-gsub('60', '90', page.3)%>%
  str_replace_all('Agraulis',name.genus)%>%
  str_replace_all('vanillae',name.sp)%>%
  read_html()%>%
  html_nodes('.dataTable_ms td')%>%
  html_text()%>%
  matrix(ncol = 5, byrow = TRUE)%>%
  data.frame()%>%
  setNames(host.cols)
host.page.4

page.3%>%
  str_replace_all('60', '30')%>%
  str_replace_all('Agraulis','Papilio')%>%
  str_replace_all('vanillae','glaucus')%>%
  read_html()%>%
  html_nodes('.dataTable_ms td')%>%
  html_text()%>%
  matrix(ncol = 5, byrow = TRUE)%>%
  data.frame()%>%
  setNames(host.cols)
