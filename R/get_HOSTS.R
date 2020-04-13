
#### pull records from HOSTS database

library(tidyverse)
library(rvest)
library(beepr)

## NABA species list
naba.sp<-read.csv('/Users/collnell/Dropbox/Projects/NABA/NABA-HOSTS/data/NABA.butterflyspecies.csv')

# access HOSTS ------------------------------------------------------------

## this is the web address for results form Agraulis vanillae, can use to search other species by replacing genus and species name
page.3<-'https://www.nhm.ac.uk/our-science/data/hostplants/search/list.dsml?PSpeciesqtype=starts+with&PFamilyqtype=starts+with&Species=vanillae&sort=Family&Familyqtype=starts+with&beginIndex=60&Speciesqtype=contains&Genus=Agraulis&Genusqtype=starts+with&PGenusqtype=starts+with&searchPageURL=index%2edsml%3fPSpeciesqtype%3dstarts%2bwith%26PFamilyqtype%3dstarts%2bwith%26Species%3dvanillae%26sort%3dFamily%26Familyqtype%3dstarts%2bwith%26Speciesqtype%3dcontains%26Genus%3dAgraulis%26Genusqtype%3dstarts%2bwith%26PGenusqtype%3dstarts%2bwith'

## searching a genus - build address for genus + species query
http.genus<-'https://www.nhm.ac.uk/our-science/data/hostplants/search/list.dsml?searchPageURL=index.dsml&Familyqtype=starts+with&Family=&PFamilyqtype=starts+with&PFamily=&Genusqtype=equals&Genus='
http.genus.sp<-'&PGenusqtype=starts+with&PGenus=&Speciesqtype=starts+with&Species='
http.sp.end<-'&PSpeciesqtype=starts+with&PSpecies=&Country=&sort=Family'

naba<-naba.sp%>%
  mutate(http = paste0(http.genus, Genus, http.genus.sp, Species, http.sp.end))

## input columns names for dataframe output
host.cols<-c('LEP_family','LEP_species','HOST_family','HOST_species','HOST_country')

## try parsing html for a single species
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

## first few functions are a part of the larger one to scrape all the host records

## function to detect whether xml nodeset is empty or not
is_empty <- function(x) if(length(x) == 0) TRUE else FALSE

## function to process htmml for pages 2+
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

## FUCNTION -- given an genus and species name, create http and pull records

get_HOSTS<-function(http, name.genus, name.sp){
  
  
  ## read in table
  host.records<-read_html(http)%>%
    html_nodes('.dataTable_ms td')
  
  ## collect data on number of records in database
  count.recs<-read_html(http)%>%
    html_nodes('.RecSetLoc')%>%
    html_text()%>%
    str_extract('Records 1 - (.+) of (.+)')%>%
    word(6)%>%as.numeric()%>%unique()
  
  # check for data in table
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
    print(paste(name.genus, name.sp))
    
    if (length(host.records$HOST_family) == 30){ ## what to do if there are multiple pages of records
      
      ## replace genus and species names and read in html
      host.page.2<-html_to_data(name.genus, name.sp, recs='30')
      host.records<-rbind(host.records, host.page.2)
      # would be better to iterate as many times for a given species as needed 
    } 
    if (length(host.records$HOST_family) == 60){
      host.page.3<-html_to_data(name.genus, name.sp, recs='60')
      host.records<-rbind(host.records, host.page.3)
      
    } 
    if (length(host.records$HOST_family) == 90){
      host.page.4<-html_to_data(name.genus, name.sp, recs='90')
      host.records<-rbind(host.records, host.page.4)
      
    } 
    if (length(host.records$HOST_family) == 120){
      host.page.5<-html_to_data(name.genus, name.sp, recs='120')
      host.records<-rbind(host.records, host.page.5)
      
    }
    if (length(host.records$HOST_family) == 150){
      host.page.6<-html_to_data(name.genus, name.sp, recs='150')
      host.records<-rbind(host.records, host.page.6)
      
    }
    if (length(host.records$HOST_family) == 180){
      host.page.7<-html_to_data(name.genus, name.sp, recs='180')
      host.records<-rbind(host.records, host.page.7)
      
    }
    if (length(host.records$HOST_family) == 210){
      host.page.8<-html_to_data(name.genus, name.sp, recs='210')
      host.records<-rbind(host.records, host.page.8)
      
    }
    if (length(host.records$HOST_family) == 240){
      host.page.9<-html_to_data(name.genus, name.sp, recs='240')
      host.records<-rbind(host.records, host.page.9)
      
    }
    if (length(host.records$HOST_family) == 270){
      host.page.10<-html_to_data(name.genus, name.sp, recs='270')
      host.records<-rbind(host.records, host.page.10)
      
    }
    if (length(host.records$HOST_family) == 300){
      host.page.11<-html_to_data(name.genus, name.sp, recs='300')
      host.records<-rbind(host.records, host.page.11)
      
    }
    if (length(host.records$HOST_family) == 330){
      host.page.12<-html_to_data(name.genus, name.sp, recs='330')
      host.records<-rbind(host.records, host.page.12)
      
    }
  }
  return(host.records)
}

## test her out
get_HOSTS(http=naba[635,'http'], name.genus='Vanessa',name.sp='cardui') # has 122 records

# search HOSTS of NABA species --------------------------------------------
str(naba)

host.output<-lapply(1:length(naba$http), function(x)get_HOSTS(http=naba[x, 'http'], name.genus=naba[x, 'Genus'], name.sp=naba[x, 'Species'])) # takes a few minutres
str(host.output, max.level=1)

## what to return if there are several pages??

host.df<-host.output%>%
  bind_rows()%>%
  rbind(get_HOSTS(http=naba[635,'http'], name.genus='Vanessa',name.sp='cardui'))%>%
  distinct(LEP_family, LEP_species, HOST_family, HOST_species, HOST_country)
str(host.df)  # 6328 rows

length(unique(host.df$LEP_species)) #648


write.csv(host.df, '/Users/collnell/Dropbox/Projects/NABA/NABA-HOSTS/data/NABA_hosts.csv', row.names=FALSE)

host.df<-read.csv('/Users/collnell/Dropbox/Projects/NABA/NABA-HOSTS/data/NABA_hosts.csv')

host.df%>%
  filter(is.na(HOST_species))%>%
  distinct(LEP_species)

# explore data ------------------------------------------------------------

# number of host records per species
record.df<-host.df%>%
  mutate(group = ifelse(is.na(HOST_species), 'no data', 'hosts'))%>%
  group_by(LEP_species, group)%>%
  summarize(rows = length(HOST_species))%>%
  mutate(rows = ifelse(group == 'no data', 0, rows))
record.df

## sample addresses from species
page.3<-'https://www.nhm.ac.uk/our-science/data/hostplants/search/list.dsml?PSpeciesqtype=starts+with&PFamilyqtype=starts+with&Species=vanillae&sort=Family&Familyqtype=starts+with&beginIndex=60&Speciesqtype=contains&Genus=Agraulis&Genusqtype=starts+with&PGenusqtype=starts+with&searchPageURL=index%2edsml%3fPSpeciesqtype%3dstarts%2bwith%26PFamilyqtype%3dstarts%2bwith%26Species%3dvanillae%26sort%3dFamily%26Familyqtype%3dstarts%2bwith%26Speciesqtype%3dcontains%26Genus%3dAgraulis%26Genusqtype%3dstarts%2bwith%26PGenusqtype%3dstarts%2bwith'

## plot it

## distribution of diet breadth
host.df%>%
  filter(!is.na(HOST_family))%>%
  group_by(LEP_family, LEP_species)%>%
  summarize(db_fam = length(unique(HOST_family)), db_sp = length(unique(HOST_species)), records = length(LEP_family))%>%
  group_by(LEP_family)%>%
  summarize(db_fam_mean=mean(db_fam), db_sp_mean = mean(db_sp), recs=sum(records),
            db_fam_se=se(db_fam), db_sp_se=se(db_sp))%>%filter(!is.na(LEP_family))%>%
  ggplot(aes(LEP_family, db_fam_mean))+
  geom_errorbar(aes(ymin=db_fam_mean-db_fam_se, ymax=db_fam_mean+db_fam_se), width=.1)+
  geom_point(size=3, shape=21, fill='white', stroke=1)+
  coord_flip()+labs(y='Host plant species richness', x='')
## something has over 30 families?
# most are around 1 
host.df%>%filter(LEP_family == 'Hesperiidae')

naba$HERB_sp<-paste(naba$Genus, naba$Species)
## compre to CFIS data
intersect(unique(traits$HERB_sp), unique(naba$HERB_sp))
?setdiff()

# Hesperiidae - Epargyreus clarus, Erynnis icelus
# Lycaenidae - Callophrys eryphon, Callophrys niphon, atyrium calanus, Satyrium caryaevorum, Satyrium liparops, 
# Nymphalidae - Asterocampa celtis, Limenitis arthemis, Limenitis archippus, Nymphalis antiopa, Polygonia comma, Polygonia faunus, Polygonia interrogationis, Polygonia saatyrus, Vanessa cardui
# Papilionidae - Papilio canadensis, Papilio eurymedon, Papilio rutulus
# Pieridae - Neophasia menapia


