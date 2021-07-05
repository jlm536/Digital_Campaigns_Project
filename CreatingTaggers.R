source("/Users/eringubrod/Dropbox/Projects/Resources/Code/R/Josh_Core_Functions.R")
library(prediction)
data=read.csv("/Users/eringubrod/Dropbox/Projects/Publications/WebsitesWhere/Data/Using/Master-Final.csv")
library(ggeffects)
library(sf)


data=data%>%
  dplyr::select(-hou_chamber,-hou_dem,-hou_rep,-hou_majority,-sen_chamber,-sen_dem,-sen_rep,-sen_majority,-h_diffs,-s_diffs)%>%  
  dplyr::mutate(website.binary=ifelse(is.na(website),0,1))%>%
  dplyr::mutate(website.binary.neg=ifelse(is.na(website),1,0))#%>%
#dplyr::filter(party.flat!="O")

summary(data)

createRandString<- function() {
  digits = 0:9
  v = c(sample(digits, 4, replace = TRUE),
        sample(LETTERS, 3, replace = TRUE))
  v=sample(v)
  return(paste0(v,collapse = ""))
}

test_array=replicate(10, createRandString(), simplify=TRUE)

tagging_2018=data%>%
  dplyr::select(name,bp_url,website,year,FIPS,postal)%>%
  dplyr::filter(year==2018)%>%
  distinct()

tagging_2018$random=replicate(dim(tagging_2018)[1],createRandString(), simplify=TRUE)

head(tagging_2018)
write_csv(tagging_2018,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2018/Tagger_File_2018.csv")


############Creating 2018 Data Sets#################

head(data)
tagger=read.csv("/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2018/Master_Tagger_File_2018.csv")
head(tagger)
dim(tagger)
length(unique(tagger$assigned_ID))

data2=data%>%
  dplyr::filter(year==2018)%>%
  left_join(tagger)

candidate_tagger=data2%>%
  dplyr::select(candidate_ID,assigned_ID,year,name,first_name,last_name,bp_url)%>%
  distinct()

candidate_tagger=read.csv("/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2018/Candidate_Data/Website_Data/Candidate_Website_Information.csv")
candidate_tagger=candidate_tagger%>%distinct()
head(candidate_tagger)
write_csv(candidate_tagger,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2018/Candidate_Data/Website_Data/Candidate_Website_Information.csv")



candidate_district=data2%>%
  dplyr::select(candidate_ID,assigned_ID,year,state,postal,FIPS,chamber,long.fips,long.name,short.fips,)
write_csv(candidate_district,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2018/Candidate_District_Information.csv")

candidate_website=data2%>%
  dplyr::select(candidate_ID,assigned_ID,year,website,website.binary,party,incumbent,party.flat)
write_csv(candidate_website,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2018/Candidate_Website_Information.csv")

head(data2)
district_metadata=data2%>%
  dplyr::select(long.fips,long.name,short.fips,year,median.age,hs.plus,bachelors.plus,ag.percent.employ,mean.income,median.income,white.percent,papers.total,papers.daily,ktype_use,k.seats,party.presence,party.comp,general.presence,general.comp,population,pop_density)%>%
  distinct()

write.csv(district_metadata,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2018/District_Meta_Data.csv")

########################################2020################################################
head(tagger)
tagger_carry=tagger%>%
  dplyr::select(name,bp_url,assigned_ID)

tagging_2020=data%>%
  dplyr::select(name,bp_url,year,FIPS)%>%
  dplyr::filter(year==2020)%>%
  distinct()%>%
  left_join(tagger_carry)

tagging_done=tagging_2020%>%
  dplyr::filter(!is.na(assigned_ID))

tagging_needed=tagging_2020%>%
  dplyr::filter(is.na(assigned_ID))



tagging_needed$assigned_ID=replicate(dim(tagging_needed)[1],createRandString(), simplify=TRUE)
tagging_2020_out=rbind(tagging_done,tagging_needed)


write_csv(tagging_2020_out,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2020/Tagger_File_2020.csv")


############Creating 2018 Data Sets#################

head(data)
tagger=read.csv("/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2020/Master_Tagger_File_2020.csv")
head(tagger)
dim(tagging_2020_out)
length(unique(tagging_2020_out$assigned_ID))


data2=data%>%
  dplyr::filter(year==2020)%>%
  left_join(tagger)%>%
  mutate(candidate_ID=paste0(postal,"-",assigned_ID,"_",last_name,"_",first_init))

candidate_tagger=data2%>%
  dplyr::select(candidate_ID,assigned_ID,year,name,first_name,last_name,bp_url)%>%
  distinct()

write_csv(candidate_tagger,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2020/Candidate_Name_Information.csv")

candidate_district=data2%>%
  dplyr::select(candidate_ID,assigned_ID,year,state,postal,FIPS,chamber,long.fips,long.name,short.fips,)%>%
  distinct()
write_csv(candidate_district,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2020/Candidate_District_Information.csv")

candidate_website=data2%>%
  dplyr::select(candidate_ID,assigned_ID,year,website,website.binary,party,incumbent,party.flat)%>%
  distinct()
write_csv(candidate_website,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2020/Candidate_Website_Information.csv")

head(data2)
district_metadata=data2%>%
  dplyr::select(long.fips,long.name,short.fips,year,median.age,hs.plus,bachelors.plus,ag.percent.employ,mean.income,median.income,white.percent,papers.total,papers.daily,ktype_use,k.seats,party.presence,party.comp,general.presence,general.comp,population,pop_density)%>%
  distinct()

write.csv(district_metadata,"/Users/eringubrod/Dropbox/Projects/Running Projects/CampaignWebsites/Data_Files/2020/District_Meta_Data.csv")

c