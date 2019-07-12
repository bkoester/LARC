auxiliary_table_joins <- function()
{
  
  
  
  
}

#this adds a division to only the DGR1_MAJOR1 using my classifications.
add.major.division <- function(data)
{
  aplan <- read.delim("/Users/bkoester/Box Sync/PublicDataSets/acad.plan.owners.csv")
  dept <- read.delim("/Users/bkoester/Box Sync/PublicDataSets/dept.info.by.division.tab")
  
  engin <- mat.or.vec(length(dept$DIVISION),1)
  natsci <- engin
  human <- engin
  socsci <- engin
  DIVISION <- engin
  s <- dept$DIVISION == 'S'
  ss <- dept$DIVISION == 'SS'
  h <- dept$DIVISION == 'H'
  e <- dept$DIVISION == 'E'
  
  natsci[s]  <- 1
  socsci[ss] <- 1
  human[h]  <- 1
  engin[e]   <- 1 
  DIVISION[s]  <- 'NS'
  DIVISION[ss] <- 'SS'
  DIVISION[h]  <- 'H'
  DIVISION[e]  <- 'E'
  
  dept <- data.frame(dept,natsci,socsci,human,engin,DIVISION)
  
  dept <- dept[,names(dept) %in% c("DEPT_DESCRFORMAL","natsci","socsci","human","engin","DIVISION")]
  aplan <- aplan[,names(aplan) %in% c('Dept.Descrformal','Acad.Plan.Descr')]
  
  #Yank minors and duplicates, MS, PhD
  aplan <- aplan[!duplicated(aplan$Acad.Plan.Descr),]
  e     <- grep('minor',aplan$Acad.Plan.Descr,ignore.case=TRUE,invert=TRUE)
  aplan <- aplan[e,]
  e     <- grep('PhD',aplan$Acad.Plan.Descr,invert=TRUE)
  aplan <- aplan[e,]
  e     <- grep('MS',aplan$Acad.Plan.Descr,invert=TRUE)
  aplan <- aplan[e,]
  e     <- grep('MSE',aplan$Acad.Plan.Descr,invert=TRUE)
  aplan <- aplan[e,]
  e     <- grep('MEng',aplan$Acad.Plan.Descr,invert=TRUE)
  aplan <- aplan[e,]
  e     <- grep('PharmD',aplan$Acad.Plan.Descr,invert=TRUE)
  aplan <- aplan[e,]
  e     <- grep('DDS',aplan$Acad.Plan.Descr,invert=TRUE)
  aplan <- aplan[e,]
  e     <- grep('MPH',aplan$Acad.Plan.Descr,invert=TRUE)
  aplan <- aplan[e,]
  
  #Now for each of 3 majors
  
  data <- data[,!names(data) %in% c("natsci","socsci","human","engin","DIVISION")]
  data <- merge(data,aplan,by.x='UM_DGR_1_MAJOR_1_DES',by.y='Acad.Plan.Descr',all.x=TRUE)
  data <- merge(data,dept,by.x='Dept.Descrformal',by.y='DEPT_DESCRFORMAL',all.x=TRUE)
  return(data)
}

#the median income for each zipcode
add.zipcode.county.geoid <- function(sr)
{
  
  library(zipcode)
  
  #sr <- sr[,names(sr) %in% c("PERSON_POSTAL","PERSON_STATE")]
  CLEAN_ZIP <- clean.zipcodes(sr$FIRST_US_PRMNNT_RES_PSTL_5_CD)
  sr$CLEAN_ZIP <- CLEAN_ZIP
  zc <- read.csv("/Users/bkoester/Google Drive/code/REBUILD/UMILA/data/zcta_county_rel_10.txt",header=TRUE,sep=",")
  zc <- zc[,names(zc) %in% c("ZCTA5","GEOID")]
  names(zc) <- c('ZCTA5','GEO_ID')
  CLEAN_ZIP <- clean.zipcodes(zc$ZCTA5)
  zc  <- data.frame(zc,CLEAN_ZIP)
  hh <- merge(sr,zc,by='CLEAN_ZIP',all.x=TRUE)
  hh <- hh[!duplicated(hh$STDNT_ID),]
  #add in the county for this zip.
  cty <- read.table('/Users/bkoester/Google Drive/code/REBUILD/UMILA/data/county_adjacency.txt',sep="\t",header=FALSE)
  cty <- cty[,names(cty) %in% c('V3','V4')]
  cty <- cty[!duplicated(cty$V3),]
  names(cty) <- c('COUNTY','GEO_ID')
  hh <- merge(hh,cty,by='GEO_ID',all.x=TRUE)
  print(dim(hh))
  
  e <- which(hh$GEO_ID < 10000)
  hh$GEO_ID[e] <- paste('0500000US0',hh$GEO_ID[e],sep="")
  e <- which(hh$GEO_ID >= 10000)
  hh$GEO_ID[e] <- paste('0500000US',hh$GEO_ID[e],sep="")
  
  #add in the median income
  inc <- read.table('/Users/bkoester/Box\ Sync/PublicDataSets/median_income_zipcode_census.txt',
                    sep='\t',header=TRUE)
  inc <- inc[,names(inc) %in% c('Zip','Median')]
  hh  <- merge(hh,inc,by.x='ZCTA5',by.y='Zip',all.x=TRUE)
  hh$MEDNUM <- as.numeric(gsub(",", "", hh$Median))
  
  hh         <- hh[order(hh$GEO_ID), ]
  hh$count <- sequence(rle(as.vector(hh$GEO_ID))$lengths)
  
  ncrse  <- length(hh$GEO_ID[!duplicated(hh$GEO_ID)])
  nstart <- which(hh$count == 1)
  ntot   <- length(hh$GEO_ID) 
  MEDINC <- mat.or.vec(ntot,1)
  
  for (i in 1:ncrse)
  {
    start_ind <- nstart[i]
    if (i < ncrse){stop_ind  <- nstart[i+1]-1}
    if (i == ncrse){stop_ind <- ntot}
    
    ind         <- c(start_ind:stop_ind)
    MEDINC[ind] <- mean(hh$MEDNUM[ind])
    
  }
  
  hh <- data.frame(hh,MEDINC)
  
  return(hh)
  
  
}