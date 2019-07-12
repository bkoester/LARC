course_division_major_distribution <- function(sr,sc)
{
  require(tidyverse)
  require(scatterplot3d)
  
  #sr <- read_tsv("Box Sync/SEISMIC/SEISMIC_Data/student_record.tsv")
  #sc <- read_tsv("Box Sync/SEISMIC/SEISMIC_Data/student_course.tsv")  
  cip   <- read_tsv('~/Box Sync/SEISMIC/SEISMIC_Data/CIP_2digit_division.tab.txt',
                    col_types=cols_only(`CIP Code`=col_integer(),DIVISION=col_character()))
  aplan <- read_tsv('/Users/bkoester/Box Sync/PublicDataSets/acad.plan.owners.csv') %>% 
           select(c("Dept Descrshort","Acad Plan Descr"))
  dept  <- read_tsv('/Users/bkoester/Box Sync/PublicDataSets/dept.info.by.division.tab') %>% select(c(DIVISION,DEPT_DESCRSHORT))
  degdiv <- left_join(aplan,dept,by=c("Dept Descrshort"="DEPT_DESCRSHORT")) %>% select(-c("Dept Descrshort"))
  sr     <- sr %>% left_join(degdiv,by=c("UM_DGR_1_MAJOR_1_DES"="Acad Plan Descr"))
  
  
  #sub <- read_tsv('/Users/bkoester/Box Sync/PublicDataSets/subject.by.division.tab') %>% select(-c(DEPT_DESCRSHORT))
  #sc  <- sc %>% left_join(sub,by=c("SBJCT_CD"="SUBJECT"))
  sc  <- sc %>% mutate(CIP = floor(as.numeric(CRSE_CIP_CD))) %>% left_join(cip,by=c("CIP"="CIP Code"))
  
  #tot  <- sc %>% gather(DIVSION,value=UNITS_ERND_NBR) %>% group_by(
  tot  <- sc %>% group_by(STDNT_ID,DIVISION) %>% summarize(TOT_CRED=sum(UNITS_ERND_NBR)) %>% spread(DIVISION,TOT_CRED)     
  sr   <- sr %>% filter(FIRST_TERM_ATTND_CD == 1810 & !is.na(UM_DGR_1_MAJOR_1_DES)) %>% 
                 select(c(STDNT_ID,PRMRY_CRER_CD,DIVISION,UM_DGR_1_MAJOR_1_DES)) %>% left_join(tot)
  
  majors <- sr %>% group_by(UM_DGR_1_MAJOR_1_DES) %>% 
    mutate(MEAN_H=mean(H,na.rm=TRUE),MEAN_S=mean(S,na.rm=TRUE),MEAN_SS=mean(SS,na.rm=TRUE),N=n()) %>% 
    distinct(UM_DGR_1_MAJOR_1_DES,.keep_all=TRUE)
  
  division <- sr %>% group_by(PRMRY_CRER_CD) %>% 
    mutate(MEAN_H=mean(H,na.rm=TRUE),MEAN_S=mean(S,na.rm=TRUE),MEAN_SS=mean(SS,na.rm=TRUE),N=n()) %>% 
    distinct(PRMRY_CRER_CD,.keep_all=TRUE) %>% 
    filter(PRMRY_CRER_CD != 'UNUR' & PRMRY_CRER_CD != 'UKIN')
  
  tt <- majors %>% filter(N > 20) 
  
  s3ddiv <- scatterplot3d(division$MEAN_H,division$MEAN_S,division$MEAN_SS,
                          xlab='Humanties',ylab='Science',zlab='Social Science')
  s3ddiv.coords <- s3ddiv$xyz.convert(division$MEAN_H,division$MEAN_S,division$MEAN_SS)
  text(s3ddiv.coords$x, s3ddiv.coords$y,             # x and y coordinates
       labels=division$PRMRY_CRER_CD,               # text to plot
       cex=2, pos=2,)
  
  #scatterplot3d(tt$MEAN_H,tt$MEAN_S,tt$MEAN_SS)
  s3d <- scatterplot3d(tt$MEAN_H,tt$MEAN_S,tt$MEAN_SS,angle=30,type='h',box=FALSE,xlab='Humanties',ylab='Science',zlab='Social Science')
  
  s3ddiv.coords <- s3d$xyz.convert(division$MEAN_H,division$MEAN_S,division$MEAN_SS)
  
  h <- which(tt$DIVISION == 'H')
  s <- which(tt$DIVISION == 'S')
  ss <- which(tt$DIVISION == 'SS')
  e <- which(tt$DIVISION == 'E')
  p <- which(tt$DIVISION == 'P')
  s3d$points3d(tt$MEAN_H[s],tt$MEAN_S[s],tt$MEAN_SS[s],col='green')
  s3d$points3d(tt$MEAN_H[h],tt$MEAN_S[h],tt$MEAN_SS[h],col='red',pch=16)
  s3d$points3d(tt$MEAN_H[ss],tt$MEAN_S[ss],tt$MEAN_SS[ss],col='blue')
  s3d$points3d(tt$MEAN_H[e],tt$MEAN_S[e],tt$MEAN_SS[e],col='purple',pch=16)
  text(s3ddiv.coords$x, s3ddiv.coords$y,             # x and y coordinates
       labels=division$PRMRY_CRER_CD,               # text to plot
       cex=1, pos=2)
  print(division$PRMRY_CRER_CD)
  
  #And finally a couple of majors plotted in the same space.
  m1 <- which(sr$UM_DGR_1_MAJOR_1_DES == 'English BA')
  m2 <- which(sr$UM_DGR_1_MAJOR_1_DES == 'Cellular & Molec Biology BS')
  m3 <- which(sr$UM_DGR_1_MAJOR_1_DES == 'Psychology BA')
  m4 <- which(sr$UM_DGR_1_MAJOR_1_DES == 'Chemical Engineering BSE')
  mall <- c(m1,m2,m3,m4)
  
  s3d <- scatterplot3d(sr$H[mall],sr$S[mall],sr$SS[mall],angle=25,box=FALSE,
                       xlab='Humanities',ylab='Science',zlab='Social Science')
  s3d$points3d(sr$H[m2],sr$S[m2],sr$SS[m2],col='blue')
  s3d$points3d(sr$H[m1],sr$S[m1],sr$SS[m1],col='red',pch=16)
  s3d$points3d(sr$H[m3],sr$S[m3],sr$SS[m3],col='purple',pch=16)
  
  return(majors)
}