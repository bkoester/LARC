label_transfer_students <- function(st,sr)
{
  require(tidyverse)
  srtemp <- sr
  #sr <- read_csv('~/Box Sync/LARC.FLAT/LARC_20190529_STDNT_INFO.csv')
  #st <- read_csv('~/Box Sync/LARC.FLAT/LARC_20190529_STDNT_TERM_INFO.csv')
  sr <- sr %>% select(STDNT_ID,FIRST_TERM_ATTND_CD) #%>% filter(FIRST_TERM_ATTND_CD >= 1560)
  st <- st %>% filter(grepl("^U",PRMRY_CRER_CD)) %>% 
               select(STDNT_ID,TERM_CD,TERM_SHORT_DES,ENTRY_TYP_SHORT_DES)  
  st <- st %>% group_by(STDNT_ID) %>% arrange(TERM_CD) %>% filter(row_number()==1)
  
  st <- st %>% mutate(TERM_CD=as.numeric(TERM_CD))
  sr <- sr %>% mutate(TERM_CD=as.numeric(FIRST_TERM_ATTND_CD))
  
  sr <- sr %>% left_join(st)#,by=c("FIRST_TERM_ATTND_CD"="TERM_CD"))
  sr <- sr %>% mutate(TRANSFER=0)
  sr$TRANSFER[sr$ENTRY_TYP_SHORT_DES == 'NwTr'] <- 1
  sr$TRANSFER[is.na(sr$ENTRY_TYP_SHORT_DES)] <- NA
  
  sr <- sr %>% select(STDNT_ID,ENTRY_TYP_SHORT_DES,TRANSFER)
  srtemp <- sr %>% left_join(srtemp,by='STDNT_ID')
  
  return(srtemp)
  
}