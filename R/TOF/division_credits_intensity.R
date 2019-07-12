division.credits.intensity <- function(sc)
{
  
  sc <- sc %>% filter(grepl("^U",PRMRY_CRER_CD))
  
  
  #merge the division INFO
  divlab <- read_tsv("/Users/bkoester/Box\ Sync/PublicDataSets/subject.by.division.tab")
  divlab <- divlab %>% select(-DEPT_DESCRSHORT)
  scs <- sc %>% select(c("SBJCT_CD","STDNT_ID","TERM_CD")) 
  scs <- left_join(scs,divlab,by=c("SBJCT_CD"="SUBJECT"))
  
  #Spread the DIVISION into indictators for summing
  scb <- scs %>% add_column(LAB=1) %>% distinct(.keep_all=TRUE) %>% spread(DIVISION,value=LAB,fill=0)
  sc  <- left_join(sc,scb,by=c("SBJCT_CD","STDNT_ID",'TERM_CD')) 
  #do the sums and drop the indictators
  sc <- sc %>% arrange(TERM_CD) %>% group_by(STDNT_ID) %>% mutate(CUM_ENGIN = cumsum(E*UNITS_ERND_NBR),
                                                                  CUM_HUM = cumsum(H*UNITS_ERND_NBR),
                                                                  CUM_O = cumsum(O*UNITS_ERND_NBR),
                                                                  CUM_P = cumsum(P*UNITS_ERND_NBR),
                                                                  CUM_S = cumsum(S*UNITS_ERND_NBR),
                                                                  CUM_SS = cumsum(SS*UNITS_ERND_NBR),
                                                                  CUM_ALL = cumsum(UNITS_ERND_NBR)) 
  sc <- sc %>% select(-c(E,H,O,S,SS,P))
  
  scdiv <- sc %>% group_by(STDNT_ID) %>%
    arrange(TERM_CD) %>%
    filter(row_number()==n())
  
  #now compute the term intensity
  scf <- sc %>% filter(str_detect(TERM_SHORT_DES,'FA') == TRUE) %>% arrange(TERM_CD) %>% 
    group_by(STDNT_ID) %>%  mutate(FA_CREDITS = sum(UNITS_ERND_NBR)) %>% distinct(TERM_CD,.keep_all=TRUE) %>% 
    mutate(NFA_TERMS = n())  %>% mutate(INTENSITY_FA = FA_CREDITS/NFA_TERMS) %>% 
    select(STDNT_ID,INTENSITY_FA,NFA_TERMS,FA_CREDITS) %>% distinct(STDNT_ID,.keep_all=TRUE)
  
  scw <- sc %>% filter(str_detect(TERM_SHORT_DES,'WN') == TRUE) %>% arrange(TERM_CD) %>% 
    group_by(STDNT_ID) %>%  mutate(WN_CREDITS = sum(UNITS_ERND_NBR)) %>% distinct(TERM_CD,.keep_all=TRUE) %>% 
    mutate(NWN_TERMS = n())  %>% mutate(INTENSITY_WN = WN_CREDITS/NWN_TERMS) %>% 
    select(STDNT_ID,INTENSITY_WN,NWN_TERMS,WN_CREDITS) %>% distinct(STDNT_ID,.keep_all=TRUE)
  
  scs <- sc %>% filter(str_detect(TERM_SHORT_DES,'S') == TRUE) %>% arrange(TERM_CD) %>% 
    group_by(STDNT_ID) %>%  mutate(S_CREDITS = sum(UNITS_ERND_NBR)) %>% distinct(TERM_CD,.keep_all=TRUE) %>% 
    mutate(NS_TERMS = n())  %>% mutate(INTENSITY_S = S_CREDITS/NS_TERMS) %>% 
    select(STDNT_ID,INTENSITY_S,NS_TERMS,S_CREDITS) %>% distinct(STDNT_ID,.keep_all=TRUE)
  
  sca <- sc %>% arrange(TERM_CD) %>% 
    group_by(STDNT_ID) %>%  mutate(TOT_CREDITS = sum(UNITS_ERND_NBR)) %>% distinct(TERM_CD,.keep_all=TRUE) %>% 
    mutate(NTERMS = n())  %>% mutate(INTENSITY = TOT_CREDITS/NTERMS) %>% 
    select(STDNT_ID,INTENSITY,NTERMS,TOT_CREDITS) %>% distinct(STDNT_ID,.keep_all=TRUE)
  
  sccred <- left_join(sca,scf)
  sccred <- left_join(sccred,scw)
  sccred <- left_join(sccred,scs)
  
  kk <- scdiv %>% select(STDNT_ID,CUM_ENGIN,CUM_HUM,CUM_O,CUM_P,CUM_S,CUM_SS)
  
  kk <- left_join(kk,sccred)             
  
  return(kk)
  
}