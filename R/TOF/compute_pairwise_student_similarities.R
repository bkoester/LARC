compute_pairwise_student_similarities <- function(sc,sr,maj)
{
  library(coop)
  
  print(Sys.time())
  START_TERM <- 1560
  
  sr1 <- sr %>% filter(FIRST_TERM_ATTND_CD == 1810 & grepl("^U",PRMRY_CRER_CD) & 
                         ENTRY_TYP_SHORT_DES == 'Freshman' & 
                         !grepl('MS',UM_DGR_1_MAJOR_1_DES) & !grepl('MA',UM_DGR_1_MAJOR_1_DES) & !grepl('MPH',UM_DGR_1_MAJOR_1_DES)) %>% 
    drop_na(STDNT_ID) %>% 
    select(c(STDNT_ID,UM_DGR_1_MAJOR_1_DES,UM_DGR_1_MAJOR_2_DES,UM_DGR_2_MAJOR_1_DES)) %>% mutate(STDNT_ID=as.integer(STDNT_ID))
  
  print(dim(sr1)[1])
  print('cleaned SR')
  
  if (dim(sr1)[1] > 6500){print('> 3000 students'); return()}
  
  #sc    <- term_count(sr,sc)
  
  CLIST <- sc %>% mutate(CRSE_UID=str_c(CRSE_ID_CD,TERM_CD)) %>% 
    filter(TERM_CD >= START_TERM & grepl("^U",PRMRY_CRER_CD) & CATLG_NBR < 500) %>% 
    #group_by(CRSE_ID_CD) %>% mutate(N_ENROLL=n(),N_TERM=n_distinct(TERM_CD)) %>% ungroup() %>%
    distinct(CRSE_ID_CD,.keep_all=TRUE) %>% #mutate(N_AVG=N_ENROLL/N_TERM) %>% 
    select(c(SBJCT_CD,CATLG_NBR,TERM_CD,TERM_SHORT_DES,CLASS_NBR,CRSE_ID_CD))
  
  print('cleaned student course')
  
  CLIST <- CLIST %>% select(CRSE_ID_CD) #filter(N_AVG > 0 & N_TERM >= 0) %>% select(c(CRSE_ID_CD,N_ENROLL,N_TERM,N_AVG))
  subsc <- left_join(CLIST,sc) #%>% filter(TERMYR <= CTERM)
  #subsc2 <- left_join(sr1,subsc)
  
  sc  <- left_join(sr1,subsc) %>% drop_na(UM_DGR_1_MAJOR_1_DES) %>% distinct(CRSE_ID_CD,STDNT_ID,.keep_all=TRUE) %>% 
    mutate(TOT_STD_CRSE=n()) %>% mutate(TOT_STD=n_distinct(STDNT_ID)) %>% ungroup() %>%
    group_by(UM_DGR_1_MAJOR_1_DES) %>% mutate(N_MAJ=n_distinct(STDNT_ID),N_MAJ_STD_CRSE=n()) %>% ungroup() %>%
    group_by(CRSE_ID_CD) %>% mutate(N_CRSE_ALL=n()) %>% ungroup() %>% drop_na(STDNT_ID) %>% mutate(MAJVEC=0)
  
  maxSIC <- max(sc$STDNT_ID)
  maxSTD <- sc %>% distinct(STDNT_ID) %>% tally()
  maxSTD <- pull(maxSTD,n)
  
  #now get the major vectors in a similar format.
  maj   <- maj %>% mutate(.,STDNT_ID=group_indices(.,UM_DGR_1_MAJOR_1_DES)) %>% mutate(TAKEN_IND=PROB)
  maj   <- maj %>% mutate(STDNT_ID=STDNT_ID+maxSIC) 
  majsr <- maj %>% select(STDNT_ID,UM_DGR_1_MAJOR_1_DES) %>% distinct(UM_DGR_1_MAJOR_1_DES,.keep_all=TRUE)
  sr1   <- bind_rows(sr1,majsr)
  
  maj <- maj %>% select(-c(UM_DGR_1_MAJOR_1_DES)) %>% mutate(MAJVEC=1) %>% filter(PROB > 0.1 & N_CRSE_MAJ > 5)
  
  sc <- bind_rows(sc,maj)
  
  sc <- sc %>% mutate(.,STD_IND=group_indices(.,STDNT_ID)) %>% mutate(TAKEN_IND=1)
  sc <- sc %>% select(STDNT_ID,CRSE_ID_CD,STD_IND,TAKEN_IND,MAJVEC) 
  
  sc <- sc %>% mutate(.,COURSE_IND=group_indices(.,CRSE_ID_CD))
  
  tt <- sc %>% select(-c(STD_IND,COURSE_IND)) %>% spread(CRSE_ID_CD,TAKEN_IND,fill=0)
  #print(names(tt))
  
  STDNT_ID <- as.character(pull(tt[,1],STDNT_ID))
  MAJVEC   <- as.integer(pull(tt[,2],MAJVEC))
  
  print('computing similarities')
  ll <- cosine(t(tt[,c(-1,-2)]))
  
  #ll <- ll[lower.tri(ll,diag=TRUE)] <- NA
  
  ll <- as_tibble(ll)
  
  ll <- ll %>% rename_at(vars(starts_with('V')), ~ STDNT_ID)
  
  #names(ll) <- as.character(STDNT_ID)
  ll <- ll %>% mutate(ID=STDNT_ID,MAJ=MAJVEC)
  ll <- ll %>% gather(1:length(STDNT_ID),key="PAIR",value="SIM")
  
  ll <- ll %>% mutate(ID=as.integer(ID),PAIR=as.integer(PAIR))
  
  ll <- left_join(ll,sr1,by=c("ID"="STDNT_ID"))
  ll <- left_join(ll,sr1,by=c("PAIR"="STDNT_ID"))
  
  len <- dim(ll)[1]
  #ll <- ll[1:(len/2),]
  ll <- ll %>% filter(ID != PAIR)
  
  mm <- ll %>% select(ID,PAIR,SIM) %>% spread(PAIR,SIM,drop=FALSE)
  
  
  mtx <- as.matrix(mm)
  rownames(mtx) <- mtx[,1]
  mtx <- as.dist(-1.0*(mtx[,-1]-1))
  ss  <- hclust(mtx,method='ward.D2')
  tt <- cutree(ss,h=7)
  tt <- tibble(CL=tt,STDNT_ID=as.integer(names(tt)))
  
  sr1 <- left_join(sr1,tt)
  
  #cls <- hclust(mtx)
  
  print(Sys.time())
  
  return(list(ll,mtx,sr1))
  
}