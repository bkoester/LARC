network.diversity <- function(sr,sc)
{
  DEG <- sr %>% group_by(UM_DGR_1_MAJOR_1_DES) %>% select(UM_DGR_1_MAJOR_1_DES) %>% distinct() 
  N   <- length(DEG$UM_DGR_1_MAJOR_1_DES)
  DEG <- add_column(DEG,IND=c(1:N))
  
  sr  <- left_join(sr,DEG)
  srsub  <- sr %>% select(STDNT_ID,UM_DGR_1_MAJOR_1_DES) 
  sc  <- left_join(sc,srsub)
                                
  CLASS_NBR <- sc %>% group_by(CLASS_NBR,UM_DGR_1_MAJOR_1_DES) %>% count()
  CLASS_NBR <- CLASS_NBR %>% group_by(CLASS_NBR) %>% summarize(COURSE_DIV=(sum(n^2)/sum(n)^2)^(1/(1-2)))
  
  sc <- left_join(sc,CLASS_NBR)
  
  #This gives us mean major diversity
  sc <- sc %>% arrange(TERM_CD) %>% group_by(STDNT_ID) %>% mutate(CUM_MAJOR_DIV = cummean(COURSE_DIV))
  
  #Now for mean subject diversity
  #pp <- larc.matched.outcomes(hh,'STDNT_GNDR_SHORT_DES','Male','Female',
  #                            c('EXCL_CLASS_CUM_GPA_140','MAX_ACT_MATH_SCR','HS_GPA',
  #                              'COURSE','PRMRY_CRER_CD','TERM_CD'),OUTCOME='GRD_PNTS_PER_UNIT_NBR_140',
  #                            type=c('N','N','N','C','C','C'))
  #print(mean(pp$CASE_STATS1-pp$CONT_STATS1,na.rm=TRUE))
  
  return(sc)
  
}

compute_course_diversity <- function(df)
{
  
  #tot <- df %>% summarise(tot=sum(n))
  #print(tot)
  #tab <- df %>% select(n)
  print(df)
  tot <- (df)
  print(tot)
  tab <- df
  
  q       <- 2
  simp <- (sum(tab^q)/tot^q)^(1/(1-q))
  return(simp)
}