compute_similarity_wrapper <- function(sc,sr,BY_TERM=FALSE,BY_CAT=FALSE,BY_DIV=FALSE)
{
  
  source("/Users/bkoester/Google Drive/code/Mellon/TOF/term_count.R")
  sub <- read_tsv('/Users/bkoester/Box Sync/PublicDataSets/subject.by.division.tab') %>% 
        select(-c(DEPT_DESCRSHORT))
  sc  <- sc %>% left_join(sub,by=c("SBJCT_CD"="SUBJECT"))
  
  if (BY_DIV == TRUE){sc <- sc %>% mutate(SBJCT_CD=DIVISION,CRSE_ID_CD=DIVISION)}
  
  sc <- term_count(sr,sc) %>% mutate(TERMYR=TERMYR*100) %>% select(-c(FIRST_TERM_ATTND_CD))
  
  if (BY_CAT == TRUE)
  {
    sc <- sc %>% mutate(NEWCAT=round(CATLG_NBR/100.)*100,CRSE_ID_CD=str_c(SBJCT_CD,NEWCAT,sep="_"))
  }
  if (BY_TERM == TRUE)
  {
    sc <- sc %>% mutate(CRSE_ID_CD=str_c(CRSE_ID_CD,TERMYR,sep="_"))
  }
  
  sc <- sc %>% select(STDNT_ID,CATLG_NBR,CRSE_ID_CD) %>% filter(CATLG_NBR < 500)
  sr <- sr %>% filter(FIRST_TERM_ATTND_CD == 1660) %>% group_by(UM_DGR_1_MAJOR_1_DES) %>% 
    mutate(NMAJ=n()) %>% filter(NMAJ > 20) %>% ungroup() %>%
    select(STDNT_ID,UM_DGR_1_MAJOR_1_DES) %>% 
    distinct(STDNT_ID,.keep_all=TRUE)
  
  temp <- basic_compute_pairwise(sc,sr,keep_cols='UM_DGR_1_MAJOR_1_DES',MIN_COURSE_SIZE=10)
  
  return(temp)
  
}

compare_a_few <- function(sc,sr)
{
  pdf('~/Desktop/temp2.pdf',width=11,height=7)
  
  jj <- compute_similarity_wrapper(sc,sr,BY_TERM=TRUE)
  p1 <- jj[[1]] %>% filter(UM_DGR_1_MAJOR_1_DES_ID == 'History BA' & UM_DGR_1_MAJOR_1_DES_PAIR == 'History BA') %>% 
        ggplot(aes(x=SIM)) + geom_histogram() + xlim(0,0.5)
  ss <- make_similarity_matrix(jj)
  #plot(ss,cex=0.4,main='COURSE-TERM')
  heatmap.2(ss,trace='none',main='COURSE-TERM')
  
  jj <- compute_similarity_wrapper(sc,sr)
  p2 <- jj[[1]] %>% filter(UM_DGR_1_MAJOR_1_DES_ID == 'History BA' & UM_DGR_1_MAJOR_1_DES_PAIR == 'History BA') %>% 
        ggplot(aes(x=SIM)) + geom_histogram() + xlim(0,0.5)
  ss <- make_similarity_matrix(jj)
  #plot(ss,cex=0.4,main='COURSE')
  heatmap.2(ss,trace='none',main='COURSE')
  
  jj <- compute_similarity_wrapper(sc,sr,BY_CAT=TRUE,BY_TERM=TRUE)
  p3 <- jj[[1]] %>% filter(UM_DGR_1_MAJOR_1_DES_ID == 'History BA' & UM_DGR_1_MAJOR_1_DES_PAIR == 'History BA') %>% 
        ggplot(aes(x=SIM)) + geom_histogram() + xlim(0,0.5)
  ss <- make_similarity_matrix(jj)
  #plot(ss,cex=0.4,main='LEVEL-TERM')
  heatmap.2(ss,trace='none',main='LEVEL-TERM')
  
  jj <- compute_similarity_wrapper(sc,sr,BY_CAT=TRUE)
  p4 <- jj[[1]] %>% filter(UM_DGR_1_MAJOR_1_DES_ID == 'History BA' & UM_DGR_1_MAJOR_1_DES_PAIR == 'History BA') %>% 
        ggplot(aes(x=SIM)) + geom_histogram() + xlim(0,0.5)
  ss <- make_similarity_matrix(jj)
  #plot(ss,cex=0.4,main='LEVEL')
  heatmap.2(ss,trace='none',main='LEVEL')
  
  jj <- compute_similarity_wrapper(sc %>% mutate(CATLG_NBR=0),sr,BY_CAT=TRUE)
  p5 <- jj[[1]] %>% filter(UM_DGR_1_MAJOR_1_DES_ID == 'History BA' & UM_DGR_1_MAJOR_1_DES_PAIR == 'History BA') %>% 
    ggplot(aes(x=SIM)) + geom_histogram() + xlim(0,0.5)
  ss <- make_similarity_matrix(jj)
  #plot(ss,cex=0.4,main='SUBJECT')
  heatmap.2(ss,trace='none',main='SUBJECT')
  
  jj <- compute_similarity_wrapper(sc %>% mutate(CATLG_NBR=0),sr,BY_DIV=TRUE)
  p5 <- jj[[1]] %>% filter(UM_DGR_1_MAJOR_1_DES_ID == 'History BA' & UM_DGR_1_MAJOR_1_DES_PAIR == 'History BA') %>% 
    ggplot(aes(x=SIM)) + geom_histogram() + xlim(0,0.5)
  ss <- make_similarity_matrix(jj)
  #plot(ss,cex=0.4,main='DIVISION')
  heatmap.2(ss,trace='none',main='DIVISION')
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  
  dev.off()
  
}


make_similarity_matrix <- function(jj)
{
  
  tt <- jj[[1]] %>% group_by(UM_DGR_1_MAJOR_1_DES_ID,UM_DGR_1_MAJOR_1_DES_PAIR) %>% 
                    summarize(mnSIM=mean(SIM,na.rm=TRUE)) %>% 
                    spread(UM_DGR_1_MAJOR_1_DES_PAIR,mnSIM,drop=FALSE,fill=0) %>% ungroup()
  ttn <- pull(tt,UM_DGR_1_MAJOR_1_DES_ID)
  tt  <- tt %>% select(-UM_DGR_1_MAJOR_1_DES_ID)
  mtx <- as.matrix(tt)
  
  rownames(mtx) <- ttn #mtx[,1]
  
  #mtx <- as.dist(-1.0*(mtx-1))
  #ss  <- hclust(mtx,method='ward.D2')
  #tt <- cutree(ss,h=7) #cutting the tree at level 7, somewhat arbitrarily

  return(mtx)
  
}

