#this is set up to run on the LARC student-course table "sc", formatted as a tibble (sc <- as_tibble(sc))
#all statistics are weighted by credits (UNITS_ENRD_NBR)
#to do, fiddle with:
#1) correct business school plus grades from +0.4 -> +0.3. This biases their SFEs upward.
#2) compute SFEs on the basis of just high enrollment courses.
#
student_course_fixed_effect <- function(sc,tol=0.01)
{
  library(tidyverse)
  
  TERMLIST <- sc %>% group_by(TERM_CD) %>% distinct(TERM_CD) %>% arrange(TERM_CD)
  NTERM    <- dim(TERMLIST)[1]
  SFE     <- mat.or.vec(dim(sc)[1],1)
  CFE     <- mat.or.vec(dim(sc)[1],1)
  CMEAN   <- mat.or.vec(dim(sc)[1],1)
  STDMEAN <- mat.or.vec(dim(sc)[1],1)
  ADJGRD  <- mat.or.vec(dim(sc)[1],1)
  TERM_CD <- sc$TERM_CD
  STDNT_ID <- sc$STDNT_ID
  
  for (i in 1:NTERM)
  {
    
    TERM_CD_test <- TERMLIST$TERM_CD[i]
    print(TERM_CD_test)
    
    e  <- which(sc$TERM_CD <= TERM_CD_test)
    e1 <- which(sc$TERM_CD[e] == TERM_CD_test)
    
    kt  <- sc[e,] %>% group_by(STDNT_ID) %>% 
      mutate(k=weighted.mean(GRD_PNTS_PER_UNIT_NBR,UNITS_ERND_NBR,na.rm=TRUE)) %>% ungroup()
    kt  <- kt %>% group_by(CLASS_NBR) %>% 
      mutate(a=weighted.mean(GRD_PNTS_PER_UNIT_NBR,UNITS_ERND_NBR,na.rm=TRUE)) %>% ungroup()
    
    kt <- kt %>% mutate(CMEAN=a) %>% mutate(STDMEAN=k) %>% mutate(ADJGRD=GRD_PNTS_PER_UNIT_NBR-CMEAN) 
    kt <- kt %>% mutate(k_old=k) %>% mutate(a_old=a) 
    
    a_diff_max <- 100
    k_diff_max <- 100
    
    flag <- 0
    
    ssdelta <- 0
    kdelta  <- 0
    adelta  <- 0
    cts <- 1
    
    while ((a_diff_max > tol) | (k_diff_max > tol))
    {
      
      kt  <- kt %>% group_by(STDNT_ID)  %>% mutate(k=weighted.mean(GRD_PNTS_PER_UNIT_NBR-a,UNITS_ERND_NBR,na.rm=TRUE)) %>% ungroup()
      kt  <- kt %>% group_by(CLASS_NBR) %>% mutate(a=weighted.mean(GRD_PNTS_PER_UNIT_NBR-k,UNITS_ERND_NBR,na.rm=TRUE)) %>% ungroup()
      
      kdiff <- abs(kt$k-kt$k_old)
      adiff <- abs(kt$a-kt$a_old)
      
      kt$k_old <- kt$k
      kt$a_old <- kt$a
      
      ss <- sum((kt$GRD_PNTS_PER_UNIT_NBR-kt$k-kt$a)^2.,na.rm=TRUE)
      
      a_diff_max <- max(kdiff,na.rm=TRUE)
      k_diff_max <- max(adiff,na.rm=TRUE)
      ssdelta <- c(ssdelta,ss)
      kdelta  <- c(kdelta,k_diff_max)
      adelta  <- c(adelta,a_diff_max)
      #print(k_diff_max)
      #print(a_diff_max)
      
      cts <-  cts + 1
    }
    
    SFE[e[e1]]     <- kt$k[e1] #the student fixed effect
    CFE[e[e1]]     <- kt$a[e1] #the course effect
    CMEAN[e[e1]]   <- kt$CMEAN[e1] #the mean course grade
    STDMEAN[e[e1]] <- kt$STDMEAN[e1] #the student's mean grade, should be approx the GPA.
    ADJGRD[e[e1]]  <- kt$ADJGRD[e1] #student grade - course mean grade. This already quite similar to the SFE.
    
    #data <- as_tibble(data.frame(SFE,CFE,CMEAN,STDMEAN,ADJGRD,TERM_CD))
    #diagnostics <- data.frame(ssdelta,adelta,kdelta,TERM_CD[e])
    
  }
  
  data <- as_tibble(data.frame(SFE,CFE,CMEAN,STDMEAN,ADJGRD,TERM_CD,STDNT_ID))
  
  return(data)
  
}