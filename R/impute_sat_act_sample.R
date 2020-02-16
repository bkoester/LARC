#this uses year-by-year ACT/SAT results to impute 
#ACT/SAT scores for students that don't have one or the other.
#resampling is done within terms.
#
impute_sat_act_sample <- function(sr)
{
  library(mice)
  tt <- sr %>% filter(grepl("^U",PRMRY_CRER_CD))
  sr <- sr %>% select("STDNT_ID","FIRST_TERM_ATTND_SHORT_DES",
                      "MEDNUM",
                      "HS_GPA","MAX_ACT_ENGL_SCR","MAX_ACT_MATH_SCR",
                      #"MAX_ACT_SCIRE_SCR","MAX_ACT_READ_SCR",#"MAX_SATI_WR_SCR",
                      "MAX_SATI_MATH_SCR","MAX_SATI_VERB_SCR",
                      #"MAX_UMPLC_CH_TEST_SCR","MAX_UMPLC_MATH_TEST_SCR",
                      "STDNT_CTZN_STAT_CD")
                      #"MAX_AP_CALAB_TEST_SCR","MAX_AP_CALBC_TEST_SCR")
                      #"MAX_AP_CH_TEST_SCR")#,#,
                      #"MAX_UMPLC_MATH_TEST_SCR",
                      
  
  TERMS <- sr %>% distinct(FIRST_TERM_ATTND_SHORT_DES) %>% 
                  filter(grepl('FA',FIRST_TERM_ATTND_SHORT_DES) & 
                         !grepl('M',FIRST_TERM_ATTND_SHORT_DES))
  NTERMS <- dim(TERMS)[1]
  
  #create some bookkeeping variables
  sr <- sr %>% mutate(SAT_IMPUTE=0,ACT_IMPUTE=0)
  sr$SAT_IMPUTE[is.na(sr$MAX_ACT_MATH_SCR) & grepl('FA',sr$FIRST_TERM_ATTND_SHORT_DES) & 
                                             !grepl('M',sr$FIRST_TERM_ATTND_SHORT_DES)] <- 1
  sr$ACT_IMPUTE[is.na(sr$MAX_SATI_MATH_SCR) & grepl('FA',sr$FIRST_TERM_ATTND_SHORT_DES) & 
                                              !grepl('M',sr$FIRST_TERM_ATTND_SHORT_DES)] <- 1
  
  
  for (i in 1:NTERMS)
  {
    
    print(TERMS$FIRST_TERM_ATTND_SHORT_DES[i])
    dd <- which(sr$FIRST_TERM_ATTND_SHORT_DES == TERMS$FIRST_TERM_ATTND_SHORT_DES[i])
    init <- mice(sr[dd,],maxit=5)
    meth = init$method
    predM = init$predictorMatrix
    
    predM[,c("STDNT_ID")]=0
    predM[,c("FIRST_TERM_ATTND_SHORT_DES")]=0
    
    meth[c("MAX_ACT_MATH_SCR")]="cart"
    meth[c("MAX_ACT_ENGL_SCR")]="cart"
    meth[c("MAX_SATI_MATH_SCR")]="cart"
    meth[c("MAX_SATI_VERB_SCR")]="cart"
    
    #meth[c("MAX_ACT_SCIRE_SCR")]=""
    #meth[c("MAX_ACT_READ_SCR")]=""
    #meth[c("MAX_SATI_WR_SCR")]=""
    #meth[c("MAX_AP_CALAB_TEST_SCR")]=""
    #meth[c("MAX_AP_CALBC_TEST_SCR")]=""
    #meth[c("MAX_AP_CH_TEST_SCR")]=""
    #meth[c("MAX_UMPLC_CH_TEST_SCR")]=""
    #meth[c("MAX_UMPLC_MATH_TEST_SCR")]=""
    meth[c("STDNT_CTZN_STAT_CD")]=""
    meth[c("HS_GPA")]=""
    meth[c("MEDNUM")]=""
    meth[c("FIRST_TERM_ATTND_SHORT_DES")]=""
    meth[c("STDNT_ID")]=""
  
    set.seed(103)
    imputed = mice(sr[dd,], method=meth, predictorMatrix=predM, m=5)
    imputed <- mice::complete(imputed)
    sr[dd,] <- imputed
    #View(imputed)
    
  }
  
  #tt$MAX_ACT_COMP_SCR <- sr$MAX_ACT_COMP_SCR
  tt$MAX_ACT_MATH_SCR <- sr$MAX_ACT_MATH_SCR
  tt$MAX_ACT_ENGL_SCR <- sr$MAX_ACT_ENGL_SCR
  tt$MAX_SATI_VERB_SCR <- sr$MAX_SATI_VERB_SCR
  tt$MAX_SATI_MATH_SCR <- sr$MAX_SATI_MATH_SCR
  #tt$MAX_SATI_TOTAL_CALC_SCR <- sr$MAX_SATI_TOTAL_CALC_SCR
  tt$ACT_IMPUTE <- sr$ACT_IMPUTE
  tt$SAT_IMPUTE  <- sr$SAT_IMPUTE
  
  return(tt)
  
}


