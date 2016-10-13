#####################################################################################
#Take the student course and student record, create grade penalty analysis of a course.
#FUNCTION: larc.reduction.pipeline
#PURPOSE : Reduce LARC flat file tables to fit Ben's analysis code.
#INPUTS  : lsi - LARC student info table.
#          lst - LARC student term table.
#          lsc - LARC student course table
#          TAG - The ID of the LARC dataset, usually included in the LARC filenames.
#          LARCDIR - The local directory where you've saved the flat files.
#          OUTDIR  - The local directory where the output tables will be written.
#EXAMPLE: > larc.reduction.pipeline(lsi,lst,lsc)
#####################################################################################
larc.reduction.pipeline <- function(lsi,lst,lsc,TAG='20160801',LARCDIR="/Users/bkoester/Box Sync/LARC.FLAT/",OUTDIR="/Users/bkoester/Box Sync/LARC.WORKING/")
{
  
  lsiname <- paste(LARCDIR,"LARC-QA_",TAG,"_STDNT_INFO.csv",sep="")
  lstname <- paste(LARCDIR,"LARC-QA_",TAG,"_STDNT_TERM.csv",sep="")
  lscname <- paste(LARCDIR,"LARC-QA_",TAG,"_STDNT_CLASS_INFO.csv",sep="")
  
  outSCname <- paste(OUTDIR,'BPK_LARC_STUDENT_COURSE_',TAG,'.tab',sep="")
  outSRname <- paste(OUTDIR,'BPK_LARC_STUDENT_RECORD_',TAG,'.tab',sep="")
  
  #read in the tables here. The initial read-in is slow if you use bz2. The lsc tables is > 4GB. May be a problem,
  #i recommend unzipping them outside of R.
  #lsi <- read.csv(lsiname,sep=",")
  #lst <- read.csv(lst,sep=",")
  #lsc <- read.csv(lscname,sep=",")
  
  #clean things up in the tables
  lsc <- reduce.lsc.table(lsc)
  lsi <- reduce.lsi.table(lsi)
  lsi <- add.graduating.gpa(lst,lsi)
  e   <- lsi$CRER_LVL_CD == 'U'
  lsi <- lsi[e,]
  lst <- reduce.lst.table(lst)
  
  
  # do ONLY outerjoins on everything
  lsi <- merge(lsi,lst,by='STDNT_ID',all=TRUE)
  
  #write things to disk
  write.table(lsc,outSCname,row.names=FALSE,quote=FALSE,sep="\t")
  write.table(lsi,outSRname,row.names=FALSE,quote=FALSE,sep="\t")
  
}


#This pulls the student info table columns. In the future, it will include any processing we need.
reduce.lsi.table <- function(lsi)
{
  
  #the basic columns to keep
  cols <- c("STDNT_ID","STDNT_GNDR_SHORT_DES","STDNT_ETHNC_GRP_SHORT_DES","STDNT_BIRTH_MO","STDNT_BIRTH_YR",
            "EST_GROSS_FAM_INC_CD","HS_GPA","PRNT_MAX_ED_LVL_CD",
            "MAX_ACT_COMP_SCR","MAX_ACT_ENGL_SCR","MAX_ACT_MATH_SCR",
            "MAX_SATI_TOTAL_CALC_SCR","MAX_SATI_MATH_SCR","MAX_SATI_VERB_SCR",
            "FIRST_US_PRMNNT_RES_PSTL_5_CD","FRST_FRGN_PRMNNT_RES_CNTRY_CD","STDNT_CTZN_STAT_CD","HS_STATE_CD",
            "LAST_TERM_ATTND_CD","FIRST_TERM_ATTND_CD","FIRST_TERM_ATTND_SHORT_DES","LAST_TERM_ATTND_SHORT_DES",
            "UM_UG_DGR_MAJOR_CNT",
            "UM_DGR_1_CMPLTN_TERM_CD","UM_DGR_2_CMPLTN_TERM_CD",
            "UM_DGR_1_MAJOR_1_DES","UM_DGR_1_MAJOR_2_DES","UM_DGR_2_MAJOR_1_DES","UM_DGR_2_MAJOR_2_DES")
  
  #all the AP scores, regardless of date, etc.
  #hh <- grep('MAX_AP.+SCR',names(lsi),perl=TRUE)
  ap <- c("MAX_AP_AMGVT_TEST_SCR",        "MAX_AP_AMHIS_TEST_SCR",        "MAX_AP_ART2D_TEST_SCR",        "MAX_AP_ART3D_TEST_SCR",       
          "MAX_AP_ARTDR_TEST_SCR",        "MAX_AP_ARTGN_TEST_SCR",        "MAX_AP_ARTHS_TEST_SCR",        "MAX_AP_BY_TEST_SCR",          
          "MAX_AP_CALAB_TEST_SCR",        "MAX_AP_CALAB_TEST_SUB_SCR",    "MAX_AP_CALBC_TEST_SCR",       
          "MAX_AP_CH_TEST_SCR",           "MAX_AP_CHINA_TEST_SCR",        "MAX_AP_CPGVT_TEST_SCR",        "MAX_AP_CSA_TEST_SCR",         
          "MAX_AP_CSAB_TEST_SCR",         "MAX_AP_EH_TEST_SCR",           "MAX_AP_EL_TEST_SCR",           "MAX_AP_ENGL_TEST_SCR",        
          "MAX_AP_ENVSC_TEST_SCR",        "MAX_AP_FR_TEST_SCR",           "MAX_AP_FRLIT_TEST_SCR",        "MAX_AP_GM_TEST_SCR",          
          "MAX_AP_HGEOG_TEST_SCR",        "MAX_AP_IT_TEST_SCR",           "MAX_AP_JAPN_TEST_SCR",         "MAX_AP_LTLIT_TEST_SCR",       
          "MAX_AP_LTVER_TEST_SCR",        "MAX_AP_MACEC_TEST_SCR",        "MAX_AP_MICEC_TEST_SCR",
          "MAX_AP_MTAUR_SUB_TEST_SCR",    "MAX_AP_MTNAU_TEST_SUB_SCR",    "MAX_AP_MUSTH_TEST_SCR",       
          "MAX_AP_PHYS1_TEST_SCR",        "MAX_AP_PHYS2_TEST_SCR",        "MAX_AP_PHYSB_TEST_SCR",        "MAX_AP_PHYSE_TEST_SCR",       
          "MAX_AP_PHYSM_TEST_SCR",        "MAX_AP_PY_TEST_SCR",           "MAX_AP_SEMNR_TEST_SCR",        "MAX_AP_SP_TEST_SCR",          
          "MAX_AP_SPLIT_TEST_SCR",        "MAX_AP_STAT_TEST_SCR",         "MAX_AP_WHIST_TEST_SCR")          
            
  umplc <- c("MAX_UMPLC_ATPT_TEST_SCR",  "MAX_UMPLC_CH_TEST_SCR",    "MAX_UMPLC_CHN_TEST_SCR",   "MAX_UMPLC_FILIP_TEST_SCR","MAX_UMPLC_FRLST_TEST_SCR",
             "MAX_UMPLC_FRRD_TEST_SCR",  "MAX_UMPLC_GM_TEST_SCR",    "MAX_UMPLC_HBRD_TEST_SCR",  "MAX_UMPLC_HINDI_TEST_SCR","MAX_UMPLC_INDON_TEST_SCR",
             "MAX_UMPLC_ITRD_TEST_SCR",  "MAX_UMPLC_JPNPL_TEST_SCR", "MAX_UMPLC_KOR_TEST_SCR",   "MAX_UMPLC_LTSCR_TEST_SCR",
             "MAX_UMPLC_MATH_TEST_SCR",  "MAX_UMPLC_PUNJA_TEST_SCR", "MAX_UMPLC_RSRD_TEST_SCR", 
             "MAX_UMPLC_SPLST_TEST_SCR", "MAX_UMPLC_SPRD_TEST_SCR",  "MAX_UMPLC_THAI_TEST_SCR",  "MAX_UMPLC_URDU_TEST_SCR", "MAX_UMPLC_VIETN_TEST_SCR")
  
  keep <- c(cols,ap,umplc)
  out  <- lsi[,keep]
  return(out)
  
}

#This extracts declarations from the student term table. It's not pretty.
reduce.lst.table <- function(lst)
{

  #first, clean for only undergrad career terms
  e <- lst$CRER_LVL_CD == 'U'
  lst <- lst[which(e),]
  
  #Now, allow up to five declares as before. We need to loop over all students, one-at-a-time to do this.
  lst        <- lst[order(lst$STDNT_ID,lst$TERM_CD), ]
  lst$count  <- sequence(rle(as.vector(lst$STDNT_ID))$lengths)
  ntot       <- length(lst$STDNT_ID)
  nid        <- length(lst$STDNT_ID[!duplicated(lst$STDNT_ID)])
  nstart     <- which(lst$count == 1)
  
  DECL       <- mat.or.vec(nid,5)
  DECL[]     <- NA
  DECL_TERM  <- DECL
  STDNT_ID   <- mat.or.vec(nid,1)
  #TERM_START <- STDNT_ID
  #TERM_START_SHORT_DES <- STDNT_ID
  
  PGM_CHECK <- c("PGM_1_MAJOR_1_DES","PGM_1_MAJOR_2_DES","PGM_2_MAJOR_1_DES","PGM_2_MAJOR_2_DES")
  pgm_inds  <- which(names(lst) %in% PGM_CHECK)
  
  for (i in 1:nid)
  {
    if (i %% 10000 == 0){print(paste('Counting declarations',i,'of',nid,'STDNT_IDs',sep=" "))}
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind    <- c(start_ind:stop_ind)
    temp   <- lst[ind,]
    terms  <- temp$TERM_CD
    nterms <- length(terms)
    
    mvec   <- mat.or.vec(nterms*4,2)
    tvec   <- mvec
    
    for (j in 1:nterms)
    {
        m1 <- as.character(temp[j,pgm_inds[1]])  
        m2 <- as.character(temp[j,pgm_inds[2]])
        m3 <- as.character(temp[j,pgm_inds[3]])
        m4 <- as.character(temp[j,pgm_inds[4]])
        
        mvec[((j-1)*4+1):((j)*4),1] <- c(terms[j],terms[j],terms[j],terms[j])
        mvec[((j-1)*4+1):((j)*4),2] <- as.vector(c(m1,m2,m3,m4))
    }
    
    e <- mvec[,2] != ""
    mvec <- mvec[e,]
    if (length(mvec) > 2){mvec <- mvec[!duplicated(mvec[,2]),]}
    
    if (is.null(dim(mvec)))
    {
      DECL[i,1] <- mvec[2]
      DECL_TERM[i,1] <- mvec[1]
    }
    
    if (!is.null(dim(mvec))) 
    {
      mvec <- mvec[!duplicated(mvec[,2]),]
      ndecl <- dim(mvec)[1]
        if (ndecl > 5){ndecl <- 5}
        if (dim(mvec)[1] > 0)
        {
          DECL[i,(1:ndecl)]      <- mvec[(1:ndecl),2]
          DECL_TERM[i,(1:ndecl)] <- mvec[(1:ndecl),1]
        }
    }
    
    STDNT_ID[i] <- temp$STDNT_ID[1]
    
    #print(DECL_TERM[i,])
    #print(DECL[i,])
    #scan()
  }
  
  DECLARE1 <- DECL[,1]
  DECLARE1_TERM <- DECL_TERM[,1]
  DECLARE2 <- DECL[,2]
  DECLARE2_TERM <- DECL_TERM[,2]
  DECLARE3 <- DECL[,3]
  DECLARE3_TERM <- DECL_TERM[,3]
  DECLARE4 <- DECL[,4]
  DECLARE4_TERM <- DECL_TERM[,4]
  DECLARE5 <- DECL[,5]
  DECLARE5_TERM <- DECL_TERM[,5]
  
  out <- data.frame(STDNT_ID,
                    DECLARE1,DECLARE1_TERM,DECLARE2,DECLARE2_TERM,
                    DECLARE3,DECLARE3_TERM,DECLARE4,DECLARE4_TERM, 
                    DECLARE5,DECLARE5_TERM)
  return(out)
}

#select student course columns form the studnet course table, keeping ONLY courses taken for a grade.
reduce.lsc.table <- function(lsc)
{
  #tables cuts here
  e    <- lsc$GRD_BASIS_ENRL_DES == 'Graded'
  lsc  <- lsc[which(e),]
  
  cols <- c("STDNT_ID","TERM_CD","TERM_SHORT_DES","CLASS_NBR","CRSE_GRD_OFFCL_CD",
            "UNITS_ERND_NBR","GRD_PNTS_PER_UNIT_NBR","EXCL_CLASS_CUM_GPA",
            "SBJCT_CD","CATLG_NBR","CRSE_ID_CD","CLASS_SCTN_CD","ASSOC_CLASS_CD",
            "CRSE_CMPNT_CD")
  data <- lsc[,cols]
  return(data)
  
}  

#This adds more information from the LARC term table to the student record,
#including PRIMARY CAREER, CAREER LVL, ENTRY_TYPE, and graduating CUM_GPAs for up to two degrees.
add.graduating.gpa <- function(lst,sr)
{
  lst  <- lst[,c('STDNT_ID','TERM_CD','CUM_GPA','ENTRY_TYP_SHORT_DES','PRMRY_CRER_CD','CRER_LVL_CD')]
  
  out1 <- merge(sr,lst,by.y=c('STDNT_ID','TERM_CD'),by.x=c('STDNT_ID','UM_DGR_1_CMPLTN_TERM_CD'),all.x=TRUE)
  out1 <- out1[,!names(out1) %in% c('TERM_CD','ENTRY_TYP_SHORT_DES','PRMRY_CRER_CD','CRER_LVL_CD')]
  e    <- names(out1) == 'CUM_GPA'
  names(out1)[e] <- 'CUM_GPA1'
  
  out1 <- merge(out1,lst,by.y=c('STDNT_ID','TERM_CD'),by.x=c('STDNT_ID','UM_DGR_2_CMPLTN_TERM_CD'),all.x=TRUE)
  out1 <- out1[,!names(out1) %in% c('TERM_CD','ENTRY_TYP_SHORT_DES','PRMRY_CRER_CD','CRER_LVL_CD')]
  e    <- names(out1) == 'CUM_GPA'
  names(out1)[e] <- 'CUM_GPA2'
  
  out1 <- merge(out1,lst,by.x=c('STDNT_ID','FIRST_TERM_ATTND_CD'),by.y=c('STDNT_ID','TERM_CD'),all.x=TRUE)
  out1 <- out1[,!names(out1) %in% c('TERM_CD','CUM_GPA')]
  
  
  return(out1)
  
}