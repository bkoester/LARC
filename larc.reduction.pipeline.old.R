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
larc.reduction.pipeline <- function(TAG='20180925',LARCDIR="/Users/bkoester/Box Sync/LARC.FLAT/",OUTDIR="/Users/bkoester/Box Sync/LARC.WORKING/")
{
  
  lsiname <- paste(LARCDIR,"LARC_",TAG,"_STDNT_INFO.csv",sep="")
  lstname <- paste(LARCDIR,"LARC_",TAG,"_STDNT_TERM_INFO.csv",sep="")
  lscname <- paste(LARCDIR,"LARC_",TAG,"_STDNT_TERM_CLASS_INFO.csv",sep="")
  
  outSCname <- paste(OUTDIR,'BPK_LARC_STUDENT_COURSE_',TAG,'.tab',sep="")
  outSRname <- paste(OUTDIR,'BPK_LARC_STUDENT_RECORD_',TAG,'.tab',sep="")
  
  #read in the tables here. The initial read-in is slow if you use bz2. The lsc tables is > 4GB. May be a problem,
  #i recommend unzipping them outside of R.
  #lsi <- read.csv(lsiname,sep=",")
  #lst <- read.csv(lstname,sep=",")
  #lsc <- read.csv(lscname,sep=",")
  
  library(tidyverse)
  jj  <- student_info_cols()
  lsi <- read_csv(lsiname,col_types=jj)
  lsi <- as.data.frame(lsi)#[-problems(lsi)$row,])
  
  jj  <- student_info_term_cols()
  lst <- read_csv(lstname,col_types=jj)
  lst <- as.data.frame(lst)#[-problems(lst)$row,])
  
  jj  <- course_term_cols()
  lsc <- read_csv(lscname,col_types=jj)
  lsc <- as.data.frame(lsc)#[-problems(lsc)$row,])
  
  #clean things up in the tables
  lsc <- reduce.lsc.table(lsc,lst)
  lsi <- reduce.lsi.table(lsi)
  lsi <- add.graduating.gpa(lst,lsi)
  e   <- lsi$CRER_LVL_CD == 'U' | is.na(lsi$CRER_LVL_CD) #added this NA b/c a lot of empties come out of lst...not sure why.
  lsi <- lsi[e,]
  lst <- reduce.lst.table(lst)
  
  
  # do ONLY outerjoins on everything
  lsi <- merge(lsi,lst,by='STDNT_ID',all.x=TRUE,all.y=TRUE)
  
  #Finall, add on the STEM_TRACK field form the SC table.
  st <- lsc[,names(lsc) %in% c('STDNT_ID','STEM_START')]
  st <- st[!duplicated(st$STDNT_ID),]
  lsi <- merge(lsi,st,by='STDNT_ID',all.x=TRUE)
  
  #write things to disk
  write.table(lsc,outSCname,row.names=FALSE,quote=FALSE,sep="\t")
  write.table(lsi,outSRname,row.names=FALSE,quote=FALSE,sep="\t")
  
}


#This pulls the student info table columns. In the future, it will include any processing we need.
reduce.lsi.table <- function(lsi)
{
  
  source('/Users/bkoester/Google Drive/code/REBUILD/LARC.GITHUB/larc.fill.act.score.R')
  
  #the basic columns to keep
  cols <- c("STDNT_ID","STDNT_GNDR_SHORT_DES","STDNT_ETHNC_GRP_SHORT_DES","STDNT_BIRTH_MO","STDNT_BIRTH_YR",
            "EST_GROSS_FAM_INC_CD","HS_GPA","PRNT_MAX_ED_LVL_CD",
            "MAX_ACT_COMP_SCR","MAX_ACT_ENGL_SCR","MAX_ACT_MATH_SCR",
            "MAX_SATI_TOTAL_CALC_SCR","MAX_SATI_MATH_SCR","MAX_SATI_VERB_SCR",
            "FIRST_US_PRMNNT_RES_PSTL_5_CD","FRST_FRGN_PRMNNT_RES_CNTRY_CD","STDNT_CTZN_STAT_CD","HS_STATE_CD",
            "LAST_TERM_ATTND_CD","FIRST_TERM_ATTND_CD","FIRST_TERM_ATTND_SHORT_DES","LAST_TERM_ATTND_SHORT_DES",
            "UM_UG_DGR_MAJOR_CNT",
            "UM_DGR_1_CMPLTN_TERM_CD","UM_DGR_2_CMPLTN_TERM_CD",
            "UM_DGR_1_MAJOR_1_DES","UM_DGR_1_MAJOR_2_DES","UM_DGR_2_MAJOR_1_DES","UM_DGR_2_MAJOR_2_DES",
            "UM_DGR_1_ACAD_CRER_CD","UM_DGR_2_ACAD_CRER_CD",
            "UM_DGR_1_MAJOR_1_CIP_CD","UM_DGR_1_MAJOR_1_CIP_DES","UM_DGR_1_MAJOR_2_CIP_CD","UM_DGR_1_MAJOR_2_CIP_DES",
            "UM_DGR_2_MAJOR_1_CIP_CD","UM_DGR_2_MAJOR_1_CIP_DES","UM_DGR_2_MAJOR_2_CIP_CD","UM_DGR_2_MAJOR_2_CIP_DES",
            "STDNT_ASIAN_IND","STDNT_BLACK_IND","STDNT_HWIAN_IND",               
            "STDNT_HSPNC_IND","STDNT_NTV_AMRCN_IND","STDNT_WHITE_IND","STDNT_HSPNC_LATINO_IND",
            "STDNT_MULTI_ETHNC_IND","STDNT_HSPNC_LATINO_IND","STDNT_UNDREP_MNRTY_IND")
            
  
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
  
  #add a STEM flag to the degrees
  nst  <- dim(lsi)[1]
  stem <- read.delim("/Users/bkoester/Google Drive/code/REBUILD/LARC.GITHUB/stem_dec2016.txt",header=FALSE,sep="\t")
  STEM_DGR_1_MAJOR_1_FLAG <- mat.or.vec(nst,1)
  STEM_DGR_1_MAJOR_2_FLAG <- STEM_DGR_1_MAJOR_1_FLAG 
  STEM_DGR_2_MAJOR_1_FLAG <- STEM_DGR_1_MAJOR_1_FLAG 
  STEM_DGR_2_MAJOR_2_FLAG <- STEM_DGR_1_MAJOR_1_FLAG 
  
  nstem   <- dim(stem)[1]
  STEMCIP <- mat.or.vec(nstem,1) 
  for (i in 1:nstem){STEMCIP[i] <- strsplit(as.character(stem$V1[i])," ")[[1]][2]}
  #now explicity remove Psych BS/BA
  ee <- STEMCIP != 42.2704
  STEMCIP <- STEMCIP[ee]
  
  
  STEM_DGR_1_MAJOR_1_FLAG[lsi$UM_DGR_1_MAJOR_1_CIP_CD %in% STEMCIP] <- 1
  STEM_DGR_1_MAJOR_2_FLAG[lsi$UM_DGR_1_MAJOR_2_CIP_CD %in% STEMCIP] <- 1
  STEM_DGR_2_MAJOR_1_FLAG[lsi$UM_DGR_2_MAJOR_1_CIP_CD %in% STEMCIP] <- 1
  STEM_DGR_2_MAJOR_2_FLAG[lsi$UM_DGR_2_MAJOR_2_CIP_CD %in% STEMCIP] <- 1
  
  #Catch all the general CIP codes. This is only for the ICE list
  e1 <- flag.general.stem(lsi$UM_DGR_1_MAJOR_1_CIP_CD)
  e2 <- flag.general.stem(lsi$UM_DGR_1_MAJOR_2_CIP_CD)
  e3 <- flag.general.stem(lsi$UM_DGR_2_MAJOR_1_CIP_CD)
  e4 <- flag.general.stem(lsi$UM_DGR_2_MAJOR_2_CIP_CD)
  STEM_DGR_1_MAJOR_1_FLAG[e1] <- 1
  STEM_DGR_1_MAJOR_2_FLAG[e2] <- 1
  STEM_DGR_2_MAJOR_1_FLAG[e3] <- 1
  STEM_DGR_2_MAJOR_2_FLAG[e4] <- 1
  
  STEM_DGR_1_MAJOR_1_FLAG[is.na(lsi$UM_DGR_1_MAJOR_1_CIP_CD)] <- NA
  STEM_DGR_1_MAJOR_2_FLAG[is.na(lsi$UM_DGR_1_MAJOR_2_CIP_CD)] <- NA
  STEM_DGR_2_MAJOR_1_FLAG[is.na(lsi$UM_DGR_2_MAJOR_1_CIP_CD)] <- NA
  STEM_DGR_2_MAJOR_2_FLAG[is.na(lsi$UM_DGR_2_MAJOR_2_CIP_CD)] <- NA
  
  out <- data.frame(out,STEM_DGR_1_MAJOR_1_FLAG,STEM_DGR_2_MAJOR_1_FLAG,STEM_DGR_1_MAJOR_2_FLAG,STEM_DGR_2_MAJOR_2_FLAG)
  
  #fill in the missing ACTs
  out <- larc.fill.act.score(out)
  
  #add in the median zip income
  out <- add.zipcode.county.geoid(out)
  
  #add in the US REGION
  US_REGION <- regional.state.codes(as.character(out$HS_STATE_CD))
  out       <- data.frame(out,US_REGION)
  
  #in the major division...only for dgr_1_major_1!
  out   <- add.major.division(out)
  
  #add in the first gen flag
  e <- out$PRNT_MAX_ED_LVL_CD %in% c(202,203,204,206,205,19,61)
  FIRST_GEN <- mat.or.vec(dim(out)[1],1)
  FIRST_GEN[which(e)] <- 1
  e <- is.na(FIRST_GEN)
  FIRST_GEN[e] <- 0
  out <- data.frame(out,FIRST_GEN)
  
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
  CIPCD      <- DECL
  DECL_TERM  <- DECL
  
  print('intending to use NSF STEM CIP codes!')
  stem <- read.delim("/Users/bkoester/Google Drive/code/REBUILD/LARC.GITHUB/stem_dec2016.txt",header=FALSE,sep="\t")
  STEM_DECL_FLAG_1 <- mat.or.vec(nid,1)
  STEM_DECL_FLAG_2 <- STEM_DECL_FLAG_1
  STEM_DECL_FLAG_3 <- STEM_DECL_FLAG_1
  STEM_DECL_FLAG_4 <- STEM_DECL_FLAG_1
  STEM_DECL_FLAG_5 <- STEM_DECL_FLAG_1
  nstem   <- dim(stem)[1]
  STEMCIP <- mat.or.vec(nstem,1) 
  for (i in 1:nstem){STEMCIP[i] <- strsplit(as.character(stem$V1[i])," ")[[1]][2]}
  #now explicity remove Psych BS/BA
  ee <- STEMCIP != 42.2704
  STEMCIP <- STEMCIP[ee]
  
  STDNT_ID   <- mat.or.vec(nid,1)
  #TERM_START <- STDNT_ID
  #TERM_START_SHORT_DES <- STDNT_ID
  
  PGM_CHECK <- c("PGM_1_MAJOR_1_DES","PGM_1_MAJOR_2_DES","PGM_2_MAJOR_1_DES","PGM_2_MAJOR_2_DES")
  CIP_CHECK <- c("PGM_1_MAJOR_1_CIP_CD","PGM_1_MAJOR_2_CIP_CD","PGM_2_MAJOR_1_CIP_CD","PGM_2_MAJOR_2_CIP_CD")
  pgm_inds  <- which(names(lst) %in% PGM_CHECK)
  cip_inds  <- which(names(lst) %in% CIP_CHECK)
  
  for (i in 1:nid)
  {
    
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind    <- c(start_ind:stop_ind)
    temp   <- lst[ind,]
    terms  <- temp$TERM_CD
    nterms <- length(terms)
    
    if (i %% 10000 == 0)
    {
      print(paste('Counting declarations',i,'of',nid,'STDNT_IDs',sep=" "))
    }
    
    mvec   <- mat.or.vec(nterms*4,2)
    cvec   <- mvec
    tvec   <- mvec
    
    for (j in 1:nterms)
    {
        m1 <- as.character(temp[j,pgm_inds[1]])  
        m2 <- as.character(temp[j,pgm_inds[2]])
        m3 <- as.character(temp[j,pgm_inds[3]])
        m4 <- as.character(temp[j,pgm_inds[4]])
        
        c1 <- as.character(temp[j,cip_inds[1]])  
        c2 <- as.character(temp[j,cip_inds[2]])
        c3 <- as.character(temp[j,cip_inds[3]])
        c4 <- as.character(temp[j,cip_inds[4]])
        
        mvec[((j-1)*4+1):((j)*4),1] <- c(terms[j],terms[j],terms[j],terms[j])
        mvec[((j-1)*4+1):((j)*4),2] <- as.vector(c(m1,m2,m3,m4))
        cvec[((j-1)*4+1):((j)*4),2] <- as.vector(c(c1,c2,c3,c4))
    }
    
    e <- mvec[,2] != ""
    mvec <- mvec[e,]
    cvec <- cvec[e,]
    if (length(mvec) > 2)
    {
      tm   <- !duplicated(mvec[,2])
      mvec <- mvec[tm,]
      cvec <- cvec[tm,]
    }
    
    if (is.null(dim(mvec)))
    {
      DECL[i,1] <- mvec[2]
      DECL_TERM[i,1] <- mvec[1]
      CIPCD[i,1] <- cvec[2]
    }
    
    if (!is.null(dim(mvec))) 
    {
      tm    <- !duplicated(mvec[,2])
      mvec <- mvec[tm,]
      cvec <- cvec[tm,]
      ndecl <- dim(mvec)[1]
        if (ndecl > 5){ndecl <- 5}
        if (dim(mvec)[1] > 0)
        {
          DECL[i,(1:ndecl)]      <- mvec[(1:ndecl),2]
          DECL_TERM[i,(1:ndecl)] <- mvec[(1:ndecl),1]
          CIPCD[i,(1:ndecl)]     <- cvec[(1:ndecl),2]
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
  
  STEM_DECL_FLAG_1[CIPCD[,1] %in% STEMCIP] <- 1
  STEM_DECL_FLAG_2[CIPCD[,2] %in% STEMCIP] <- 1
  STEM_DECL_FLAG_3[CIPCD[,3] %in% STEMCIP] <- 1
  STEM_DECL_FLAG_4[CIPCD[,4] %in% STEMCIP] <- 1
  STEM_DECL_FLAG_5[CIPCD[,5] %in% STEMCIP] <- 1
  
  #Now, catch the all-inclusive CIP codes that we've missed and label NA in the declare fields as NA in the FLAGs
  e1 <- flag.general.stem(CIPCD[,1])
  e2 <- flag.general.stem(CIPCD[,2])
  e3 <- flag.general.stem(CIPCD[,3])
  e4 <- flag.general.stem(CIPCD[,4])
  e5 <- flag.general.stem(CIPCD[,5])
  STEM_DECL_FLAG_1[e1] <- 1
  STEM_DECL_FLAG_2[e2] <- 1
  STEM_DECL_FLAG_3[e3] <- 1
  STEM_DECL_FLAG_4[e4] <- 1
  STEM_DECL_FLAG_5[e5] <- 1
  
  STEM_DECL_FLAG_1[which(is.na(DECLARE1))] <- NA
  STEM_DECL_FLAG_2[which(is.na(DECLARE2))] <- NA
  STEM_DECL_FLAG_3[which(is.na(DECLARE3))] <- NA
  STEM_DECL_FLAG_4[which(is.na(DECLARE4))] <- NA
  STEM_DECL_FLAG_5[which(is.na(DECLARE5))] <- NA
  
  out <- data.frame(STDNT_ID,
                    DECLARE1,DECLARE1_TERM,DECLARE2,DECLARE2_TERM,
                    DECLARE3,DECLARE3_TERM,DECLARE4,DECLARE4_TERM, 
                    DECLARE5,DECLARE5_TERM,
                    STEM_DECL_FLAG_1,STEM_DECL_FLAG_2,STEM_DECL_FLAG_3,STEM_DECL_FLAG_4,STEM_DECL_FLAG_5)
  View(out)
  return(out)
}


flag.general.stem <- function(vec)
{
  e <- (vec >= 14 & vec < 15) |  (vec >= 26 & vec < 28) | (vec >= 40 & vec < 41)
  print(length(which(e)))
  return(which(e))
}

#select student course columns form the studnet course table, keeping ONLY courses taken for a grade.
reduce.lsc.table <- function(lsc,lst)
{
  
  source('/Users/bkoester/Google Drive/code/REBUILD/LARC.GITHUB/larc.cluster.grade.patterns.R')
  
  print('filling out SC table')
  #tables cuts here
  e    <- lsc$GRD_BASIS_ENRL_DES == 'Graded'
  lsc  <- lsc[which(e),]
  
  #fix b-school grades
  lsc$GRD_PNTS_PER_UNIT_NBR[which(lsc$GRD_PNTS_PER_UNIT_NBR == 3.4)] <- 3.3
  lsc$GRD_PNTS_PER_UNIT_NBR[which(lsc$GRD_PNTS_PER_UNIT_NBR == 2.4)] <- 2.3
  lsc$GRD_PNTS_PER_UNIT_NBR[which(lsc$GRD_PNTS_PER_UNIT_NBR == 1.4)] <- 1.3
  lsc$GRD_PNTS_PER_UNIT_NBR[which(lsc$GRD_PNTS_PER_UNIT_NBR == 4.4 | lsc$GRD_PNTS_PER_UNIT_NBR == 4.3)] <- 4.0
  
  
  cols <- c("STDNT_ID","TERM_CD","TERM_SHORT_DES","CLASS_NBR","CRSE_GRD_OFFCL_CD",
            "UNITS_ERND_NBR","GRD_PNTS_PER_UNIT_NBR","EXCL_CLASS_CUM_GPA",
            "SBJCT_CD","CATLG_NBR","CRSE_ID_CD","CLASS_SCTN_CD","ASSOC_CLASS_CD",
            "CRSE_CMPNT_CD","CRSE_CIP_CD","CRSE_CIP_DES")
  data <- lsc[,cols]
  
  #flag the stem courses
  clist <- c('AERO','AEROSP','ANAT','ANATOMY','ANESTH','AOSS','APPPHYS','ASTRO','AUTO',
             'BIOINF','BIOLCHEM','BIOLOGY','BIOMATLS','BIOMEDE','BIOPHYS','BIOSTAT',
             'BOTANY','CANCBIO','CEE','CHE','CHEM','CHEMBIO','CLIMATE','CMPLXSYS','CMPTRSC', #COGSCI
             'CS','EARTH','EEB','EECS','ENGR','ENSCEN','ENVIRON','ENVRNSTD','EPID','ESENG',
             'GEOSCI','HUMGEN','IOE',
             'MACROMOL','MATH','MATSCIE','MCDB','MECHENG','MEDCHEM','MEMS','MFG','MICROBIOL',
             'NAVARCH','MILSCI','NAVSCI','NERS','NEUROL','NEUROSCI',
             'PHARMACY','PHARMADM','PHARMCEU','PHARMCHM','PHARMCOG','PHARMSCI','PHYSICS','PHYSIOL',
             'PIBS','PUBHLTH', #PYSCH
             'RADIOL','SI','STATS','SPACE','ZOOLOGY')
  
  ncrse        <- dim(data)[1]
  STEM_COURSE  <- mat.or.vec(ncrse,1)
  e            <- data$SBJCT_CD %in% clist
  STEM_COURSE[e]   <- 1
  data          <- data.frame(data,STEM_COURSE)
  
  
  #merge in the total credits at the end of each term a class was taken.
  temp <- lst[,names(lst) %in% c('STDNT_ID','TERM_CD','ACAD_LVL_BOT_SHORT_DES','PRMRY_CRER_CD')]
  data <- merge(data,temp,by=c('STDNT_ID','TERM_CD'))
  
  
  #add an end-of-term GPA
  data        <- data[order(data$STDNT_ID,data$TERM_CD), ]
  data$count  <- sequence(rle(as.vector(data$STDNT_ID))$lengths)
  ntot       <- length(data$STDNT_ID)
  nid        <- length(data$STDNT_ID[!duplicated(data$STDNT_ID)])
  nstart     <- which(data$count == 1)
  
  EOT_GPA    <- mat.or.vec(ntot,1)
  BOT_GPA    <- mat.or.vec(ntot,1)
  BOT_GPA[]  <- NA
  STEM_START <- EOT_GPA
  
  for (i in 1:nid)
  {
    #print(i)
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind    <- c(start_ind:stop_ind)
    
    sub    <- data[ind,]
    terms  <- sub$TERM_CD[!duplicated(sub$TERM_CD)]
    nterms <- length(terms)
    
    flag   <- 0
    
    for (j in 1:nterms)
    {
      h <- which(sub$TERM_CD <= terms[j])
      m <- which(sub$TERM_CD == terms[j])
      d <- which(sub$TERM_CD < terms[j])
      if (flag == 0 & (grepl('FA',sub$TERM_SHORT_DES) | grepl('WN',sub$TERM_SHORT_DES)))
      {
        STEM_START[ind] <- sum(sub$STEM_COURSE[m])
        flag <- 1
      }
      EOT_GPA[ind[m]] <- sum(sub$GRD_PNTS_PER_UNIT_NBR[h]*sub$UNITS_ERND_NBR[h],na.rm=TRUE)/sum(sub$UNITS_ERND_NBR[h],na.rm=TRUE)
      if (length(d) > 0)
      {
        BOT_GPA[ind[m]] <- sum(sub$GRD_PNTS_PER_UNIT_NBR[d]*sub$UNITS_ERND_NBR[d],na.rm=TRUE)/sum(sub$UNITS_ERND_NBR[d],na.rm=TRUE)
      }
    }
    
  }
  
  
  #now do the clustering
  out <- data.frame(data,EOT_GPA,BOT_GPA,STEM_START)
  out <- larc.cluster.grade.patterns(out)
  
  #and get the fixed effects for the undergrad grades only. the CFE, SFE, and CMEAN will be NA for all others.
  e <- out$CRSE_GRD_OFFCL_CD != 'W' & grepl('^U',out$PRMRY_CRER_CD) #& out$CATLG_NBR < 500
  FE <- student_course_fixed_effect(out$GRD_PNTS_PER_UNIT_NBR[e],out$STDNT_ID[e],out$CLASS_NBR[e],tol=0.001)
  out$SFE <- NA
  out$CFE <- NA
  out$CMEAN <- NA
  
  out$SFE[e]   <- FE$SFE
  out$CFE[e]   <- FE$CFE
  out$CMEAN[e] <- FE$CMEAN
  
  
  return(out)
  
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

#classify national region by high school state CD 
regional.state.codes <- function(twoCD)
{
  US_REGION <- mat.or.vec(length(twoCD),1)
  US_REGION[] <- 'OTH/UNK'
  
  s   <- twoCD %in% c('FL','AL','GA','SC','NC','LA','MS','TX','AR','TN')
  w   <- twoCD %in% c('CA','OR','WA','MT','ID','NV','UT','AZ','NM','CO','WY','HI','AK')
  gl  <- twoCD %in% c('IL','WI','OH','IN')
  mw  <- twoCD %in% c('MO','KS','NE','IA','MN','SD','ND','KY','OK','WV')
  mi  <- twoCD %in% 'MI'
  ca  <- twoCD %in% c('ON','BC','QC','AB')   
  e   <- twoCD %in% c('MN','VT','NH','NY','PA','RI','NJ','MA','VA','DC','CT','DE')
  is  <- twoCD %in% c('PR','GU')
  
  US_REGION[s] <- 'SOUTH'
  US_REGION[w] <- 'WEST'
  US_REGION[gl] <- 'GL'
  US_REGION[mw] <- 'MIDWEST'
  US_REGION[mi] <- 'MI'
  US_REGION[ca] <- 'CA'
  US_REGION[e]  <- 'EAST'
  US_REGION[is]  <- 'PR/GU'
  
  return(US_REGION)
}

student_course_fixed_effect <- function(grade,student,course,tol=0.001)
{
  k <- ave(grade,student,FUN='mean')
  a <- ave(grade,course,FUN='mean')
  CMEAN   <- a
  STDMEAN <- k
  ADJGRD  <- grade-CMEAN
  
  
  #a[] <- 0
  
  k_old <- k
  a_old <- a
  a_diff_max <- 100
  k_diff_max <- 100
  
  flag <- 0
  
  ssdelta <- 0
  kdelta  <- 0
  adelta  <- 0
  cts <- 1
  
  while ((a_diff_max > tol) | (k_diff_max > tol))
  {
    print(cts)
    k <- ave(grade-a_old,student,FUN='mean') #your grade - course grade = residual student
    a <- ave(grade-k,course,FUN='mean')      #your grade - student grades = actual course
    
    kdiff <- abs(k-k_old)
    adiff <- abs(a-a_old)
    
    k_old <- k
    a_old <- a
    
    ss <- sum((grade-k-a)^2.,na.rm=TRUE)
    
    a_diff_max <- max(kdiff,na.rm=TRUE)
    k_diff_max <- max(adiff,na.rm=TRUE)
    ssdelta <- c(ssdelta,ss)
    kdelta  <- c(kdelta,k_diff_max)
    adelta  <- c(adelta,a_diff_max)
    print(k_diff_max)
    print(a_diff_max)
    
    cts <-  cts + 1
  }
  
  SFE <- k
  CFE <- a
  
  data <- data.frame(SFE,CFE,CMEAN,STDMEAN,ADJGRD)
  diagnostics <- data.frame(ssdelta,adelta,kdelta)
  
  return(data)
  
}

student_info_cols <- function()
{
  
  tt <- cols(
    SNPSHT_RPT_DT = col_character(),
    STDNT_ID = col_double(),
    STDNT_GNDR_CD = col_double(),
    STDNT_GNDR_SHORT_DES = col_character(),
    STDNT_BIRTH_YR = col_double(),
    STDNT_BIRTH_MO = col_double(),
    STDNT_ASIAN_IND = col_double(),
    STDNT_BLACK_IND = col_double(),
    STDNT_HWIAN_IND = col_double(),
    STDNT_HSPNC_IND = col_double(),
    STDNT_NTV_AMRCN_IND = col_double(),
    STDNT_WHITE_IND = col_double(),
    STDNT_ETHNC_GRP_CD = col_double(),
    STDNT_ETHNC_GRP_SHORT_DES = col_character(),
    STDNT_MULTI_ETHNC_IND = col_double(),
    STDNT_HSPNC_LATINO_IND = col_double(),
    STDNT_UNDREP_MNRTY_IND = col_double(),
    STDNT_NTV_ENG_SPKR_IND = col_double(),
    FIRST_US_PRMNNT_RES_PSTL_CD = col_character(),
    FIRST_US_PRMNNT_RES_PSTL_5_CD = col_character(),
    FRST_FRGN_PRMNNT_RES_CNTRY_CD = col_character(),
    FRST_FRGN_PRMNNT_RES_CNTRY_DES = col_character(),
    STDNT_CTZN_STAT_CD = col_character(),
    STDNT_CTZN_STAT_SHORT_DES = col_character(),
    STDNT_CTZN_CNTRY_1_CD = col_character(),
    STDNT_CTZN_CNTRY_1_DES = col_character(),
    STDNT_CTZN_CNTRY_2_CD = col_character(),
    STDNT_CTZN_CNTRY_2_DES = col_character(),
    STDNT_INTL_IND = col_double(),
    FIRST_TERM_ATTND_CD = col_character(),
    FIRST_TERM_ATTND_SHORT_DES = col_character(),
    FIRST_TERM_ATTND_BEGIN_YR_MO = col_character(),
    FIRST_TERM_ATTND_END_YR_MO = col_character(),
    LAST_TERM_ATTND_CD = col_character(),
    LAST_TERM_ATTND_SHORT_DES = col_character(),
    LAST_TERM_ATTND_BEGIN_YR_MO = col_character(),
    LAST_TERM_ATTND_END_YR_MO = col_character(),
    ADMSSN_VTRN_IND = col_double(),
    PRNT_MAX_ED_LVL_CD = col_double(),
    PRNT_MAX_ED_LVL_DES = col_character(),
    PRNT_DEP_NBR_CD = col_double(),
    PRNT_DEP_NBR_DES = col_character(),
    EST_GROSS_FAM_INC_CD = col_double(),
    EST_GROSS_FAM_INC_DES = col_character(),
    SNGL_PRNT_IND = col_double(),
    HS_CALC_IND = col_double(),
    HS_CHEM_LAB_IND = col_double(),
    HS_GPA = col_double(),
    HS_CEEB_CD = col_character(),
    HS_CITY_NM = col_character(),
    HS_STATE_CD = col_character(),
    HS_PSTL_CD = col_character(),
    HS_DSCRPTR_PLUS_CLSTR_CD = col_character(),
    HS_DSCRPTR_PLUS_CLSTR_DES = col_character(),
    NGHBRHD_DSCRPTR_PLUS_CLSTR_CD = col_character(),
    NGHBRHD_DSCRPTR_PLUS_CLSTR_DES = col_character(),
    MAX_ACT_TEST_DT = col_character(),
    MAX_ACT_ENGL_SCR = col_double(),
    MAX_ACT_ENGL_PCTL = col_double(),
    MAX_ACT_MATH_SCR = col_double(),
    MAX_ACT_MATH_PCTL = col_double(),
    MAX_ACT_READ_SCR = col_double(),
    MAX_ACT_READ_PCTL = col_double(),
    MAX_ACT_SCIRE_SCR = col_double(),
    MAX_ACT_SCIRE_PCTL = col_double(),
    MAX_ACT_COMP_SCR = col_double(),
    MAX_ACT_COMP_PCTL = col_double(),
    MAX_ACT_EW_SCR = col_double(),
    MAX_ACT_WR_SUB_SCR = col_double(),
    MAX_ACT_WRS_SCR = col_double(),
    MAX_ACT_WRS_PCTL = col_double(),
    MAX_ACT_WRT_SCR = col_double(),
    MAX_ACT_WRT_PCTL = col_double(),
    MAX_ACT_WDIA_SCR = col_double(),
    MAX_ACT_WDDS_SCR = col_double(),
    MAX_ACT_WDO_SCR = col_double(),
    MAX_ACT_WDLC_SCR = col_double(),
    MAX_ACT_ELA_SCR = col_double(),
    MAX_ACT_ELA_PCTL = col_double(),
    MAX_ACT_STEM_SCR = col_double(),
    MAX_ACT_STEM_PCTL = col_double(),
    MAX_SATI_TEST_DT = col_character(),
    MAX_SATI_MATH_SCR = col_double(),
    MAX_SATI_MATH_PCTL = col_double(),
    MAX_SATI_VERB_SCR = col_double(),
    MAX_SATI_VERB_PCTL = col_double(),
    MAX_SATI_TOTAL_CALC_SCR = col_double(),
    MAX_SATI_WR_SCR = col_double(),
    MAX_SATI_ES_SUB_SCR = col_double(),
    MAX_SATI_MC_SUB_SCR = col_double(),
    MAX_SATI_TOTAL_MSS_ERWS_SCR = col_double(),
    MAX_SATI_TOTAL_MSS_ERWS_PCTL = col_double(),
    MAX_SATI_ERWS_SCR = col_double(),
    MAX_SATI_ERWS_PCTL = col_double(),
    MAX_SATI_MSS_SCR = col_double(),
    MAX_SATI_MSS_PCTL = col_double(),
    MAX_SATI_RT_SCR = col_double(),
    MAX_SATI_RT_PCTL = col_double(),
    MAX_SATI_WLT_SCR = col_double(),
    MAX_SATI_WLT_PCTL = col_double(),
    MAX_SATI_MT_SCR = col_double(),
    MAX_SATI_MT_PCTL = col_double(),
    MAX_SATI_ASC_X_SCR = col_double(),
    MAX_SATI_ASC_X_PCTL = col_double(),
    MAX_SATI_AHSSC_X_SCR = col_double(),
    MAX_SATI_AHSSC_X_PCTL = col_double(),
    MAX_SATI_RWC_SUB_SCR = col_double(),
    MAX_SATI_RWC_SUB_SCR_PCTL = col_double(),
    MAX_SATI_CE_SUB_SCR = col_double(),
    MAX_SATI_CE_SUB_SCR_PCTL = col_double(),
    MAX_SATI_EI_SUB_SCR = col_double(),
    MAX_SATI_EI_SUB_SCR_PCTL = col_double(),
    MAX_SATI_SEC_SUB_SCR = col_double(),
    MAX_SATI_SEC_SUB_SCR_PCTL = col_double(),
    MAX_SATI_HA_SUB_SCR = col_double(),
    MAX_SATI_HA_SUB_SCR_PCTL = col_double(),
    MAX_SATI_PAM_SUB_SCR = col_double(),
    MAX_SATI_PAM_SUB_SCR_PCTL = col_double(),
    MAX_SATI_PSDA_SUB_SCR = col_double(),
    MAX_SATI_PSDA_SUB_SCR_PCTL = col_double(),
    MAX_SATI_ESR_SUB_SCR = col_double(),
    MAX_SATI_ESA_SUB_SCR = col_double(),
    MAX_SATI_ESW_SUB_SCR = col_double(),
    MAX_GRE_TEST_DT = col_character(),
    MAX_GRE_ANLY_SCR = col_double(),
    MAX_GRE_ANLY_PCTL = col_double(),
    MAX_GRE_QNT2_SCR = col_double(),
    MAX_GRE_QNT2_PCTL = col_double(),
    MAX_GRE_VRB2_SCR = col_double(),
    MAX_GRE_VRB2_PCTL = col_double(),
    MAX_GRE_QUAN_SCR = col_double(),
    MAX_GRE_QUAN_PCTL = col_double(),
    MAX_GRE_VERB_SCR = col_double(),
    MAX_GRE_VERB_PCTL = col_double(),
    MAX_GRE_WR_SCR = col_double(),
    MAX_GRE_WR_PCTL = col_double(),
    MAX_GRE_TOT_SCR = col_double(),
    MAX_TOEFL_TEST_DT = col_character(),
    MAX_TOEFL_TEST_TYP_CD = col_character(),
    MAX_TOEFL_TOTAL_SCR = col_double(),
    MAX_TOEFL_LIST_SCR = col_double(),
    MAX_TOEFL_READ_SCR = col_double(),
    MAX_TOEFL_WRIT_SCR = col_double(),
    MAX_TOEFL_ESSY_SCR = col_double(),
    MAX_TOEFL_SPK_SCR = col_double(),
    MAX_TOEFL_TEST_ADMIN_CD = col_character(),
    MAX_AP_AMGVT_TEST_DT = col_character(),
    MAX_AP_AMGVT_TEST_SCR = col_double(),
    MAX_AP_AMHIS_TEST_DT = col_character(),
    MAX_AP_AMHIS_TEST_SCR = col_double(),
    MAX_AP_ART2D_TEST_DT = col_character(),
    MAX_AP_ART2D_TEST_SCR = col_double(),
    MAX_AP_ART3D_TEST_DT = col_character(),
    MAX_AP_ART3D_TEST_SCR = col_double(),
    MAX_AP_ARTDR_TEST_DT = col_character(),
    MAX_AP_ARTDR_TEST_SCR = col_double(),
    MAX_AP_ARTGN_TEST_DT = col_character(),
    MAX_AP_ARTGN_TEST_SCR = col_double(),
    MAX_AP_ARTHS_TEST_DT = col_character(),
    MAX_AP_ARTHS_TEST_SCR = col_double(),
    MAX_AP_BY_TEST_DT = col_character(),
    MAX_AP_BY_TEST_SCR = col_double(),
    MAX_AP_CALAB_TEST_DT = col_character(),
    MAX_AP_CALAB_TEST_SCR = col_double(),
    MAX_AP_CALAB_TEST_SUB_SCR_DT = col_character(),
    MAX_AP_CALAB_TEST_SUB_SCR = col_double(),
    MAX_AP_CALBC_TEST_DT = col_character(),
    MAX_AP_CALBC_TEST_SCR = col_double(),
    MAX_AP_CH_TEST_DT = col_character(),
    MAX_AP_CH_TEST_SCR = col_double(),
    MAX_AP_CHINA_TEST_DT = col_character(),
    MAX_AP_CHINA_TEST_SCR = col_double(),
    MAX_AP_CPGVT_TEST_DT = col_character(),
    MAX_AP_CPGVT_TEST_SCR = col_double(),
    MAX_AP_CSA_TEST_DT = col_character(),
    MAX_AP_CSA_TEST_SCR = col_double(),
    MAX_AP_CSAB_TEST_DT = col_character(),
    MAX_AP_CSAB_TEST_SCR = col_double(),
    MAX_AP_CSPRC_TEST_DT = col_character(),
    MAX_AP_CSPRC_TEST_SCR = col_double(),
    MAX_AP_EH_TEST_DT = col_character(),
    MAX_AP_EH_TEST_SCR = col_double(),
    MAX_AP_EL_TEST_DT = col_character(),
    MAX_AP_EL_TEST_SCR = col_double(),
    MAX_AP_ENGL_TEST_DT = col_character(),
    MAX_AP_ENGL_TEST_SCR = col_double(),
    MAX_AP_ENVSC_TEST_DT = col_character(),
    MAX_AP_ENVSC_TEST_SCR = col_double(),
    MAX_AP_FR_TEST_DT = col_character(),
    MAX_AP_FR_TEST_SCR = col_double(),
    MAX_AP_FRLIT_TEST_DT = col_character(),
    MAX_AP_FRLIT_TEST_SCR = col_double(),
    MAX_AP_GM_TEST_DT = col_character(),
    MAX_AP_GM_TEST_SCR = col_double(),
    MAX_AP_HGEOG_TEST_DT = col_character(),
    MAX_AP_HGEOG_TEST_SCR = col_double(),
    MAX_AP_IT_TEST_DT = col_character(),
    MAX_AP_IT_TEST_SCR = col_double(),
    MAX_AP_JAPN_TEST_DT = col_character(),
    MAX_AP_JAPN_TEST_SCR = col_double(),
    MAX_AP_LTLIT_TEST_DT = col_character(),
    MAX_AP_LTLIT_TEST_SCR = col_double(),
    MAX_AP_LTVER_TEST_DT = col_character(),
    MAX_AP_LTVER_TEST_SCR = col_double(),
    MAX_AP_MACEC_TEST_DT = col_character(),
    MAX_AP_MACEC_TEST_SCR = col_double(),
    MAX_AP_MICEC_TEST_DT = col_character(),
    MAX_AP_MICEC_TEST_SCR = col_double(),
    MAX_AP_MTAUR_TEST_SUB_SCR_DT = col_character(),
    MAX_AP_MTAUR_SUB_TEST_SCR = col_double(),
    MAX_AP_MTNAU_TEST_SUB_SCR_DT = col_character(),
    MAX_AP_MTNAU_TEST_SUB_SCR = col_double(),
    MAX_AP_MUSTH_TEST_DT = col_character(),
    MAX_AP_MUSTH_TEST_SCR = col_double(),
    MAX_AP_PHYS1_TEST_DT = col_character(),
    MAX_AP_PHYS1_TEST_SCR = col_double(),
    MAX_AP_PHYS2_TEST_DT = col_character(),
    MAX_AP_PHYS2_TEST_SCR = col_double(),
    MAX_AP_PHYSB_TEST_DT = col_character(),
    MAX_AP_PHYSB_TEST_SCR = col_double(),
    MAX_AP_PHYSE_TEST_DT = col_character(),
    MAX_AP_PHYSE_TEST_SCR = col_double(),
    MAX_AP_PHYSM_TEST_DT = col_character(),
    MAX_AP_PHYSM_TEST_SCR = col_double(),
    MAX_AP_PY_TEST_DT = col_character(),
    MAX_AP_PY_TEST_SCR = col_double(),
    MAX_AP_RSRCH_TEST_DT = col_character(),
    MAX_AP_RSRCH_TEST_SCR = col_double(),
    MAX_AP_SEMNR_TEST_DT = col_character(),
    MAX_AP_SEMNR_TEST_SCR = col_double(),
    MAX_AP_SP_TEST_DT = col_character(),
    MAX_AP_SP_TEST_SCR = col_double(),
    MAX_AP_SPLIT_TEST_DT = col_character(),
    MAX_AP_SPLIT_TEST_SCR = col_double(),
    MAX_AP_STAT_TEST_DT = col_character(),
    MAX_AP_STAT_TEST_SCR = col_double(),
    MAX_AP_WHIST_TEST_DT = col_character(),
    MAX_AP_WHIST_TEST_SCR = col_double(),
    MAX_UMPLC_ATPT_TEST_DT = col_character(),
    MAX_UMPLC_ATPT_TEST_SCR = col_double(),
    MAX_UMPLC_ATPT_RCMD_CD = col_character(),
    MAX_UMPLC_ATPT_RCMD_DES = col_character(),
    MAX_UMPLC_CH_TEST_DT = col_character(),
    MAX_UMPLC_CH_TEST_SCR = col_double(),
    MAX_UMPLC_CH_RCMD_CD = col_character(),
    MAX_UMPLC_CH_RCMD_DES = col_character(),
    MAX_UMPLC_CHN_TEST_DT = col_character(),
    MAX_UMPLC_CHN_TEST_SCR = col_double(),
    MAX_UMPLC_CHN_RCMD_CD = col_character(),
    MAX_UMPLC_CHN_RCMD_DES = col_character(),
    MAX_UMPLC_FILIP_TEST_DT = col_character(),
    MAX_UMPLC_FILIP_TEST_SCR = col_double(),
    MAX_UMPLC_FILIP_RCMD_CD = col_character(),
    MAX_UMPLC_FILIP_RCMD_DES = col_character(),
    MAX_UMPLC_FRE_TEST_DT = col_character(),
    MAX_UMPLC_FRLST_TEST_SCR = col_double(),
    MAX_UMPLC_FRRD_TEST_SCR = col_double(),
    MAX_UMPLC_FRE_RCMD_CD = col_character(),
    MAX_UMPLC_FRE_RCMD_DES = col_character(),
    MAX_UMPLC_GM_TEST_DT = col_character(),
    MAX_UMPLC_GM_TEST_SCR = col_double(),
    MAX_UMPLC_GM_RCMD_CD = col_character(),
    MAX_UMPLC_GM_RCMD_DES = col_character(),
    MAX_UMPLC_HBRD_TEST_DT = col_character(),
    MAX_UMPLC_HBRD_TEST_SCR = col_double(),
    MAX_UMPLC_HBRD_RCMD_CD = col_character(),
    MAX_UMPLC_HBRD_RCMD_DES = col_character(),
    MAX_UMPLC_HINDI_TEST_DT = col_character(),
    MAX_UMPLC_HINDI_TEST_SCR = col_double(),
    MAX_UMPLC_HINDI_RCMD_CD = col_character(),
    MAX_UMPLC_HINDI_RCMD_DES = col_character(),
    MAX_UMPLC_INDON_TEST_DT = col_character(),
    MAX_UMPLC_INDON_TEST_SCR = col_double(),
    MAX_UMPLC_INDON_RCMD_CD = col_character(),
    MAX_UMPLC_INDON_RCMD_DES = col_character(),
    MAX_UMPLC_ITRD_TEST_DT = col_character(),
    MAX_UMPLC_ITRD_TEST_SCR = col_double(),
    MAX_UMPLC_ITRD_RCMD_CD = col_character(),
    MAX_UMPLC_ITRD_RCMD_DES = col_character(),
    MAX_UMPLC_JPNPL_TEST_DT = col_character(),
    MAX_UMPLC_JPNPL_TEST_SCR = col_double(),
    MAX_UMPLC_JPNPL_RCMD_CD = col_character(),
    MAX_UMPLC_JPNPL_RCMD_DES = col_character(),
    MAX_UMPLC_KOR_TEST_DT = col_character(),
    MAX_UMPLC_KOR_TEST_SCR = col_double(),
    MAX_UMPLC_KOR_RCMD_CD = col_character(),
    MAX_UMPLC_KOR_RCMD_DES = col_character(),
    MAX_UMPLC_LTSCR_TEST_DT = col_character(),
    MAX_UMPLC_LTSCR_TEST_SCR = col_double(),
    MAX_UMPLC_LTSCR_RCMD_CD = col_character(),
    MAX_UMPLC_LTSCR_RCMD_DES = col_character(),
    MAX_UMPLC_MATH_TEST_DT = col_character(),
    MAX_UMPLC_MATH_TEST_SCR = col_double(),
    MAX_UMPLC_MATH_RCMD_CD = col_character(),
    MAX_UMPLC_MATH_RCMD_DES = col_character(),
    MAX_UMPLC_PUNJA_TEST_DT = col_character(),
    MAX_UMPLC_PUNJA_TEST_SCR = col_double(),
    MAX_UMPLC_PUNJA_RCMD_CD = col_character(),
    MAX_UMPLC_PUNJA_RCMD_DES = col_character(),
    MAX_UMPLC_RSRD_TEST_DT = col_character(),
    MAX_UMPLC_RSRD_TEST_SCR = col_double(),
    MAX_UMPLC_RSRD_RCMD_CD = col_character(),
    MAX_UMPLC_RSRD_RCMD_DES = col_character(),
    MAX_UMPLC_SPA_TEST_DT = col_character(),
    MAX_UMPLC_SPLST_TEST_SCR = col_double(),
    MAX_UMPLC_SPRD_TEST_SCR = col_double(),
    MAX_UMPLC_SPA_RCMD_CD = col_character(),
    MAX_UMPLC_SPA_RCMD_DES = col_character(),
    MAX_UMPLC_THAI_TEST_DT = col_character(),
    MAX_UMPLC_THAI_TEST_SCR = col_double(),
    MAX_UMPLC_THAI_RCMD_CD = col_character(),
    MAX_UMPLC_THAI_RCMD_DES = col_character(),
    MAX_UMPLC_URDU_TEST_DT = col_character(),
    MAX_UMPLC_URDU_TEST_SCR = col_double(),
    MAX_UMPLC_URDU_RCMD_CD = col_character(),
    MAX_UMPLC_URDU_RCMD_DES = col_character(),
    MAX_UMPLC_VIETN_TEST_DT = col_character(),
    MAX_UMPLC_VIETN_TEST_SCR = col_double(),
    MAX_UMPLC_VIETN_RCMD_CD = col_character(),
    MAX_UMPLC_VIETN_RCMD_DES = col_character(),
    UM_UG_DGR_CNT = col_double(),
    UM_UG_DGR_MAJOR_CNT = col_double(),
    UM_UG_DGR_MINOR_CNT = col_double(),
    UM_GRAD_DGR_CNT = col_double(),
    UM_GRAD_DGR_MAJOR_CNT = col_double(),
    UM_GRAD_DGR_MINOR_CNT = col_double(),
    UM_OTHER_DGR_CNT = col_double(),
    UM_DGR_1_CD = col_character(),
    UM_DGR_1_DES = col_character(),
    UM_DGR_1_HONORS_CD = col_character(),
    UM_DGR_1_HONORS_DES = col_character(),
    UM_DGR_1_ACAD_CRER_CD = col_character(),
    UM_DGR_1_ACAD_CRER_DES = col_character(),
    UM_DGR_1_ED_LVL_CD = col_double(),
    UM_DGR_1_ED_LVL_SHORT_DES = col_character(),
    UM_DGR_1_CMPLTN_TERM_CD = col_character(),
    UM_DGR_1_CMPLTN_TERM_DES = col_character(),
    UM_DGR_1_CNFR_DT = col_character(),
    UM_DGR_1_MAJOR_1_CD = col_character(),
    UM_DGR_1_MAJOR_1_DES = col_character(),
    UM_DGR_1_MAJOR_1_CIP_CD = col_character(),
    UM_DGR_1_MAJOR_1_CIP_DES = col_character(),
    UM_DGR_1_MAJOR_2_CD = col_character(),
    UM_DGR_1_MAJOR_2_DES = col_character(),
    UM_DGR_1_MAJOR_2_CIP_CD = col_character(),
    UM_DGR_1_MAJOR_2_CIP_DES = col_character(),
    UM_DGR_1_MINOR_1_CD = col_character(),
    UM_DGR_1_MINOR_1_DES = col_character(),
    UM_DGR_1_MINOR_1_CIP_CD = col_character(),
    UM_DGR_1_MINOR_1_CIP_DES = col_character(),
    UM_DGR_1_MINOR_2_CD = col_character(),
    UM_DGR_1_MINOR_2_DES = col_character(),
    UM_DGR_1_MINOR_2_CIP_CD = col_double(),
    UM_DGR_1_MINOR_2_CIP_DES = col_character(),
    UM_DGR_2_CD = col_character(),
    UM_DGR_2_DES = col_character(),
    UM_DGR_2_HONORS_CD = col_character(),
    UM_DGR_2_HONORS_DES = col_character(),
    UM_DGR_2_ACAD_CRER_CD = col_character(),
    UM_DGR_2_ACAD_CRER_DES = col_character(),
    UM_DGR_2_ED_LVL_CD = col_double(),
    UM_DGR_2_ED_LVL_SHORT_DES = col_character(),
    UM_DGR_2_CMPLTN_TERM_CD = col_character(),
    UM_DGR_2_CMPLTN_TERM_DES = col_character(),
    UM_DGR_2_CNFR_DT = col_character(),
    UM_DGR_2_MAJOR_1_CD = col_character(),
    UM_DGR_2_MAJOR_1_DES = col_character(),
    UM_DGR_2_MAJOR_1_CIP_CD = col_character(),
    UM_DGR_2_MAJOR_1_CIP_DES = col_character(),
    UM_DGR_2_MAJOR_2_CD = col_character(),
    UM_DGR_2_MAJOR_2_DES = col_character(),
    UM_DGR_2_MAJOR_2_CIP_CD = col_character(),
    UM_DGR_2_MAJOR_2_CIP_DES = col_character(),
    UM_DGR_2_MINOR_1_CD = col_character(),
    UM_DGR_2_MINOR_1_DES = col_character(),
    UM_DGR_2_MINOR_1_CIP_CD = col_character(),
    UM_DGR_2_MINOR_1_CIP_DES = col_character(),
    UM_DGR_2_MINOR_2_CD = col_character(),
    UM_DGR_2_MINOR_2_DES = col_character(),
    UM_DGR_2_MINOR_2_CIP_CD = col_character(),
    UM_DGR_2_MINOR_2_CIP_DES = col_character(),
    UM_DGR_3_CD = col_character(),
    UM_DGR_3_DES = col_character(),
    UM_DGR_3_HONORS_CD = col_character(),
    UM_DGR_3_HONORS_DES = col_character(),
    UM_DGR_3_ACAD_CRER_CD = col_character(),
    UM_DGR_3_ACAD_CRER_DES = col_character(),
    UM_DGR_3_ED_LVL_CD = col_double(),
    UM_DGR_3_ED_LVL_SHORT_DES = col_character(),
    UM_DGR_3_CMPLTN_TERM_CD = col_character(),
    UM_DGR_3_CMPLTN_TERM_DES = col_character(),
    UM_DGR_3_CNFR_DT = col_character(),
    UM_DGR_3_MAJOR_1_CD = col_character(),
    UM_DGR_3_MAJOR_1_DES = col_character(),
    UM_DGR_3_MAJOR_1_CIP_CD = col_character(),
    UM_DGR_3_MAJOR_1_CIP_DES = col_character(),
    UM_DGR_3_MAJOR_2_CD = col_character(),
    UM_DGR_3_MAJOR_2_DES = col_character(),
    UM_DGR_3_MAJOR_2_CIP_CD = col_character(),
    UM_DGR_3_MAJOR_2_CIP_DES = col_character(),
    UM_DGR_3_MINOR_1_CD = col_character(),
    UM_DGR_3_MINOR_1_DES = col_character(),
    UM_DGR_3_MINOR_1_CIP_CD = col_character(),
    UM_DGR_3_MINOR_1_CIP_DES = col_character(),
    UM_DGR_3_MINOR_2_CD = col_character(),
    UM_DGR_3_MINOR_2_DES = col_character(),
    UM_DGR_3_MINOR_2_CIP_CD = col_character(),
    UM_DGR_3_MINOR_2_CIP_DES = col_character(),
    UG_LAST_SCHL_ATTND_CD = col_character(),
    UG_LAST_SCHL_ATTND_DES = col_character(),
    UG_LAST_SCHL_ATTND_CEEB_CD = col_character(),
    UG_LAST_SCHL_ATTND_SCHL_TYP_CD = col_character(),
    UG_LAST_SCHL_ATTND_SCHL_TYP_DS = col_character(),
    UG_LAST_SCHL_ATTND_END_DT = col_character(),
    UG_LAST_SCHL_ATTND_END_DUR_NBR = col_double())
  
  
}
  
student_info_term_cols <- function()
{
  
  tt <- cols(
  SNPSHT_RPT_DT = col_character(),
  STDNT_ID = col_double(),
  TERM_CD = col_character(),
  TERM_SHORT_DES = col_character(),
  RESCO_IND = col_double(),
  SPPLMNT_STUDY_IND = col_double(),
  TEACH_CERT_IND = col_double(),
  HONORS_PGM_IND = col_double(),
  ATHLTC_PRTCPT_SPORT_CD = col_double(),
  ATHLTC_PRTCPT_SPORT_SHORT_DES = col_character(),
  ACAD_CRER_CNT = col_double(),
  ACAD_PGM_CNT = col_double(),
  ACAD_MAJOR_CNT = col_double(),
  ACAD_MINOR_CNT = col_double(),
  PRMRY_CRER_CD = col_character(),
  PRMRY_CRER_DES = col_character(),
  REG_STAT_CD = col_character(),
  REG_STAT_SHORT_DES = col_character(),
  RES_CD = col_character(),
  RES_SHORT_DES = col_character(),
  ACAD_LOAD_CD = col_character(),
  ACAD_LOAD_SHORT_DES = col_character(),
  ENTRY_TYP_SHORT_DES = col_character(),
  CRER_LVL_CD = col_character(),
  ACAD_LVL_BOT_CD = col_double(),
  ACAD_LVL_BOT_SHORT_DES = col_character(),
  ACAD_LVL_EOT_CD = col_double(),
  ACAD_LVL_EOT_SHORT_DES = col_character(),
  GRD_PNTS = col_double(),
  UNIT_TAKEN_GPA = col_double(),
  UNIT_TAKEN_NO_GPA = col_double(),
  CURR_GPA = col_double(),
  CUM_GPA = col_double(),
  PREV_TERM_CUM_GPA = col_double(),
  PGM_1_CD = col_character(),
  PGM_1_DES = col_character(),
  PGM_1_MAJOR_1_CD = col_character(),
  PGM_1_MAJOR_1_DES = col_character(),
  PGM_1_MAJOR_1_CIP_CD = col_character(),
  PGM_1_MAJOR_1_CIP_DES = col_character(),
  PGM_1_MAJOR_1_DCLR_DT = col_character(),
  PGM_1_MAJOR_1_SUBPLN_1_CD = col_character(),
  PGM_1_MAJOR_1_SUBPLN_1_DES = col_character(),
  PGM_1_MAJOR_1_SUBPLN_1_CIP_CD = col_character(),
  PGM_1_MAJOR_1_SUBPLN_1_CIP_DES = col_character(),
  PGM_1_MAJOR_1_SUBPLN_1_DCLR_DT = col_character(),
  PGM_1_MAJOR_1_SUBPLN_2_CD = col_character(),
  PGM_1_MAJOR_1_SUBPLN_2_DES = col_character(),
  PGM_1_MAJOR_1_SUBPLN_2_CIP_CD = col_double(),
  PGM_1_MAJOR_1_SUBPLN_2_CIP_DES = col_character(),
  PGM_1_MAJOR_1_SUBPLN_2_DCLR_DT = col_character(),
  PGM_1_MAJOR_2_CD = col_character(),
  PGM_1_MAJOR_2_DES = col_character(),
  PGM_1_MAJOR_2_CIP_CD = col_character(),
  PGM_1_MAJOR_2_CIP_DES = col_character(),
  PGM_1_MAJOR_2_DCLR_DT = col_character(),
  PGM_1_MAJOR_2_SUBPLN_1_CD = col_character(),
  PGM_1_MAJOR_2_SUBPLN_1_DES = col_character(),
  PGM_1_MAJOR_2_SUBPLN_1_CIP_CD = col_double(),
  PGM_1_MAJOR_2_SUBPLN_1_CIP_DES = col_character(),
  PGM_1_MAJOR_2_SUBPLN_1_DCLR_DT = col_character(),
  PGM_1_MAJOR_2_SUBPLN_2_CD = col_character(),
  PGM_1_MAJOR_2_SUBPLN_2_DES = col_character(),
  PGM_1_MAJOR_2_SUBPLN_2_CIP_CD = col_character(),
  PGM_1_MAJOR_2_SUBPLN_2_CIP_DES = col_character(),
  PGM_1_MAJOR_2_SUBPLN_2_DCLR_DT = col_character(),
  PGM_1_MINOR_1_CD = col_character(),
  PGM_1_MINOR_1_DES = col_character(),
  PGM_1_MINOR_1_CIP_CD = col_double(),
  PGM_1_MINOR_1_CIP_DES = col_character(),
  PGM_1_MINOR_1_DCLR_DT = col_character(),
  PGM_1_MINOR_2_CD = col_character(),
  PGM_1_MINOR_2_DES = col_character(),
  PGM_1_MINOR_2_CIP_CD = col_double(),
  PGM_1_MINOR_2_CIP_DES = col_character(),
  PGM_1_MINOR_2_DCLR_DT = col_character(),
  PGM_2_CD = col_character(),
  PGM_2_DES = col_character(),
  PGM_2_MAJOR_1_CD = col_character(),
  PGM_2_MAJOR_1_DES = col_character(),
  PGM_2_MAJOR_1_CIP_CD = col_character(),
  PGM_2_MAJOR_1_CIP_DES = col_character(),
  PGM_2_MAJOR_1_DCLR_DT = col_character(),
  PGM_2_MAJOR_1_SUBPLN_1_CD = col_character(),
  PGM_2_MAJOR_1_SUBPLN_1_DES = col_character(),
  PGM_2_MAJOR_1_SUBPLN_1_CIP_CD = col_double(),
  PGM_2_MAJOR_1_SUBPLN_1_CIP_DES = col_character(),
  PGM_2_MAJOR_1_SUBPLN_1_DCLR_DT = col_character(),
  PGM_2_MAJOR_1_SUBPLN_2_CD = col_character(),
  PGM_2_MAJOR_1_SUBPLN_2_DES = col_character(),
  PGM_2_MAJOR_1_SUBPLN_2_CIP_CD = col_character(),
  PGM_2_MAJOR_1_SUBPLN_2_CIP_DES = col_character(),
  PGM_2_MAJOR_1_SUBPLN_2_DCLR_DT = col_character(),
  PGM_2_MAJOR_2_CD = col_character(),
  PGM_2_MAJOR_2_DES = col_character(),
  PGM_2_MAJOR_2_CIP_CD = col_character(),
  PGM_2_MAJOR_2_CIP_DES = col_character(),
  PGM_2_MAJOR_2_DCLR_DT = col_character(),
  PGM_2_MAJOR_2_SUBPLN_1_CD = col_character(),
  PGM_2_MAJOR_2_SUBPLN_1_DES = col_character(),
  PGM_2_MAJOR_2_SUBPLN_1_CIP_CD = col_character(),
  PGM_2_MAJOR_2_SUBPLN_1_CIP_DES = col_character(),
  PGM_2_MAJOR_2_SUBPLN_1_DCLR_DT = col_character(),
  PGM_2_MAJOR_2_SUBPLN_2_CD = col_character(),
  PGM_2_MAJOR_2_SUBPLN_2_DES = col_character(),
  PGM_2_MAJOR_2_SUBPLN_2_CIP_CD = col_character(),
  PGM_2_MAJOR_2_SUBPLN_2_CIP_DES = col_character(),
  PGM_2_MAJOR_2_SUBPLN_2_DCLR_DT = col_character(),
  PGM_2_MINOR_1_CD = col_character(),
  PGM_2_MINOR_1_DES = col_character(),
  PGM_2_MINOR_1_CIP_CD = col_character(),
  PGM_2_MINOR_1_CIP_DES = col_character(),
  PGM_2_MINOR_1_DCLR_DT = col_character(),
  PGM_2_MINOR_2_CD = col_character(),
  PGM_2_MINOR_2_DES = col_character(),
  PGM_2_MINOR_2_CIP_CD = col_character(),
  PGM_2_MINOR_2_CIP_DES = col_character(),
  PGM_2_MINOR_2_DCLR_DT = col_character(),
  PGM_3_CD = col_character(),
  PGM_3_DES = col_character(),
  PGM_3_MAJOR_1_CD = col_character(),
  PGM_3_MAJOR_1_DES = col_character(),
  PGM_3_MAJOR_1_CIP_CD = col_character(),
  PGM_3_MAJOR_1_CIP_DES = col_character(),
  PGM_3_MAJOR_1_DCLR_DT = col_character(),
  PGM_3_MAJOR_1_SUBPLN_1_CD = col_character(),
  PGM_3_MAJOR_1_SUBPLN_1_DES = col_character(),
  PGM_3_MAJOR_1_SUBPLN_1_CIP_CD = col_character(),
  PGM_3_MAJOR_1_SUBPLN_1_CIP_DES = col_character(),
  PGM_3_MAJOR_1_SUBPLN_1_DCLR_DT = col_character(),
  PGM_3_MAJOR_1_SUBPLN_2_CD = col_character(),
  PGM_3_MAJOR_1_SUBPLN_2_DES = col_character(),
  PGM_3_MAJOR_1_SUBPLN_2_CIP_CD = col_character(),
  PGM_3_MAJOR_1_SUBPLN_2_CIP_DES = col_character(),
  PGM_3_MAJOR_1_SUBPLN_2_DCLR_DT = col_character(),
  PGM_3_MAJOR_2_CD = col_character(),
  PGM_3_MAJOR_2_DES = col_character(),
  PGM_3_MAJOR_2_CIP_CD = col_character(),
  PGM_3_MAJOR_2_CIP_DES = col_character(),
  PGM_3_MAJOR_2_DCLR_DT = col_character(),
  PGM_3_MAJOR_2_SUBPLN_1_CD = col_character(),
  PGM_3_MAJOR_2_SUBPLN_1_DES = col_character(),
  PGM_3_MAJOR_2_SUBPLN_1_CIP_CD = col_character(),
  PGM_3_MAJOR_2_SUBPLN_1_CIP_DES = col_character(),
  PGM_3_MAJOR_2_SUBPLN_1_DCLR_DT = col_character(),
  PGM_3_MAJOR_2_SUBPLN_2_CD = col_character(),
  PGM_3_MAJOR_2_SUBPLN_2_DES = col_character(),
  PGM_3_MAJOR_2_SUBPLN_2_CIP_CD = col_character(),
  PGM_3_MAJOR_2_SUBPLN_2_CIP_DES = col_character(),
  PGM_3_MAJOR_2_SUBPLN_2_DCLR_DT = col_character(),
  PGM_3_MINOR_1_CD = col_character(),
  PGM_3_MINOR_1_DES = col_character(),
  PGM_3_MINOR_1_CIP_CD = col_character(),
  PGM_3_MINOR_1_CIP_DES = col_character(),
  PGM_3_MINOR_1_DCLR_DT = col_character(),
  PGM_3_MINOR_2_CD = col_character(),
  PGM_3_MINOR_2_DES = col_character(),
  PGM_3_MINOR_2_CIP_CD = col_character(),
  PGM_3_MINOR_2_CIP_DES = col_character(),
  PGM_3_MINOR_2_DCLR_DT = col_character())

}

course_term_cols <- function()
{
 tt <- 
   cols(
     `#SNPSHT_RPT_DT` = col_date(format = ""),
     STDNT_ID = col_double(),
     TERM_CD = col_character(),
     CLASS_NBR = col_double(),
     TERM_SHORT_DES = col_character(),
     GRD_BASIS_ENRL_CD = col_character(),
     GRD_BASIS_ENRL_DES = col_character(),
     CRSE_GRD_INPUT_CD = col_character(),
     CRSE_GRD_OFFCL_CD = col_character(),
     UNITS_TAKEN_NBR = col_double(),
     UNITS_ERND_NBR = col_double(),
     EARN_CR_IND = col_double(),
     INCL_GPA_IND = col_double(),
     GRD_PNTS_PER_UNIT_NBR = col_double(),
     EXCL_CLASS_CURR_GPA = col_double(),
     EXCL_CLASS_CUM_GPA = col_double(),
     RPT_CD = col_character(),
     RPT_SHORT_DES = col_character(),
     SBJCT_CD = col_character(),
     SBJCT_DES = col_character(),
     CATLG_NBR = col_character(),
     CRSE_ID_CD = col_character(),
     CRSE_OFFER_NBR = col_double(),
     SESSN_CD = col_character(),
     SESSN_SHORT_DES = col_character(),
     CLASS_SCTN_CD = col_character(),
     ASSOC_CLASS_CD = col_double(),
     CRSE_CMPNT_CD = col_character(),
     CRSE_CMPNT_SHORT_DES = col_character(),
     CLASS_HOME_IND_CD = col_character(),
     CMBN_SCTN_MEET_TGTHR_IND = col_double(),
     CMBN_SCTN_CROSS_LSTD_IND = col_double(),
     CMBN_SCTN_UG_GRAD_IND = col_double(),
     CMBN_SCTN_ID_CD = col_character(),
     CLASS_LONG_DES = col_character(),
     CLASS_GRDD_IND = col_double(),
     CLASS_HONORS_IND = col_double(),
     CLASS_ENRL_TOTAL_NBR = col_double(),
     CMBN_CLASS_ENRL_TOTAL_NBR = col_double(),
     INSTRN_MODE_CD = col_character(),
     INSTRN_MODE_SHORT_DES = col_character(),
     CRSE_CIP_CD = col_character(),
     CRSE_CIP_DES = col_character(),
     FCLTY_ID_CD = col_character(),
     FCLTY_DES = col_character(),
     FCLTY_SOUND_PGM_IND = col_double(),
     FCLTY_SOUND_VOICE_IND = col_double(),
     FCLTY_BLACK_OUT_IND = col_double(),
     FCLTY_PC_IND = col_double(),
     FCLTY_MAC_IND = col_double(),
     FCLTY_PDM_PC_IND = col_double(),
     FCLTY_PDM_MAC_IND = col_double(),
     FCLTY_WHITE_BOARD_25_FT_IND = col_double(),
     FCLTY_CHALK_BOARD_25_FT_IND = col_double(),
     FCLTY_VCR_IND = col_double(),
     FCLTY_DVD_IND = col_double(),
     FCLTY_VIDEO_CNFRNC_IND = col_double(),
     FCLTY_LCTR_CPTR_IND = col_double(),
     FCLTY_DOC_CAMERA_IND = col_double(),
     FCLTY_16MM_FILM_IND = col_double(),
     FCLTY_35MM_FILM_IND = col_double(),
     FCLTY_DGTL_DATA_AND_VIDEO_IND = col_double(),
     FCLTY_TIERED_FLR_IND = col_double(),
     FCLTY_STAGE_IND = col_double(),
     FCLTY_ADTRM_SEAT_IND = col_double(),
     FCLTY_ANY_TBLS_IND = col_double(),
     FCLTY_MVBL_TBLS_IND = col_double(),
     FCLTY_MVBL_TBLS_CHAIR_IND = col_double(),
     FCLTY_TBL_CNFRNC_SMNR_IND = col_double(),
     MULT_CLASS_MTG_IND = col_double(),
     CLASS_MTG_START_DT = col_character(),
     CLASS_MTG_END_DT = col_character(),
     CLASS_MTG_START_TM = col_character(),
     CLASS_MTG_END_TM = col_character(),
     CLASS_MTG_MON_IND = col_double(),
     CLASS_MTG_TUES_IND = col_double(),
     CLASS_MTG_WED_IND = col_double(),
     CLASS_MTG_THURS_IND = col_double(),
     CLASS_MTG_FRI_IND = col_double(),
     CLASS_MTG_SAT_IND = col_double(),
     CLASS_MTG_SUN_IND = col_double(),
     CLASS_SCTN_TOPIC_DES = col_character(),
     CLASS_GPA = col_double(),
     CRSE_GPA = col_double())
     
  
}
