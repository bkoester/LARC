basic_derived_columns <- function()
{
  
  
  
}

#This pulls the student info table columns. In the future, it will include any processing we need.
reduce.lsi.table <- function(lsi)
{
  
  #source('/Users/bkoester/Google Drive/code/REBUILD/LARC.GITHUB/larc.fill.act.score.R')
  
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
            #"UM_DGR_1_MAJOR_1_CIP_CD","UM_DGR_1_MAJOR_1_CIP_DES","UM_DGR_1_MAJOR_2_CIP_CD","UM_DGR_1_MAJOR_2_CIP_DES",
            #"UM_DGR_2_MAJOR_1_CIP_CD","UM_DGR_2_MAJOR_1_CIP_DES","UM_DGR_2_MAJOR_2_CIP_CD","UM_DGR_2_MAJOR_2_CIP_DES",
            #"STDNT_ASIAN_IND","STDNT_BLACK_IND","STDNT_HWIAN_IND",               
            #"STDNT_HSPNC_IND","STDNT_NTV_AMRCN_IND","STDNT_WHITE_IND","STDNT_HSPNC_LATINO_IND",
            #"STDNT_MULTI_ETHNC_IND","STDNT_HSPNC_LATINO_IND",
            "STDNT_DMSTC_UNDREP_MNRTY_CD")
  
  
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
  
  #too keep all AP/UMPLC add them to the vector list: keep <- c(cols,ap,umplc)
  keep <- cols
  out  <- lsi[,keep]
  
  #add in the first gen flag
  e <- out$PRNT_MAX_ED_LVL_CD %in% c(202,203,204,206,205,19,61)
  FIRST_GEN <- mat.or.vec(dim(out)[1],1)
  FIRST_GEN[which(e)] <- 1
  e <- is.na(FIRST_GEN)
  FIRST_GEN[e] <- 0
  out <- data.frame(out,FIRST_GEN)
  
  return(out)
  
}

#select student course columns form the studnet course table, keeping ONLY courses taken for a grade.
reduce.lsc.table <- function(lsc,lst)
{
  
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
  
  return(out)
  
}  

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