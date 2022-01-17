#this takes the LARC student record table as input.
stem_degree_tracking <- function(TAG,CODEDIR,LARCDIR,OUTDIR)
{
  
  #TAG='20210722'
  #CODEDIR='/Users/bkoester/Google Drive/code/REBUILD/LARC.GITHUB/'
  #LARCDIR="/Users/bkoester/Box Sync/LARC.FLAT/"
  #OUTDIR="/Users/bkoester/Box Sync/LARC.WORKING/"
  
  lsiname <- paste(LARCDIR,"LARC_",TAG,"_STDNT_INFO.csv",sep="")
  lstname <- paste(LARCDIR,"LARC_",TAG,"_STDNT_TERM_INFO.csv",sep="")
  
  jj  <- student_info_cols()
  lsi <- read_csv(lsiname,col_types=jj)
  jj  <- student_info_term_cols()
  lst <- read_csv(lstname,col_types=jj)
  
  
  dept_sub <- read_tsv('/Users/bkoester/Box Sync/PublicDataSets/subject_division_2021.txt') 
  maj <- read_tsv('/Users/bkoester/Box Sync/PublicDataSets/majors_divisions_2021.txt') %>% select(-Rowname)
  
  sft <- lsi %>% select(STDNT_ID,FIRST_TERM_ATTND_CD)
  #either science or engineering will count as STEM
  dec <- label_stem_declares(lst,maj) %>% left_join(sft)
  deg <- label_stem_degrees(lsi,maj)  %>% left_join(sft)
  
  TERMYR <- compute_termyear(as.integer(dec$FIRST_TERM_ATTND_CD),as.integer(dec$TERM_CD))
  dec$TERMYR <- TERMYR
  TERMYR <- compute_termyear(as.integer(deg$FIRST_TERM_ATTND_CD),as.integer(deg$TERM_CD))
  deg$TERMYR <- TERMYR
  
  
  
  return(list(dec,deg))
  
}  

label_stem_declares <- function(lst,maj)
{
  lst <- lst %>% filter(CRER_LVL_CD == 'U')
  
  hh1 <- lst %>% group_by(STDNT_ID,PGM_1_MAJOR_1_DES) %>% select(STDNT_ID,TERM_CD,TERM_SHORT_DES,PGM_1_MAJOR_1_DES) %>% 
        arrange(TERM_CD) %>% filter(row_number() == 1) %>% rename(MAJOR=PGM_1_MAJOR_1_DES)
  hh1 <- hh1 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES')) %>% drop_na()
  
  hh2 <- lst %>% group_by(STDNT_ID,PGM_1_MAJOR_2_DES) %>% select(STDNT_ID,TERM_CD,TERM_SHORT_DES,PGM_1_MAJOR_2_DES) %>% 
        arrange(TERM_CD) %>% filter(row_number() == 1) %>% rename(MAJOR=PGM_1_MAJOR_2_DES)
  hh2 <- hh2 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES')) %>% drop_na()
  
  hh3 <- lst %>% group_by(STDNT_ID,PGM_2_MAJOR_1_DES) %>% select(STDNT_ID,TERM_CD,TERM_SHORT_DES,PGM_2_MAJOR_1_DES) %>% 
        arrange(TERM_CD) %>% filter(row_number() == 1) %>% rename(MAJOR=PGM_2_MAJOR_1_DES)
  hh3 <- hh3 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES')) %>% drop_na()
  
  hh4 <- lst %>% group_by(STDNT_ID,PGM_2_MAJOR_2_DES) %>% select(STDNT_ID,TERM_CD,TERM_SHORT_DES,PGM_2_MAJOR_2_DES) %>% 
        arrange(TERM_CD) %>% filter(row_number() == 1) %>% rename(MAJOR=PGM_2_MAJOR_2_DES)
  hh4 <- hh4 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES')) %>% drop_na()
  
  hh5 <- lst %>% group_by(STDNT_ID,PGM_3_MAJOR_1_DES) %>% select(STDNT_ID,TERM_CD,TERM_SHORT_DES,PGM_3_MAJOR_1_DES) %>% 
        arrange(TERM_CD) %>% filter(row_number() == 1) %>% rename(MAJOR=PGM_3_MAJOR_1_DES)
  hh5 <- hh5 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES')) %>% drop_na()
  
  hh6 <- lst %>% group_by(STDNT_ID,PGM_3_MAJOR_2_DES) %>% select(STDNT_ID,TERM_CD,TERM_SHORT_DES,PGM_3_MAJOR_2_DES) %>% 
        arrange(TERM_CD) %>% filter(row_number() == 1) %>% rename(MAJOR=PGM_3_MAJOR_2_DES)
  hh6 <- hh6 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES')) %>% drop_na()
  
  junk <- bind_rows(hh1,hh2,hh3,hh4,hh5,hh6) %>% drop_na(MAJOR)
  #junk <- hh1
  
  return(junk)
  
}


label_stem_degrees <- function(lsi,maj)
{
  
  hh1 <- lsi %>% filter(UM_DGR_1_ED_LVL_CD == 1) %>% distinct(STDNT_ID,UM_DGR_1_CMPLTN_TERM_CD,UM_DGR_1_MAJOR_1_DES) %>% 
         rename(TERM_CD=UM_DGR_1_CMPLTN_TERM_CD,MAJOR=UM_DGR_1_MAJOR_1_DES) 
  hh1 <- hh1 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES'))
  
  hh2 <- lsi %>% filter(UM_DGR_1_ED_LVL_CD == 1) %>% distinct(STDNT_ID,UM_DGR_1_CMPLTN_TERM_CD,UM_DGR_1_MAJOR_2_DES) %>% 
         rename(TERM_CD=UM_DGR_1_CMPLTN_TERM_CD,MAJOR=UM_DGR_1_MAJOR_2_DES) 
  hh2 <- hh2 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES'))
  
  hh3 <- lsi %>% filter(UM_DGR_2_ED_LVL_CD == 1) %>% distinct(STDNT_ID,UM_DGR_2_CMPLTN_TERM_CD,UM_DGR_2_MAJOR_1_DES) %>% 
         rename(TERM_CD=UM_DGR_2_CMPLTN_TERM_CD,MAJOR=UM_DGR_2_MAJOR_1_DES) 
  hh3 <- hh3 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES'))
  
  hh4 <- lsi %>% filter(UM_DGR_2_ED_LVL_CD == 1) %>% distinct(STDNT_ID,UM_DGR_2_CMPLTN_TERM_CD,UM_DGR_2_MAJOR_2_DES) %>% 
         rename(TERM_CD=UM_DGR_2_CMPLTN_TERM_CD,MAJOR=UM_DGR_2_MAJOR_2_DES) 
  hh4 <- hh4 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES'))
  
  hh5 <- lsi %>% filter(UM_DGR_3_ED_LVL_CD == 1) %>% distinct(STDNT_ID,UM_DGR_3_CMPLTN_TERM_CD,UM_DGR_3_MAJOR_1_DES) %>% 
         rename(TERM_CD=UM_DGR_3_CMPLTN_TERM_CD,MAJOR=UM_DGR_3_MAJOR_1_DES) 
  hh5 <- hh5 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES'))
  
  hh6 <- lsi %>% filter(UM_DGR_3_ED_LVL_CD == 1) %>% distinct(STDNT_ID,UM_DGR_3_CMPLTN_TERM_CD,UM_DGR_3_MAJOR_2_DES) %>% 
         rename(TERM_CD=UM_DGR_3_CMPLTN_TERM_CD,MAJOR=UM_DGR_3_MAJOR_2_DES) 
  hh6 <- hh6 %>% left_join(maj,by=c('MAJOR'='UM_DGR_1_MAJOR_1_DES'))
  
  junk <- bind_rows(hh1,hh2,hh3,hh4,hh5,hh6) %>% drop_na(MAJOR)
  
  return(junk)
  
  
  
  
  
  
}

compute_termyear <- function(start,stop)
{
  delta <- stop-start
  TERMYR <- mat.or.vec(length(delta),1)
  
  #compute the number of years you've been here since your entrance term.
  # this may not deal well with winter entrance.
  e0 <- which(delta %% 50 == 0)
  e1 <- which(delta %% 50 == 10)
  e2 <- which(delta %% 50 == 20)
  e3 <- which(delta %% 50 == 30)
  e4 <- which(delta %% 50 == 40)
  
  TERMYR[e0] <- delta[e0]/50+1
  TERMYR[e1] <- (delta[e1]-10)/50+0.5+1
  TERMYR[e2] <- (delta[e2]-20)/50+0.6+1
  TERMYR[e3] <- (delta[e3]-30)/50+0.7+1
  TERMYR[e4] <- (delta[e4]-40)/50+0.8+1
  
  TERMYR <- round(TERMYR*2)
  
  return(TERMYR)
  
}



student_info_cols <- function()
{
  
  tt <- cols_only(
    "STDNT_ID" = col_double(),
    #"STDNT_SEX_CD" = col_double(),
    #"STDNT_SEX_SHORT_DES" = col_character(),
    #"STDNT_BIRTH_YR" = col_double(),
    #"STDNT_BIRTH_MO" = col_double(),
    #"STDNT_ASIAN_IND" = col_double(),
    #"STDNT_BLACK_IND" = col_double(),
    #"STDNT_HWIAN_IND" = col_double(),
    #"STDNT_HSPNC_IND" = col_double(),
    #"STDNT_NTV_AMRCN_IND" = col_double(),
    #"STDNT_WHITE_IND" = col_double(),
    #"STDNT_ETHNC_GRP_CD" = col_double(),
    #"STDNT_ETHNC_GRP_SHORT_DES" = col_character(),
    #"STDNT_MULTI_ETHNC_IND" = col_double(),
    #"STDNT_HSPNC_LATINO_IND" = col_double(),
    #"STDNT_DMSTC_UNDREP_MNRTY_CD" = col_double(),
    #"STDNT_NTV_ENG_SPKR_IND" = col_double(),
    #"FIRST_US_PRMNNT_RES_PSTL_CD" = col_character(),
    #"FIRST_US_PRMNNT_RES_PSTL_5_CD" = col_character(),
    #"FRST_FRGN_PRMNNT_RES_CNTRY_CD" = col_character(),
    #"FRST_FRGN_PRMNNT_RES_CNTRY_DES" = col_character(),
    #"STDNT_CTZN_STAT_CD" = col_character(),
    #"STDNT_CTZN_STAT_SHORT_DES" = col_character(),
    #"STDNT_CTZN_CNTRY_1_CD" = col_character(),
    #"STDNT_CTZN_CNTRY_1_DES" = col_character(),
    #STDNT_CTZN_CNTRY_2_CD = col_character(),
    #STDNT_CTZN_CNTRY_2_DES = col_character(),
    #"STDNT_INTL_IND" = col_double(),
    "FIRST_TERM_ATTND_CD" = col_character(),
    "FIRST_TERM_ATTND_SHORT_DES" = col_character(),
    #"FIRST_TERM_ATTND_BEGIN_YR_MO" = col_character(),
    #"FIRST_TERM_ATTND_END_YR_MO" = col_character(),
    "LAST_TERM_ATTND_CD" = col_character(),
    "LAST_TERM_ATTND_SHORT_DES" = col_character(),
    #"LAST_TERM_ATTND_BEGIN_YR_MO" = col_character(),
    #"LAST_TERM_ATTND_END_YR_MO" = col_character(),
    #"ADMSSN_VTRN_IND" = col_double(),
    #"PRNT_MAX_ED_LVL_CD" = col_double(),
    ###"PRNT_MAX_ED_LVL_DES" = col_character(),
    #"PRNT_DEP_NBR_CD" = col_double(),
    #"PRNT_DEP_NBR_DES" = col_character(),
    #"EST_GROSS_FAM_INC_CD" = col_double(),
    #"EST_GROSS_FAM_INC_DES" = col_character(),
    #"SNGL_PRNT_IND" = col_double(),
    #HS_CALC_IND = col_double(),
    #HS_CHEM_LAB_IND = col_double(),
    #"HS_GPA" = col_double(),
    #"HS_CEEB_CD" = col_character(),
    #"HS_CITY_NM" = col_character(),
    #"HS_STATE_CD" = col_character(),
    #"HS_PSTL_CD" = col_character(),
    #HS_DSCRPTR_PLUS_CLSTR_CD = col_character(),
    #HS_DSCRPTR_PLUS_CLSTR_DES = col_character(),
    #NGHBRHD_DSCRPTR_PLUS_CLSTR_CD = col_character(),
    #NGHBRHD_DSCRPTR_PLUS_CLSTR_DES = col_character(),
    #MAX_ACT_TEST_DT = col_character(),
    #"MAX_ACT_ENGL_SCR" = col_double(),
    #MAX_ACT_ENGL_PCTL = col_double(),
    #"MAX_ACT_MATH_SCR" = col_double(),
    #MAX_ACT_MATH_PCTL = col_double(),
    #"MAX_ACT_READ_SCR" = col_double(),
    #MAX_ACT_READ_PCTL = col_double(),
    #"MAX_ACT_SCIRE_SCR" = col_double(),
    #MAX_ACT_SCIRE_PCTL = col_double(),
    #"MAX_ACT_COMP_SCR" = col_double(),
    #MAX_ACT_COMP_PCTL = col_double(),
    #MAX_ACT_EW_SCR = col_double(),
    #MAX_ACT_WR_SUB_SCR = col_double(),
    #MAX_ACT_WRS_SCR = col_double(),
    #MAX_ACT_WRS_PCTL = col_double(),
    #MAX_ACT_WRT_SCR = col_double(),
    #MAX_ACT_WRT_PCTL = col_double(),
    #MAX_ACT_WDIA_SCR = col_double(),
    #MAX_ACT_WDDS_SCR = col_double(),
    #MAX_ACT_WDO_SCR = col_double(),
    #MAX_ACT_WDLC_SCR = col_double(),
    #MAX_ACT_ELA_SCR = col_double(),
    #MAX_ACT_ELA_PCTL = col_double(),
    #MAX_ACT_STEM_SCR = col_double(),
    #MAX_ACT_STEM_PCTL = col_double(),
    #MAX_SATI_TEST_DT = col_character(),
    #"MAX_SATI_MATH_SCR" = col_double(),
    #MAX_SATI_MATH_PCTL = col_double(),
    #"MAX_SATI_VERB_SCR" = col_double(),
    #MAX_SATI_VERB_PCTL = col_double(),
    #"MAX_SATI_TOTAL_CALC_SCR" = col_double(),
    #"MAX_SATI_WR_SCR" = col_double(),
    #MAX_SATI_ES_SUB_SCR = col_double(),
    #MAX_SATI_MC_SUB_SCR = col_double(),
    #MAX_SATI_TOTAL_MSS_ERWS_SCR = col_double(),
    #MAX_SATI_TOTAL_MSS_ERWS_PCTL = col_double(),
    #MAX_SATI_ERWS_SCR = col_double(),
    #MAX_SATI_ERWS_PCTL = col_double(),
    #MAX_SATI_MSS_SCR = col_double(),
    #MAX_SATI_MSS_PCTL = col_double(),
    #MAX_SATI_RT_SCR = col_double(),
    #MAX_SATI_RT_PCTL = col_double(),
    #MAX_SATI_WLT_SCR = col_double(),
    #MAX_SATI_WLT_PCTL = col_double(),
    #MAX_SATI_MT_SCR = col_double(),
    #MAX_SATI_MT_PCTL = col_double(),
    #MAX_SATI_ASC_X_SCR = col_double(),
    #MAX_SATI_ASC_X_PCTL = col_double(),
    #MAX_SATI_AHSSC_X_SCR = col_double(),
    #MAX_SATI_AHSSC_X_PCTL = col_double(),
    #MAX_SATI_RWC_SUB_SCR = col_double(),
    #MAX_SATI_RWC_SUB_SCR_PCTL = col_double(),
    #MAX_SATI_CE_SUB_SCR = col_double(),
    #MAX_SATI_CE_SUB_SCR_PCTL = col_double(),
    #MAX_SATI_EI_SUB_SCR = col_double(),
    #MAX_SATI_EI_SUB_SCR_PCTL = col_double(),
    #MAX_SATI_SEC_SUB_SCR = col_double(),
    #MAX_SATI_SEC_SUB_SCR_PCTL = col_double(),
    #MAX_SATI_HA_SUB_SCR = col_double(),
    #MAX_SATI_HA_SUB_SCR_PCTL = col_double(),
    #MAX_SATI_PAM_SUB_SCR = col_double(),
    #MAX_SATI_PAM_SUB_SCR_PCTL = col_double(),
    #MAX_SATI_PSDA_SUB_SCR = col_double(),
    #MAX_SATI_PSDA_SUB_SCR_PCTL = col_double(),
    #MAX_SATI_ESR_SUB_SCR = col_double(),
    #MAX_SATI_ESA_SUB_SCR = col_double(),
    #MAX_SATI_ESW_SUB_SCR = col_double(),
    #MAX_GRE_TEST_DT = col_character(),
    #MAX_GRE_ANLY_SCR = col_double(),
    #MAX_GRE_ANLY_PCTL = col_double(),
    #MAX_GRE_QNT2_SCR = col_double(),
    #MAX_GRE_QNT2_PCTL = col_double(),
    #MAX_GRE_VRB2_SCR = col_double(),
    #MAX_GRE_VRB2_PCTL = col_double(),
    #MAX_GRE_QUAN_SCR = col_double(),
    #MAX_GRE_QUAN_PCTL = col_double(),
    #MAX_GRE_VERB_SCR = col_double(),
    #MAX_GRE_VERB_PCTL = col_double(),
    #MAX_GRE_WR_SCR = col_double(),
    #MAX_GRE_WR_PCTL = col_double(),
    #MAX_GRE_TOT_SCR = col_double(),
    #MAX_TOEFL_TEST_DT = col_character(),
    #MAX_TOEFL_TEST_TYP_CD = col_character(),
    #MAX_TOEFL_TOTAL_SCR = col_double(),
    #MAX_TOEFL_LIST_SCR = col_double(),
    #MAX_TOEFL_READ_SCR = col_double(),
    #MAX_TOEFL_WRIT_SCR = col_double(),
    #MAX_TOEFL_ESSY_SCR = col_double(),
    #MAX_TOEFL_SPK_SCR = col_double(),
    #MAX_TOEFL_TEST_ADMIN_CD = col_character(),
    #MAX_AP_AMGVT_TEST_DT = col_character(),
    #MAX_AP_AMGVT_TEST_SCR = col_double(),
    #MAX_AP_AMHIS_TEST_DT = col_character(),
    #MAX_AP_AMHIS_TEST_SCR = col_double(),
    #MAX_AP_ART2D_TEST_DT = col_character(),
    #MAX_AP_ART2D_TEST_SCR = col_double(),
    #MAX_AP_ART3D_TEST_DT = col_character(),
    #MAX_AP_ART3D_TEST_SCR = col_double(),
    #MAX_AP_ARTDR_TEST_DT = col_character(),
    #MAX_AP_ARTDR_TEST_SCR = col_double(),
    #MAX_AP_ARTGN_TEST_DT = col_character(),
    #MAX_AP_ARTGN_TEST_SCR = col_double(),
    #MAX_AP_ARTHS_TEST_DT = col_character(),
    #MAX_AP_ARTHS_TEST_SCR = col_double(),
    #MAX_AP_BY_TEST_DT = col_character(),
    #MAX_AP_BY_TEST_SCR = col_double(),
    #MAX_AP_CALAB_TEST_DT = col_character(),
    #"MAX_AP_CALAB_TEST_SCR" = col_double(),
    #"MAX_AP_CALAB_TEST_SUB_SCR_DT" = col_character(),
    #"MAX_AP_CALAB_TEST_SUB_SCR" = col_double(),
    #"MAX_AP_CALBC_TEST_DT" = col_character(),
    #"MAX_AP_CALBC_TEST_SCR" = col_double(),
    #"MAX_AP_CH_TEST_DT" = col_character(),
    #"MAX_AP_CH_TEST_SCR" = col_double(),
    #MAX_AP_CHINA_TEST_DT = col_character(),
    #MAX_AP_CHINA_TEST_SCR = col_double(),
    #MAX_AP_CPGVT_TEST_DT = col_character(),
    #MAX_AP_CPGVT_TEST_SCR = col_double(),
    #MAX_AP_CSA_TEST_DT = col_character(),
    #MAX_AP_CSA_TEST_SCR = col_double(),
    #MAX_AP_CSAB_TEST_DT = col_character(),
    #MAX_AP_CSAB_TEST_SCR = col_double(),
    #MAX_AP_CSPRC_TEST_DT = col_character(),
    #MAX_AP_CSPRC_TEST_SCR = col_double(),
    #MAX_AP_EH_TEST_DT = col_character(),
    #MAX_AP_EH_TEST_SCR = col_double(),
    #MAX_AP_EL_TEST_DT = col_character(),
    #MAX_AP_EL_TEST_SCR = col_double(),
    #MAX_AP_ENGL_TEST_DT = col_character(),
    #MAX_AP_ENGL_TEST_SCR = col_double(),
    #MAX_AP_ENVSC_TEST_DT = col_character(),
    #MAX_AP_ENVSC_TEST_SCR = col_double(),
    #MAX_AP_FR_TEST_DT = col_character(),
    #MAX_AP_FR_TEST_SCR = col_double(),
    #MAX_AP_FRLIT_TEST_DT = col_character(),
    #MAX_AP_FRLIT_TEST_SCR = col_double(),
    #MAX_AP_GM_TEST_DT = col_character(),
    #MAX_AP_GM_TEST_SCR = col_double(),
    #MAX_AP_HGEOG_TEST_DT = col_character(),
    #MAX_AP_HGEOG_TEST_SCR = col_double(),
    #MAX_AP_IT_TEST_DT = col_character(),
    #MAX_AP_IT_TEST_SCR = col_double(),
    #MAX_AP_JAPN_TEST_DT = col_character(),
    #MAX_AP_JAPN_TEST_SCR = col_double(),
    #MAX_AP_LTLIT_TEST_DT = col_character(),
    #MAX_AP_LTLIT_TEST_SCR = col_double(),
    #MAX_AP_LTVER_TEST_DT = col_character(),
    #MAX_AP_LTVER_TEST_SCR = col_double(),
    #MAX_AP_MACEC_TEST_DT = col_character(),
    #MAX_AP_MACEC_TEST_SCR = col_double(),
    #MAX_AP_MICEC_TEST_DT = col_character(),
    #MAX_AP_MICEC_TEST_SCR = col_double(),
    #MAX_AP_MTAUR_TEST_SUB_SCR_DT = col_character(),
    #MAX_AP_MTAUR_SUB_TEST_SCR = col_double(),
    #MAX_AP_MTNAU_TEST_SUB_SCR_DT = col_character(),
    #MAX_AP_MTNAU_TEST_SUB_SCR = col_double(),
    #MAX_AP_MUSTH_TEST_DT = col_character(),
    #MAX_AP_MUSTH_TEST_SCR = col_double(),
    #MAX_AP_PHYS1_TEST_DT = col_character(),
    #MAX_AP_PHYS1_TEST_SCR = col_double(),
    #MAX_AP_PHYS2_TEST_DT = col_character(),
    #MAX_AP_PHYS2_TEST_SCR = col_double(),
    #MAX_AP_PHYSB_TEST_DT = col_character(),
    #MAX_AP_PHYSB_TEST_SCR = col_double(),
    #MAX_AP_PHYSE_TEST_DT = col_character(),
    #MAX_AP_PHYSE_TEST_SCR = col_double(),
    #MAX_AP_PHYSM_TEST_DT = col_character(),
    #MAX_AP_PHYSM_TEST_SCR = col_double(),
    #MAX_AP_PY_TEST_DT = col_character(),
    #MAX_AP_PY_TEST_SCR = col_double(),
    #MAX_AP_RSRCH_TEST_DT = col_character(),
    #MAX_AP_RSRCH_TEST_SCR = col_double(),
    #MAX_AP_SEMNR_TEST_DT = col_character(),
    #MAX_AP_SEMNR_TEST_SCR = col_double(),
    #MAX_AP_SP_TEST_DT = col_character(),
    #MAX_AP_SP_TEST_SCR = col_double(),
    #MAX_AP_SPLIT_TEST_DT = col_character(),
    #MAX_AP_SPLIT_TEST_SCR = col_double(),
    #MAX_AP_STAT_TEST_DT = col_character(),
    #MAX_AP_STAT_TEST_SCR = col_double(),
    #MAX_AP_WHIST_TEST_DT = col_character(),
    #MAX_AP_WHIST_TEST_SCR = col_double(),
    #MAX_UMPLC_ATPT_TEST_DT = col_character(),
    #MAX_UMPLC_ATPT_TEST_SCR = col_double(),
    #MAX_UMPLC_ATPT_RCMD_CD = col_character(),
    #MAX_UMPLC_ATPT_RCMD_DES = col_character(),
    #"MAX_UMPLC_CH_TEST_DT" = col_character(),
    #"MAX_UMPLC_CH_TEST_SCR" = col_double(),
    #"MAX_UMPLC_CH_RCMD_CD" = col_character(),
    #"MAX_UMPLC_CH_RCMD_DES" = col_character(),
    #MAX_UMPLC_CHN_TEST_DT = col_character(),
    #MAX_UMPLC_CHN_TEST_SCR = col_double(),
    #MAX_UMPLC_CHN_RCMD_CD = col_character(),
    #MAX_UMPLC_CHN_RCMD_DES = col_character(),
    #MAX_UMPLC_FILIP_TEST_DT = col_character(),
    #MAX_UMPLC_FILIP_TEST_SCR = col_double(),
    #MAX_UMPLC_FILIP_RCMD_CD = col_character(),
    #MAX_UMPLC_FILIP_RCMD_DES = col_character(),
    #MAX_UMPLC_FRE_TEST_DT = col_character(),
    #MAX_UMPLC_FRLST_TEST_SCR = col_double(),
    #MAX_UMPLC_FRRD_TEST_SCR = col_double(),
    #MAX_UMPLC_FRE_RCMD_CD = col_character(),
    #MAX_UMPLC_FRE_RCMD_DES = col_character(),
    #MAX_UMPLC_GM_TEST_DT = col_character(),
    #MAX_UMPLC_GM_TEST_SCR = col_double(),
    #MAX_UMPLC_GM_RCMD_CD = col_character(),
    #MAX_UMPLC_GM_RCMD_DES = col_character(),
    #MAX_UMPLC_HBRD_TEST_DT = col_character(),
    #MAX_UMPLC_HBRD_TEST_SCR = col_double(),
    #MAX_UMPLC_HBRD_RCMD_CD = col_character(),
    #MAX_UMPLC_HBRD_RCMD_DES = col_character(),
    #MAX_UMPLC_HINDI_TEST_DT = col_character(),
    #MAX_UMPLC_HINDI_TEST_SCR = col_double(),
    #MAX_UMPLC_HINDI_RCMD_CD = col_character(),
    #MAX_UMPLC_HINDI_RCMD_DES = col_character(),
    #MAX_UMPLC_INDON_TEST_DT = col_character(),
    #MAX_UMPLC_INDON_TEST_SCR = col_double(),
    #MAX_UMPLC_INDON_RCMD_CD = col_character(),
    #MAX_UMPLC_INDON_RCMD_DES = col_character(),
    #MAX_UMPLC_ITRD_TEST_DT = col_character(),
    #MAX_UMPLC_ITRD_TEST_SCR = col_double(),
    #MAX_UMPLC_ITRD_RCMD_CD = col_character(),
    #MAX_UMPLC_ITRD_RCMD_DES = col_character(),
    #MAX_UMPLC_JPNPL_TEST_DT = col_character(),
    #MAX_UMPLC_JPNPL_TEST_SCR = col_double(),
    #MAX_UMPLC_JPNPL_RCMD_CD = col_character(),
    #MAX_UMPLC_JPNPL_RCMD_DES = col_character(),
    #MAX_UMPLC_KOR_TEST_DT = col_character(),
    #MAX_UMPLC_KOR_TEST_SCR = col_double(),
    #MAX_UMPLC_KOR_RCMD_CD = col_character(),
    #MAX_UMPLC_KOR_RCMD_DES = col_character(),
    #MAX_UMPLC_LTSCR_TEST_DT = col_character(),
    #MAX_UMPLC_LTSCR_TEST_SCR = col_double(),
    #MAX_UMPLC_LTSCR_RCMD_CD = col_character(),
    #MAX_UMPLC_LTSCR_RCMD_DES = col_character(),
    #MAX_UMPLC_MATH_TEST_DT = col_character(),
    #"MAX_UMPLC_MATH_TEST_SCR" = col_double(),
    #"MAX_UMPLC_MATH_RCMD_CD" = col_character(),
    #"MAX_UMPLC_MATH_RCMD_DES" = col_character(),
    #MAX_UMPLC_PUNJA_TEST_DT = col_character(),
    #MAX_UMPLC_PUNJA_TEST_SCR = col_double(),
    #MAX_UMPLC_PUNJA_RCMD_CD = col_character(),
    #MAX_UMPLC_PUNJA_RCMD_DES = col_character(),
    #MAX_UMPLC_RSRD_TEST_DT = col_character(),
    #MAX_UMPLC_RSRD_TEST_SCR = col_double(),
    #MAX_UMPLC_RSRD_RCMD_CD = col_character(),
    #MAX_UMPLC_RSRD_RCMD_DES = col_character(),
    #MAX_UMPLC_SPA_TEST_DT = col_character(),
    #MAX_UMPLC_SPLST_TEST_SCR = col_double(),
    #MAX_UMPLC_SPRD_TEST_SCR = col_double(),
    #MAX_UMPLC_SPA_RCMD_CD = col_character(),
    #MAX_UMPLC_SPA_RCMD_DES = col_character(),
    #MAX_UMPLC_THAI_TEST_DT = col_character(),
    #MAX_UMPLC_THAI_TEST_SCR = col_double(),
    #MAX_UMPLC_THAI_RCMD_CD = col_character(),
    #MAX_UMPLC_THAI_RCMD_DES = col_character(),
    #MAX_UMPLC_URDU_TEST_DT = col_character(),
    #MAX_UMPLC_URDU_TEST_SCR = col_double(),
    #MAX_UMPLC_URDU_RCMD_CD = col_character(),
    #MAX_UMPLC_URDU_RCMD_DES = col_character(),
    #MAX_UMPLC_VIETN_TEST_DT = col_character(),
    #MAX_UMPLC_VIETN_TEST_SCR = col_double(),
    #MAX_UMPLC_VIETN_RCMD_CD = col_character(),
    #MAX_UMPLC_VIETN_RCMD_DES = col_character(),
    "UM_UG_DGR_CNT" = col_double(),
    "UM_UG_DGR_MAJOR_CNT" = col_double(),
    "UM_UG_DGR_MINOR_CNT" = col_double(),
    #UM_GRAD_DGR_CNT = col_double(),
    #UM_GRAD_DGR_MAJOR_CNT = col_double(),
    #UM_GRAD_DGR_MINOR_CNT = col_double(),
    #UM_OTHER_DGR_CNT = col_double(),
    "UM_DGR_1_CD" = col_character(),
    "UM_DGR_1_DES" = col_character(),
    #"UM_DGR_1_HONORS_CD" = col_character(),
    #"UM_DGR_1_HONORS_DES" = col_character(),
    "UM_DGR_1_ACAD_CRER_CD" = col_character(),
    "UM_DGR_1_ACAD_CRER_DES" = col_character(),
    "UM_DGR_1_ED_LVL_CD" = col_double(),
    "UM_DGR_1_ED_LVL_SHORT_DES" = col_character(),
    "UM_DGR_1_CMPLTN_TERM_CD" = col_character(),
    "UM_DGR_1_CMPLTN_TERM_DES" = col_character(),
    "UM_DGR_1_CNFR_DT" = col_character(),
    "UM_DGR_1_MAJOR_1_CD" = col_character(),
    "UM_DGR_1_MAJOR_1_DES" = col_character(),
    #UM_DGR_1_MAJOR_1_CIP_CD = col_character(),
    #UM_DGR_1_MAJOR_1_CIP_DES = col_character(),
    "UM_DGR_1_MAJOR_2_CD" = col_character(),
    "UM_DGR_1_MAJOR_2_DES" = col_character(),
    #UM_DGR_1_MAJOR_2_CIP_CD = col_character(),
    #UM_DGR_1_MAJOR_2_CIP_DES = col_character(),
    #UM_DGR_1_MINOR_1_CD = col_character(),
    "UM_DGR_1_MINOR_1_DES" = col_character(),
    #UM_DGR_1_MINOR_1_CIP_CD = col_character(),
    #UM_DGR_1_MINOR_1_CIP_DES = col_character(),
    #UM_DGR_1_MINOR_2_CD = col_character(),
    "UM_DGR_1_MINOR_2_DES" = col_character(),
    #UM_DGR_1_MINOR_2_CIP_CD = col_double(),
    #UM_DGR_1_MINOR_2_CIP_DES = col_character(),
    "UM_DGR_2_CD" = col_character(),
    "UM_DGR_2_DES" = col_character(),
    "UM_DGR_2_HONORS_CD" = col_character(),
    "UM_DGR_2_HONORS_DES" = col_character(),
    "UM_DGR_2_ACAD_CRER_CD" = col_character(),
    "UM_DGR_2_ACAD_CRER_DES" = col_character(),
    "UM_DGR_2_ED_LVL_CD" = col_double(),
    "UM_DGR_2_ED_LVL_SHORT_DES" = col_character(),
    "UM_DGR_2_CMPLTN_TERM_CD" = col_character(),
    "UM_DGR_2_CMPLTN_TERM_DES" = col_character(),
    "UM_DGR_2_CNFR_DT" = col_character(),
    "UM_DGR_2_MAJOR_1_CD" = col_character(),
    "UM_DGR_2_MAJOR_1_DES" = col_character(),
    #UM_DGR_2_MAJOR_1_CIP_CD = col_character(),
    #UM_DGR_2_MAJOR_1_CIP_DES = col_character(),
    "UM_DGR_2_MAJOR_2_CD" = col_character(),
    "UM_DGR_2_MAJOR_2_DES" = col_character(),
    #UM_DGR_2_MAJOR_2_CIP_CD = col_character(),
    #UM_DGR_2_MAJOR_2_CIP_DES = col_character(),
    #UM_DGR_2_MINOR_1_CD = col_character(),
    "UM_DGR_2_MINOR_1_DES" = col_character(),
    #UM_DGR_2_MINOR_1_CIP_CD = col_character(),
    #UM_DGR_2_MINOR_1_CIP_DES = col_character(),
    #UM_DGR_2_MINOR_2_CD = col_character(),
    "UM_DGR_2_MINOR_2_DES" = col_character(),
    #UM_DGR_2_MINOR_2_CIP_CD = col_character(),
    #UM_DGR_2_MINOR_2_CIP_DES = col_character(),
    "UM_DGR_3_CD" = col_character(),
    "UM_DGR_3_DES" = col_character(),
    "UM_DGR_3_HONORS_CD" = col_character(),
    "UM_DGR_3_HONORS_DES" = col_character(),
    "UM_DGR_3_ACAD_CRER_CD" = col_character(),
    "UM_DGR_3_ACAD_CRER_DES" = col_character(),
    "UM_DGR_3_ED_LVL_CD" = col_double(),
    "UM_DGR_3_ED_LVL_SHORT_DES" = col_character(),
    "UM_DGR_3_CMPLTN_TERM_CD" = col_character(),
    "UM_DGR_3_CMPLTN_TERM_DES" = col_character(),
    "UM_DGR_3_CNFR_DT" = col_character(),
    "UM_DGR_3_MAJOR_1_CD" = col_character(),
    "UM_DGR_3_MAJOR_1_DES" = col_character(),
    #UM_DGR_3_MAJOR_1_CIP_CD = col_character(),
    #UM_DGR_3_MAJOR_1_CIP_DES = col_character(),
    "UM_DGR_3_MAJOR_2_CD" = col_character(),
    "UM_DGR_3_MAJOR_2_DES" = col_character(),
    #UM_DGR_3_MAJOR_2_CIP_CD = col_character(),
    #UM_DGR_3_MAJOR_2_CIP_DES = col_character(),
    #UM_DGR_3_MINOR_1_CD = col_character(),
    "UM_DGR_3_MINOR_1_DES" = col_character(),
    #UM_DGR_3_MINOR_1_CIP_CD = col_character(),
    #UM_DGR_3_MINOR_1_CIP_DES = col_character(),
    #UM_DGR_3_MINOR_2_CD = col_character(),
    "UM_DGR_3_MINOR_2_DES" = col_character(),
    #UM_DGR_3_MINOR_2_CIP_CD = col_character(),
    #UM_DGR_3_MINOR_2_CIP_DES = col_character(),
    #UG_LAST_SCHL_ATTND_CD = col_character(),
    #UG_LAST_SCHL_ATTND_DES = col_character(),
    #UG_LAST_SCHL_ATTND_CEEB_CD = col_character(),
    #UG_LAST_SCHL_ATTND_SCHL_TYP_CD = col_character(),
    #UG_LAST_SCHL_ATTND_SCHL_TYP_DS = col_character(),
    #UG_LAST_SCHL_ATTND_END_DT = col_character(),
    #UG_LAST_SCHL_ATTND_END_DUR_NBR = col_double()
  )
  return(tt)
}

#define the columns to read into memory (and keep) from the 
#student term table
student_info_term_cols <- function()
{
  
  tt <- cols_only(
    "STDNT_ID" = col_double(),
    "TERM_CD" = col_character(),
    "TERM_SHORT_DES" = col_character(),
    #RESCO_IND = col_double(),
    #SPPLMNT_STUDY_IND = col_double(),
    #TEACH_CERT_IND = col_double(),
    #"HONORS_PGM_IND" = col_double(),
    #ATHLTC_PRTCPT_SPORT_CD = col_double(),
    #ATHLTC_PRTCPT_SPORT_SHORT_DES = col_character(),
    "ACAD_CRER_CNT" = col_double(),
    "ACAD_PGM_CNT" = col_double(),
    "ACAD_MAJOR_CNT" = col_double(),
    "ACAD_MINOR_CNT" = col_double(),
    "PRMRY_CRER_CD" = col_character(),
    "PRMRY_CRER_DES" = col_character(),
    #REG_STAT_CD = col_character(),
    #REG_STAT_SHORT_DES = col_character(),
    #RES_CD = col_character(),
    #RES_SHORT_DES = col_character(),
    #ACAD_LOAD_CD = col_character(),
    #ACAD_LOAD_SHORT_DES = col_character(),
    #"ENTRY_TYP_SHORT_DES" = col_character(),
    "CRER_LVL_CD" = col_character(),
    #"ACAD_LVL_BOT_CD" = col_double(),
    #"ACAD_LVL_BOT_SHORT_DES" = col_character(),
    #"ACAD_LVL_EOT_CD" = col_double(),
    #"ACAD_LVL_EOT_SHORT_DES" = col_character(),
    #"GRD_PNTS" = col_double(),
    #"UNIT_TAKEN_GPA" = col_double(),
    #"UNIT_TAKEN_NO_GPA" = col_double(),
    #"CURR_GPA" = col_double(),
    #"CUM_GPA" = col_double(),
    #PREV_TERM_CUM_GPA = col_double(),
    "PGM_1_CD" = col_character(),
    "PGM_1_DES" = col_character(),
    "PGM_1_MAJOR_1_CD" = col_character(),
    "PGM_1_MAJOR_1_DES" = col_character(),
    #"PGM_1_MAJOR_1_CIP_CD" = col_character(),
    #"PGM_1_MAJOR_1_CIP_DES" = col_character(),
    "PGM_1_MAJOR_1_DCLR_DT" = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_1_CD = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_1_DES = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_1_CIP_CD = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_1_CIP_DES = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_1_DCLR_DT = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_2_CD = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_2_DES = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_2_CIP_CD = col_double(),
    #PGM_1_MAJOR_1_SUBPLN_2_CIP_DES = col_character(),
    #PGM_1_MAJOR_1_SUBPLN_2_DCLR_DT = col_character(),
    "PGM_1_MAJOR_2_CD" = col_character(),
    "PGM_1_MAJOR_2_DES" = col_character(),
    "PGM_1_MAJOR_2_CIP_CD" = col_character(),
    "PGM_1_MAJOR_2_CIP_DES" = col_character(),
    "PGM_1_MAJOR_2_DCLR_DT" = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_1_CD = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_1_DES = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_1_CIP_CD = col_double(),
    #PGM_1_MAJOR_2_SUBPLN_1_CIP_DES = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_1_DCLR_DT = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_2_CD = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_2_DES = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_2_CIP_CD = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_2_CIP_DES = col_character(),
    #PGM_1_MAJOR_2_SUBPLN_2_DCLR_DT = col_character(),
    #PGM_1_MINOR_1_CD = col_character(),
    #PGM_1_MINOR_1_DES = col_character(),
    #PGM_1_MINOR_1_CIP_CD = col_double(),
    #PGM_1_MINOR_1_CIP_DES = col_character(),
    #PGM_1_MINOR_1_DCLR_DT = col_character(),
    #PGM_1_MINOR_2_CD = col_character(),
    #PGM_1_MINOR_2_DES = col_character(),
    #PGM_1_MINOR_2_CIP_CD = col_double(),
    #PGM_1_MINOR_2_CIP_DES = col_character(),
    #PGM_1_MINOR_2_DCLR_DT = col_character(),
    "PGM_2_CD" = col_character(),
    "PGM_2_DES" = col_character(),
    "PGM_2_MAJOR_1_CD" = col_character(),
    "PGM_2_MAJOR_1_DES" = col_character(),
    #"PGM_2_MAJOR_1_CIP_CD" = col_character(),
    #"PGM_2_MAJOR_1_CIP_DES" = col_character(),
    "PGM_2_MAJOR_1_DCLR_DT" = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_1_CD = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_1_DES = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_1_CIP_CD = col_double(),
    #PGM_2_MAJOR_1_SUBPLN_1_CIP_DES = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_1_DCLR_DT = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_2_CD = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_2_DES = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_2_CIP_CD = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_2_CIP_DES = col_character(),
    #PGM_2_MAJOR_1_SUBPLN_2_DCLR_DT = col_character(),
    "PGM_2_MAJOR_2_CD" = col_character(),
    "PGM_2_MAJOR_2_DES" = col_character(),
    #"PGM_2_MAJOR_2_CIP_CD" = col_character(),
    #"PGM_2_MAJOR_2_CIP_DES" = col_character(),
    "PGM_2_MAJOR_2_DCLR_DT" = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_1_CD = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_1_DES = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_1_CIP_CD = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_1_CIP_DES = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_1_DCLR_DT = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_2_CD = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_2_DES = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_2_CIP_CD = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_2_CIP_DES = col_character(),
    #PGM_2_MAJOR_2_SUBPLN_2_DCLR_DT = col_character(),
    #PGM_2_MINOR_1_CD = col_character(),
    #PGM_2_MINOR_1_DES = col_character(),
    #PGM_2_MINOR_1_CIP_CD = col_character(),
    #PGM_2_MINOR_1_CIP_DES = col_character(),
    #PGM_2_MINOR_1_DCLR_DT = col_character(),
    #PGM_2_MINOR_2_CD = col_character(),
    #PGM_2_MINOR_2_DES = col_character(),
    #PGM_2_MINOR_2_CIP_CD = col_character(),
    #PGM_2_MINOR_2_CIP_DES = col_character(),
    #PGM_2_MINOR_2_DCLR_DT = col_character(),
    "PGM_3_CD" = col_character(),
    "PGM_3_DES" = col_character(),
    "PGM_3_MAJOR_1_CD" = col_character(),
    "PGM_3_MAJOR_1_DES" = col_character(),
    #"PGM_3_MAJOR_1_CIP_CD" = col_character(),
    #"PGM_3_MAJOR_1_CIP_DES" = col_character(),
    "PGM_3_MAJOR_1_DCLR_DT" = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_1_CD = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_1_DES = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_1_CIP_CD = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_1_CIP_DES = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_1_DCLR_DT = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_2_CD = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_2_DES = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_2_CIP_CD = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_2_CIP_DES = col_character(),
    #PGM_3_MAJOR_1_SUBPLN_2_DCLR_DT = col_character(),
    "PGM_3_MAJOR_2_CD" = col_character(),
    "PGM_3_MAJOR_2_DES" = col_character(),
    #"PGM_3_MAJOR_2_CIP_CD" = col_character(),
    #"PGM_3_MAJOR_2_CIP_DES" = col_character(),
    "PGM_3_MAJOR_2_DCLR_DT" = col_character()
    #PGM_3_MAJOR_2_SUBPLN_1_CD = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_1_DES = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_1_CIP_CD = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_1_CIP_DES = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_1_DCLR_DT = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_2_CD = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_2_DES = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_2_CIP_CD = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_2_CIP_DES = col_character(),
    #PGM_3_MAJOR_2_SUBPLN_2_DCLR_DT = col_character(),
    #PGM_3_MINOR_1_CD = col_character(),
    #PGM_3_MINOR_1_DES = col_character(),
    #PGM_3_MINOR_1_CIP_CD = col_character(),
    #PGM_3_MINOR_1_CIP_DES = col_character(),
    #PGM_3_MINOR_1_DCLR_DT = col_character(),
    #PGM_3_MINOR_2_CD = col_character(),
    #PGM_3_MINOR_2_DES = col_character(),
    #PGM_3_MINOR_2_CIP_CD = col_character(),
    #PGM_3_MINOR_2_CIP_DES = col_character(),
    #PGM_3_MINOR_2_DCLR_DT = col_character())
  )
  return(tt)
  
}

