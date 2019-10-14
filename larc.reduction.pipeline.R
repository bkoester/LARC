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

larc_table_reduction <- function(TAG='20190529',
                                 CODEDIR='/Users/bkoester/Google Drive/code/REBUILD/LARC.GITHUB/',
                                 LARCDIR="/Users/bkoester/Box Sync/LARC.FLAT/",
                                 OUTDIR="/Users/bkoester/Box Sync/LARC.WORKING/")
{
  
  library(tidyverse)
  library(zipcode)
  
  DIR_TO_R    <- str_c(CODEDIR,'R/',sep="")
  DIR_TO_DATA <- str_c(CODEDIR,'data/',sep="")
  
  lsiname <- paste(LARCDIR,"LARC_",TAG,"_STDNT_INFO.csv",sep="")
  lstname <- paste(LARCDIR,"LARC_",TAG,"_STDNT_TERM_INFO.csv",sep="")
  lscname <- paste(LARCDIR,"LARC_",TAG,"_STDNT_TERM_CLASS_INFO.csv",sep="")
  
  outSCname <- paste(OUTDIR,'BPK_LARC_STUDENT_COURSE_',TAG,'.tab',sep="")
  outSRname <- paste(OUTDIR,'BPK_LARC_STUDENT_RECORD_',TAG,'.tab',sep="")
  outSTname <- paste(OUTDIR,'BPK_LARC_STUDENT_TERM_',TAG,'.tab',sep="")
  
  #outSCnameTEMP <- paste(OUTDIR,'TEMP_BPK_LARC_STUDENT_COURSE_',TAG,'.tab',sep="")
  #outSRnameTEMP  <- paste(OUTDIR,'TEMP_BPK_LARC_STUDENT_RECORD_',TAG,'.tab',sep="")
  #outSTnameTEMP  <- paste(OUTDIR,'TEMP_BPK_LARC_STUDENT_TERM_',TAG,'.tab',sep="")
  
  #most basic is to read and reduced the three tables to include things after Fall 1998.
  source(paste(DIR_TO_R,"basic_table_reduction.R",sep=""))
  
  jj  <- student_info_term_cols()
  lst <- read_csv(lstname,col_types=jj)
  lst <- lst %>% filter(TERM_CD >= 1210) #CRER_LVL_CD == 'U' & 
  
  jj  <- course_term_cols()
  lsc <- read_csv(lscname,col_types=jj)
  lsc <- lsc %>% filter(GRD_BASIS_ENRL_DES == 'Graded' & TERM_CD >= 1210)
  
  jj  <- student_info_cols()
  lsi <- read_csv(lsiname,col_types=jj)
  lsi <- lsi %>% filter(FIRST_TERM_ATTND_CD >= 1210)
  
  #the next thing to is join tables so that the course table
  #contains the student career the term a course was taken, as well
  #as a measure of clustering of courses by grade pattern. 
  source(paste(DIR_TO_R,"basic_derived_columns.R",sep=""))
  source(paste(DIR_TO_R,"larc.cluster.grade.patterns.R",sep=""))
  lst <- as.data.frame(lst)
  lsc <- as.data.frame(lsc)
  lsc <- reduce.lsc.table(lsc,lst) #add student career, standing to course table, course clustering
  lsi <- as.data.frame(lsi)
  lsi <- reduce.lsi.table(lsi)     #add zip code median income and DIVISION of major.
  
  #merge in incoome, major-division data
  source(paste(DIR_TO_R,"auxiliary_table_joins.R",sep=""))
  lsi <- add.zipcode.county.geoid(lsi,DATADIR=DIR_TO_DATA)
  lsi <- add.major.division(lsi,DATADIR=DIR_TO_DATA)
          
  #label the transfer students
  source(paste(DIR_TO_R,"label_transfer_students.R",sep=""))
  lsi <- label_transfer_students(lst,lsi)
  
                       
  write_tsv(lsc,outSCname)
  write_tsv(lsi,outSRname)
  write_tsv(lst,outSTname)
  
  return()
}









