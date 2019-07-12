#############################
#FUNCTION: larc.alt.gpao
#PURPOSE: recompute GPAO using your favorite grade scale
#INSTRUCTIONS: 
#1) set the grade scale in the function 'new.grade.scale()' below. I've given a basic example
#2) run the function, it will out the same table, only with the rescaled grade and GPAO added.
#Note: 
#This runs slow: on 1 x 10^7 lines in the student course info table, this takes roughly an hour on my laptop.
#
#############################
larc.alt.gpao <- function(lsc)
{
  print('sorting')
  lsc       <- lsc[order(lsc$STDNT_ID,lsc$TERM_CD),]
  print('finished sort')
  lsc$count <- sequence(rle(as.vector(lsc$STDNT_ID))$lengths)
  
  nid    <- length(lsc$STDNT_ID[!duplicated(lsc$STDNT_ID)])
  nstart <- which(lsc$count == 1)
  ntot   <- length(lsc$STDNT_ID)
  
  GPAO_NEW <- mat.or.vec(ntot,1)
  GRADE_RESCALE <- new.grade.scale(lsc)
  lsc <- data.frame(lsc,GRADE_RESCALE) #in the GPAO formula, GRADE_RESCALE will replace the larc GRD_PTS_PER_UNIT_NBR
  
  for (i in 1:nid)
  {
    #print(paste(i,' of ',nid,sep=""))
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    
    #sub   <- lsc[ind,]
    terms <- lsc$TERM_CD[ind[!duplicated(lsc$TERM_CD[ind])]]
    nterm <- length(terms)
    
    for (j in 1:nterm)
    {
        e <- lsc$TERM_CD[ind] <= terms[j] #& sub$INCL_GPA_IND == 1
        f <- lsc$TERM_CD[ind] == terms[j] 
        TOTGP <- sum(lsc$GRADE_RESCALE[ind[e]]*lsc$UNITS_ERND_NBR[ind[e]]*lsc$INCL_GPA_IND[ind[e]])
        TOTCR <- sum(lsc$UNITS_ERND_NBR[ind[e]]*lsc$INCL_GPA_IND[ind[e]])
        GPAO_NEW[ind[f]] <- (TOTGP-lsc$GRADE_RESCALE[ind[f]]*lsc$UNITS_ERND_NBR[ind[f]]*lsc$INCL_GPA_IND[ind[f]])/
                            (TOTCR-lsc$UNITS_ERND_NBR[ind[f]]*lsc$INCL_GPA_IND[ind[f]])
    }
  }
  
  
  lsc <- data.frame(lsc,GPAO_NEW)
  return(lsc)
}

#put your favorite grading scale here
new.grade.scale <- function(lsc)
{
  GRADE_RESCALE <- mat.or.vec(dim(lsc)[1],1)
  GRADE_RESCALE[] <- NA
  e <- lsc$CRSE_GRD_INPUT_CD == 'A+'
  GRADE_RESCALE[e] <- 4.4
  e <- lsc$CRSE_GRD_INPUT_CD == 'B+'
  GRADE_RESCALE[e] <- 3.4
  
  return(GRADE_RESCALE)
  
}