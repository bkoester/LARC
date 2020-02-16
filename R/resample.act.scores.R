#FUNCTION: impute.act.scores
#PURPOSE: Fill in missing ACT scores to square-off tables by using imputation as
#         opposed to look-up tables.
#INPUT:   LARC student-info table with SAT & ACT columns.
#         BY_TERM: Do imputation overall (default) or by entry term.
#OUTPUT:  The same table, now with impute ACT scores and a flag that indicates if an ACT score was imputed.
###############

impute.act.scores <- function(in_data,BY_TERM=FALSE)
{
  cbreaks <- c(1:36) #Composite
  mbreaks <- c(1:36) #SAT Math
  ACT_IMPUTE   <- mat.or.vec(length(in_data$STDNT_ID),1)
  ACT_IMPUTE[] <- 0
  
  in_data    <- data.frame(in_data,ACT_IMPUTE)
  TERMS      <- in_data$FIRST_TERM_ATTND_CD[!duplicated(in_data$FIRST_TERM_ATTND_CD)]
  nterms     <- length(TERMS)
  if (BY_TERM == FALSE){nterms <- 1}
  
  for (kk in 1:nterms)
  {
    if (BY_TERM == TRUE)
    {
      ind <- which(in_data$FIRST_TERM_ATTND_CD == TERMS[kk] & !is.na(in_data$FIRST_TERM_ATTND_CD))
      data <- in_data[ind,]
    } else
    {
      data <- in_data
    }
    
    #First do the math scores
    mact    <- data$MAX_SATI_MATH_SCR[!duplicated(data$MAX_SATI_MATH_SCR)]
    mact    <- mact[!is.na(mact)]
    mact    <- sort(mact)
    
    for (i in mact)
    {
    dd <- data$MAX_SATI_MATH_SCR == i & 
      !is.na(data$MAX_ACT_MATH_SCR) &
      data$MAX_ACT_MATH_SCR > 0
    
    if (sum(dd,na.rm=TRUE) > 0)
    {
      gg <- hist(data$MAX_ACT_MATH_SCR[which(dd)],breaks=mbreaks,plot=FALSE)
      e  <- which(is.na(data$MAX_ACT_MATH_SCR) & data$MAX_SATI_MATH_SCR == i)
      len <- length(e)
      out <- sample(gg$mids-0.5,len,prob=gg$counts/sum(gg$counts),replace=TRUE)
      #par(mfrow=c(2,1))
      #hist(data$MAX_SATI_MATH_SCR[dd],breaks=mbreaks,main=paste('ACT = ',i))
      #hist(out,breaks=mbreaks)
      #scan()
      data$MAX_ACT_MATH_SCR[e] <- out 
      data$ACT_IMPUTE[e] <- 1
    }
    }   
    #And then English/Verbal
    #First do the math scores
    mact    <- data$MAX_SATI_VERB_SCR[!duplicated(data$MAX_SATI_VERB_SCR)]
    mact    <- mact[!is.na(mact)]
    mact    <- sort(mact)
  
    for (i in mact)
    {
    dd <- data$MAX_SATI_VERB_SCR == i & 
      !is.na(data$MAX_ACT_ENGL_SCR) &
      data$MAX_ACT_ENGL_SCR > 0
    
    if (sum(dd,na.rm=TRUE) > 0)
    {
      gg <- hist(data$MAX_ACT_ENGL_SCR[which(dd)],breaks=mbreaks,plot=FALSE)
      e  <- which(is.na(data$MAX_ACT_ENGL_SCR) & data$LAST_SATI_VERB_SCORE == i)
      len <- length(e)
      out <- sample(gg$mids-0.5,len,prob=gg$counts/sum(gg$counts),replace=TRUE)
      #par(mfrow=c(2,1))
      #hist(data$MAX_SATI_MATH_SCR[dd],breaks=mbreaks,main=paste('ACT = ',i))
      #hist(out,breaks=mbreaks)
      #scan()
      data$MAX_ACT_ENGL_SCR[e] <- out 
    }
  }   
  
  
  #And then the composite numbers
  cact    <- data$MAX_SATI_TOTAL_CALC_SCR[!duplicated(data$MAX_SATI_TOTAL_CALC_SCR)]
  cact    <- cact[!is.na(cact)]
  cact    <- sort(cact)
  
  for (i in cact)
  {
    dd <- data$MAX_SATI_TOTAL_CALC_SCR == i & 
      !is.na(data$MAX_ACT_COMP_SCR) &
      data$MAX_ACT_COMP_SCR > 0
    #print(i)
    #print(sum(dd,na.rm=TRUE))
    if (sum(dd,na.rm=TRUE) > 0)
    {
      gg <- hist(data$MAX_ACT_COMP_SCR[which(dd)],breaks=cbreaks,plot=FALSE)
      e  <- which(is.na(data$MAX_ACT_COMP_SCR) & data$MAX_SATI_TOTAL_CALC_SCR == i)
      len <- length(e)
      out <- sample(gg$mids-0.5,len,prob=gg$counts/sum(gg$counts),replace=TRUE)
      #par(mfrow=c(2,1))
      #hist(data$MAX_SATI_MATH_SCR[dd],breaks=mbreaks,main=paste('ACT = ',i))
      #hist(out,breaks=mbreaks)
      #scan()
      data$MAX_ACT_COMP_SCR[e] <- out 
    }
  }
  if (BY_TERM == TRUE)
    {
      in_data$MAX_ACT_COMP_SCORE[ind] <- data$MAX_ACT_COMP_SCR
      in_data$MAX_ACT_MATH_SCR[ind]   <- data$MAX_ACT_MATH_SCR
      in_data$MAX_ACT_ENGL_SCR[ind]   <- data$MAX_ACT_ENGL_SCR
      in_data$ACT_IMPUTE[ind]         <- data$ACT_IMPUTE
    }
    else
    {
      in_data <- data
    }
  }
  
  return(in_data)
}
