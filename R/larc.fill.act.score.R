#this takes the full SR table
#-uses students with both ACT/SAT scores to estimate an ACT score
# with those missing.
#
larc.fill.act.score <- function(sr)
{
  out <- sample.act.scores(sr)
  out <- sample.act.scores(out,type='ENGL')
  return(out)
}

sample.act.scores <- function(data,type='MATH')
{
  
  if (type == 'MATH')
  {
    SAT <- data$MAX_SATI_MATH_SCR
    ACT <- data$MAX_ACT_MATH_SCR
  }
  if (type == 'ENGL')
  {
    SAT <- data$MAX_SATI_VERB_SCR
    ACT <- data$MAX_ACT_ENGL_SCR
  }
  
  data <- data.frame(data,SAT,ACT)

  #Vectorize:
  #Sort by SAT score and add 'counting' indices using RLE and SEQUENCE

  data <- data[order(data$SAT),]
  data$count <- sequence(rle(as.vector(data$SAT))$lengths)

  nid    <- length(data$SAT[!duplicated(data$SAT)])
  nstart <- which(data$count == 1)
  ntot   <- length(data$SAT)
  impACT    <- data$ACT
  ACTFLAG   <- mat.or.vec(ntot,1)
  
  #Loop over each of the SAT bins
  for (i in 1:nid)
  {
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    sub <- data[ind,]
    e <- !is.na(sub$ACT)
    f <- is.na(sub$ACT)
    nna <- sum(f,na.rm=TRUE)
    
    #impACT[ind[f]] <- sample(sub$ACT[f],nna,replace=TRUE)
    if (nna > 1)
    {
      impACT[ind[f]] <- round(mean(sub$ACT[e],na.rm=TRUE))
      ACTFLAG[ind[f]] <- 1
    }
    if (nna < 2){ACTFLAG[ind[f]] <- 2}
    
  }

  if (type == 'MATH')
  {
    data <- data[,!names(data) %in% c('MAX_ACT_MATH_SCR','SAT','ACT')]
    MAX_ACT_MATH_SCR <- impACT
    ACTFLAG_MATH     <- ACTFLAG
    data <- data.frame(data,MAX_ACT_MATH_SCR,ACTFLAG_MATH)
  }
  
  if (type == 'ENGL')
  {
    data <- data[,!names(data) %in% c('MAX_ACT_ENGL_SCR','SAT','ACT')]
    MAX_ACT_ENGL_SCR <- impACT
    ACTFLAG_ENGL     <- ACTFLAG
    data <- data.frame(data,MAX_ACT_ENGL_SCR,ACTFLAG_ENGL)
  }
  
  return(data)
  
}