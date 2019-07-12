range.of.experience <- function(sr,data)
{
  
  data$CATFLAG <- flag.level(data)
  data <- compute.enrollment(data)
  
  data       <- data[order(data$STDNT_ID), ]
  data$count <- sequence(rle(as.vector(data$STDNT_ID))$lengths)
  ntot       <- length(data$STDNT_ID)
  
  nid    <- length(data$STDNT_ID[!duplicated(data$STDNT_ID)])
  nstart <- which(data$count == 1)
  
  MEAN_ENROLL <- mat.or.vec(ntot,1)
  STDNT_ID    <- MEAN_ENROLL 
  MAX_FMT     <- MEAN_ENROLL
  FMT1        <- MEAN_ENROLL
  FMT2        <- MEAN_ENROLL
  FMT3        <- MEAN_ENROLL
  FMT4        <- MEAN_ENROLL
  FMT5        <- MEAN_ENROLL
  FMT1_FRAC        <- MEAN_ENROLL
  FMT2_FRAC        <- MEAN_ENROLL
  FMT3_FRAC        <- MEAN_ENROLL
  FMT4_FRAC        <- MEAN_ENROLL
  FMT5_FRAC        <- MEAN_ENROLL
  DEPTH            <- MEAN_ENROLL
  
  for (i in 1:nid)
  {
    if (i %% 1000 == 0){print(i)}
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind        <- c(start_ind:stop_ind)
    if (length(ind) > 1)
    {
      MEAN_ENROLL[i] <- sum(data$COURSE_SIZE[ind]*data$UNITS_ERND_NBR[ind],na.rm=TRUE)/
                        sum(data$UNITS_ERND_NBR[ind],na.rm=TRUE)
      STDNT_ID[i]    <- data$STDNT_ID[start_ind]
      tt             <- table(data$CRSE_CMPNT_CD[ind])
      MAX_FMT[i]     <- names(tt[which.max(tt)])[1]
      print(length(ind))
      tab <- as.data.frame(aggregate(UNITS_ERND_NBR ~ CRSE_CMPNT_CD,data=data[ind,],FUN=sum))
    
      if (dim(tab)[1] > 1){tab <- tab[order(-tab$UNITS_ERND_NBR),]}
    
      ntyp <- dim(tab)[1]
      tab$UNITS_ERND_NBR <- tab$UNITS_ERND_NBR/sum(tab$UNITS_ERND_NBR)
      if (ntyp >= 1){FMT1[i] <- as.character(tab$CRSE_CMPNT_CD[1]); FMT1_FRAC[i] <- tab$UNITS_ERND_NBR[1]}
      if (ntyp >= 2){FMT2[i] <- as.character(tab$CRSE_CMPNT_CD[2]); FMT2_FRAC[i] <- tab$UNITS_ERND_NBR[2]}
      if (ntyp >= 3){FMT3[i] <- as.character(tab$CRSE_CMPNT_CD[3]); FMT3_FRAC[i] <- tab$UNITS_ERND_NBR[3]}
      if (ntyp >= 4){FMT4[i] <- as.character(tab$CRSE_CMPNT_CD[4]); FMT4_FRAC[i] <- tab$UNITS_ERND_NBR[4]}
      if (ntyp >= 5){FMT5[i] <- as.character(tab$CRSE_CMPNT_CD[5]); FMT5_FRAC[i] <- tab$UNITS_ERND_NBR[5]}
    
      tab2 <- table(as.character(data$CATFLAG[ind]))
      DEPTH[i] <- sum(as.numeric(names(tab2))*as.numeric(tab2))/sum(as.numeric(tab2))
    }
  }
  
  #out <- data.frame(STDNT_ID,MEAN_ENROLL,MAX_FMT,FMT1,FMT2,FMT3,FMT4,FMT5,
  #                  FMT1_FRAC,FMT2_FRAC,FMT3_FRAC,FMT4_FRAC,FMT5_FRAC,DEPTH)
  out  <-  data.frame(STDNT_ID,MEAN_ENROLL,MAX_FMT,FMT1,FMT1_FRAC,DEPTH)
  
  sr  <- merge(sr,out,by='STDNT_ID',all.x=TRUE)
  
  return(sr)
  
  
}

compute.enrollment <- function(data)
{

  UID        <- paste(data$CRSE_ID_CD,data$TERM_CD,data$CLASS_SCTN_CD,sep=".")
  data       <- data.frame(data,UID)
                      
  data       <- data[order(data$UID), ]
  data$count <- sequence(rle(as.vector(data$UID))$lengths)
  ntot       <- length(data$UID)
  
  nid    <- length(data$UID[!duplicated(data$UID)])
  nstart <- which(data$count == 1)
  
  COURSE_SIZE <- mat.or.vec(ntot,1)
  
  for (i in 1:nid)
  {
    #print(i)
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind        <- c(start_ind:stop_ind)
    COURSE_SIZE[ind] <- length(ind)
  }
 
  data <- data.frame(data,COURSE_SIZE)

  return(data)
  
}

flag.level <- function(sc)
{
  e1 <- as.character(sc$CATLG_NBR) < 200 
  e2 <- as.character(sc$CATLG_NBR) >= 200 & as.character(sc$CATLG_NBR) < 300 
  e3 <- as.character(sc$CATLG_NBR) >= 300 & as.character(sc$CATLG_NBR) < 400 
  e4 <- as.character(sc$CATLG_NBR) >= 400 & as.character(sc$CATLG_NBR) < 500 
  e5 <- as.character(sc$CATLG_NBR) >= 500 & as.character(sc$CATLG_NBR) < 600
  e6 <- as.character(sc$CATLG_NBR) >= 600 & as.character(sc$CATLG_NBR) < 700
  
  CATFLAG <- mat.or.vec(dim(sc)[1],1)
  CATFLAG[which(e1)] <- 100
  CATFLAG[which(e2)] <- 200
  CATFLAG[which(e3)] <- 300
  CATFLAG[which(e4)] <- 400
  CATFLAG[which(e5)] <- 500
  CATFLAG[which(e6)] <- 600
  
  return(CATFLAG)
  
}