disciplinary.depth <- function(sr,sc)
{
  
  e <- sc$PRMRY_CRER_CD %in% 
    c("AO","UARC","UART","UBA","UDH","UEDU","UENG","UINF", 
      "UJND", "UKIN", "ULSA", "UMED", "UMUS", "UND",  "UNRE", "UNUR", "UPHR", "UPP","URC")
  sc <- sc[which(e),]
  
  sc       <- sc[order(sc$STDNT_ID), ]
  sc$count <- sequence(rle(as.vector(sc$STDNT_ID))$lengths)
  ntot       <- length(sc$STDNT_ID)
  
  nid    <- length(sc$STDNT_ID[!duplicated(sc$STDNT_ID)])
  nstart <- which(sc$count == 1)
  
  out <- mat.or.vec(nid,1)
  STDNT_ID  <- mat.or.vec(nid,1)
  FAVSUB  <- STDNT_ID
  MAXSUB  <- STDNT_ID
  FRACSUB <- STDNT_ID
  SUBDIV  <- STDNT_ID
  q       <- 2
  
  for (i in 1:nid)
  {
    
    if (i %% 1000 == 0){print(i)}
    
    start_ind <- nstart[i]
    
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind        <- c(start_ind:stop_ind)
    
    tab <- table(as.character(sc$CRSE_CIP_DES[ind]))
    kk <- tab[order(-tab)]
    
    STDNT_ID[i]      <- sc$STDNT_ID[start_ind]
    FAVSUB[i]  <- names(kk)[1]
    MAXSUB[i]  <- as.numeric(kk)[1]
    FRACSUB[i] <- MAXSUB[i]/sum(as.numeric(kk))
    
    e <- names(tab) != "NA's" & names(tab) != "" & as.numeric(tab > 0)
    tab <- tab[e]
    tot <- sum(as.numeric(tab))
    simp <- (sum(as.numeric(tab^q))/tot^q)^(1/(1-q))
    SUBDIV[i] <- simp
    
  }
  
  out <- data.frame(STDNT_ID,FAVSUB,MAXSUB,FRACSUB,SUBDIV)
  out <- as_tibble(out)
  
  sr <- left_join(sr,out)
  
  return(sr)
}