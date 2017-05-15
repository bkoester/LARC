 #This can be run on the larc student course tables. By default it
#1) fits things into exactly 15 clusters
#2) ignores courses with enrolmment < 100 (CLUSTER = 0)
larc.cluster.grade.patterns <- function(sc)
{
  print('clustering and labeling grade distributions')
  sc <- kmeans.cluster(sc)
  sc <- sc[,!names(sc) %in% c('CRSE_LONG','CRSE_SHRT')]
  return(sc)

}

#run this also to create the input WWW dataset
kmeans.cluster <- function(sc,ncl=15)
{
  
  GRD     <- c('A+','A','A-','B+','B','B-','C+','C','C-',
               'D+','D','D-','E','F','W','I')
  ngrd <- length(GRD)
  
  #this gets a grade dist'n by course-term matrix
  mydata <- cluster.grading.patterns(sc)
  
  full  <- make.cluster.input(sc,GRD)
  full$CRSE_SHRT <- paste(full$SBJCT_CD,full$CATLG_NBR,sep="")
  #full  <- full[,names(full) %in% c('CRSE_ID_CD','CRSE_LONG','CRSE_SHRT','CRSE_GRD_OFFCL_CD','GRD_PNTS_PER_UNIT_NBR')]
  CLUSTER <- mat.or.vec(dim(full)[1],1)
  TYPE    <- CLUSTER
  
  dir <- '/Users/bkoester/Google Drive/code/REBUILD/grade patterns/www/data/'
  fit <- kmeans(mydata, ncl,iter.max=100) # 5 cluster solution
  # get cluster means 
  aggregate(mydata,by=list(fit$cluster),FUN=mean)
  
  for (i in 1:ncl)
  {
    e <- which(fit$cluster == i)
    e <- sample(e,length(e))
    nsub <- length(e)
    nms <- rownames(mydata)[e]
    
    jj <- full$CRSE_LONG %in% rownames(mydata)[e] 
    CLUSTER[jj] <- i
    
  }
  
  full <- data.frame(full,CLUSTER)
  
  course  <- rownames(mydata)
  cluster <- fit$cluster
  
  CRSE <- rownames(mydata)
  
  data <- data.frame(CRSE,mydata,cluster)
  names(data)[2:17] <- GRD
  
  return(full)
  
}

#this takes an SC table that has been passed through "make.cluster.input"
#REturns a grade matrix (proporition with grade = X by each course Y)
cluster.grading.patterns <- function(sc)
{
  
  GRD <- c('A+','A','A-','B+','B','B-','C+','C','C-',
           'D+','D','D-','E','F','W','I')
  ngrd <- length(GRD)
  
  #just get the useful grades
  sc <- make.cluster.input(sc,GRD,clean=TRUE)
  sc       <- sc[order(sc$CRSE_LONG), ]
  sc$count <- sequence(rle(as.vector(sc$CRSE_LONG))$lengths)
  
  CRSE     <- sc$CRSE_LONG[!duplicated(sc$CRSE_LONG)]
  CRSE_LIST <- CRSE
  TERM     <- CRSE
  ncrse    <- length(CRSE)
  nstart   <- which(sc$count == 1)
  ntot     <- length(sc$CRSE_LONG) 
  
  grdmtx <- mat.or.vec(ncrse,ngrd)
  tot    <- mat.or.vec(ncrse,1)
  
  BIGGRD  <- mat.or.vec(ntot,1)
  BIGCRSE <- BIGGRD
  BIGTERM <- BIGGRD
  BIGTOT  <- BIGGRD
  
  for (i in 1:ncrse)
  {
    #print(paste(i,ncrse,sep=" "))
    start_ind <- nstart[i]
    if (i < ncrse){stop_ind  <- nstart[i+1]-1}
    if (i == ncrse){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    CRSE_LIST[i] <- paste(sc$SBJCT_CD[start_ind],sc$CATLG_NBR[stop_ind],'_',sc$TERM_CD[start_ind],sep="")
    TERM[i]      <- sc$TERM_CD[start_ind]
    tot[i] <- length(ind)
    jj  <- table(sc$CRSE_GRD_OFFCL_CD[ind])
    
    ng  <- names(jj)
    cts <- as.numeric(jj)
    
    kk <- match(ng,GRD)
    
    grdmtx[i,kk] <- cts
    
    BIGGRD[ind]  <- sc$CRSE_GRD_OFFCL_CD[ind]
    BIGCRSE[ind] <- CRSE_LIST[i]
    BIGTERM[ind] <- sc$TERM_CD[ind]
    BIGTOT[ind]  <- length(ind)
    
  }
  
  rownames(grdmtx) <- CRSE_LIST
  colnames(grdmtx) <- GRD
  
  print('only clustering COURSE-TERMS with > 100 students!')
  #ONLY LOOKING at courses > 100 for now
  e <- tot > 100
  grdmtx <- grdmtx[e,]
  ncrse <- dim(grdmtx)[1]
  for (i in 1:ncrse){grdmtx[i,] <- grdmtx[i,]/sum(grdmtx[i,])}
 
  return(grdmtx)
}

#This flags course-grades that we want to include in our estimates and adds two columns to an SC table.
#CLUSTER_FLAG: yes, use this grade-course combo in the modeling
#CRSE_LNG: unique ID for each course term...CRSE_ID_CD will not suffice as it's the same for all terms.
make.cluster.input <- function(sc,GRD,clean=FALSE)
{
  
  e <- sc$CRSE_GRD_OFFCL_CD %in% GRD
 
  if (clean == TRUE){sc       <- sc[which(e),]}
  CRSE_GRD_OFFCL_CD <- as.character(sc$CRSE_GRD_OFFCL_CD)
  
  #CLUSTER_FLAG <- mat.or.vec(dim(sc)[1],1)
  #CLUSTER_FLAG[e] <- 1
  
  sc <- sc[,!names(sc) %in% 'CRSE_GRD_OFFCL_CD']
  sc <- data.frame(sc,CRSE_GRD_OFFCL_CD)
  
  e        <- as.character(sc$CATLG_NBR) >= 500 #sc$TERM_CD >= 1560 & 
  #sc       <- sc[which(e),]
  
  sc$CRSE_LONG <- paste(as.character(sc$SBJCT_CD),sc$CATLG_NBR,'_',sc$TERM_CD,sep="")
  #sc$CRSE_ID_CD <- sc$CRSE_LONG
  #sc$CLUSTER_FLAG <- CLUSTER_FLAG
  
  return(sc)
  
}