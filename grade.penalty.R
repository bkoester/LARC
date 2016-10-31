#####################################################################################
#Take the student course and student record, create grade penalty analysis of a course.
#This should on the student_info and class_info tables from LARC, but I haven't tested it yet.
#FUNCTION: grade.penalty
#PURPOSE : Compute grade penalties, gender effects with regression and matching in a course.
#INPUTS  : sr - student record table, trimmed to include only the student group of interest.
#          sc - full student course table
#          SBJCT_CD     - course subject used for labeling
#          CATLG_NBR - course catalog number used for labeling
#          TERM_RANGE  - lower and upper limits (using TERM_CD) of terms to be analyzed
#          PDF         - Write plots to PDF. Default is TRUE. Plots go to 'course_portrain.pdf' in CWD.
#          REGRESSION  - Run a basic linear regression, return the coefficient on the gender term.
#          MATCHING    - Run a matching analysis to compare grades of matched males and females.
#          LASSO       - Run LASSO on pre-selected variables (currently hard-coded), print to plot window
#OUTPUTS : Plots sent to grade.penalty.pdf in the CWD.
#Packages: optmatch,xtable
#EXAMPLE: out <- grade.penalty(sr,sc,'PHYSICS',135,REGRESSION=TRUE,MATCHING=TRUE,LASSO=TRUE)
#####################################################################################
grade.penalty <- function(sr,sc,SBJCT_CD,CATLG_NBR,TERM_RANGE=c(0,10000),
                          PDF=FALSE,REGRESSION=FALSE,MATCHING=FALSE,LASSO=FALSE)
{  
  
  require(optmatch)
  require(xtable)
  
  e <- sc$SBJCT_CD == SBJCT_CD & sc$CATLG_NBR == CATLG_NBR
  CRSE_ID_CD <- sc$CRSE_ID_CD[which(e)[1]]
  
  #SELECT the TERMS and students
  e    <- sc$CRSE_ID_CD == CRSE_ID_CD & 
          sc$TERM_CD >= TERM_RANGE[1] & sc$TERM_CD <= TERM_RANGE[2] &
          sc$CRSE_GRD_OFFCL_CD != 'W' & #don't include withdrawls - GRD_PTS = NA
          !is.na(sc$EXCL_CLASS_CUM_GPA) #uknown why these are GPAO == NA?
  
  sc <- sc[which(e),]
  terms <- sc$TERM_CD[!duplicated(sc$TERM_CD)]
  nterms <- length(terms)
  nst <- sum(e)

  #run LASSO 
  if (LASSO == TRUE){lass <- run.lasso(sr,sc,CRSE_ID_CD)}
  
  #Merge the course and record tables together.
  data <- merge(sc,sr,by='STDNT_ID',all.x=TRUE)
  title <- paste(SBJCT_CD,CATLG_NBR,sep=" ")
  
  #Select the male and female subsets and compute the Cohen's D for the difference
  #in grade penatly between the two groups
  m    <- data$STDNT_GNDR_SHORT_DES == 'Male'
  gm   <- data[which(m),]
  f    <- data$STDNT_GNDR_SHORT_DES == 'Female'
  gf   <- data[which(f),]
  ltitle1 <- 'males: '
  ltitle2 <- 'females: '
  
  #Compute the aggregate Cohen's d on the grade penalty
  gpm_agg <- gm$GRD_PNTS_PER_UNIT_NBR-gm$EXCL_CLASS_CUM_GPA
  gpf_agg <- gf$GRD_PNTS_PER_UNIT_NBR-gf$EXCL_CLASS_CUM_GPA
  #chd     <- cohen.d(gpm_agg,gpf_agg,pooled=TRUE)

  MEAN_MALE_GRADE <- mean(gm$GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE)
  MEAN_MALE_GPAO <- mean(gm$EXCL_CLASS_CUM_GPA,na.rm=TRUE)
  MEAN_FEMALE_GRADE <- mean(gf$GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE)
  MEAN_FEMALE_GPAO <- mean(gf$EXCL_CLASS_CUM_GPA,na.rm=TRUE)
  
  #Compute and plot the binned grade penalty
  fem.binned <- compute.gpa.binned.grades(gf,15)
  mal.binned <- compute.gpa.binned.grades(gm,15)

  title <- paste(SBJCT_CD,CATLG_NBR,"(","N = ",nst,")",sep=" ")
  if (length(terms) == 1){title <- paste(title,'(',data$TERM_SHORT_DES[1],')',sep=" ")}
  plot.binned.grades(mal.binned,title,col='black')
  lines(c(0,4),c(0,4))
  plot.binned.grades(fem.binned,title,col='red')

  dda <- compute.overall.grade.penalty(data)
  ddm <- round(compute.overall.grade.penalty(gm),2)
  ddf <- round(compute.overall.grade.penalty(gf),2)

  text(1,3.75,'AVERAGE GRADE ANOMALY',pos=4)
  text(1,3.5,paste(ltitle1,ddm$mn,'+/-',ddm$se,sep=" "),pos=4)
  text(1,3.25,paste(ltitle2,ddf$mn,'+/-',ddf$se,sep=" "),col='red',pos=4)
  #text(1,3.0,paste('Cohen d = ',signif(as.numeric(chd$estimate),3),'[',
  #               signif(as.numeric(chd$conf.int[1]),3),',',
  #               signif(as.numeric(chd$conf.int[2]),3),']',sep=""),pos=4)

  N <- nst
  N_FEMALES  <- length(which(f))
  N_MALES    <- length(which(m))
  MN_GA_ALL     <- dda$mn
  SE_GA_ALL     <- dda$se
  MN_GA_MALES   <- ddm$mn
  SE_GA_MALES   <- ddm$se
  MN_GA_FEMALES <- ddf$mn
  SE_GA_FEMALES <- ddf$se
 
  out <- data.frame(SBJCT_CD,CATLG_NBR,N,N_FEMALES,N_MALES,
                    MN_GA_ALL,SE_GA_ALL,
                    MEAN_MALE_GRADE,MEAN_FEMALE_GRADE,MEAN_MALE_GPAO,MEAN_FEMALE_GPAO,
                    MN_GA_MALES,SE_GA_MALES,MN_GA_FEMALES,SE_GA_FEMALES)

#Done making the basic plot, now get fancier if requested.
if (MATCHING == TRUE)
{  
  gg <- matching.analysis(data)
  MATCHED_MEAN_MALES   <- mean(gg$gpenm)
  MATCHED_SE_MALES     <- sd(gg$gpenm)/sqrt(length(gg$gpenm))
  MATCHED_MEAN_FEMALES <- mean(gg$gpenf)
  MATCHED_SE_FEMALES   <- sd(gg$gpenf)/sqrt(length(gg$gpenf))
  
  mmn <- round(mean(gg$gpenm),3)
  mse   <- round(sd(gg$gpenm)/sqrt(length(gg$gpenm)),3)
  fmn <- round(mean(gg$gpenf),3)
  fse <- round(sd(gg$gpenf)/sqrt(length(gg$gpenf)),3)
  
  #text(1,2.75,'MATCHED MEAN GRADE:',pos=4)
  #text(1,2.5,paste(ltitle1,mmn,'+/-',mse,sep=" "),pos=4)
  #text(1,2.25,paste(ltitle2,fmn,'+/-',fse,sep=" "),col='red',pos=4)
  #text(1,2.0,paste('Cohen d = ',signif(as.numeric(chd$estimate),3),'[',
  #                 signif(as.numeric(chd$conf.int[1]),3),',',
  #                 signif(as.numeric(chd$conf.int[2]),3),']',sep=""),pos=4)
  out <- data.frame(out,MATCHED_MEAN_MALES,MATCHED_SE_MALES,MATCHED_MEAN_FEMALES,MATCHED_SE_FEMALES)
  
}

#Run the regression if desired, return the coefficient (and SE) of the GENDER term
if (REGRESSION == TRUE)
{
 uber_reg <- grade.regression(data)
 res <- summary(uber_reg)
 SEX_REG    <- res$coefficients[3,1]
 SEX_REG_SE <- res$coefficients[3,2]
 #text(1,3.0,paste('SEX_REG_COEFF:',round(SEX_REG,3),'+/-',round(SEX_REG_SE,3)),pos=4)
 out <- data.frame(out,SEX_REG,SEX_REG_SE)
 
}
  return(out)
  
}
#####################################################################################
#Run a simple grade regression,wary of all the standard assumptions
#(i.e. gaussian error distribution, independence of covariates) we have violated in doing so!
#FUNCTION: grade.regression
#PURPOSE : Run a multivariate regression to estimate the gender effect, with grade as the dependent variable, 
#INPUTS  : data  - A merged student-record/student-course data frame
#          cname - the plot title
#          col   - Plot color. Default is black, and by setting it to black,
#                  a new plot is initiated.
#OUTPUTS : A plot (or overplotted points) directed to the current output device.
#####################################################################################
grade.regression <- function(data)
{
  #View(data)
  #reformat things for the regression
  GRD_PNTS_PER_UNIT_NBR        <- as.numeric(data$GRD_PNTS_PER_UNIT_NBR)
  STDNT_GNDR_SHORT_DES         <- as.factor(data$STDNT_GNDR_SHORT_DES)
  MAX_ACT_MATH_SCR       <- as.numeric(data$MAX_ACT_MATH_SCR)
  MAX_ACT_ENGL_SCR       <- as.numeric(data$MAX_ACT_ENGL_SCR)
  EXCL_CLASS_CUM_GPA     <- as.numeric(data$EXCL_CLASS_CUM_GPA)
  HS_GPA                   <- as.numeric(data$HS_GPA)
  
  model <- glm(GRD_PNTS_PER_UNIT_NBR ~ EXCL_CLASS_CUM_GPA+STDNT_GNDR_SHORT_DES+MAX_ACT_MATH_SCR+MAX_ACT_ENGL_SCR+HS_GPA)
  #print(summary(model))
  #textplot(signif(summary(model)$coefficients,3))
  #title('Linear Regression, Dependent Variable = Grade')
  #cmtx <- -1.0*cov2cor(vcov(model)) #factor of -1 b/c the correlations are all negative somehow (I spot-checked)
  #heatmap.2(cmtx,Rowv=FALSE,Colv=FALSE,main='Covariate Correlation', 
  #          cexRow=0.75,cexCol=0.75,trace='none',cellnote=signif(cmtx,2),notecol='black')
  return(model)
}

#####################################################################################
#Intiate the grade penalty plot
#FUNCTION: plot.binned grades
#PURPOSE : Plot (or overplot!) grade penalty data.
#INPUTS  : data  - A merged student-record/student-course data frame
#          cname - the plot title
#          col   - Plot color. Default is black, and by setting it to black,
#                  a new plot is initiated.
#OUTPUTS : A plot (or overplotted points) directed to the current output device.
#####################################################################################
plot.binned.grades <- function(data,cname,col='black',xlim=c(1,4),ylim=c(1,4))
{
  if (col == 'black')
  {
    plot(data$meangpa,data$meangd,pch=19,xlab='GPAO',ylab='<Grade>',main=cname,xlim=xlim,ylim=ylim)
    for (i in 1:length(data$meangpa))
    {
      arrows(data$meangpa,data$meangd-data$segd,data$meangpa,data$meangd+data$segd,code=0)
    }
  }
  if (col != 'black')
  {
    points(data$meangpa,data$meangd,pch=19,col=col)
    for (i in 1:length(data$meangpa))
    {
      arrows(data$meangpa,data$meangd-data$segd,data$meangpa,data$meangd+data$segd,code=0,col=col)
    }
  } 
}

#####################################################################################
#Take the student course and student record, create grade penalty analysis of a course.
#FUNCTION: compute.gpa.binned.grades
#PURPOSE : Return binned grade penalty point-estimates for plotting.
#INPUTS  : A merged student-record/student-course data frame
#          
#OUTPUTS : A data frame containing binned grade/gpao statistics for plotting..
#####################################################################################
compute.gpa.binned.grades <- function(data,nbins=10)
{
  min_gpao <- 1#min(data$GPAO)
  max_gpao <- 4#max(data$GPAO)
  binsize  <- (max_gpao-min_gpao)/nbins
  
  meangpa <- mat.or.vec(nbins,1)
  meangd  <- mat.or.vec(nbins,1)
  segd    <- mat.or.vec(nbins,1)
  count   <- 1  
  start_ind <- 0
  stop_ind <- 0
  
  for (i in 1:nbins)
  {
    tmin <- min_gpao+binsize*(i-1)
    tmax <- tmin+binsize
    ind        <- data$EXCL_CLASS_CUM_GPA > tmin & data$EXCL_CLASS_CUM_GPA < tmax
    
    meangpa[i] <- mean(data$EXCL_CLASS_CUM_GPA[which(ind)],na.rm=TRUE)
    meangd[i]  <- mean(data$GRD_PNTS_PER_UNIT_NBR[which(ind)],na.rm=TRUE)
    segd[i]    <- sd(data$GRD_PNTS_PER_UNIT_NBR[ind],na.rm=TRUE)/sqrt(length(which((ind))))
  }
  
  stats <- data.frame(meangpa,meangd,segd)
  return(stats)
}

#####################################################################################
#Match males and females for comparison.
#FUNCTION: matching.analysis
#PURPOSE : This matches males and females for a course based on serveral covariates using
#          the optmatch library.
#INPUTS  : A merged student-record/student-course data frame
#          
#OUTPUTS : A data frame containing binned grade/gpao statistics for plotting..
#####################################################################################  
matching.analysis <- function(data)
{
    library(optmatch)
    print('matching')
    #Clean out problem areas
    e         <- data$STDNT_GNDR_SHORT_DES != 'U' & 
                 !is.na(data$MAX_ACT_MATH_SCR) & !is.na(data$MAX_ACT_ENGL_SCR) &
                 data$HS_GPA > 0 
    data      <- data[which(e),]  
    
    #Define/coerce the matching variables.
    STDNT_GNDR_SHORT_DES       <- mat.or.vec(length(data$STDNT_ID),1)
    e         <- data$STDNT_GNDR_SHORT_DES == 'Female'
    STDNT_GNDR_SHORT_DES[e]    <- 1
    ACT.MATH  <- as.numeric(data$MAX_ACT_MATH_SCR)
    ACT.ENGL  <- as.numeric(data$MAX_ACT_ENGL_SCR)
    GRADE     <- as.numeric(data$GRD_PNTS_PER_UNIT_NBR)
    EXCL_CLASS_CUM_GPA      <- as.numeric(data$EXCL_CLASS_CUM_GPA)
    HS_GPA     <- as.numeric(data$HS_GPA)
    STDNT_ID    <- as.numeric(data$STDNT_ID)
    data      <- data.frame(STDNT_ID,STDNT_GNDR_SHORT_DES,ACT.MATH,ACT.ENGL,GRADE,EXCL_CLASS_CUM_GPA,HS_GPA)
    
    #Supply matching propensity scores
    model <- glm(STDNT_GNDR_SHORT_DES ~ ACT.MATH+ACT.ENGL+EXCL_CLASS_CUM_GPA+HS_GPA,family=binomial(),data=data)
    #...and execute the matching with a caliper set at 0.2 to increase speed...
     m1    <- fullmatch(match_on(model,caliper=0.2),data=data)
    
    #Now attach the matching structure to the data
    data <- cbind(data,matches=as.numeric(substr(m1,3,7)))
    out <- data
    
    #And sort once for speed, computing mean grades for the matched groups
    data       <- data[order(data$matches,data$STDNT_GNDR_SHORT_DES),] #This sort is crucial. Keeping the SEX makes sure that female is always index 1.
    data$count <- sequence(rle(as.vector(data$matches))$lengths)
    
    nid    <- length(data$matches[!duplicated(data$matches)])
    nstart <- which(data$count == 1)
    ntot   <- length(data$matches)
    
    rnames <- row.names(data) #mat.or.vec(nid,1)
    
    #Now compute the grades for males and matched females
    #Note that this matching ONLY considers one-to-one matching, as opposed to averaging
    #all N matches to a single individuals.
    
    gpenf <- mat.or.vec(nid,1)
    gpenm <- gpenf
    
    for (i in 1:nid)
    {
      start_ind <- nstart[i]
      if (i < nid){stop_ind  <- nstart[i+1]-1}
      if (i == nid){stop_ind <- ntot}
      ind <- c(start_ind:stop_ind)
      gpenf[i] <- as.numeric(data$GRADE[stop_ind])
      gpenm[i] <- as.numeric(data$GRADE[start_ind])
    }
    
    return(data.frame(gpenm,gpenf,nid))
  }  
  
#####################################################################################
#Compute un-adjusted, raw grade penalty for input data.
#FUNCTION: compute.overall.grade.penalty
#PURPOSE : Return raw grade penalty stats using either basic stats or boostrapping (default)
#INPUTS  : data: A bin (or full sample) of grade penalty-formatted data.
#        : BOOT: do boostrapping if set to TRUE
#          
#OUTPUTS : A data frame containing output statistics.
#####################################################################################
compute.overall.grade.penalty <- function(data,BOOT=TRUE)
{
  
  nstd <- length(data)         #Number of students
  gpen <- data$GRD_PNTS_PER_UNIT_NBR-data$EXCL_CLASS_CUM_GPA
  e <- is.na(gpen)
  View(data[which(e),])
  
  #Now compute the grade penalty
  if (BOOT == FALSE)
  {
    mn   <- mean(gpen)              #Mean Grade Penalty
    sd   <- sd(gpen)                #Standard deviation
    se   <- sd/sqrt(nstd)           #Naive standard error
    return(data.frame(mn,se))
  }
  
  #Bootstrap by resampling students. 
  #Obviously many ways to do this (e.g. resample terms?), depending on the question.
  #BOOTNUM == 1000 for > 10000 students takes t ~ 1 sec. 
  #Bootstrap SEs will probably be more useful for smaller sample sizes.
  if (BOOT == TRUE)
  {
    out <- bootstrap.bin.gpao(gpen)
    return(out)
  }
  
}

#####################################################################################
#Compute bootstrap mean and SE for 
#FUNCTION: bootstrap.bin.gpao
#PURPOSE : Return boostrap mean, SE on mean, 97.5% CI for input data
#INPUTS  : data: A bin (or full sample) of grade penalty-formatted data.
#        : NBOOT: number of bootstrap resamples to make (defeault = 100)
#          
#OUTPUTS : A data frame containing the mean, SE on mean, and 97.5% CI for the input data..
#####################################################################################
bootstrap.bin.gpao <- function(data,NBOOT=100)
{
  nstd  <- length(data)
  mnvec <- mat.or.vec(NBOOT,1)
  se95low  <-  mat.or.vec(NBOOT,1)
  se95high  <- mat.or.vec(NBOOT,1)
  
  for (i in 1:NBOOT)
  {
    sub <- sample(data,nstd,replace=TRUE)
    mnvec[i] <- mean(sub)
  }
  mn <- mean(mnvec)
  se <- sqrt(var(mnvec))
  se95low  <- signif(mnvec[0.025*NBOOT],3)
  se95high <- signif(mnvec[0.975*NBOOT],3)
  return(data.frame(mn,se,se95low,se95high))
}

remove.duplicates <- function(data,keep='NONE',verbose=FALSE)
{
  data       <- data[order(data$STDNT_ID,data$TERM_CD), ]
  data$count <- sequence(rle(as.vector(data$STDNT_ID))$lengths)
  
  ntot   <- length(data$ANONID)
  
  if (keep == "FIRST")
  { 
    good <- which(data$count == 1)
    ngood <- length(good)
    if (verbose == TRUE)
    {
      print('keeping the first grade only')
      print(paste('kept ',ngood,' records of ',ntot,sep=""))
    }
  }    
  else
  {
    nid    <- length(data$ANONID[!duplicated(data$ANONID)])
    nstart <- which(data$count == 1)
    
    kdup   <- mat.or.vec(ntot,1)
    kfirst <- mat.or.vec(ntot,1)
    klast  <- mat.or.vec(ntot,1)
    
    for (i in 1:nid)
    {
      start_ind <- nstart[i]
      if (i < nid){stop_ind  <- nstart[i+1]-1}
      if (i == nid){stop_ind <- ntot}
      ind <- c(start_ind:stop_ind)
      kdup[ind] <- length(ind)
      kfirst[ind] <- start_ind
      klast[ind]  <- stop_ind
    }
    
    if (keep == "NONE") 
    {
      good  <- which(klast == kfirst)
      ngood <- length(good)
      if (verbose == TRUE)
      {
        print('discarding duplicate students')
        print(paste('kept ',ngood,' students of ',nid,sep=""))
      }
    }
    if (keep == "LAST") 
    {
      good <- which(data$count == kdup)
      ngood <- length(good)
      if (verbose == TRUE)
      {
        print('keeping the last grade only')
        print(paste('kept ',ngood,' records of ',ntot,sep=""))
      }   
    }
  }
  
  return(good)
  
}

#For a given CRSE_ID, run LASSO on the columns defined in 'vnames' below
#to sort out the 'best' predictors of grade.
run.lasso <- function(sr,sc,crseid)
{
  e <- sc$CRSE_ID_CD == crseid
  sc <- sc[which(e),]
  data <- merge(sc,sr,by='STDNT_ID',all.x=TRUE)
  
  library(lars) #the library that contains the LASSO code
  vnames <- c('EST_GROSS_FAM_INC_CD','STDNT_GNDR_SHORT_DES','STDNT_ETHNC_GRP_SHORT_DES','MAX_ACT_MATH_SCR','MAX_ACT_ENGL_SCR',
              'ACAD_LVL_BOT_SHORT_DES','PRMRY_CRER_CD','HS_GPA',
              'EXCL_CLASS_CUM_GPA','TERM_CD')
  e <- !is.na(data$EST_GROSS_FAM_INC_CD) & !is.na(data$STDNT_GNDR_SHORT_DES) & !is.na(data$STDNT_ETHNC_GRP_SHORT_DES) & 
    !is.na(data$HS_GPA) & data$HS_GPA > 0 &
    !is.na(data$MAX_ACT_MATH_SCR) & !is.na(data$MAX_ACT_ENGL_SCR) & 
    !is.na(data$ACAD_LVL_BOT_SHORT_DES) & !is.na(data$PRMRY_CRER_CD) & !is.na(data$EXCL_CLASS_CUM_GPA) & 
    #is.finite(data$CUMGPA_SS) & is.finite(data$CUMGPA_S) & 
    #is.finite(data$CUMGPA_H) & is.finite(data$EXCL_CLASS_CUM_GPA) &
    (data$STDNT_GNDR_SHORT_DES == 'Female' | data$STDNT_GNDR_SHORT_DES == 'Male')
  #data$ADMIT_TERM >= 1810
  data <- data[which(e),]
  print(paste('running LASSO on ',length(which(e)),' of ',length(data$STDNT_GNDR_SHORT_DES),sep=""))

  y <- data$GRD_PNTS_PER_UNIT_NBR
  nstd <- length(y)
  
  x    <- mat.or.vec(length(vnames),nstd)
  x[1,] <- as.factor(data$EST_GROSS_FAM_INC_CD)
  x[2,] <- as.numeric(data$STDNT_GNDR_SHORT_DES)
  x[3,] <- as.numeric(data$STDNT_ETHNC_GRP_SHORT_DES)
  x[4,] <- as.numeric(data$MAX_ACT_MATH_SCR)
  x[5,] <- as.numeric(data$MAX_ACT_ENGL_SCR)
  x[6,] <- as.numeric(data$ACAD_LVL_BOT_SHORT_DES)
  x[7,] <- as.numeric(data$PRMRY_CRER_CD)
  x[8,] <- as.numeric(data$HS_GPA)
  x[9,] <- as.numeric(data$EXCL_CLASS_CUM_GPA)
  #x[10,] <- as.numeric(data$CUMGPA_S)
  #x[11,] <- as.numeric(data$CUMGPA_SS)
  #x[12,] <- as.numeric(data$CUMGPA_H)
  x[10,] <- as.numeric(data$TERM_SHORT_DES)
  
  #hist(data$GRD_PNTS_PER_UNIT_NBR)
  rownames(x) <- vnames
  x <- x[c(1:10),]

  x <- t(x)
  colnames(x)[4] <- 'ACT_MATH'
  colnames(x)[5] <- 'ACT_ENGL'
  hh <- lars(x,y,type='lasso')
  
 
  #LARS package fancy plot
  plot(hh)
  #Printe a table of the LASSO results.
  #print(summary(hh))
  library(grid)
  library(gridExtra)
  grid.newpage()
  grid.table(signif(coef(hh),3))
  
  if (crseid == 5892){print(xtable(signif(coef(hh),3)))}
  
  #MSE plot telling us roughly where to cut.
  cv.lars(x,y)
  
  #print(signif(coef(hh),3))
  #Write the LASSO table to disk
  #title <- paste('LASSO:Grade',subject,catnum,sep="")
  #write('\\documentclass{article}',file=paste('/Users/bkoester/Box Sync/MBIO/Aug2015/',title,'.tex',sep=""))
  #print(xtable(signif(coef(hh),3)))
  #write(xtable(signif(coef(hh),3)),file=paste('/Users/bkoester/Box Sync/MBIO/Aug2015/',title,'.tex',sep=""),append=TRUE)
  #write('\\end{document}',file=paste('/Users/bkoester/Box Sync/MBIO/Aug2015/',title,'.tex',sep=""),append=TRUE)
  
  
  return(hh)
  
}

#compute means, SE, etc for some columns given by varname
compute.basic.stats <- function(small,big,varname)
{
  
  small <- merge(small,big,by='EMPLID',all.x=TRUE)
  
  f <- which(small$STDNT_GNDR_SHORT_DES == 'Female')
  m <- which(small$STDNT_GNDR_SHORT_DES == 'Male')
  fb <- which(big$STDNT_GNDR_SHORT_DES == 'Female')
  mb <- which(big$STDNT_GNDR_SHORT_DES == 'Male')
  
  #GEt the gender subsets
  smallf <- small[f,]
  smallm <- small[m,]
  bigf   <- big[fb,]
  bigm   <- big[mb,]
  
  #get ONLY the thing we want.
  smallf <- smallf[,names(smallf) %in% varname]
  smallm <- smallm[,names(smallm) %in% varname]
  bigf   <- bigf[,names(bigf) %in% varname]
  bigm   <- bigm[,names(bigm) %in% varname]
  small  <- small[,names(small) %in% varname]
  big    <- big[,names(big) %in% varname]
  
  #View(big)
  
  MN_SMALL_F <- mean(smallf,na.rm=TRUE)
  SD_SMALL_F <- sd(smallf,na.rm=TRUE)
  N_SMALL_F  <- length(smallf)
  SE_SMALL_F <- sd(smallf,na.rm=TRUE)/sqrt(N_SMALL_F)
  
  MN_SMALL_M <- mean(smallm,na.rm=TRUE)
  SD_SMALL_M <- sd(smallm,na.rm=TRUE)
  N_SMALL_M  <- length(smallm)
  SE_SMALL_M <- sd(smallm,na.rm=TRUE)/sqrt(N_SMALL_M)                              
  
  MN_BIG_F <- mean(bigf,na.rm=TRUE)
  SD_BIG_F <- sd(bigf,na.rm=TRUE)
  N_BIG_F  <- length(bigf)
  SE_BIG_F <- sd(bigf,na.rm=TRUE)/sqrt(N_BIG_F)
  
  MN_BIG_M <- mean(bigm,na.rm=TRUE)
  SD_BIG_M <- sd(bigm,na.rm=TRUE)
  N_BIG_M  <- length(bigm)
  SE_BIG_M <- sd(bigm,na.rm=TRUE)/sqrt(N_BIG_M)
  
  MN_SMALL <- mean(small,na.rm=TRUE)
  SD_SMALL <- sd(small,na.rm=TRUE)
  N_SMALL  <- length(small)
  SE_SMALL <- sd(small,na.rm=TRUE)/sqrt(N_SMALL)
  
  MN_BIG   <- mean(big,na.rm=TRUE)
  SD_BIG   <- sd(big,na.rm=TRUE)
  N_BIG    <- length(big)
  SE_BIG   <- sd(big,na.rm=TRUE)/sqrt(N_BIG)
  
  return(data.frame(MN_SMALL_F,SD_SMALL_F,N_SMALL_F,SE_SMALL_F,
                    MN_SMALL_M,SD_SMALL_M,N_SMALL_M,SE_SMALL_M,         
                    MN_BIG_F,SD_BIG_F,N_BIG_F,SE_BIG_F,        
                    MN_BIG_M,SD_BIG_M,N_BIG_M,SE_BIG_M,
                    MN_SMALL,SD_SMALL,N_SMALL,SE_SMALL,
                    MN_BIG,SD_BIG,N_BIG,SE_BIG))
}

#These are local functions that make pretty plots for the paper.
make.master.grade.penalty.plots <- function()
{
  data <- read.table('/Users/bkoester/Box\ Sync/InComingPlots/grom_paper/all_grade_penalties/all.course.notes.v2.txt',sep="\t",header=TRUE)
  #Plot 1: S,SS,Hu,E
  e <- data$CATALOG_NBR != 126
  data <- data[which(e),]
  s  <- data$DIV == 'S' | data$DIV == 'E'
  ss <- data$DIV == 'SS'
  h  <- data$DIV == 'H'
  #e  <- data$DIV == 'E'
  #l  <- (data$SUBJECT == 'CHEM' & (data$CATALOG_NBR == 211 | data$CATALOG_NBR == 126 | data$CATALOG_NBR == 216)) | 
  #      (data$SUBJECT == 'PHYSICS' & (data$CATALOG_NBR == 136 | data$CATALOG_NBR == 236 | data$CATALOG_NBR == 141 | data$CATALOG_NBR == 241)) |
  #      (data$SUBJECT == 'BIOLOGY' & (data$CATALOG_NBR == 173))
  l   <- data$TYPE == 'LAB' & (data$DIV == 'S') 
  c   <- (data$SUBJECT == 'PHYSICS' & (data$CATALOG_NBR == 140 | data$CATALOG_NBR == 240 | data$CATALOG_NBR == 235 | data$CATALOG_NBR == 125)) |
         (data$SUBJECT == 'CHEM' & (data$CATALOG_NBR == 130 | data$CATALOG_NBR == 210 | data$CATALOG_NBR == 215 )) |
         (data$SUBJECT == 'MATH' & (data$CATALOG_NBR == 115 | data$CATALOG_NBR == 116 )) |
         (data$SUBJECT == 'BIOLOGY' & (data$CATALOG_NBR == 171 | data$CATALOG_NBR == 172))
  ns  <- length(which(s))
  nss <- length(which(ss))
  nh  <- length(which(h))
  nl  <- length(which(l))
  nc  <- length(which(c))
  
  #or a simple STEM-non lab flag
  
  
  #First the all course grade penalty plot
  #plot(data$MN_ALL,data$MN_FEMALES-data$MN_MALES,pch=0,main='Grade Penalties by Division',
  #     xlab='Grade Penalty (Grade - GPAO)',ylab='Female GP - Male GP',cex=0.1)
  pdf('/Users/bkoester/Box\ Sync/InComingPlots/grom_paper/all.divisions.pdf',width=11,height=7)
  plot(0,0,main='Grade Anomaly by Division',
       xlab='Average Grade Anomaly',ylab='Gendered Performance Difference',
       cex=0.1,ylim=c(-0.4,0.4),xlim=c(-0.65,0.65))
  lines(c(-0.65,0.65),c(0,0))
  lines(c(0,0),c(-0.4,0.4))
  points(data$MN_GA_ALL[s],data$MN_GA_FEMALES[s]-data$MN_GA_MALES[s],pch=19,col='red')
  overplot.error.bars(data$MN_GA_ALL[s],data$MN_GA_FEMALES[s]-data$MN_GA_MALES[s],
                      data$SE_GA_ALL[s],data$SE_GA_FEMALES[s],col='red')
  points(data$MN_GA_ALL[ss],data$MN_GA_FEMALES[ss]-data$MN_GA_MALES[ss],pch=19,col='blue')
  overplot.error.bars(data$MN_GA_ALL[ss],data$MN_GA_FEMALES[ss]-data$MN_GA_MALES[ss],
                      data$SE_GA_ALL[ss],data$SE_GA_FEMALES[ss],col='blue')
  points(data$MN_GA_ALL[h],data$MN_GA_FEMALES[h]-data$MN_GA_MALES[h],pch=5,col='blue',bg='white')
  overplot.error.bars(data$MN_GA_ALL[h],data$MN_GA_FEMALES[h]-data$MN_GA_MALES[h],
                      data$SE_GA_ALL[h],data$SE_GA_FEMALES[h],col='blue')
  
  #points(data$MN_ALL[e],data$MN_FEMALES[e]-data$MN_MALES[e],pch=8,col='blue')
  legend(0.3,0.4,c('Science and Engineering','Social Sciences','Humanities'),
         col=c('red','blue','blue'),pch=c(19,19,5))
  text(-0.6,-0.4,'Males Favored',font=2,col='red',pos=4)
  text(-0.6,0.4 ,'Females Favored',font=2,col='red',pos=4)
  text(0.1,0.3,'Grade Bonus',srt=90,font=2,col='red',pos=3)
  text(-0.1,0.3,'Grade Penalty',srt=90,font=2,col='red',pos=3)
  dev.off()
  
  
  #Then just STEM and lab course grade penalties
  pdf('/Users/bkoester/Box\ Sync/InComingPlots/grom_paper/stem.pdf',width=11,height=7)
  plot(data$MN_GA_ALL[s],data$MN_GA_FEMALES[s]-data$MN_GA_MALES[s],pch=19,main='Grade Anomalies in STEM',
       xlab='Average Grade Anomaly',ylab='Gendered Performance Difference',cex=0.5,xlim=c(-0.65,0.65))
  overplot.error.bars(data$MN_GA_ALL[s],data$MN_GA_FEMALES[s]-data$MN_GA_MALES[s],
                      data$SE_GA_ALL[s],data$SE_GA_FEMALES[s],col='black')
  points(data$MN_GA_ALL[l],data$MN_GA_FEMALES[l]-data$MN_GA_MALES[l],pch=19,col='red')
  overplot.error.bars(data$MN_GA_ALL[l],data$MN_GA_FEMALES[l]-data$MN_GA_MALES[l],
                      data$SE_GA_ALL[l],data$SE_GA_FEMALES[l],col='red')
  points(data$MN_GA_ALL[c],data$MN_GA_FEMALES[c]-data$MN_GA_MALES[c],pch=8)
  #overplot.error.bars(data$MN_GA_ALL[c],data$MN_GA_FEMALES[c]-data$MN_GA_MALES[c],
  #                    data$SE_GA_ALL[c],data$SE_GA_FEMALES[c],col='black')
  
  lines(c(0,0),c(-0.3,0.15))
  lines(c(-0.6,0.6),c(0,0))
  
  text(data$MN_GA_ALL[s],data$MN_GA_FEMALES[s]-data$MN_GA_MALES[s],cex=0.5,
       labels=paste(data$SUBJECT[s],data$CATALOG_NBR[s],sep=" "),pos=4)
  text(data$MN_GA_ALL[l],data$MN_GA_FEMALES[l]-data$MN_GA_MALES[l],cex=0.5,
       labels=paste(data$SUBJECT[l],data$CATALOG_NBR[l],sep=" "),pos=4,col='red')
  legend(-0.6,0.15,c('LEC','LAB','REQ-LEC'),col=c('black','red','black'),pch=c(19,19,8))
  dev.off()
  
  #Now just STEM courses with the MATCHED differences
  pdf('/Users/bkoester/Box\ Sync/InComingPlots/grom_paper/matched.pdf',width=11,height=7)
  plot(data$MN_GA_ALL[s],data$MATCHED_MEAN_FEMALES[s]-data$MATCHED_MEAN_MALES[s],pch=19,cex=0.5,
       main='Matched Analysis Does Not Correct the Gender Gap',
       xlab='Average Grade Anomaly',ylab='Matched (Female-Male) Grades',xlim=c(-0.65,0.65))
  overplot.error.bars(data$MN_GA_ALL[s],data$MATCHED_MEAN_FEMALES[s]-data$MATCHED_MEAN_MALES[s],
                      data$SE_GA_ALL[s],sqrt(data$MATCHED_SE_FEMALES[s]^2+data$MATCHED_SE_MALES[s]^2))
  points(data$MN_GA_ALL[l],data$MATCHED_MEAN_FEMALES[l]-data$MATCHED_MEAN_MALES[l],pch=19,col='red')
  overplot.error.bars(data$MN_GA_ALL[l],data$MATCHED_MEAN_FEMALES[l]-data$MATCHED_MEAN_MALES[l],
                      data$SE_GA_ALL[l],sqrt(data$MATCHED_SE_FEMALES[l]^2+data$MATCHED_SE_MALES[l]^2),col='red')
  points(data$MN_GA_ALL[c],data$MATCHED_MEAN_FEMALES[c]-data$MATCHED_MEAN_MALES[c],pch=8)
  overplot.error.bars(data$MN_GA_ALL[c],data$MATCHED_MEAN_FEMALES[c]-data$MATCHED_MEAN_MALES[c],
                      data$SE_GA_ALL[c],sqrt(data$MATCHED_SE_FEMALES[c]^2+data$MATCHED_SE_MALES[c]^2))
  
  lines(c(0,0),c(-1,1))
  lines(c(-1,1),c(0,0))
  text(data$MN_GA_ALL[s],data$MATCHED_MEAN_FEMALES[s]-data$MATCHED_MEAN_MALES[s],cex=0.5,
       labels=paste(data$SUBJECT[s],data$CATALOG_NBR[s],sep=" "),pos=4)
  text(data$MN_GA_ALL[l],data$MATCHED_MEAN_FEMALES[l]-data$MATCHED_MEAN_MALES[l],cex=0.5,
       labels=paste(data$SUBJECT[l],data$CATALOG_NBR[l],sep=" "),pos=4,col='red')
  dev.off()
  
  #Finally, the comparison of the matched GP and the raw GP
  pdf('/Users/bkoester/Box\ Sync/InComingPlots/grom_paper/matched.vs.raw.pdf',width=11,height=7)
  plot(data$MN_GA_FEMALES[s]-data$MN_GA_MALES[s],
       data$MATCHED_MEAN_FEMALES[s]-data$MATCHED_MEAN_MALES[s],pch=19,cex=0.5,
       xlab='Standard AGA Gap',ylab='Matched AGA Gap',main='Gender Gap is Similar in Matched and Unmatched Data')
  points(data$MN_GA_FEMALES[l]-data$MN_GA_MALES[l],data$MATCHED_MEAN_FEMALES[l]-data$MATCHED_MEAN_MALES[l],pch=19,col='red')
  points(data$MN_GA_FEMALES[c]-data$MN_GA_MALES[c],data$MATCHED_MEAN_FEMALES[c]-data$MATCHED_MEAN_MALES[c],pch=8)
  lines(c(-0.25,0.1),c(-0.25,0.1))
  text(data$MN_GA_FEMALES[s]-data$MN_GA_MALES[s],
       data$MATCHED_MEAN_FEMALES[s]-data$MATCHED_MEAN_MALES[s],cex=0.5,
       labels=paste(data$SUBJECT[s],data$CATALOG_NBR[s],sep=" "),pos=4)
  text(data$MN_GA_FEMALES[l]-data$MN_GA_MALES[l],
       data$MATCHED_MEAN_FEMALES[l]-data$MATCHED_MEAN_MALES[l],cex=0.5,
       labels=paste(data$SUBJECT[l],data$CATALOG_NBR[l],sep=" "),pos=4,col='red')
  dev.off()
  
}

covariate.distributions <- function(small,big)
{
  
  f <- which(small$STDNT_GNDR_SHORT_DES == 'Female')
  m <- which(small$STDNT_GNDR_SHORT_DES == 'Male')
  fb <- which(big$STDNT_GNDR_SHORT_DES == 'Female')
  mb <- which(big$STDNT_GNDR_SHORT_DES == 'Male')
  
  
  #GPAO
  GPAO <- mat.or.vec()
  
  
}


overplot.error.bars <- function(x,y,sex,sey,col='black')
{
  nx <- length(x)
  for (i in 1:nx)
  {
    arrows(x[i]-sex[i],y[i],x[i]+sex[i],y[i],code=0,col=col) 
    arrows(x[i],y[i]-sey[i],x[i],y[i]+sey[i],code=0,col=col) 
  }
}