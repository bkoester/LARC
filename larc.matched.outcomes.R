#####################################################################################
#match two groups for comparison of outcomes.Data set should
#be cleaned before entry
#FUNCTION: larc.matched.outcomes
#PURPOSE : This matches two groups.
#INPUTS  : A data frame containing at least
#          1) covariates 
#          2) the test group (variate), and the two comparison groups within this test.
#         The identities of these columns MUST be specified as arguments
#         variate: which column will be the test column. should be ordinal for now.
#         group1 : the control group from the variate column
#         gropu2 : the case group from the same column
#         covariates: what to match on.
#         type: character fields in the covariates, or numerical?
#OUTPUTS : 1) Graduation rates of matched groups
#          2) Tabulated graduating majors by division
#          3) A data frame containing majors of matched individuals.
#NOTES: - This only analyzes COMPLETE fields, i.e. NA in any column will cause the record be discarded!
#       - this requires library(optmatch)
#Ex: The ex compares graduating majors of white and black students, matching SAT test scorees and HS_GPA.
#Ex: kk <- larc.matched.outcomes(data,'STDNT_ETHNC_GRP_SHORT_DES',"White","Black",
#                                    c('MAX_SATI_MATH_SCR','MAX_SATI_VERB_SCR','HS_GPA'),
#                                     type=c('N','N','N'))   
#
#####################################################################################  
larc.matched.outcomes <- function(data,variate,group1,group2,covariates,type=c('C','C'))
{
  library(optmatch) 
  ncov <- length(covariates)
  
  #only keep what we want, don't chuck incomplete outcomes!
  data <- data[,names(data) %in% c(variate,covariates,'UM_DGR_1_MAJOR_1_DES','DIVISION')]
  temp <- data[,!names(data) %in% c('UM_DGR_1_MAJOR_1_DES')]
  e    <- complete.cases(temp)
  data <- data[which(e),]
  
  #pick out and clean up the case-control pairing
  temp <- as.character(data[,names(data) %in% variate])
  cln  <- temp == group1 | temp == group2
  lvl  <- levels(as.factor(temp[cln]))
  data <- data[which(cln),]
  nst  <- length(data[,1])
  
  #set the outcome variable
  out    <- mat.or.vec(nst,1)
  e      <- data$UM_DGR_1_MAJOR_1_DES != ""
  print(sum(e))
  out[e] <- 1
  outdescr <- data$UM_DGR_1_MAJOR_1_DES
  outdiv   <- data$DIVISION
  
  #now read the case vector and standardize it.
  assign(variate,as.numeric(data[,names(data) %in% variate]))
  temp <- eval(as.symbol(variate))
  jj   <- levels(as.factor(temp))
  e    <- temp == jj[1]
  e2   <- temp == jj[2]
  case <- mat.or.vec(length(temp),1)
  if (sum(e)  >  sum(e2))
  {
    case[e2] <- 1
    print(paste('using',lvl[2],'as cases'))
    pcasename  <- lvl[2]
    pctrlname  <- lvl[1]
  }
  if (sum(e2) >= sum(e))
  {
    case[e]  <- 1
    print(paste('using',lvl[1],'as cases'))
    pcasename  <- lvl[1]
    pctrlname  <- lvl[2]
  }
  
  datalm <- data.frame(case,out,outdescr,outdiv)
  
  for (i in 1:ncov)
  {
    assign(covariates[i],as.character(data[,names(data) %in% covariates[i]]))  
    if (i == 1){clist <- covariates[i]}
    if (i > 1) {clist <- paste(clist,covariates[i],sep="+")}
    tt <- eval(as.symbol(covariates[i]))
    if (type[i] == 'N'){tt <- as.numeric(tt)}
    if (type[i] == 'C'){tt <- as.character(tt)}
    datalm <- data.frame(datalm,tt)
    names(datalm)[i+4] <- covariates[i]
  }
  
  #make the formula
  mod <- paste('case',"~",clist)
  model <- do.call("glm",list(as.formula(mod),data=as.name("datalm"),family=binomial()))
  
  #...and execute the matching with a caliper set at 0.2 to increase speed...
  m1    <- fullmatch(match_on(model,caliper=0.2),data=datalm)
  
  #Now attach the matching structure to the data
  datalm <- cbind(datalm,matches=as.numeric(substr(m1,3,7)))
  
  #And sort once for speed, computing mean grades for the matched groups
  datalm       <- datalm[order(datalm$matches,datalm$case),] #This sort is crucial. Keeping the SEX makes sure that female is always index 1.
  datalm$count <- sequence(rle(as.vector(datalm$matches))$lengths)
  
  nid    <- length(datalm$matches[!duplicated(datalm$matches)])
  nstart <- which(datalm$count == 1)
  ntot   <- length(datalm$matches)
  
  #rnames <- row.names(data) #mat.or.vec(nid,1)
  
  #Now compute the grades for males and matched females
  #Note that this matching ONLY considers one-to-one matching, as opposed to averaging
  #all N matches to a single individuals.
  
  gpenf <- mat.or.vec(nid,1)
  gpenm <- gpenf
  outdescf <- gpenf
  outdescm <- gpenf
  outdivf  <- gpenf
  outdivm  <- gpenf
  
  for (i in 1:nid)
  {
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    
    gpenf[i] <- as.numeric(datalm$out[stop_ind])
    gpenm[i] <- as.numeric(datalm$out[start_ind])
    
    outdescf[i] <- as.character(datalm$outdescr[stop_ind])
    outdescm[i] <- as.character(datalm$outdescr[start_ind])
    
    outdivf[i] <- as.character(datalm$outdiv[stop_ind])
    outdivm[i] <- as.character(datalm$outdiv[start_ind])
    
  }
  
  print(paste('N =',nid))
  pctrl  <- signif(sum(gpenm)/nid,4)
  sectrl <- signif(sqrt(pctrl*(1-pctrl)/nid),4)
  pcase  <- signif(sum(gpenf)/nid,4)
  secase <- signif(sqrt(pcase*(1-pcase)/nid),4)
  
  print(paste(pctrlname,'=',pctrl,'+/-',sectrl))
  print(paste(pcasename,'=',pcase,'+/-',secase))
  
  print('case table')
  print(table(outdivf))
  print('control table')
  print(table(outdivm))
  
  return(data.frame(gpenm,gpenf,outdescf,outdescm,outdivf,outdivm,nid))
}

