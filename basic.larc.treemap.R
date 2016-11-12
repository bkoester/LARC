#####################################################################################
#match two groups for comparison of outcomes.Data set should
#be cleaned before entry
#FUNCTION: basic.larc.treemap
#PURPOSE : Make one or two-level treemaps colored by a something quantitative
#INPUTS  : data: A data frame containing the columns you want to consider
#          L1: lowest level in the hierarchy (e.g. majors within a college)
#          L2: highest level in hierarchy (e.g. colleges)
#          C1: the column to use to color scale (e.g ACT MATH)
#          crange: the min/max of the color scale, in units of C1 (e.g. c(14,36))
#          freqcut: the minimum classification size to use
#          title: title to place on the plot
#OUTPUTS : Two sets of statistics:
#          plots to current device, which usually does not look great in RStudio, best to route to PDF
#NOTES: -  this requires library(treemap). To install: > install.packages('treemap')
#Ex:       Build a PDF treemap of majors clustered by entering school, colored by ACT MATH
#          pdf('~/Desktop/majors.pdf',height=7,width=11)
#          basic.larc.treemap(data,crange=c(14,36),freqcut=1000,L1="UM_DGR_1_MAJOR_1_DES",
#                             L2='PRMRY_CRER_CD',C1='MAX_ACT_MATH_SCR',title='Major by Incoming College')
#          dev.off()
##################################################################################### 
basic.larc.treemap <- function(datain,freqcut=2,title="none",crange=c(0,10),
                               L1="UM_DGR_1_MAJOR_1_DES",L2='NONE',C1='NONE')
{
    require(treemap)
    len <- dim(datain)[1]
    ID <- 1:len 
    H1 <- datain[,names(datain) %in% L1]
    data <- data.frame(ID,H1)
    
    h2flag <- 0
    qflag  <- 0
  
    if (L2 != 'NONE')
    {
      h2flag <- 1
      H2 <- datain[,names(datain) %in% L2]
      data <- data.frame(data,H2)
    }
    
    if (C1 != 'NONE')
    {
      qflag <- 1
      Q1 <- datain[,names(datain) %in% C1]
      data <- data.frame(data,Q1)
    }

    #in this data set, don't give us incomplete data
    e <- complete.cases(data)
    print(paste(length(data$H1),' records',sep=""))
    print(paste(sum(e),' complete records',sep=""))
    
    data <- data[e,]
  
    #count up the number of H1 categories
    h1div  <- data$H1[!duplicated(data$H1)]
    nh1    <- length(h1div)
    
    #count up the number of H2 categories. this will be empty if no H2
    if (h2flag == 1)
    {
      h2tab <- table(data$H2)
      h2div <- names(h2tab)
      nh2   <- length(h2div)
      h2cts <- as.numeric(h2tab)
    }
    
    #Start at the H1 level and loop.
    data       <- data[order(data$H1), ]
    data$count <- sequence(rle(as.vector(data$H1))$lengths)
    
    nh1  <- length(data$H1[!duplicated(data$H1)])
    nstart <- which(data$count == 1)
    ntot   <- length(data$H1) 
    h2     <- mat.or.vec(nh1,1)
    h1     <- h2
    freq   <- h2
    q1     <- h2
   
    for (i in 1:nh1)
    {
      start_ind <- nstart[i]
      if (i < nh1){stop_ind  <- nstart[i+1]-1}
      if (i == nh1){stop_ind <- ntot}
      
      ind  <- c(start_ind:stop_ind)
      freq[i]     <- length(ind)
      
      if (h2flag == 1)
      {
        h2t   <- as.character(data$H2[start_ind])
        e     <- h2t == h2div
        h2[i] <- paste(h2t,"(",h2cts[e],")",sep="")
      }
      
      h1[i]  <- paste(as.character(data$H1[start_ind]),"(",freq[i],")",sep="")
      
      if (freq[i] > 2 & qflag == 1)
      {
        q1[i]     <- mean(data$Q1[ind])
      }
      
    }
    
    out <- data.frame(h1,h2,q1,freq)
    e   <- which(out$freq > freqcut)
    out <- out[e,]
    
    treemap(out,c("h2","h1"),vSize='freq',palette='Spectral',overlap.labels=1,
            vColor='q1',type='manual',fontsize.labels=c(50,10),range=crange,
            title=title,title.legend=C1)
    
    return()
  }


  