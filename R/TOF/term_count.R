term_count <- function(sr,sc)
{
  sr <- sr %>% select(STDNT_ID,FIRST_TERM_ATTND_CD)
  sc <- left_join(sc,sr)
  
  #sc <- sc %>% distinct(STDNT_ID,TERM_CD,.keep_all=TRUE)
  
  delta <- sc$TERM_CD-sc$FIRST_TERM_ATTND_CD
  sc$TERMYR <- mat.or.vec(length(delta),1)
  
  #compute the number of years you've been here since your entrance term.
  # this may not deal well with winter entrance.
  e0 <- which(delta %% 50 == 0)
  e1 <- which(delta %% 50 == 10)
  e2 <- which(delta %% 50 == 20)
  e3 <- which(delta %% 50 == 30)
  e4 <- which(delta %% 50 == 40)
  
  sc$TERMYR[e0] <- delta[e0]/50+1
  sc$TERMYR[e1] <- (delta[e1]-10)/50+0.5+1
  #sc$TERMYR[e2] <- (delta[e2]-20)/50+0.6+1
  #sc$TERMYR[e3] <- (delta[e3]-30)/50+0.7+1
  #sc$TERMYR[e4] <- (delta[e4]-40)/50+0.8+1
  sc$TERMYR[e2] <- (delta[e2]-20)/50+0.75+1
  sc$TERMYR[e3] <- (delta[e3]-30)/50+0.75+1
  sc$TERMYR[e4] <- (delta[e4]-40)/50+0.75+1
  
  return(sc)
  
}