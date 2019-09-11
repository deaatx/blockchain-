attack_calc <- function(q,z){
  
  p <- 1.0 - q
  lambda <- z*(q/p)
  sum <- 1.0
  k <- 0
  i <- 1
  
  for (k in 0:z){
    poisson <- exp(-lambda)
    if(k >= 1){
      for(i in 1:k){
        poisson <- poisson * (lambda/i)
        i <- i+1
      }
    }
    sum <- sum - (poisson * (1-(q/p)**(z-k)))
    k <- k+1
  }
  return(sum)
}

# q10<- c(attack_calc(0.1,0),
#attack_calc(0.1,1),
#attack_calc(0.1,2),
#attack_calc(0.1,3),
#attack_calc(0.1,4),
#attack_calc(0.1,5),
#attack_calc(0.1,6),
#attack_calc(0.1,7),
#attack_calc(0.1,8),
#attack_calc(0.1,9),
#attack_calc(0.1,10)
#)

#q5<- c(attack_calc(0.05,0),
#attack_calc(0.05,1),
#attack_calc(0.05,2),
#attack_calc(0.05,3),
#attack_calc(0.05,4),
#attack_calc(0.05,5),
#attack_calc(0.05,6),
#attack_calc(0.05,7),
#attack_calc(0.05,8),
#attack_calc(0.05,9),
#attack_calc(0.05,10)
#)

#q20<- c(attack_calc(0.2,0),
#       attack_calc(0.2,1),
#       attack_calc(0.2,2),
#       attack_calc(0.2,3),
#       attack_calc(0.2,4),
#       attack_calc(0.2,5),
#       attack_calc(0.2,6),
#       attack_calc(0.2,7),
#       attack_calc(0.2,8),
#       attack_calc(0.2,9),
#       attack_calc(0.2,10)
#)

#block <- c(0:10)

#dta <- data.frame(block,q5,q10,q20)

#write.csv(dta,'block_sim_dta.csv')
