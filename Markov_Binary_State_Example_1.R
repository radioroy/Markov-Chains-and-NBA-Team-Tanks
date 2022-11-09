# Roy Gross
# Adapted from: https://willhipson.netlify.app/post/markov-sim/markov_chain/
# Modeling NBA team "tanking"
#
# include libs:
# if you don't have these installed, you should see a small box
# pop up asking you if you want to install required libraries
# click "yes"

library(tidyverse)
library(plotly)

#initial distribution
AInitialPer <- 0.6
BInitialPer <- 0.4

#transition matrix (transition probabilities)
ProbAtoB <- 0.8
ProbBtoA <- 0.1


markov1 <- function(n = 100, iter = 100, start_probs = c(BInitialPer, AInitialPer), trans_probs = c(ProbAtoB, ProbBtoA), plot_prob = TRUE) {
  #Check to see if probability entries are valid
  if(sum(start_probs) != 1 | any(start_probs < 0)) 
    stop("start_probs must be non-negative and sum to 1")
  if(sum(trans_probs) > 1 | any(trans_probs < 0))
    stop("trans_probs must be non-negative and not sum to greater than 1")
  
  dt <- matrix(NA, nrow = iter, ncol = n) #initialize matrix to hold iterations
  #run markov chain
  for(i in 1:iter) {
    for(j in 1:n) {
      if(i == 1) {#if at the beginning of the simulation
        dt[i, j] <- sample(x = c(0, 1), size = 1, prob = start_probs)
      } else {
        if(dt[i - 1, j] == 0) { #if the previous state was 0
          dt[i, j] <- sample(x = c(0, 1), size = 1, prob = c(1 - trans_probs[1], trans_probs[1]))
        } else { #if the previous state was !0, it must have been 1
          dt[i, j] <- sample(x = c(1, 0), size = 1, prob = c(1 - trans_probs[2], trans_probs[2]))
        }
      }
    }
  }
  # Plot results?
  if(plot_prob) {
    df <- data.frame(x = 1:iter, y = apply(dt, 1, mean))
    p <- ggplot(df, aes(x, y)) +
      geom_line() +
      labs(x = "Iteration",
           y = "Proportion B") +
      theme_minimal(base_size = 16)
    
    show(p)
  }
  return(as.data.frame(dt))
}

df <- markov1() #execute