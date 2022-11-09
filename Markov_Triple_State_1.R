# Roy Gross
# Adapted from: https://willhipson.netlify.app/post/markov-sim/markov_chain/
# Modeling NBA team "tanking"
#

set.seed(123457)
# change this to get different "random" results
# should be able to run with random seed every time, but I couldn't
# figure it out

#initial proportions
a <- 1
b <- 0
c <- 0

      markov2 <- function(n = 100, iter = 20, start_probs = c(a, b, c), trans_probs = matrix(c(.48, .21, .05,
                                                                                                .38, .38, .18,
                                                                                                .14, .41, .77), ncol = 3),
                    plot_prob = TRUE) {
  #check if probability entries are valid.
  if(sum(start_probs) != 1 | any(start_probs < 0)) 
    stop("start_probs must be non-negative and sum to 1.")
  if(any(apply(trans_probs, 1, sum) != 1))
    stop("trans_probs matrix rows must sum to 1.")
  if(any(trans_probs < 0))
    stop("elements of trans_probs must be non-negative")
  
  dt <- matrix(NA, nrow = iter, ncol = n) # Initialize matrix to hold iterations
  #run markov chain
  for(i in 1:iter) {
    for(j in 1:n) {
      if(i == 1) { # if we're at the beginning of the simulation
        dt[i, j] <- sample(x = c(0, 1, 2), size = 1, prob = start_probs)
      } else {
        if(dt[i - 1, j] == 0) { # if the previous state was 0
          dt[i, j] <- sample(x = c(0, 1, 2), size = 1, prob = trans_probs[,1])
        } else if (dt[i - 1, j] == 1) { # if the previous state was 1
          dt[i, j] <- sample(x = c(0, 1, 2), size = 1, prob = trans_probs[,2])
        } else {
          dt[i, j] <- sample(x = c(0, 1, 2), size = 1, prob = trans_probs[,3])
        }
      }
    }
  }
  
  #plot
  if(plot_prob) {
    df <- data.frame(x = 1:iter,
                     n0 = apply(subset(dt == 0), 1, sum),
                     n1 = apply(subset(dt == 1), 1, sum),
                     n2 = apply(subset(dt == 2), 1, sum))
    p <- df %>%
      pivot_longer(cols = c(n0, n1, n2)) %>%
      ggplot(aes(x, value, color = name)) +
      geom_line() +
      scale_color_discrete(labels = c("0", "1", "2")) +
      labs(x = "Iteration",
           y = "Prop.") +
      theme_minimal(base_size = 16)
    
    show(p)
  }
  
  #return chain as dataframe
  return(as.data.frame(dt))
}

df2 <- markov2()