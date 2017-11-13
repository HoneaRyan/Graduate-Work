##############################################################################
##                                                                          ##
##              Mood Test Asymptotic Efficiency Simulation                  ##
##                                                                          ##
##    This simulation will show the asymptotic efficiency of the mood       ##
##    test for increasing n where n is the size of each sample. It will     ##
##    calculate the number of times the test is rejected which it should    ##
##    not be as two equal distributions should have equal scale.            ##
##                                                                          ##
##############################################################################

# Creating a sequence of N values from 10-10000
# {10,20,30,...,9990, 10000}
N <- seq(10,1000,by=10)

# Just creating a collection for our efficiencies to be stored in
efficiency <- c()

# Main Loop: Loops through the various values that N will take
for (i in 1:length(N)) {
  reject_num <- 0   # sum of rejections for each simulation of size N[i]
  for (j in 1:10000) {
    sim_0 <- rnorm(N[i],0,20) #simulation 1
    sim_01 <- rnorm(N[i],0,20) #simulation 2
    
    # if statement determines if the test determines that the scales
    # are not equal with 95% confidence.
    if (mood.test(sim_0, sim_01)$p.value < .05) {
      reject_num <- reject_num + 1
    }
  }
  
  # proportion failed to reject versus total simulations
  efficiency[i] <- ((10000-reject_num)/10000)
  
  # Just used to check completion of simulation
  cat(i/length(N), "\n")
}

plot(N, efficiency, type = "l", ylim = c(0,1))
