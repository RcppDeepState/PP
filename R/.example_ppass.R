

library(eRm)


my_data <- eRm::sim.rasch(200, 12)
RMobj <- RM(my_data)
class(res_rasch)

res_rasch$betapar
dim(res_rasch$X)


