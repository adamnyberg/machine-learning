setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment5/")
library("geosphere")

diff_hour = function(t1, t2) {
  return(as.numeric(difftime(strptime(t1, "%H:%M:%S"),
                             strptime(t2, "%H:%M:%S"))))
}

diff_date = function(t1, t2) {
  return(as.numeric(difftime(strptime(t1, "%Y-%m-%d "),
                             strptime(t2, "%Y-%m-%d ")))%%365) 
}

gaussian_kernel = function(euclidian_distances, h) {
  return(exp( -((euclidian_distances / h ))^2))
}

# Kernel 1: Account for the distance from a station to the point of interest
k1 = function(s, stations, h) {
  return(gaussian_kernel(distHaversine(s, stations), h))
}


# Kernel 2: Account for the distance between the day a temperature measurement
# was made and the day of interest
k2 = function(d, days, h) {
  return(gaussian_kernel(diff_date(d, days), h))
}

# Kernel 2: Account for the distance between the hour of the day a temperature mea-
# surement was made and the hour of interest
k3 = function(t, times, h) {
  return(gaussian_kernel(diff_hour(t, times), h))
}

k = function(Xs, Y) {
  k1_score = k1(as.numeric(Y[4:5]), Xs[, 4:5], 160000)
  k2_score = k2(Y[9], Xs[,9], 40)
  k3_score = k3(Y[10], Xs[,10], 4*3600)
  k_score =  0.1 * k1_score + k2_score + 0.1 * k3_score
  guessed_temperature = sum(k_score*Xs[,11])/sum(k_score)
  actual_temperature = as.numeric(Y[11])
  return(guessed_temperature)
}

apply_kernels = function(Xs, Ys) {
  Ys = apply(Ys, 1, function(Y, Xs_) k(Xs_, Y), Xs_ = Xs)
  return(Ys)
}

get_weight_plot = function() {
  
  plot(gaussian_kernel(matrix(seq(1,600000,10000)), 160000), main="Weights for geographical distance (width = 160000)", ylab="Weight", xlab="Distance (swedish miles)", pch=21, bg="red")
  
  plot(gaussian_kernel(matrix(seq(1,365,1)), 40), xlim=range(0,100), main="Weights for difference in day of the year (width = 40)", ylab="Weight", xlab="Difference (days)", pch=21, bg="red")
  
  plot(gaussian_kernel(matrix(seq(0,24*3600,2*3600)), 4*3600), main="Weights for difference in hour of the day (width = 14400)", ylab="Weight", xlab="Difference (hours)", pch=21, bg="red")
}

set.seed(1234567890) 
stations = read.csv("stations.csv")
temperatures = read.csv("temps50k.csv")
complete_data = merge(stations, temperatures, by="station_number")

# Dont have to use all data. Sample it
ind = sample(1:50000, 50000)
complete_data = complete_data[ind,]

# Shuffle data before cross-validation
complete_data = complete_data[sample(nrow(complete_data)),]


times = c("04:00:00",
          "06:00:00",
          "08:00:00",
          "10:00:00",
          "12:00:00",
          "14:00:00",
          "16:00:00",
          "18:00:00",
          "20:00:00",
          "22:00:00",
          "22:00:00")


n=dim(complete_data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.75)) 
train=complete_data[id,] 
test=complete_data[-id,] 

date <- "2013-01-01"
a <- 58.4374
b <- 14.516
pred_data = complete_data[1:11,]

for(i in 1:nrow(pred_data)){
  pred_data[i,4] = a
  pred_data[i,5] = b
  pred_data[i,9] = date
  pred_data[i,10] = times[i] 
}

#result = apply_kernels(train, test)
result = apply_kernels(complete_data, pred_data)
#plot(result, test$air_temperature, ylim = range(-30,30), xlim = range(-30,30), main = "Predicted vs actual temperature", xlab="Predicted", ylab="Actual")
#abline(1,1)

plot(result, ylab="Temperature", xlab="Hour of the day (interval of two hours)", main=date)
