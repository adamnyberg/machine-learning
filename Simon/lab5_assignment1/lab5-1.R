setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab5_assignment1")
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

#ind <- sample(1:50000, 5000)
#st <- st[ind,]

gaussian_kernel = function(euclidian_dist) {
  return(exp(-((euclidian_dist))^2))
}

#Calculate the time between two dates
date_diff = function(t1, t2){
  return(as.numeric(difftime(strptime(t1, "%Y-%m-%d "),
                      strptime(t2, "%Y-%m-%d ")))%%365)
}

#Calculate the time between two hours
hour_diff = function(t1,t2){
  return(as.numeric(difftime(strptime(t1, "%H:%M:%S"),
                             strptime(t2, "%H:%M:%S"))))
}

#Distance from a station to the point of interest
k1 = function(p, stations, h){
  return(gaussian_kernel(distHaversine(p, stations)/h))
}

#Distance from a date to the date of interest
k2 = function(t, times, h){
  return(gaussian_kernel(date_diff(t, times)/h))
}

#Distance from an hour to the hour of interest
k3 = function(t, times, h){
  return(gaussian_kernel(hour_diff(t, times)/h))
}

k = function(Xs,holder, h) {
  h_distance = 160000
  h_date = 50
  h_time=1*3600
  c_distance = 0.1
  c_date = 1
  c_time = 0.1
  
  k1_score = k1(as.numeric(holder[4:5]), Xs[, 4:5], h_distance)
  k2_score = k2(holder[9], Xs[,9], h_date)
  k3_score = k3(holder[10], Xs[,10], h_time)
  
  k_score = c_distance*k1_score+c_date*k2_score+c_time*k3_score

  guessed_temperature = sum(k_score*Xs[,11])/sum(k_score)
  actual_temperature = as.numeric(holder[11])
  #return(guessed_temperature - actual_temperature)
  return (guessed_temperature)
}

apply_kernels = function(train_data, validation_data, h) {
  holder = validation_data[1,]
  Ys = apply(validation_data, 1, function(Y, Xs_, h_) k(Xs_, Y, h_), Xs_ = train_data, h_ = h)
  return(Ys)
}

get_weight_plot = function() {
  
  plot(gaussian_kernel(matrix(seq(1,2000000,10000))/160000), xlim=range(0,100), ylab="Weight", xlab="Distance [10km]", main="Station distance kernel")
  
  plot(gaussian_kernel(matrix(seq(1,365,1))/50), xlim=range(0,100), ylab="Weight", xlab="Day diff", main="Date distance kernel")
  
  plot(gaussian_kernel(matrix(seq(0,24*3600,2*3600))/(1*3600)), ylab="Weight", xlab="Time diff", main="Hour distance kernel")
}


n=dim(st)[1]
id=sample(1:n, floor(n*0.75)) 
train=st[id,] 
test=st[-id,]
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
          "23:00:00")
date <- "2013-04-20"
a <- 58.413497
b <- 15.582597
pred_data = st[1:11,]
for(i in 1:nrow(pred_data)){
  pred_data[i,4] = a
  pred_data[i,5] = b
  pred_data[i,9] = date
  pred_data[i,10] = times[i]
}

start.time <- Sys.time()

#res = apply_kernels(train,test,1)
#plot(res,test$air_temperature, xlim=range(-30,30), 
#ylim=range(-30,30), xlab="Predicted temp", ylab="Actual temp", main="Actual vs predicted temp", grid(6,6))
#abline(1,1)
res=apply_kernels(st,pred_data,0.1)
plot(res, main = date, ylab="Temperature")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
