setwd("~/code/skola/tdde01/adam")

library(geosphere)

set.seed(1234567890)

stations = read.csv("lab5/stations.csv")
temps = read.csv("lab5/temps50k.csv")
st = merge(stations, temps, by="station_number")[sample(1:50000, 2000),]

n=dim(st)[1]
id=sample(1:n, floor(n*0.75)) 
train=st[id,]
test=st[-id,]

difftime_distance = function(x, y, unit_, format_) {
  date_strings = c(x, y)
  datetimes = strptime(date_strings, format=format_)
  diff = as.double(difftime(datetimes[2], datetimes[1], units=unit_))
  return(diff)
}

diff_hour = function(t1, t2) {
  return(abs(as.numeric(difftime(strptime(t1, "%H:%M:%S"),
                             strptime(t2, "%H:%M:%S")))))
}
diff_date = function(t1, t2) {
  return(as.numeric(difftime(strptime(t1, "%Y-%m-%d "),
                             strptime(t2, "%Y-%m-%d "))))
}

k_gaussian = function(u, h) {
  return(exp(-(abs(u/h)^2)))
}

station_distance = function(p1, p2) {
  return(k_gaussian(distHaversine(p1,p2), h_distance))
}

date_distance = function(d1, d2) {
  return(k_gaussian((diff_date(d1, d2) %% 365), h_date))
}

hour_distance = function(h1, h2) {
  return(k_gaussian(diff_hour(h1, h2), h_time))
}

calc_temp = function(dist, Y) {
  sumsum_dist = sum(dist)
  norm_dist = dist/sumsum_dist
  temp = as.vector(t(norm_dist))%*%as.vector(Y)
}

h_distance <- 60000 # These three values are up to the students
h_date <- 40
h_time <- 2*3600

c_distance = 0.1
c_date = 1
c_time = 0.1

a = 58.413497  # The point to predict (up to the students)
b = 15.582597
point = c(a, b)
date = "2013-04-20" # The date to predict (up to the students)
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
          "24:00:00")
temp = vector(length=length(times))
# Students’ code here

predict_temps = function(point_, date_, hour_) {
  station_dist = station_distance(point_, train[4:5])
  date_dist = date_distance(date_, train$date)
  
  temps_ = c()
  if (missing(hour_)) {
    for(i in 1:length(times)) {
      hour_dist = hour_distance(times[i], train$time)
      sum_dist = c_distance*station_dist + c_date*date_dist + c_time*hour_dist
      temps_[i] = calc_temp(sum_dist, train$air_temperature)
    }
    
    return(temps_)
  } else {
    hour_dist = hour_distance(hour_, train$time)
    sum_dist = c_distance*station_dist + c_date*date_dist + c_time*hour_dist
    temp = calc_temp(sum_dist, train$air_temperature)
    return(temp)
  }
}



# Predict only one day
pred_one_day = predict_temps(point, date)
plot(1:length(pred_one_day), pred_one_day)

# train test

#Yhat = c()
#for(i in 1:nrow(test)) {
#  test_row = test[i,]
#  Yhat[i] = predict_temps(c(test_row$latitude, test_row$longitud), test_row$date, test_row$time)
#}

plot(Yhat, test$air_temperature, ylim=range(-30,30), xlim=range(-30,30))
abline(1,1)


plot_h_values = function() {
  
  plot(k_gaussian(matrix(seq(1,600000,10000)), h_distance), main="Weights for geographical distance", ylab="Weight", xlab="Distance (swedish miles)", col="blue")
  
  plot(k_gaussian(matrix(seq(1,365,1)), h_date), xlim=range(0,100), main="Weights for difference in day of the year", ylab="Weight", xlab="Difference (days)",  col="blue")
  
  plot(k_gaussian(matrix(seq(0,24*3600,2*3600)), h_time), main="Weights for difference in hour of the day", ylab="Weight", xlab="Difference (hours)", col="blue")
}




