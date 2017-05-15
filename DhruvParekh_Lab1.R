#Lab1
#Load the seatbelt dataset
seatbelts<-read.csv("seatbelts.csv")

#Q1-Data cleaning
class(seatbelts)
#[1] "data.frame"

#Get some information about the data frame
#dimensions
dim(seatbelts)
#[1] 192  10 i.e. 192 rows, 10 col

#Colnames- gives col names
colnames(seatbelts)
#Gives column names they are: [1] "year.month"    "year"          "DriversKilled" "drivers"       "front"         "rear"         
#[7] "kms"           "PetrolPrice"   "VanKilled"     "law"
#Summary of the data
summary(seatbelts)
#   year.month        year      DriversKilled      drivers         front             rear      
#Min.   :1969   Min.   :1969   Min.   : 60.0   Min.   :1057   Min.   : 426.0   Min.   :224.0  
#1st Qu.:1973   1st Qu.:1973   1st Qu.:104.8   1st Qu.:1462   1st Qu.: 715.5   1st Qu.:344.8  
#Median :1977   Median :1977   Median :118.5   Median :1631   Median : 828.5   Median :401.5  
#Mean   :1977   Mean   :1977   Mean   :122.8   Mean   :1670   Mean   : 837.2   Mean   :401.2  
#3rd Qu.:1981   3rd Qu.:1981   3rd Qu.:138.0   3rd Qu.:1851   3rd Qu.: 950.8   3rd Qu.:456.2  
#Max.   :1985   Max.   :1985   Max.   :198.0   Max.   :2654   Max.   :1299.0   Max.   :646.0  
#kms         PetrolPrice        VanKilled           law        
#Min.   : 7685   Min.   :0.08118   Min.   : 2.000   Min.   :0.0000  
#1st Qu.:12685   1st Qu.:0.09258   1st Qu.: 6.000   1st Qu.:0.0000  
#Median :14987   Median :0.10448   Median : 8.000   Median :0.0000  
#Mean   :14994   Mean   :0.10362   Mean   : 9.057   Mean   :0.1198  
#3rd Qu.:17203   3rd Qu.:0.11406   3rd Qu.:12.000   3rd Qu.:0.0000  
#Max.   :21626   Max.   :0.13303   Max.   :17.000   Max.   :1.0000

#Q2- Computing Averages
#Average deaths for 1970, 1978
by(seatbelts[,"DriversKilled"],seatbelts[,"year"],mean)
#Average no of fatalities in 1970- 122.4167, in 1978- 125.5833

#Average no of rear seat fatalities in 1972, 1980
by(seatbelts[,"rear"],seatbelts[,"year"],mean)
#Average no of rear seat fatalities in 1972- 440.25, in 1980- 380.8333

#Q3- Exploring Relationships I
plot(seatbelts[,"kms"],seatbelts[,"drivers"])
abline(lm(seatbelts$drivers~seatbelts$kms))
#From the plot it can be seen that as the distance travelled increases,
#less driver fatalities decreases. There could be negative linear relation could be present
plot(seatbelts[,"PetrolPrice"],seatbelts[,"drivers"])
abline(lm(seatbelts$drivers~seatbelts$PetrolPrice))
#From the plot it can be seen that as the petrol price has increases, 
#less driver fatalities decreases. Again, a negative linear relation could be present

#Q4- Exploring Relationships II
#We will find hypotheses between year and driver fatalities
#Mean driver fatalities after law implementation-mean of driver fatalities is around 1320
#with a less steep gradient
plot(subset(seatbelts,year.month>=1983.031,select=c(year,drivers)),main="Driver fatalities after law",xlab="Year",ylab="Driver fatalities")
abline(h=mean(seatbelts$drivers[seatbelts$year.month>=1983.031]))
abline(lm(seatbelts$drivers~seatbelts$year))
#Mean driver fatalities before law implementation - mean of driver fatalities is around 1760
#with higher gradient
plot(subset(seatbelts,year.month<1983.031,select=c(year,drivers)),main="Driver fatalities before law",xlab="Year",ylab="Driver fatalities")
abline(h=mean(seatbelts$drivers[seatbelts$year.month<1983.031]))
abline(lm(seatbelts$drivers~seatbelts$year))

#Q5-Bonus point
#---Year vs Vankilled- decreases per year 
plot(seatbelts$year,seatbelts$VanKilled)
abline(lm(seatbelts$VanKilled~seatbelts$year))
