## Download all relevant packages and libraries:

library(gdata);
library(car);
library(arules);
library(fBasics);
library(car);
library(corrplot);
library(DMwR);
library(Rlof);


# 1.a. Download and extract the data from Moodle into a local folder designated for this assignment.

# 1.b. Set your working directory to be your assignment folder for easy access. 
# From now on, if needed, do not use the full path, but only the name of the file within this path.
################################

setwd("C:/Users/dvirn/Documents/R/Assignment1")
rm(list=ls())


# 1.c. Import the CSV file " chicago_taxi_data.csv " into R and save it by the name data. 
# Notice the data in the file has row numbers that are redundant (so pay attention to the function arguments).
################################
    
data <- read.csv("chicago_taxi_data.csv")


# 1.d Make sure the data was loaded properly by showing the first few rows of the data.
################################

head(data)


# 2.a Sample 10000 rows from the dataset without replacement. This file will be our dataset throughout the exercise. 
# Before you sample, set your random seed to be 1. 
################################

set.seed(1)
dataset = data[sample(nrow(data),10000,FALSE),]


# 2.b We will not use any of geographical columns (pickup/ dropoff - longtitude/ latitude). Delete these columns.
################################

dataset[c("pickup_latitude","pickup_longitude","dropoff_latitude","dropoff_longitude")]=NULL


# 2.c Show the names and the data type of all the features.
################################

sapply(dataset, class)


# 2.d. The Column pickup_census_tract has only NA values. Create a verification check for this claim. 
#Delete the column pickup_census_tract and dropoff_census_tract from the dataset.
#Could we have known in advanced that this column is problematic? Tip: use your answer the previous question.
################################

sum(is.na(dataset$pickup_census_tract))-length(dataset$pickup_census_tract)
# If this last calculation has returned "0" it means that this column has only NA values. 
dataset[c("pickup_census_tract","dropoff_census_tract")]=NULL
# We could have known that this column is problematic because its type is logical while it should be numeric like dropoff_census_tract.


# 2.e What's your opinion about the current type of the column 'company'? Is it adequate?
#If yes, explain why. It not, explain why not and change the type of this column and other similar columns.
################################

# We guess that the numeric inputs for this variable are company codes.
# Because there is a limited number of companies and those numbers stand for companies' names,
# it is better to change this column's type to a factor.
dataset$company = as.factor(dataset$company)


# 2.f. Create a summary statistics of the dataset (using one-line command). 
# What is the difference between the output for the numerical columns and the non-numeric columns?
################################

summary(dataset)
# For the numerical columns we get a statistical summary with a minimum\maximum value, median and mean and the 1st and 3rd quartiles.
# For the non-numerical columns we get a count by value.


# 3.a. Calculate the percentage of rows with at least one missing value (NA)
################################

num_of_NA_per_row = rowSums(is.na(dataset))
cat("The data set has ",length(num_of_NA_per_row)," records.
Out of which, the number of rows with atleast one missing value is ",sum(num_of_NA_per_row>0),
          ", or in other words ", sum(num_of_NA_per_row>0)/length(num_of_NA_per_row)*100,"%.", sep = "")


# 3.b. Delete all rows with more than 1 missing value (NA)
################################

temp_length=nrow(dataset)
dataset = dataset[!(num_of_NA_per_row>1),]
cat("Number of records removed (with more than one missing value) are:",temp_length - nrow(dataset))


# 3.c. Create a histogram of a categorical column (to your choice). Explain the findings of the chart. 
# Pay attention that some histogram functions work only with numerical values, so a transformation is needed.
################################

hist(as.numeric(dataset$payment_type), main = "Methods of Payment", xlab = "Methods of Payment")
# We can see that approximately 2/3 of the clients paid in cash, 1/3 in credit cards
# and any other method of payment was really rare.


# 3.d. Choose and implement the best way to deal with the missing values in the dataset, in your opinion (according to your previous findings). 
# As for the columns: [trip_seconds, trip_miles, trip_total] , deal with 0's (zeros) as if they were NA's .
#Pay attention - you can decide to delete specific rows or columns, while impute some other remaining missing values. Explain all of your choices.
################################

trip_seconds_na = dataset$trip_seconds == 0
trip_miles_na = dataset$trip_miles == 0 
trip_total_na = dataset$trip_total == 0 
Sum_of_rows_with_missing_value = sum(trip_total_na | trip_miles_na | trip_seconds_na)
cat("Number of rows with trip_seconds and/or trip_miles and/or trip_total value equal to zero is:",Sum_of_rows_with_missing_value)

# By looking at the data we can see there are only few records with trip_total 0.
# Therefore the best strategy is to omit these records:
cat("We have",sum(trip_total_na),"records with trip total NA. we shall remove these.")
dataset = dataset[!(trip_total_na),]

# It's also possible to see that there are only 2 more records with trip_total close to 0 (0.01, 0.21) 
# and beside these the minimum value of trip_total is 3.25 and it's very frequent.
# We understand these two records are also noise, we'll omit them.
dataset = dataset[!(dataset$trip_total<3.25),]

trip_seconds_na = dataset$trip_seconds == 0
trip_miles_na = dataset$trip_miles == 0 

# Let's understand how many records we have with trip_seconds=0 and/or trip_miles=0:
cat("We have",sum(dataset$trip_miles==0 & dataset$trip_seconds>0),"records with NA in trip_miles but not in trip_seconds.")
cat("We have",sum(dataset$trip_seconds==0 & dataset$trip_miles>0),"records with NA in trip_seconds but not in trip_miles.")
cat("We have",sum(dataset$trip_seconds==0 & dataset$trip_miles==0),"records with NA in trip_seconds and in trip_miles.")

# First we'll deal with the third kind - when both trip_miles and trip_seconds are NA's.

# It's interesting to see how trip_total splits in these cases:
barplot(table(dataset$trip_total[trip_seconds_na & trip_miles_na]))
# It's clear to see that the great majority of records with both miles & seconds NA are a minimum fare trips.
# Let's look at the split of trip_seconds and trip_miles while total_trip is under 3.5 (minimum with tolerance):
barplot(table(dataset$trip_seconds[dataset$trip_total<3.5]))
barplot(table(dataset$trip_miles[dataset$trip_total<3.5]))
# Almost all of the trips with minimum fare have trip_seconds and trip_miles 0.
# Leaving these cases aside, the great majority of trips have trip_seconds of 60 and/or trip_miles of 0.1.
barplot(table(dataset$trip_seconds[dataset$trip_total<3.5 & !trip_seconds_na]))
barplot(table(dataset$trip_miles[dataset$trip_total<3.5 &!trip_miles_na]))
# (These values are obviously also the medians in those cases)

# So we apply imputation using similarities for all records with trip_total<3.5.
both_na_not_min_fare = trip_seconds_na & trip_miles_na & dataset$trip_total<3.5
cat("We have only",sum(both_na_not_min_fare),
    "records with both trip_seconds and trip_miles NA and trip_toatal which isn't minimum, we'll omit those.")
dataset = dataset[!both_na_not_min_fare,]
trip_seconds_na = dataset$trip_seconds == 0
trip_miles_na = dataset$trip_miles == 0 
dataset$trip_seconds[trip_seconds_na & trip_miles_na] = 60
dataset$trip_miles[trip_seconds_na & trip_miles_na] = 0.1

trip_seconds_na = dataset$trip_seconds == 0
trip_miles_na = dataset$trip_miles == 0 

# Now, as we've seen there are only few records from the second type (NA in trip_seconds but not in trip_miles).
# We shall omit these records:
dataset = dataset[!(dataset$trip_seconds==0 & dataset$trip_miles>0),]

trip_seconds_na = dataset$trip_seconds == 0
trip_miles_na = dataset$trip_miles == 0 

# About 15% of the records are from the first type (NA in trip_miles but not in trip_seconds).
# It's not little, but not too much so we can still omit it.
# We would like to impute these missing values using linear regression,
# but the assumptions needed aren't fulfilled:
# 1. As we'll see further on trip_miles doesn't distribute normally.
# 2. There's no linear relation between trip_miles and trip_seconds (traffic...)
# We can look how does trip_miles distribute for bins of trip_seconds
# and then pick random results from this distribution for the records with trip_miles 0 according to their trip_seconds.
# But that is far too much complicated for this case, so it's better to just omit them.
dataset = dataset[!trip_miles_na,]

# There's one more problematic feature:
cat("Number of records with NA company:",sum(is.na(dataset$company)),"out of total:",nrow(dataset))
# Almost half of the records are NA, no impute strategy will make sense here and this feature isn't in any use for us.
# For these three reasons - we'll omit the feature:
dataset$company = NULL

# Let's look what NA's we've got left:
summary(dataset)
# Few NA's in pickup/dropoff_community_area features and one in taxi_id.

# It's too bad to loose good records for NA in pickup/dropoff_community_area features.
# We'll turn these NA's into 0  so it will mark "unknown" in this numeric feature.
dataset$pickup_community_area[is.na(dataset$pickup_community_area)] = 0
dataset$dropoff_community_area[is.na(dataset$dropoff_community_area)] = 0

# And for the last NA (and maybe hidden ones...) :
dataset = na.omit(dataset)

# In addition, for the correctness of the data, we'll omit records which are unreasonable.

# First, records with Trip total which doesn't match the sum of this trip's fare, tips, extras and tolls (with tolerance of 1).
incorrect_records = abs(dataset$fare + dataset$tips + dataset$extras + dataset$tolls - dataset$trip_total) > 1
cat("We have",sum(incorrect_records),"records with trip total not equals to fare+extras+tips+tolls (by more than 1).")
if (sum(incorrect_records)>0)
{
  dataset = dataset[!(incorrect_records),]
}

# Second, records with impossible ratio between distance and time, which imply that the taxi has driven more than 90 mph in average:
incorrect_records = (dataset$trip_miles / ((((dataset$trip_seconds)+1)/3600))) > 90
# (We divided the seconds feature by 3600 so our calculation will result in miles per hour, we added 1 to avoid dividing by 0)
cat("We have",sum(incorrect_records),"records with impossible ratio between distance and time.")
if (sum(incorrect_records)>0)
{
  dataset = dataset[!(incorrect_records),]
}


# 4.a. Make a Q-Q plot for each of the following columns: [trip_seconds, trip_miles, trip_total]. 
# Explain what we can learn from a Q-Q plot about the distribution of the data.
################################

qqPlot(dataset$trip_seconds, line = "quartiles")
qqPlot(dataset$trip_miles, line = "quartiles")
qqPlot(dataset$trip_total, line = "quartiles")


# The Q-Q plots teaches us if a dataset comes from a population with a normal distribution.
# As closer as the dots in the graph to the red line the data distributes closer to normal.
# The dashed lines mark the 95% confidence intervals.
# In this case we can see that the data is very far from the normal distribution.
# The three features distribute with a srtong right (positive) skew.

# 4.b. (7) According to the Q-Q plots ,do we need to normalize these features? Which normalization function should we use for each feature, if any?
# For each feature, in case you decided to normalize it, create a new normalized column of the feature (eg. norm.trip_seconds).
################################

# As we have seen from the Q-Q plots these features are far from the normal distribution
# and very skewed in a way that the majority of the records are with values that are close to zero.
# Beside the resolution that most of the cab rides in Chicago are short it would be very hard to conclude anything more.
# In addition, the scale for each fature is different (e.g. trip_seconds goes as far as 20,000, but trip_miles only to 40).
# This situation makes it difficult for us to measure  different features one against eachother.
# We can deal these two issues by using the LOG transformation to normalize the data and then scale it around zero.
# The Log transformaion takes a large spread of values and narrows it and hence very useful for skewed distributed data.
# For evidence we can see how it makes the mean and the median much closer.
# The scale function will shift the values to be around zero by: New value = (old value - mean)/SD

cat("Trip seconds: Median is",round(median(dataset$trip_seconds),2),", Mean is :",round(mean(dataset$trip_seconds),2))
cat("Scale(Log(Trip seconds)): Median is",round(median(scale(log(dataset$trip_seconds))),2),", Mean is :",round(mean(scale(log(dataset$trip_seconds))),2))

cat("Trip total: Median is",round(median(dataset$trip_total),2),", Mean is :",round(mean(dataset$trip_total),2))
cat("Scale(Log(Trip total)): Median is",round(median(scale(log(dataset$trip_total))),2),", Mean is :", round(mean(scale(log(dataset$trip_total))),2))

cat("Trip miles: Median is",round(median(dataset$trip_miles),2),", Mean is :", round(mean(dataset$trip_miles), 2))
cat("Scale(Log(Trip Miles)): Median is",round(median(scale(log(dataset$trip_miles))),2),", Mean is :", round(mean(scale(log(dataset$trip_miles))),2))

# So we'll define new, normalized, variables:
dataset$normalized_trip_seconds = scale(log(dataset$trip_seconds))
dataset$normalized_trip_total   = scale(log(dataset$trip_total))
dataset$normalized_trip_miles   = scale(log(dataset$trip_miles))

# Now lets look at the normalized fatures' Q-Q plots:
qqPlot(dataset$normalized_trip_seconds, line = "quartiles")
qqPlot(dataset$normalized_trip_miles, line = "quartiles")
qqPlot(dataset$normalized_trip_total, line = "quartiles")

# We can see that they got much closer to the normal distribution, but still not that close,
# and that the values are around the same numbers for all (normalized) features.


# 5.a. Create a boxplot of the normalized trip_miles column (or the original column in case you chose not to normalize) Remove the column's 
# outliers from the data based on the box plot. Hint: use the boxplot object.
################################

boxplot(dataset$normalized_trip_miles,outline = FALSE)


# 5.b. Implement a min-max transformation on the normalized columns of [trip_seconds, trip_miles, trip_total] 
# (or the original columns in case you chose not to normalize). 
# Create new column with the transformed data (eg. minmax.trip_seconds) 
################################

# We will apply this method using new_max = 1 and new_min = 0
# and by that causing all the values to be between 0-1.
dataset$minmax.trip_seconds = (dataset$normalized_trip_seconds - min(dataset$normalized_trip_seconds)) / (max(dataset$normalized_trip_seconds) - min(dataset$normalized_trip_seconds))
dataset$minmax.trip_miles   = (dataset$normalized_trip_miles   - min(dataset$normalized_trip_miles))   / (max(dataset$normalized_trip_miles)   - min(dataset$normalized_trip_miles))
dataset$minmax.trip_total   = (dataset$normalized_trip_total   - min(dataset$normalized_trip_total))   / (max(dataset$normalized_trip_total)   - min(dataset$normalized_trip_total))


# 5.c. (10) Using the 3 columns you created, you will use a hierarchical-clustering method, followed by density-based method.
# First, use hierarchical-clustering method to evaluate the probability of each instance to be an outlier. 
# Exclude all instances with 0.75 chance or higher. Hint: use "DMwR" package.
# Then, using LOF, pick k=10 and remove all instances that their LOF score is above 1.4. Hint: use "Rlof" package.

# Outlier detection: Normalized trip seconds
hist(dataset$minmax.trip_seconds)
outlier_score = outliers.ranking(dataset$minmax.trip_seconds ,clus=list(dist='euclidean',alg='hclust',meth='average'))
outliers_seconds = outlier_score$prob.outliers > 0.75
cat("Trip seconds: According to hierarchical-clustering - number of outliers are: ",sum(outliers_seconds)," Out of total of: ",nrow(dataset)," rows in database.")

outlier_score = lof(dataset$minmax.trip_seconds, 10)
outliers_density_seconds = outlier_score > 1.4 
cat("Trip seconds: According to LOF - number of outliers are: ",sum(outliers_density_seconds,na.rm = TRUE)," Out of total of: ",nrow(dataset)," rows in database.")


#Outlier detection: Normalized trip miles
hist(dataset$minmax.trip_miles)
outlier_score_miles = outliers.ranking(dataset$minmax.trip_miles ,clus=list(dist='euclidean',alg='hclust',meth='average'))
outliers_miles = outlier_score_miles$prob.outliers > 0.75 
cat("Trip seconds: According to hierarchical-clustering - number of outliers are: ",sum(outliers_miles)," Out of total of: ",nrow(dataset)," rows in database.")

outlier_score_miles = lof(dataset$minmax.trip_miles, 10)
outlier_density_miles = outlier_score_miles > 1.4 
cat("Trip miles: According to LOF - number of outliers are: ",sum(outlier_density_miles,na.rm = TRUE)," Out of total of: ",nrow(dataset)," rows in database.")


#Outlier detection: Normalized trip total
hist(dataset$minmax.trip_total)
outlier_score_total = outliers.ranking(dataset$minmax.trip_total ,clus=list(dist='euclidean',alg='hclust',meth='average'))
outliers_total = outlier_score_total$prob.outliers > 0.75 
cat("Trip seconds: According to hierarchical-clustering - number of outliers are: ",sum(outliers_total)," Out of total of: ",nrow(dataset)," rows in database.")

outlier_score_total = lof(dataset$minmax.trip_total, 10)
outlier_density_total = outlier_score_total > 1.4 
cat("Trip miles: According to LOF - number of outliers are: ",sum(outlier_density_total,na.rm = TRUE)," Out of total of: ",nrow(dataset)," rows in database.")


# The hierarchical-clustering method determines that we need to omit most of our data.
# Because we want our data for the rest of the assigment we'll use only the second method to omit outliers:
cat("Total records to remove based on density outlier rejection is",sum(outliers_density_seconds | outlier_density_miles | outlier_density_total, na.rm = TRUE))

index_to_remove = unique(c(which(outliers_density_seconds) , which(outlier_density_miles) , which(outlier_density_total)))
dataset = dataset[-index_to_remove,]
cat("There are now ",nrow(dataset)," rows in data set")


################################

#6.a. Create a correlation matrix of all the relevant numerical features. In addition, Display a correlation plot for this matrix. 
# Write 3 business insights we can learn from the correlation matrix.
################################

# First we isolate the relevant fetures:
numeric_dataset = sapply(dataset, is.numeric)
numeric_dataset[c("X","taxi_id","pickup_community_area","dropoff_community_area")]=FALSE

corr_matrix = cor(dataset[,numeric_dataset])
corrplot(corr_matrix, type="upper", order="hclust")

# Let's talk business:
# Tips has a weak but positive correlation to all the other relevant variables (time, distance, fare).
# We learn two things from that:
# From the fact that the correlation is positive - Business insight #1:
# Short trips = Small tips   ==>  Take longer rides for larger tips.
# From the fact that the correlation is weak - Business insight #2:
# The best guarantee for a good tip is not associated with technical features - so it's probably the smile :)
# Trip_total is better correlates with trip_miles than with trip_seconds so-
# Business insight #3: If you care for your income, better make your trip longer in DISTANCE than in TIME.
# Standing in the traffic is first annoying and second less rewarding than getting out to the highway.


#6.b. Create 5 different statistical outputs based on the dataset. Visualize at least 3 of them. Add an explanation. Try to be creative.
#Examples:
#  1.	A bar chart that displays the average and median amount of trip_total, for each payment_type. 
#  2. Density plots of trip_second - one for each day.
################################

# Stat A: The next plot will show the areas with the most pickups and dropoffs:

hist_val = hist(dataset$pickup_community_area, breaks = length(unique(dataset$pickup_community_area)))
most_pickups = which.max(hist_val$density)
cat("The area code with the most pickups is area",most_pickups,"with",format(hist_val$density[most_pickups]*100, digits = 4),"% of total pickups")
hist_val = hist(dataset$dropoff_community_area, breaks = length(unique(dataset$dropoff_community_area)))
most_dropoffs = which.max(hist_val$density)
cat("The area code with the most dropoffs is area",most_dropoffs,"with",format(hist_val$density[most_dropoffs]*100, digits = 4),"% of total pickups")


# Stat B: The pickup community area with the "best" trip_total

# Lets Say we are interested in focusing on a specific community area. We have two cashflow strategies.
# First strategy: Steady income. We are interested in choosing a community area with a steady income value. 
# That is why we will choose a community area with the lowest standard error.
# Second strategy: We are looking for the highest income. We will take the communitry area with the highest median. 
# We take median and not mean because we want to reject the outliers.

highest_median = -1
community_code_with_highest_median = 0
community_code_with_lowest_std = 0
value_in_lowest_std = 0
lowest_std = 1000
pickup_vector = dataset$pickup_community_area
for (pickup_iterator in 1:length(unique(dataset$pickup_community_area))){
  current_index = dataset$pickup_community_area == pickup_iterator
  if (sum(current_index,na.rm =TRUE) <5)
  {
    next;
  }
  curr_median_val = median(dataset$trip_total[current_index],na.rm = TRUE)
  curr_sd_val = sd(dataset$trip_total[current_index],na.rm=TRUE)
  
  if ((curr_sd_val) > 0 & (curr_sd_val < lowest_std))
  {
    lowest_std = curr_sd_val
    community_code_with_lowest_std = pickup_iterator
    median_in_lowest_std = curr_median_val
  }
  if (curr_median_val > 0 & curr_median_val > community_code_with_highest_median)
  {
    highest_median = curr_median_val
    community_code_with_highest_median = pickup_iterator
    std_value_in_highest_median = curr_sd_val
  }
}
cat("Pickup community area",community_code_with_highest_median,"has the statistically highest trip_total in average with median of",highest_median,"$ (with STD of",std_value_in_highest_median,")")
cat("Pickup community area",community_code_with_lowest_std,"has the lowest SD of trip_total with a standard error of",lowest_std,"$ (with median value of",median_in_lowest_std,")")


# Stat C: What is the busiest time and the least busy in the day in our busiest area?

trip_start_time_at_most_pickups = dataset$trip_start_time[dataset$pickup_community_area == most_pickups]
hours = as.numeric(format(strptime(trip_start_time_at_most_pickups,format = '%m/%d/%Y %H:%M'), "%H"))
hist(hours, breaks = 24, main = paste("Number of pickups by hour in the busiest pickup area: ",most_pickups))
# Send less cabs around 5 A.M. and more around 5 P.M. !!!


# Stat D: Number of trips for each day:

dataset$date = as.Date(dataset$trip_start_timestamp, format = "%m/%d/%Y")
barplot(table(dataset$date), main="Trips by date", xlab="Date", border = 'red', density = table(dataset$date)/50)
# We can see that there was a sharp increase in trips in the first 2 days and from then a small increase.
# The first date is sunday and also Christmas, maybe it has something to do with the small amount of trips.
# In that case we could make business insights from this information.
# But maybe it has something to do with collecting the data that with the experience had improved along the week.


# Stat E: The split between Cash and Credit card payment according to cost of the trip:

# First, we'll discretize the trip_total feature to 3 catagories:
dis_trip_total = discretize(dataset$trip_total, method = "fixed", categories = c(0,10,40,Inf),
                            labels =c("below 10","10-40","more than 40"))
# Second, we'll make a table for the amount of trips for each catagory and payment method:
payment_by_total = table(dis_trip_total, dataset$payment_type)
# It looks like this:
head(payment_by_total)
# Now we'll remove all payment methods which aren't "Cash" or "Credit Card" because they are insignificant
# and transpose the table for the graph:
payment_by_total = t(payment_by_total[,1:2])
# The graph:
barplot(payment_by_total, beside = TRUE, col = c("dark green","light green"))
legend("topright", c("Cash","Credit card"), fill = c("dark green","light green"), cex = 0.5)
# You can see that when the cost is low people pay by cash
# and while the payment increase the method is changing to credit card.
