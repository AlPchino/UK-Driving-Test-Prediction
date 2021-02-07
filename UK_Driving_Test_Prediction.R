install.packages("readODS")
install.packages('tidyverse')
library(plyr)
library(tidyverse)
library(MASS)

ID = 202017866
XYZprofile<-function(ID){
  if(ID >= 202100000 || ID <=200000000) stop("Wrong LSE Student ID!")
  set.seed(ID)
  gender = sample(c("Male","Female"),1)
  age = sample(17:25,1)
  # List all the UK driving centres
  locations = c("Ashfield", "Ashford (London Middlesex)", "Atherton (Manchester)", "Banbury", "Barking (London)", "Barnet (London)", "Barnsley", "Rotherham",
                "Basildon", "Basingstoke", "Belvedere (London)", "Birmingham (Garretts Green)", "Birmingham (Kings Heath)", "Birmingham (Kingstanding)",
                "Birmingham (Shirley)", "Birmingham (South Yardley)", "Birmingham (Sutton Coldfield)", "Bishops Stortford", "Blackburn with Darwen", "Blackpool",
                "Bletchley", "Blyth", "Bolton (Manchester)", "Borehamwood (London)", "Bournemouth", "Bradford (Heaton)", "Bradford (Thornbury)", "Bredbury (Manchester)",
                "Bristol (Brislington)", "Bristol (Kingswood)", "Bristol (Southmead)", "Burgess Hill", "Burton on Trent", "Bury (Manchester)", "Camborne", 
                "Cambridge (Chesterton Road)", "Cambridge (Cowley Road)", "Cardiff (Llanishen)", "Cardington", "Cheetham Hill (Manchester)", "Cheltenham", "Chertsey (London)",
                "Chester", "Chichester", "Chingford (London)", "Chorley", "Colchester", "Coventry", "Crawley", "Croydon (London)","Darlington", "Doncaster", "Dorchester", 
                "Dundee", "Durham", "Eastbourne", "Edinburgh (Currie)", "Edinburgh (Musselburgh)", "Elswick", "Enfield (London)", "Erith (London)", "Exeter", "Warwick", 
                "Failsworth (Manchester)", "Farnborough", "Garston (Liverpool)", "Gateshead", "Gillingham", "Glasgow (Anniesland)", "Glasgow (Baillieston)",  "Nelson", 
                "Glasgow (Shieldhall)", "Gloucester", "High Wycombe", "Hither Green (London)", "Leeds", "Leicester (Wigston)", "Peterborough", "Pinner (London)",
                "Goodmayes (London)", "Greenford (London)", "Guildford", "Halifax", "Hamilton", "Hayes (London)", "Heckmondwike", "Hendon (London)", "Herne Bay",
                "Hornchurch (London)", "Horsforth", "Huddersfield", "Hyde (Manchester)", "Ipswich", "Isleworth (London)", "Kettering", "Kirkcaldy", "Lee On The Solent",
                "Lichfield", "Lincoln", "Loughton (London)", "Lower Gornal", "Luton", "Maidstone", "Middlesbrough", "Mill Hill (London)", "Mitcham (London)", "Morden (London)",
                "Newport (Gwent)", "Norris Green (Liverpool)", "Northampton", "Norwich", "Nottingham (Beeston)", "Nottingham (Colwick)", "Oxford (Cowley)", "Paisley",  
                "Plymouth", "Pontefract", "Pontypridd", "Portsmouth", "Preston", "Reading", "Reigate", "Rochdale (Manchester)", "Sale (Manchester)", "Sheffield (Handsworth)", 
                "Sheffield (Middlewood Road)", "Sidcup (London)", "Slough (London)", "South Shields", "Southall (London)", "Southampton (Forest Hills)", "Southampton (Maybush)", 
                "Southend-on-Sea", "Southport (Liverpool)", "St Albans", "St Helens (Liverpool)", "Stoke-On-Trent (Cobridge)", "Stoke-on-Trent (Newcastle-Under-Lyme)", "Sunderland", 
                "Sutton (London)", "Swansea", "Swindon", "Taunton", "Telford", "Tilbury", "Tolworth (London)", "Upton", "Uxbridge (London)", "Wakefield", "Wanstead (London)", 
                "Watford", "Wednesbury", "West Didsbury (Manchester)", "West Wickham (London)", "Wolverhampton", "Worcester", "Worksop", "Worthing", "York")
  centre = sample(locations,1)
  cat(paste("The profile of XYZ:", paste("- Age: ", age), paste("- Gender: ", gender), paste("- Home address: ", centre), "" ,sep="\n"))
}
XYZprofile(ID)

#Read the data into R as a list of Dataframes
library(readODS)

#Get Number of sheets in ODS file
sheets <- get_num_sheets_in_ods("dvsa1203_final.ods")

#Convert 'the above into an iterable'sheets' into an iterable number of sheets
sheetList <- (1:sheets) 

#Apply read_ODS function to each sheet in the file
list_with_sheets <- lapply(sheetList, function(i)read_ods("dvsa1203_final.ods", 
                                                          sheet = i, col_names = FALSE))
names(list_with_sheets) <- list_ods_sheets('dvsa1203_final.ods')

# Combine all DataFrames into one large one
driving_data <- do.call("rbind",list_with_sheets) 

# Create a list of new names and rename the columns with concatenated names
new_names <- paste0(as.character(driving_data[1,]), as.character(driving_data[2,]))  
names(driving_data) <- new_names

# Select only relevant rows now 
driving_data <- driving_data[3:nrow(driving_data), ] 
driving_data <- na.omit(driving_data)

# Create the final Data Frame:
driving_data_2 <- driving_data[driving_data$NACenter %in% c("Colchester", "Wood Green (London)"), ]

# Change the datatypes of the variables to suitable types
driving_data_2[,2:11] <- sapply(driving_data_2[,2:11],as.numeric)
driving_data_2$NACenter <- as.factor(driving_data_2$NACenter)
attach(driving_data_2) #Attach column names

# Rename the data frame columns
colnames(driving_data_2) <- c("Center","Age", "Male testsConducted", "Male testsPasses", "Male testsPass rate (%)", 
                              "Female testsConducted", "Female testsPasses","Female testsPass rate (%)",
                              "Total testsConducted","Total testsPasses","Total testsPass rate (%)", "Date")

# Calculate the mean all ages for both centers
Mean_20 <-  aggregate(driving_data_2, list(center = Center, Age = Age), FUN = mean)
# Change row names to easily extract mean for relevant rows and delete irrelevant columns
Mean_20$Centre_Age <- paste(Mean_20$center, Mean_20$Age)

#Replace row names with new column containing descriptive row names
row.names(Mean_20) <- Mean_20$Centre_Age 
Mean_20[1:4] <- NULL #Delete irrelevant columns

#Get the mean for Colchester and WoodGreen
Colchester_Rate <- Mean_20["Colchester 20","Female testsPass rate (%)"]
WoodGreen_Rate <- Mean_20["Wood Green (London) 20","Female testsPass rate (%)"]

#Create boxplots for female data on the cleaned data frame
splits <- split(driving_data_2, driving_data_2$Center)
par(mfrow = c(1,2))
boxplot(`Female testsPass rate (%)` ~ Age, data = splits[[1]], main = "Colchester EDA", col = 'purple')
boxplot(`Female testsPass rate (%)` ~ Age, data = splits[[2]], main = "Wood Green EDA", col = 'orange')

#Prepare data for Logistic regression
female_data <- driving_data_2[,-c(3,4,5,9,10,11)] #Select only female data
bigdata <- data.frame() #Create empty data frame to house expanded female data
for (i in 1:nrow(female_data)){
  row = female_data[i,]
  passed = `Female testsPasses`[i]
  failed = `Female testsConducted`[i] - passed
  df1 = data.frame(lapply(row, rep, passed))
  df2 = data.frame(lapply(row, rep, failed))
  df1$passed = 1
  df2$passed = 0
  thisdata = rbind(df1, df2)
  bigdata = data.frame(rbind(bigdata, thisdata))
}
bigdata$sex <- 'Female' #Create column to indicate this is female data

bigdata2 <- data.frame() #Create empty data frame to house expanded male data
for (i in 1:nrow(driving_data_2)){
  row2 = driving_data_2[i,]
  passed2 = driving_data_2$`Male testsPasses`[i]
  failed2 = driving_data_2$`Male testsConducted`[i] - passed2
  df3 = data.frame(lapply(row, rep, passed2))
  df4 = data.frame(lapply(row, rep, failed2))
  df3$passed2 = 1
  df4$passed2 = 0
  thisdata2 = rbind(df3, df4)
  bigdata2 = data.frame(rbind(bigdata2, thisdata2))
}
bigdata2$sex <- 'Male'  #Create column to indicate this is male data

# Select relevant fields for male and female data
bigdata2 <- bigdata2[,-c(3:6)] 
bigdata <- bigdata[,-c(3:6)]

#Rename columns to align names of both data frames and join both data together
colnames(bigdata2)[colnames(bigdata2) == 'passed2'] <- 'passed'
final_data <- rbind(bigdata, bigdata2)

# Make 'center' and 'sex' dummy variables
final_data$center_Colchester <- ifelse(final_data$Center == 'Colchester', 1, 0)
final_data$center_WoodGreenLondon <- ifelse(final_data$Center == 'Wood Green (London)', 1, 0)
final_data$sex_male <- ifelse(final_data$sex == 'Male', 1, 0)
final_data$sex_female <- ifelse(final_data$sex == 'Female', 1, 0)

# Use glm function to plot logistic regression model with all variables and view
model = glm(passed ~., data =final_data, family = binomial)
summary(model)
#Note: there is multicollinearity expressed through the NA values

#Use Step-wise selection to get the best model
#The multicollinearity is removed by the› step-wise selection
step.model <- model %>% stepAIC(trace = FALSE)
summary(step.model) 

#Calculate the response values for plotting
b0 = step.model$coef[1]
age = step.model$coef[2]
center = step.model$coef[3] # 1 for Colchester and 0 for Wood Green
sex = step.model$coef[4] # 1 for male and 0 for female

# Use range of -100 to 100 to show S shape of Sigmoid function
age_range <- seq(-100, 100, by = .001)
colch = b0 + age*age_range + sex*0 + center*1 #Sex = 0 because XYZ is female
wood = b0 + age*age_range + sex*0 + center*0 #Center = 0 because there is no coefficient for Wood_Green

#Use Sigmoid function to convert model output to probabilities for both centers
colch_probs <- exp(colch)/(1 + exp(colch))
wood_probs<- exp(wood)/(1 + exp(wood))

#Plot the graphs
plot(age_range,colch_probs,
     ylim=c(0,1),
     xlim = c(0,25),
     type="l",
     lwd=3,
     lty=2, 
     col="violetred3",
     xlab="Age", ylab="P(pass|age,gender)", 
     main="Probability of pass rate at centres")

lines(age_range,wood_probs, 
      type="l", 
      lwd=3, 
      lty=3, 
      pch = "*",
      col="turquoise3")

legend(17.5,0.99, legend = c('Wood Green', 'Colchester'), col = c("turquoise3","violetred3"),
       pch = c(20,20), title = 'Centers', text.font = 2)
legend(0.1,0.35, legend = c('P(pass) = 0.384', 'P(pass) = 0.372'), col = c("turquoise3","violetred3"),
       cex = 0.7, pch = c(10,10), title = 'probabilities', text.font = 2)

#Calculate the probabilities of a 20 year old passing at both centers
#and plot lines to show them on the graph
colch_20 = b0 + age*20 + sex*0 + center*1
wood_20 = b0 + age*20 + sex*0 + center*0

# Use Sigmoid function to convert to probabilities
colch_probs_20 = exp(colch_20)/(1 + exp(colch_20))
wood_probs_20 = exp(wood_20)/(1 + exp(wood_20))

#Plot vertical and horizontal lines on the graph
segments(x0=20,y0=-1,y1=0.372, col = 'violetred3')
segments(x0=-0.9, y0=0.372, x1=20, col = 'violetred3')
segments(x0=20,y0=-1,y1=0.384, col='turquoise3')
segments(x0=-1, y0=0.384, x1=20, col='turquoise3')
