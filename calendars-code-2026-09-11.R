#A null model baseline for the weeks data

#Olivier Morin & Janne Yrjö-Koskinnen

#0. Loading packages #########

#packages 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(viridis)
library(forcats)
library(ggrepel)
library(stringr)
library(maps)
library(patchwork)

#1. Possible and real calendars ####

#In this script, calendars are generally represented as binary strings. 
#Each bit stands for a presence or absence of a calendar cycle, in this order:

#Solar year
#Solar month
#Lunar month
#Lunar year
#Lunar week
#Arbitrary week

#1.1. Generating all possible calendar types, in binary format #####

## Generate all combinations of 0 and 1 for 6 positions
all_binary <- expand.grid(rep(list(c(0,1)), 6))
# Collapse each row into a single string
all_binary_strings <- apply(all_binary, 1, paste0, collapse = "")
all_binary_strings

#Constraint: No solar months without a solar year.
solar.constraint  <- all_binary_strings[!(
  substr(all_binary_strings, 1, 1) == "0" &  # first position is 0
    substr(all_binary_strings, 2, 2) == "1"    # second position is 1
)]

#Constraint: No lunar week or year without a lunar month
lunar.constraint <- solar.constraint[!(
  substr(solar.constraint, 3, 3) == "0" &  # position 3 is 0
    (substr(solar.constraint, 4, 4) == "1" | substr(solar.constraint, 5, 5) == "1")  # position 4 or 5 is 1
)]

#Remove the calendars with both solar & lunar year: 
all.possible.types <- lunar.constraint[!(substr(lunar.constraint, 1, 1) == "1" & substr(lunar.constraint, 4, 4) == "1")]
all.possible.types

#I list the cycles / units in a vector, useful for later:

all.cycles <- as.vector(c("solar.year", "lunar.year", "solar.month", "lunar.month", "lunar.week", "arbitrary.week"))

#Naming all the calendars: Giving a numerical label to each possible calendar.

#The goal is simply to get more legible labels.

labels <- data.frame(
  BinaryString = character(),
  label = character()
)

#Null calendar:
labels[nrow(labels) + 1, ] <- list("000000", "C0")
#Weeks only:
labels[nrow(labels) + 1, ] <- list("000001", "C1")
#Solar calendars:
labels[nrow(labels) + 1, ] <- list("100000", "C2")
labels[nrow(labels) + 1, ] <- list("100001", "C3")
labels[nrow(labels) + 1, ] <- list("110000", "C4")
labels[nrow(labels) + 1, ] <- list("110001", "C5")
#Lunar calendars:
labels[nrow(labels) + 1, ] <- list("001000", "C6")
labels[nrow(labels) + 1, ] <- list("001001", "C7")
labels[nrow(labels) + 1, ] <- list("001100", "C8")
labels[nrow(labels) + 1, ] <- list("001101", "C9")
labels[nrow(labels) + 1, ] <- list("001010", "C10")
labels[nrow(labels) + 1, ] <- list("001011", "C11")
labels[nrow(labels) + 1, ] <- list("001110", "C12")
labels[nrow(labels) + 1, ] <- list("001111", "C13")
#Lunisolar
labels[nrow(labels) + 1, ] <- list("101000", "C14")
labels[nrow(labels) + 1, ] <- list("101001", "C15")
labels[nrow(labels) + 1, ] <- list("111000", "C16")
labels[nrow(labels) + 1, ] <- list("111001", "C17")
labels[nrow(labels) + 1, ] <- list("101010", "C18")
labels[nrow(labels) + 1, ] <- list("101011", "C19")
labels[nrow(labels) + 1, ] <- list("111010", "C20")
labels[nrow(labels) + 1, ] <- list("111011", "C21")

#Sanity check: all 22 unique strings are here
length(unique(labels$BinaryString))

#1.2. Real calendars #######

#Opening the dataset:
d.complete <- read.csv("included_calendars.csv")

#Optional: Removing calendars not in the Standard Cross-Cultural Sample: 
#d <- subset(d, d$SCCS == "Present")
#length(unique(d$Culture_name))
#unique(d$Culture_name)

#Simplifying the dataframe to only get what we will be using for this code
colnames(d.complete)
calendar.info<- d.complete[,c("Culture_name", "Calendar_name"   )]

d <- d.complete[,c("Solar_year" , "Solar_month", "Lunar_month" ,  "Lunar_year" ,  "Lunar_week" ,"Arbitrary_week"  )]
         
n.real.calendars <- nrow(d)

#Getting the probability d.excluded#Getting the probability for basic units
prop_solarYear <-  (nrow(d[d$Solar_year == "1a",]) +  nrow(d[d$Solar_year == "1b",]) ) / n.real.calendars
prop_solarYear
prop_LunarMonth <-  (nrow(d[d$Lunar_month == "1a",]) +  nrow(d[d$Lunar_month == "1b",]) ) / n.real.calendars
prop_LunarMonth
prop_ArbitraryWeek <-  (nrow(d[d$Arbitrary_week == "1a",]) +  nrow(d[d$Arbitrary_week == "1b",]) ) / n.real.calendars
prop_ArbitraryWeek

#1.3. Getting existing calendar types ########

#Transforming the 1as and 1bs to simple 1s, same for 0s
d[] <- lapply(d, function(x) as.numeric(gsub("\\D", "", x)))
#Extracting types:
d$calendar.type <- apply(d, 1, function(x) paste(x, collapse = ""))

d <- d %>%
  rename("BinaryString" = "calendar.type")

d <- d %>%
  left_join(labels, by = "BinaryString")

d <- cbind(calendar.info, d)

head(d)

the.real.types <- unique(d$BinaryString)
the.real.types

#Sanity check: all real types are possible
setdiff(the.real.types, all.possible.types)
#character(0) = there is no real type that is not possible.

#Exploring existing types:
unique(d$label)
d[d$label == "C5",]$Subsistence_type
d[d$label == "C5",]$Calendar_name


#For instance, looking at all luni-solar calendars:
d[d$label%in% c("C14", "C15", "C16","C17","C18","C19","C20","C21"),]$Calendar_name
d[d$label%in% c("C14", "C15", "C16","C17","C18","C19","C20","C21"),]$Subsistence_type

#Counting all calendars with a solar or lunar year:
nrow(d[d$Solar_year == "1" | d$Lunar_year == "1",])

#Same with lunar months
nrow(d[d$Lunar_month == "1",])

#Cleaning up
rm(lunar.constraint, solar.constraint, all_binary, all_binary_strings)
 
#2. Main data analysis function #####

#2.1. Core loop: Generates n calendars #####

core.loop <- function(
      equiprobable,
      basic.constraints,
      years.imply.months,
      months.imply.years,
      no.moon.alone,
      n.simulated.calendars
    ){
  
  df <- data.frame(
    ID       = 1:n.simulated.calendars,
    Solar_year      = NA,
    Solar_month   = NA,
    Lunar_month = NA,
    Lunar_year    = NA,
    Lunar_week =NA,
    Arbitrary_week   = NA
  )
  
    for (i in 1:n.simulated.calendars){
      
      #If the model treats every calendar type as equiprobable, 
      #this (and none of the rest) happens:
      
      if(equiprobable == "Yes"){
      #Selecting a calendar type at random: 
        a.random.type <- sample(all.possible.types, 1)
        #Turning it into a binary vector:
        a.random.type <- as.integer(strsplit(a.random.type, "")[[1]])
        df[i, c(2:7)] <- a.random.type
        
        df[i,]
      }
      
      
      #All the other operations only happen if the calendar types are not treated as 
      #equiprobable. 
      
      if(equiprobable == "No"){
        
      #If the model is fine-tuned with the original data for
      #the basic cycles ("basic constraints" model), this section happens.
      # Instead of drawing the presence or absence of solar years,
      #lunar months, arbirary weeks, etc. with a 50% chance, 
      #these cycles are present with a probability equal to
      #their frequency in our dataset.
        
      if(basic.constraints == "Yes"){
        proba.ArbitraryWeek <- prop_ArbitraryWeek}else{
          proba.ArbitraryWeek <- 0.5   
        }
      
      if(basic.constraints == "Yes"){
        proba.SolarYear <- prop_solarYear}else{
          proba.SolarYear <- 0.5   
        }
      
      if(basic.constraints == "Yes"){
        proba.LunarMonth <- prop_LunarMonth}else{
          proba.LunarMonth <- 0.5   
        }
      
      #Now we start deriving: 
      
      #drawing basic cycles:
      df[i,'Solar_year'] <- rbinom(1, size = 1, prob = proba.SolarYear)
      df[i,'Lunar_month']  <- rbinom(1, size = 1, prob = proba.LunarMonth)  
      df[i,'Arbitrary_week']  <- rbinom(1, size = 1, prob = proba.ArbitraryWeek)
      
      #drawing derived cycles:
      
      #Solar months, only possible if a solar year is present:
      ifelse(df[i,'Solar_year'] == 1, 
             df[i,'Solar_month']  <- rbinom(1, size = 1, prob = 0.5),
             df[i,'Solar_month'] <- 0
      )
      #Lunar weeks, only possible if a lunar month is present:
      ifelse(df[i,'Lunar_month'] == 1, 
             df[i,'Lunar_week'] <- rbinom(1, size = 1, prob = 0.5),
             df[i,'Lunar_week'] <- 0
      ) 
      #Lunar years, only possible if a lunar month is present
      #AND if a solar year is not present 
      #(this last constraint is due to the way we build 
      #our coding scheme):
      if(df[i,'Solar_year'] == 0){
        ifelse(df[i,'Lunar_month'] == 1, 
               df[i,'Lunar_year'] <- rbinom(1, size = 1, prob = 0.5),
               df[i,'Lunar_year'] <- 0
        )     
      }else{ df[i,'Lunar_year'] <- 0    }
      
      #NO MOON ALONE 
      
      #The lines below implement the "no purely lunar calendar" rule ("no moon alone"):
      #If a simulated calendar has only lunar units,  
      #A coin is flipped, and either the lunar units are removed, 
      #Or solar units are added
      if(no.moon.alone == "Yes"){
        # Detecting purely lunar calendars:
        if(df[i,'Lunar_month'] == 1 & df[i,'Solar_year'] == 0 ){
          #  print('moon alone')
          # print(df[i,])
          df[i,'Lunar_month']
          draw <- sample(c(0, 1), 1)
          if (draw == 0) {
            df[i,'Lunar_month'] <- 0
            df[i,'Lunar_week']  <- 0
            df[i,'Lunar_year']  <- 0
          } else {
            df[i,'Solar_year'] <- 1
            df[i,'Lunar_year'] <- 0
          }
          #  print(df[i,])
          
        }
        # If we turned the solar year to 1 then we need to add a solar month with probability 0.5          
        if(df[i,'Solar_year'] == 1){
          draw <- sample(c(0, 1), 1)
          if (draw == 1) {
            df[i,'Solar_month'] <- 1
          } else {
            df[i,'Solar_month'] <- 0
          }
        }
      }      
      
      #Adding the constraint: if there is a year there must be a month & vice versa
      #More specifically: 
      #If there is a solar year and no month of any kind,
      #Randomly assume either a solar month or a lunar month.
      
      if(years.imply.months == "Yes"){
        if(df[i,'Solar_year'] == 1 & df[i,'Lunar_month'] == 0 & df[i,'Lunar_month'] == 0 ){
          draw <- sample(c(0, 1), 1)
          ifelse(draw ==0, df[i,'Lunar_month'] <-  1, df[i,'Solar_month'] <-   1)
        }else{} 
        #Sanity check
        if(df[i,'Solar_year'] == 1 & df[i,'Lunar_month'] == 0 & df[i,'Solar_month'] == 0 ){
          print("error")}else{}
      }else{} 
      
      #And now: Months => Year
      #More specifically: 
      #If there is a lunar month without a Year (can't have a solar month without 
      #solar year at least), add a solar year.

      if(months.imply.years == "Yes"){
        if(df[i,'Lunar_month'] == 1  & df[i,'Lunar_year'] == 0 & df[i,'Solar_year'] == 0 ){
          df[i,'Solar_year'] <-  1
        }else{}
        
        #Sanity check
        if(df[i,'Lunar_month'] == 1 & df[i,'Solar_year'] == 0 & df[i,'Lunar_year'] == 0){
          print("month years error")}else{}
      }else{}
    
    }
  
    }
  
      
  df$calendar.type <- apply(df[,c(2:7)], 1, function(x) paste(x, collapse = ""))
  
  #  return the dataframe
  
  df
  }
  

#Trying it out:
core.simulations <- core.loop(
  equiprobable = "Yes",
  basic.constraints = "No",
  years.imply.months = "No",
  months.imply.years = "No",
  no.moon.alone = "No",
  n.simulated.calendars = 2
  )

core.simulations


#2.2. Analysis loop: Compares real vs. generated types ######

analyse.core.loop <- function(  equiprobable,
                                basic.constraints,
                                years.imply.months,
                                months.imply.years,
                                no.moon.alone,
                                n.simulated.calendars,
                                the.real.types,
                                all.possible.types){
  
  true.positives <- c()
  true.negatives <- c()
  false.positives <- c()
  false.negatives <- c()
  accuracy <- c()
  impossible.types.generated <- c()

   df <-  core.loop(
     equiprobable = equiprobable,
      basic.constraints = basic.constraints,
      years.imply.months = years.imply.months,
      months.imply.years = months.imply.years,
      no.moon.alone = no.moon.alone,
      n.simulated.calendars = n.simulated.calendars
    )
   
   simulated.types <- unique(df$calendar.type)
   non.existent.types <- setdiff(all.possible.types, the.real.types)
   not.generated.types <- setdiff(all.possible.types, simulated.types)
   
   true.positives <- length(intersect(simulated.types, the.real.types))
   true.negatives <- length(intersect(non.existent.types, not.generated.types))
   false.positives <- length(intersect(simulated.types, non.existent.types))
   false.negatives  <- length(intersect(not.generated.types, the.real.types))
   accuracy <- 1- sum(false.positives +false.negatives) / length(all.possible.types)
  impossible.types.generated <- length(setdiff(simulated.types, all.possible.types))
  
  results <- as.data.frame(cbind( true.positives,
                                  true.negatives,
                                  false.positives,
                                  false.negatives,
                                  accuracy,
                                  impossible.types.generated))
  
  results
}

#Trying it out
sample.analysis <- analyse.core.loop(
                  equiprobable = "Yes",
                  basic.constraints = "No",
                  years.imply.months = "No",
                  months.imply.years = "No",
                  no.moon.alone = "No",
                  n.simulated.calendars = 2,
                  the.real.types = the.real.types,
                  all.possible.types = all.possible.types)
sample.analysis


#2.3 model calibration loop #########

#This function is to find out how many calendars need to be generated, under a certain model,
#in order to get the real number of unique calendars. 

model.calibration <- function( equiprobable,
                              basic.constraints,
                               years.imply.months,
                               months.imply.years,
                               no.moon.alone,
                               n.iterations, 
                               the.real.types
){

  #Vectors for storing the results
  calendars.n <- c()
  n.distinct.types <- c()

  #This loop generates many simulations,
  #each time generating a variable number of calendars.
  #We count the number of distinct calendar types for each simulation.
  
  for (i in 1:n.iterations){
    calendars.n[i] <- sample(1:n.real.calendars, 1) 

    d <- core.loop(
      equiprobable = equiprobable,
      basic.constraints = basic.constraints,
      years.imply.months = years.imply.months,
      months.imply.years = months.imply.years,
      no.moon.alone = no.moon.alone,
      n.simulated.calendars = calendars.n[i]
    )
    
    n.distinct.types[i] <- length(unique(d$calendar.type))
    
  }

  results <- as.data.frame(cbind(calendars.n,  n.distinct.types ))
  
  #We get the mean number of calendars generates  by the simulations that yielded the right 
  #number of calendar types.
  output <-     mean(results[results$n.distinct.types == length(the.real.types), ]$calendars.n)
  
  output                  
}

#Trying it out
calibration.sample <- model.calibration(  equiprobable = "No",
                                          basic.constraints = "No",
                                          years.imply.months = "No",
                                          months.imply.years = "No",
                                          no.moon.alone = "No", 
                                          n.iterations = 100,
                                          the.real.types = the.real.types)
calibration.sample

#2.4. Figure loop #######

figure.for.one.simulation <- function(
    equiprobable,
    basic.constraints,
    years.imply.months,
    months.imply.years,
    no.moon.alone,
    n.simulated.calendars,
    the.real.types
){

  one.simulation <- core.loop(
    equiprobable = equiprobable,
    basic.constraints = basic.constraints,
    years.imply.months = years.imply.months,
    months.imply.years = months.imply.years,
    no.moon.alone = no.moon.alone,
    n.simulated.calendars = n.simulated.calendars
  )

  
  # Count frequency of each string
  type_counts <- table(one.simulation$calendar.type)
  
  all.the.simulated.types <- unique(one.simulation$calendar.type)
  length(type_counts)
  
  # Sort by frequency (descending)
  sorted_counts <- sort(type_counts, decreasing = TRUE)

  df <- as.data.frame(sorted_counts)
  colnames(df) <- c("BinaryString", "Frequency")
  
  # Add a column indicating if each string is among the real types
  df$Type <- ifelse(df$BinaryString %in% the.real.types, "Real", "Not.real")
  
  #adding an extra dataframe for calendars with frequency 0:
  other.types <- setdiff(all.possible.types, all.the.simulated.types)
  other.types
  
  if(length(other.types) > 0){
  new_rows <- data.frame(
    BinaryString = other.types ,   
    Frequency = 0,
    Type = "Not.real"
  )

  df <- rbind(df, new_rows)
  }
  
  #Replacing the binary strings with labels for better readability
  
  df_labelled <- df %>%
    left_join(labels, by = "BinaryString")
  
  df_labelled <- df_labelled %>%
    mutate(label = reorder(label, -Frequency))
  
  # Plot with color by type
  frequency.plot <- ggplot(df_labelled, aes(x = label, y = Frequency, fill = Type)) +
    geom_bar(stat = "identity") +
    labs(title = "Calendar types generated by 1,000 simulations",
         x = "Calendar type",
         y = "Frequency") +
    scale_fill_manual(values = c("Real" = "darkgreen", "Other" = "Not.real")) +
    theme_minimal()
  
frequency.plot
  
}


figure.for.one.simulation(  
  equiprobable = "Yes",
  basic.constraints= "No",
years.imply.months = "No",
months.imply.years = "No",
no.moon.alone = "No",
n.simulated.calendars = 1000,
the.real.types = the.real.types
)


#2.5. Global function for one model #######

full.loop <-  function(    equiprobable,
                           basic.constraints,
                           years.imply.months,
                           months.imply.years,
                           no.moon.alone,
                           the.real.types,
                           all.possible.types,
                          n.calibration.steps,
                          n.iterations

                           )
  {

  calibration.result <- model.calibration(  equiprobable = equiprobable,
                                            basic.constraints = basic.constraints,
                                            years.imply.months = years.imply.months,
                                            months.imply.years = months.imply.years,
                                            no.moon.alone = no.moon.alone, 
                                            n.iterations = n.calibration.steps,
                                            the.real.types = the.real.types)
  
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  
  for(i in 1: n.iterations){
  
analysis <- analyse.core.loop(
  equiprobable = equiprobable,
  basic.constraints = basic.constraints,
  years.imply.months = years.imply.months,
  months.imply.years = months.imply.years,
  no.moon.alone = no.moon.alone,
  n.simulated.calendars = calibration.result,
    the.real.types = the.real.types,
    all.possible.types = all.possible.types)

  df <- rbind(df, analysis)
  }
  
  df
}
  
#3. Final analysis and figure #####

#Parameter setting: 
n.calibration.steps <- 500
n.iterations <- 2000


equiprobable.simulations <- full.loop (
  equiprobable = "Yes",
  basic.constraints = "No",
  years.imply.months= "No",
  months.imply.years= "No",
  no.moon.alone= "No",
  the.real.types = the.real.types,
  all.possible.types = all.possible.types,
  n.calibration.steps = n.calibration.steps ,
  n.iterations = n.iterations
)

equiprobable.simulations
hist(equiprobable.simulations$accuracy)


null.generative.simulations <- full.loop (
  equiprobable = "No",
  basic.constraints = "No",
  years.imply.months= "No",
  months.imply.years= "No",
  no.moon.alone= "No",
  the.real.types = the.real.types,
  all.possible.types = all.possible.types,
  n.calibration.steps = n.calibration.steps ,
  n.iterations = n.iterations
)

null.generative.simulations
hist(null.generative.simulations$accuracy)


null.constrained.simulations <- full.loop (
  equiprobable = "No",
  basic.constraints = "Yes",
  years.imply.months= "No",
  months.imply.years= "No",
  no.moon.alone= "No",
  the.real.types = the.real.types,
  all.possible.types = all.possible.types,
  n.calibration.steps = n.calibration.steps ,
  n.iterations = n.iterations
)

null.constrained.simulations
hist(null.constrained.simulations$accuracy)

years.months.simulations <- full.loop (
  equiprobable = "No",
  basic.constraints = "No",
  years.imply.months= "Yes",
  months.imply.years= "Yes",
  no.moon.alone= "No",
  the.real.types = the.real.types,
  all.possible.types = all.possible.types,
  n.calibration.steps = n.calibration.steps ,
  n.iterations = n.iterations
)

years.months.simulations
hist(years.months.simulations$accuracy)

no.moon.alone.simulations <- full.loop (
  equiprobable = "No",
  basic.constraints = "No",
  years.imply.months= "No",
  months.imply.years= "No",
  no.moon.alone= "Yes",
  the.real.types = the.real.types,
  all.possible.types = all.possible.types,
  n.calibration.steps = n.calibration.steps ,
  n.iterations = n.iterations
)

no.moon.alone.simulations
hist(no.moon.alone.simulations$accuracy)


#4. Accuracy comparison figure ######

#4.1. Main figure #####

#Getting accuracies for all models

equiprobable.simulations.accuracy <- as.data.frame(equiprobable.simulations$accuracy)
equiprobable.simulations.accuracy$model <- "null model"
colnames(equiprobable.simulations.accuracy) <- c("accuracy", "model")

null.generative.simulations.accuracy <- as.data.frame(null.generative.simulations$accuracy)
null.generative.simulations.accuracy$model <- "generative model"
colnames(null.generative.simulations.accuracy) <- c("accuracy", "model")

null.constrained.simulations.accuracy <- as.data.frame(null.constrained.simulations$accuracy)
null.constrained.simulations.accuracy$model <- "generative model + baselines"
colnames(null.constrained.simulations.accuracy) <- c("accuracy", "model")

years.months.simulations.accuracy <- as.data.frame(years.months.simulations$accuracy)
years.months.simulations.accuracy$model <- "years <=> months"
colnames(years.months.simulations.accuracy) <- c("accuracy", "model")

no.moon.alone.simulations.accuracy <- as.data.frame(no.moon.alone.simulations$accuracy)
no.moon.alone.simulations.accuracy$model <- "no moon alone"
colnames(no.moon.alone.simulations.accuracy) <- c("accuracy", "model")

figure.d <- rbind(equiprobable.simulations.accuracy,
                  null.generative.simulations.accuracy,
                  years.months.simulations.accuracy,
                  null.constrained.simulations.accuracy,
                  no.moon.alone.simulations.accuracy
                  )
#Ordering the models by mean accuracy:
figure.d$model <- fct_reorder(figure.d$model,  figure.d$accuracy , .fun = mean)

p <- ggplot(figure.d, aes(x = accuracy, y = model, fill = model)) +
  geom_density_ridges(
    #THe parameter below determines how smooth the density curves are:
    bandwidth = 0.02,
    alpha = 0.75,
    scale = 1.1,
    color = NA,
    linewidth = 0.4
  ) +
  labs(
    title = "Predicting which calendar types exist or not",
    x = "Model accuracy",
    y = NULL
  )  +
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    axis.text.y = element_text(face = "bold")
  ) 

p

ggsave("fig-qhui2.png", plot = p, width = 7, height = 9, dpi = 300, bg = "white")

#4.2. Same for true positives #####

equiprobable.simulations.TP<- as.data.frame(equiprobable.simulations$true.positives)
equiprobable.simulations.TP$model <- "equiprobable"
colnames(equiprobable.simulations.TP) <- c("true.positives", "model")

null.generative.simulations.TP<- as.data.frame(null.generative.simulations$true.positives)
null.generative.simulations.TP$model <- "generative.null"
colnames(null.generative.simulations.TP) <- c("true.positives", "model")

null.constrained.simulations.TP<- as.data.frame(null.constrained.simulations$true.positives)
null.constrained.simulations.TP$model <- "generative.constrained"
colnames(null.constrained.simulations.TP) <- c("true.positives", "model")

years.months.simulations.TP<- as.data.frame(years.months.simulations$true.positives)
years.months.simulations.TP$model <- "years <=> months"
colnames(years.months.simulations.TP) <- c("true.positives", "model")

no.moon.alone.simulations.TP<- as.data.frame(no.moon.alone.simulations$true.positives)
no.moon.alone.simulations.TP$model <- "no moon alone"
colnames(no.moon.alone.simulations.TP) <- c("true.positives", "model")

figure.d <- rbind(equiprobable.simulations.TP,
                  null.generative.simulations.TP,
                  years.months.simulations.TP,
                  null.constrained.simulations.TP,
                  no.moon.alone.simulations.TP
)

#Ordering the models by mean accuracy:
figure.d$model <- fct_reorder(figure.d$model,  figure.d$true.positives, .fun = mean)

p <- ggplot(figure.d, aes(x = true.positives, y = model, fill = model)) +
  geom_density_ridges(
    #THe parameter below determines how smooth the density curves are:
    bandwidth = 0.1,
    alpha = 0.75,
    scale = 1.1,
    color = NA,
    linewidth = 0.4
  ) +
  labs(
    title = "Predicting which calendar types exist or not",
    x = "True positives",
    y = NULL
  )  +
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold")
  ) 


p


#4.3. Same for true negatives #####

equiprobable.simulations.TN<- as.data.frame(equiprobable.simulations$true.negatives)
equiprobable.simulations.TN$model <- "equiprobable"
colnames(equiprobable.simulations.TN) <- c("true.negatives", "model")

null.generative.simulations.TN<- as.data.frame(null.generative.simulations$true.negatives)
null.generative.simulations.TN$model <- "generative.null"
colnames(null.generative.simulations.TN) <- c("true.negatives", "model")

null.constrained.simulations.TN<- as.data.frame(null.constrained.simulations$true.negatives)
null.constrained.simulations.TN$model <- "generative.constrained"
colnames(null.constrained.simulations.TN) <- c("true.negatives", "model")

years.months.simulations.TN<- as.data.frame(years.months.simulations$true.negatives)
years.months.simulations.TN$model <- "years <=> months"
colnames(years.months.simulations.TN) <- c("true.negatives", "model")

no.moon.alone.simulations.TN<- as.data.frame(no.moon.alone.simulations$true.negatives)
no.moon.alone.simulations.TN$model <- "no moon alone"
colnames(no.moon.alone.simulations.TN) <- c("true.negatives", "model")

figure.d <- rbind(equiprobable.simulations.TN,
                  null.generative.simulations.TN,
                  years.months.simulations.TN,
                  null.constrained.simulations.TN,
                  no.moon.alone.simulations.TN
)

#Ordering the models by mean accuracy:
figure.d$model <- fct_reorder(figure.d$model,  figure.d$true.negatives, .fun = mean)

figure.d


p <- ggplot(figure.d, aes(x = true.negatives, y = model, fill = model)) +
  geom_density_ridges(
    #THe parameter below determines how smooth the density curves are:
    bandwidth = 0.1,
    alpha = 0.75,
    scale = 1.1,
    color = NA,
    linewidth = 0.4
  ) +
  labs(
    title = "Predicting which calendar types exist or not",
    x = "True negatives",
    y = NULL
  )  +
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold")
  ) 


p


#4.4. For false negatives ######


equiprobable.simulations.FN<- as.data.frame(equiprobable.simulations$false.negatives)
equiprobable.simulations.FN$model <- "equiprobable"
colnames(equiprobable.simulations.FN) <- c("false.negatives", "model")

null.generative.simulations.FN<- as.data.frame(null.generative.simulations$false.negatives)
null.generative.simulations.FN$model <- "generative.null"
colnames(null.generative.simulations.FN) <- c("false.negatives", "model")

null.constrained.simulations.FN<- as.data.frame(null.constrained.simulations$false.negatives)
null.constrained.simulations.FN$model <- "generative.constrained"
colnames(null.constrained.simulations.FN) <- c("false.negatives", "model")

years.months.simulations.FN<- as.data.frame(years.months.simulations$false.negatives)
years.months.simulations.FN$model <- "years <=> months"
colnames(years.months.simulations.FN) <- c("false.negatives", "model")

no.moon.alone.simulations.FN<- as.data.frame(no.moon.alone.simulations$false.negatives)
no.moon.alone.simulations.FN$model <- "no moon alone"
colnames(no.moon.alone.simulations.FN) <- c("false.negatives", "model")

figure.d <- rbind(equiprobable.simulations.FN,
                  null.generative.simulations.FN,
                  years.months.simulations.FN,
                  null.constrained.simulations.FN,
                  no.moon.alone.simulations.FN
)

#Ordering the models by mean accuracy:
figure.d$model <- fct_reorder(figure.d$model,  figure.d$false.negatives, .fun = mean, .desc = TRUE)
figure.d


p <- ggplot(figure.d, aes(x = false.negatives, y = model, fill = model)) +
  geom_density_ridges(
    #THe parameter below determines how smooth the density curves are:
    bandwidth = 0.1,
    alpha = 0.75,
    scale = 1.1,
    color = NA,
    linewidth = 0.4
  ) +
  labs(
    title = "Predicting which calendar types exist or not",
    x = "False negatives",
    y = NULL
  )  +
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold")
  ) 


p


#4.4. For false positives ######


equiprobable.simulations.FP<- as.data.frame(equiprobable.simulations$false.positives)
equiprobable.simulations.FP$model <- "equiprobable"
colnames(equiprobable.simulations.FP) <- c("false.positives", "model")

null.generative.simulations.FP<- as.data.frame(null.generative.simulations$false.positives)
null.generative.simulations.FP$model <- "generative.null"
colnames(null.generative.simulations.FP) <- c("false.positives", "model")

null.constrained.simulations.FP<- as.data.frame(null.constrained.simulations$false.positives)
null.constrained.simulations.FP$model <- "generative.constrained"
colnames(null.constrained.simulations.FP) <- c("false.positives", "model")

years.months.simulations.FP<- as.data.frame(years.months.simulations$false.positives)
years.months.simulations.FP$model <- "years <=> months"
colnames(years.months.simulations.FP) <- c("false.positives", "model")

no.moon.alone.simulations.FP<- as.data.frame(no.moon.alone.simulations$false.positives)
no.moon.alone.simulations.FP$model <- "no moon alone"
colnames(no.moon.alone.simulations.FP) <- c("false.positives", "model")

figure.d <- rbind(equiprobable.simulations.FP,
                  null.generative.simulations.FP,
                  years.months.simulations.FP,
                  null.constrained.simulations.FP,
                  no.moon.alone.simulations.FP
)

#Ordering the models by mean accuracy:
figure.d$model <- fct_reorder(figure.d$model,  figure.d$false.positives, .fun = mean, .desc = TRUE)


p <- ggplot(figure.d, aes(x = false.positives, y = model, fill = model)) +
  geom_density_ridges(
    #THe parameter below determines how smooth the density curves are:
    bandwidth = 0.1,
    alpha = 0.75,
    scale = 1.1,
    color = NA,
    linewidth = 0.4
  ) +
  labs(
    title = "Predicting which calendar types exist or not",
    x = "False positives",
    y = NULL
  )  +
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold")
  ) 


p

#5. Figures on types predictions ######

#5.1. Generating the simulations ####

n.simulated.calendars <- 10000

data.for.one.simulation <- function(
    equiprobable,
    basic.constraints,
    years.imply.months,
    months.imply.years,
    no.moon.alone,
    n.simulated.calendars,
    the.real.types
){
  
  one.simulation <- core.loop(
    equiprobable = equiprobable,
    basic.constraints = basic.constraints,
    years.imply.months = years.imply.months,
    months.imply.years = months.imply.years,
    no.moon.alone = no.moon.alone,
    n.simulated.calendars = n.simulated.calendars
  )
  
  # Count frequency of each string
  type_counts <- table(one.simulation$calendar.type)
  
  all.the.simulated.types <- unique(one.simulation$calendar.type)
 #Turn it into percentages:
  
  df <- as.data.frame(type_counts)
  colnames(df) <- c("BinaryString", "Frequency")
  df$Frequency <- 100 * df$Frequency / sum(df$Frequency)

  
  # Add a column indicating if each string is among the real types
  df$Type <- ifelse(df$BinaryString %in% the.real.types, "Real", "Not.real")
  
  #adding an extra dataframe for calendars with frequency 0:
  other.types <- setdiff(all.possible.types, all.the.simulated.types)
  other.types
  
  if(length(other.types) > 0){
    new_rows <- data.frame(
      BinaryString = other.types ,   
      Frequency = 0,
      Type = "Not.real"
    )
    
    df <- rbind(df, new_rows)
  }
  
  #Replacing the binary strings with labels for better readability
  
  df_labelled <- df %>%
    left_join(labels, by = "BinaryString")
  
  df_labelled <- df_labelled %>%
    mutate(label = reorder(label, -Frequency))
  
  df_labelled
}


data.for.one.simulation(  
  equiprobable = "No",
  basic.constraints= "Yes",
  years.imply.months = "No",
  months.imply.years = "No",
  no.moon.alone = "No",
  n.simulated.calendars = 1000,
  the.real.types = the.real.types
)


equiprobable.results <- data.for.one.simulation(  
  equiprobable = "Yes",
  basic.constraints= "No",
  years.imply.months = "No",
  months.imply.years = "No",
  no.moon.alone = "No",
  n.simulated.calendars = 1000,
  the.real.types = the.real.types
)

equiprobable.results$model <- "equiprobable model"


generative.null.results <- data.for.one.simulation(  
  equiprobable = "No",
  basic.constraints= "No",
  years.imply.months = "No",
  months.imply.years = "No",
  no.moon.alone = "No",
  n.simulated.calendars = 1000,
  the.real.types = the.real.types
)

generative.null.results$model <- "null generative model"

generative.constrained.results <- data.for.one.simulation(  
  equiprobable = "No",
  basic.constraints= "Yes",
  years.imply.months = "No",
  months.imply.years = "No",
  no.moon.alone = "No",
  n.simulated.calendars = 1000,
  the.real.types = the.real.types
)
generative.constrained.results$model <- "constrained generative model"

years.months.results <- data.for.one.simulation(  
  equiprobable = "No",
  basic.constraints= "No",
  years.imply.months = "Yes",
  months.imply.years = "Yes",
  no.moon.alone = "No",
  n.simulated.calendars = 1000,
  the.real.types = the.real.types
)
years.months.results$model <- "years <=> months model"


no.moon.results <- data.for.one.simulation(  
  equiprobable = "No",
  basic.constraints= "No",
  years.imply.months = "No",
  months.imply.years = "No",
  no.moon.alone = "Yes",
  n.simulated.calendars = 1000,
  the.real.types = the.real.types
)
no.moon.results$model <- "no moon alone model"

#5.2. Getting real world data #####

head(d)

real.types.count <- d %>%
  count(label)

#Normalising
real.types.count$n <- 100 * real.types.count$n / sum(real.types.count$n)

real.types.count <- real.types.count %>%
  rename(Frequency = n)

#adding binary codes 
real.types.count <- real.types.count %>%
  left_join(labels, by = "label")

real.types.count$Type = "Real"

#Adding non-existent types, with frequency 0

other.types <- setdiff(all.possible.types, real.types.count$BinaryString)
other.types

new_rows <- data.frame(
  BinaryString = other.types ,   
  Frequency = 0,
  Type = "Not.real"
)

new_rows <- new_rows %>%
  left_join(labels, by = "BinaryString")

new_rows <- new_rows %>%
  select(label, Frequency,  BinaryString, Type)
new_rows
real.types.count
real.types.count <- rbind(real.types.count, new_rows)
real.types.count$model = "real data"
real.types.count <- real.types.count %>%
  select(BinaryString,Frequency, Type, label, model)
real.types.count


#5.3. Building the figure #####

figure.data <- rbind(equiprobable.results,
                     generative.null.results,
                     generative.constrained.results,
                     years.months.results,
                     no.moon.results,
                     real.types.count)

#Sorting calendar types alphabetically:
figure.data <- figure.data %>%
  arrange(label)

figure.data$label <- factor(
  figure.data$label,
  levels = sort(unique(as.character(figure.data$label)))
)

#Sorting models in the order I want
figure.data$model <- factor(
  figure.data$model,
  levels = c("real data", "equiprobable model", "null generative model", "constrained generative model", "no moon alone model", "years <=> months model")
)


plot <- ggplot(figure.data,
       aes(
           x = factor(label,
                      levels = rev(c("C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10",
                                 "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20",
                                 "C21", "C22"))),
           y = Frequency,
           fill = Type)) +
  
  geom_col(width = 0.75,
           colour = "white",
           linewidth = 0.2) +
  
  coord_flip() +
  
  facet_wrap(~model,
             nrow = 1,
             scales = "fixed") +
  
  scale_fill_manual(values = c(
    "Real" = "#CC79A7",  
    "Fake" = "#D9D9D9"
  )) +
  
  scale_y_continuous(expand = expansion(mult = c(0, .03))) +
  
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "none",
    
    strip.text = element_text(face = "bold", size = 7),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    
    panel.spacing = unit(1, "lines")
  )
 
plot

ggsave("fig.ksdd.png", plot = plot, width = 9, height = 6, dpi = 300, bg = "white")


#6. Figure: Global map of the cultures in our survey ######

#Requires downloading the societies.csv dataset from D-PLACE.

#6.1. Giving each datapoint a location #####

# D-PLACE dataset for pairing most of the calendars with a location of the society/culture using it (downloaded from https://github.com/D-PLACE/dplace-data/blob/master/datasets/EA/societies.csv)
dplace <- read_csv("societies.csv")

# Removing the numbers after each ID in d.complete
d.complete<- d.complete%>%
  mutate(
    ID_clean = str_to_upper(str_trim(as.character(ID))) %>%
      str_remove("_[A-Z]$")
  )

dplace_clean <- dplace %>%
  mutate(
    ID_clean = str_to_upper(str_trim(
      str_extract(as.character(HRAF_name_ID), "(?<=\\()[^)]+(?=\\))")
    ))
  ) %>%
  filter(!is.na(ID_clean), ID_clean != "") %>%
  arrange(ID_clean) %>%
  distinct(ID_clean, .keep_all = TRUE)

merged <- d.complete%>%
  left_join(
    dplace_clean %>% select(ID_clean, Lat, Long),
    by = "ID_clean"
  )

# Checking which remain unmatched
unmatched <- merged %>% filter(is.na(Lat))
cat("\nUnmatched IDs:\n", paste(sort(unique(unmatched$ID_clean)), collapse = ", "), "\n")
cat("\nunmatched rows:", nrow(unmatched),
    "| distinct unmatched IDs:", n_distinct(unmatched$ID_clean), "\n\n")

# Manually assigning approximate coordinates for ID's not present in the D-PLACE export. Notes on the chosen locations.
manual_coords <- tribble(
  ~ID_clean, ~Lat_manual,  ~Long_manual,  ~note,
  # 
  "RF10",     55.75,        37.62,        "Soviets - Moscow",
  "AE15",     39.90,        116.40,       "Chinese (Qing dynasty) - Beijing",
  "AO07",     13.75,        100.50,       "Thai/Central Thai - Bangkok",
  "EQ01",     64.90,        -19.00,       "Icelanders",
  "MA01",     35.70,        51.40,        "Persians - Tehran",
  "MF07",     31.78,        35.22,        "Ancient Israelites - Jerusalem",
  "MH07",     32.54,        44.42,        "Sumerians/Babylonians - Babylon",
  "NG05",     44.50,        -80.00,       "Huron/Wendat - Georgian Bay, Ontario",
  "NT43",     33.00,        -112.50,      "Maricopa - Gila River, Arizona",
  "NU46",     19.40,        -99.10,       "Nahua - Valley of Mexico",
  "OL07",     -9.10,        152.80,       "Northeastern Massim (Muyuw) - Woodlark Island, PNG",
  "ON26",     -5.62,        154.98,       "Tinputz - Bougainville, PNG",
  "RJ01",     40.18,        44.51,        "Armenians - Yerevan",
  "RV03",     65.00,        150.00,       "Yukaghir - Kolyma, Siberia",
  "X41",       4.50,        38.00,        "Borana - Ethiopia/Kenya border",
  "X51",       5.00,        -70.00,       "Guahibo - Colombia/Venezuela llanos",
  "X52",       9.77,        140.51,       "Fais Islanders - Micronesia",
  "X53",       1.80,        128.00,       "Galela - Halmahera, Indonesia",
  "X54",      18.80,        99.00,        "Northern Thai - Chiang Mai",
  "X1",       25.70,        32.60,        "Ancient Egyptians - Thebes/Nile",
  "X2",       42.00,        12.00,        "Western world - Rome",
  "X3",       21.42,        39.83,        "Muslims - Mecca",
  "X4",       25.32,        83.00,        "Hindus - Varanasi",
  "X5",       29.61,        52.53,        "Baha'is - Iran",  
  "X6",       34.55,        40.89,        "Ancient Amorites - Mari, Syria",
  "X7",       31.78,        35.22,        "Ancient Hasmoneans - Jerusalem",
  "X8",       41.01,        28.98,        "Eastern Orthodox Church - Constantinople",
  "X13",      48.85,         2.35,        "French - Paris",
  "X11",      27.70,        85.30,        "Nepalese - Kathmandu"
)

#Merging the manual coordinates with the others
merged <- merged %>%
  left_join(manual_coords %>% select(-note), by = "ID_clean") %>%
  mutate(
    Lat  = coalesce(Lat, Lat_manual),
    Long = coalesce(Long, Long_manual)
  ) %>%
  select(-Lat_manual, -Long_manual)

# Sanity check
nrow(merged %>% filter(is.na(Lat)))  # should be 0 now


#6.2. World map: Calendars by subsistence types #####

world_map <- map_data("world")

#Desired order for the subsistence types
desired_order_full <- c(
  "Commercial economy",
  "Intensive Agriculturalists",
  "Agro-Pastoralists",
  "Pastoralists",
  "Horticulturalists",
  "Primarily Hunter-gatherers",
  "Hunter-gatherers",
  "Other Subsistence Combinations"
)

merged$Subsistence_type <- factor(merged$Subsistence_type, levels = desired_order_full)

#Assigning colors
manual_colors_full <- c(
  "Commercial economy"             = "#E8A65D",
  "Intensive Agriculturalists"     = "#F0C066",
  "Agro-Pastoralists"              = "#4C9A63",
  "Pastoralists"                   = "#8FCB7E",
  "Horticulturalists"              = "forestgreen",
  "Primarily Hunter-gatherers"     = "#4472C4",
  "Hunter-gatherers"               = "#4472C4",
  "Other Subsistence Combinations" = "black"
)

#Choosing the signs
manual_shapes_full <- c(
  "Commercial economy"             = 15,  # filled square
  "Intensive Agriculturalists"     = 16,  # filled circle
  "Agro-Pastoralists"              = 17,  # filled triangle
  "Pastoralists"                   = 18,  # filled diamond
  "Horticulturalists"              = 1,   # open circle
  "Primarily Hunter-gatherers"     = 7,   # square with X
  "Hunter-gatherers"               = 0,   # open square
  "Other Subsistence Combinations" = 3    # plus sign
)

#Plotting

world_map <- map_data("world") %>%
  filter(region != "Antarctica")

# Changing the map so that it only prints the area with data (with margins)
xr <- range(merged$Long, na.rm = TRUE) + c(-8, 20)
yr <- range(merged$Lat,  na.rm = TRUE) + c(-8, 8)

#Creating the plot
plot_full <- ggplot() +
  geom_map(
    data = world_map, map = world_map,
    aes(x = long, y = lat, map_id = region),
    color = "gray", fill = "lightgray", linewidth = 0.3
  ) +
  geom_point(
    data = merged,
    aes(x = Long, y = Lat, shape = Subsistence_type, color = Subsistence_type),
    size = 4, alpha = 0.85
  ) +
  geom_text_repel(
    data = merged,
    aes(x = Long, y = Lat,
        label = str_wrap(
          str_remove(Calendar_name, "\\s*[Cc]alendar\\s*$"),
          width = 12
        )),
    size = 2.5,
    lineheight = 0.7,
    segment.color = "grey50",
    max.overlaps = Inf,
    min.segment.length = 0,
    box.padding   = 0.4,
    point.padding = 0.3,
    force = 2,
    force_pull = 0.5,
    xlim = c(-Inf, Inf),
    ylim = c(-Inf, Inf),
    max.time = 2,
    max.iter = 50000,
    seed = 4
  ) +
  scale_shape_manual(name = "Subsistence type", values = manual_shapes_full) +
  scale_color_manual(name = "Subsistence type", values = manual_colors_full) +
  coord_fixed(
    ratio = 1.3,
    xlim = xr, ylim = yr,
    expand = FALSE, clip = "off"
  ) +
  ggtitle("") +
  theme_minimal() +
  theme(
    axis.title  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    panel.grid  = element_blank(),
    legend.position   = "bottom",
    legend.box.margin = margin(-10, 0, 0, 0),
    plot.margin = margin(4, 4, 4, 4)
  )

plot_full

ggsave("fig.wjpu.png", plot = plot_full, width = 10, height = 6, dpi = 300, bg = "white")

#6.3. World map: Unit presence #####

unit_cols <- c("Solar_year", "Solar_month",
               "Lunar_year", "Lunar_month", "Lunar_week",
               "Arbitrary_week")

# Rearranging the data so that each row becomes a location + unit combination (each calendar becomes 6 unit rows)
merged_long <- merged %>%
  select(Long, Lat, all_of(unit_cols)) %>%
  mutate(across(all_of(unit_cols),
                ~ as.numeric(str_extract(str_trim(as.character(.x)), "^[01]")))) %>%   # keeps the 0/1 from codes like "0a", "1b"
  pivot_longer(all_of(unit_cols), names_to = "Unit", values_to = "Present") %>%
  mutate(
    Unit    = factor(str_replace_all(Unit, "_", " "),
                     levels = str_replace_all(unit_cols, "_", " ")),
    Present = factor(Present, levels = c(0, 1), labels = c("Absent", "Present"))
  )

#Plotting
world_map <- map_data("world") %>% filter(region != "Antarctica")

plot_units <- ggplot() +
  geom_map(
    data = world_map, map = world_map,
    aes(x = long, y = lat, map_id = region),
    color = "#D8C8AC", fill = "#F5EDE0", linewidth = 0.2
  ) +
  geom_point(
    data = merged_long,
    aes(x = Long, y = Lat, shape = Present, color = Present),
    size = 1.5, alpha = 0.85
  ) +
  scale_color_manual(values = c("Absent" = "#7A7A7A", "Present" = "#C0392B"),
                     na.translate = FALSE) +
  scale_shape_manual(values = c("Absent" = 1, "Present" = 16),
                     na.translate = FALSE) +
  coord_fixed(ratio = 1.8, xlim = c(-190, 190), ylim = c(-60, 83), expand = FALSE) +
  facet_wrap(~Unit, ncol = 3) +
  labs(title = "", shape = NULL, color = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text  = element_text(face = "bold"),
    axis.title  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    panel.grid  = element_blank()
  )

plot_units


#7. Figure: Unit lengths #######

# Re-mporting the data 
d <- read.csv("included_calendars.csv", sep = ",")

# 7.1. Unit length analysis ####

# Dividing each calendar into rows according to the units present (including multiple unit lengths separated by  "|".)
long_form <- function(data, cols) {
  data %>%
    mutate(across(all_of(cols), as.character)) %>%
    mutate(across(all_of(cols), ~ na_if(str_squish(.), "not used"))) %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "variable",
      values_to = "value"
    ) %>%
    separate_longer_delim(value, delim = "|") %>%
    mutate(value = as.numeric(str_squish(value))) %>%
    filter(!is.na(value))
}

# All units pooled (Excluding AW_supercycle_length)
cols_to_pool <- c(
  "SY_length",
  "LY_length",
  "SM_length",
  "LM_length",
  "LW_length",
  "AW_length",
  "AW_extra_length"
)

arbitrary_vars <- c("AW_length", "AW_extra_length")
solar_vars <- c("SY_length", "SM_length")
lunar_vars <- c("LM_length", "LY_length", "LW_length")

d_long <- long_form(d, cols_to_pool) %>%
  mutate(
    color_group = case_when(
      variable %in% arbitrary_vars ~ "Arbitrary_lengths",
      variable %in% solar_vars ~ "Solar_based_lengths",
      variable %in% lunar_vars ~ "Lunar_based_lengths"
    )
  )

# (should be 82 calendars, as 20 are Type 0, i.e. null-calendars)
n_distinct(d_long$Calendar_name)
setdiff(d$Calendar_name, d_long$Calendar_name)   # List of calendars without units (not included in the following)

### 7.2. Graph ####

unit_labels <- c(
  AW_length = "Arbitrary weeks",
  LW_length = "Lunar weeks",
  LM_length = "Lunar months",
  LY_length = "Lunar years",
  SM_length = "Solar months",
  SY_length = "Solar years"
)

unit_colors <- c(
  AW_length = "#0F6E56",
  LW_length = "#4A90E2",
  LM_length = "#4A90E2",
  LY_length = "#4A90E2",
  SM_length = "#F4A300",
  SY_length = "#F4A300"
)

integer_breaks <- function(n = 8, ...) {
  function(x) {
    b <- floor(pretty(x, n, ...))
    unique(b[b >= floor(min(x)) & b <= ceiling(max(x))])
  }
}

make_panel <- function(vars, lo, hi, show_x_title = FALSE) {
  d_long %>%
    mutate(
      variable = if_else(variable == "AW_extra_length", "AW_length", variable)
    ) %>%
    filter(variable %in% vars, value >= lo, value < hi) %>%
    mutate(variable = factor(variable, levels = vars)) %>%
    count(value, variable) %>%
    ggplot(aes(x = value, y = n, fill = variable)) +
    geom_col(width = 0.9, color = "white", linewidth = 0.05) +
    scale_fill_manual(
      values = unit_colors[vars],
      labels = unit_labels[vars],
      breaks = vars
    ) +
    scale_x_continuous(
      breaks = integer_breaks(8),
      labels = scales::label_number(accuracy = 1)
    ) +
    labs(
      x    = if (show_x_title) "Unit length (in days)" else NULL,
      y    = "Count",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x          = element_text(angle = 45, hjust = 1),
      axis.line            = element_line(color = "black"),
      legend.position      = "right",
      legend.justification = "top"
    )
}

p_weeks  <- make_panel(c("AW_length", "LW_length"),   0, 50)
p_months <- make_panel(c("LM_length", "SM_length"),   0, 100)
p_years  <- make_panel(c("LY_length", "SY_length"), 200, 400, show_x_title = TRUE)

lengths_plot <- p_weeks / p_months / p_years +
  plot_annotation(title = "The distribution of unit lengths in days")

ggsave("fig.oerr.png", plot = lengths_plot, width = 5, height = 7, dpi = 300, bg = "white")

lengths_plot
