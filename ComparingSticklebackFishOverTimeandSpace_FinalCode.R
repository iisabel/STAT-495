library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(data.table)
getwd()
setwd("Downloads/STAT 495")

########################################################################################################
############################################# BONSALL 2020 #############################################
########################################################################################################
bonsall <- read.csv("Bonsall2020/Bonsall2020.csv")


# Add new variable that is just the distances
df <- bonsall %>%
  mutate(distance = substr(imageID, 10, 13)) %>%
  arrange(by = distance)

# Calculate mm from pixels
df <- df %>%
  mutate(eye.diam.mm = (eye.diam.pix/scale.lateral.30mm.pix) * 10) %>%
  mutate(head.depth.mm = (head.depth.pix/scale.lateral.30mm.pix) * 10) %>%
  mutate(body.depth.mm = (body.depth.pix/scale.lateral.30mm.pix) * 10) %>%
  mutate(second.dorsal.spine.length.mm = (X2nd.dorsal.spine.length.pix/scale.lateral.30mm.pix) * 10) %>%
  mutate(dorsal.fin.length.mm = (dorsal.fin.length.pixels/scale.lateral.30mm.pix) * 10) %>%
  mutate(caudal.peduncle.depth.mm = (caudal.peduncle.depth.pixels/scale.lateral.30mm.pix) * 10)


df2 = df[,!(names(df) %in% c("n", "scale.lateral.30mm.pix", "eye.diam.pix", "head.depth.pix", "body.depth.pix", 
                             "X2nd.dorsal.spine.length.pix","dorsal.fin.length.pixels", "caudal.peduncle.depth.pixels", "X", "X.1"))]
View(df2)
#view(df2) #467 fish in bonsall 2020
########################################################################################################
############################################# BONSALL 2006 #############################################
########################################################################################################

bonsall2006 <- read.csv("Vines2006/Bonsall_Morph_Gen_data.csv")
bonsall2006$distance <-as.numeric(paste(substr(bonsall2006$site, 1, 1), ".", substr(bonsall2006$site, 2, nchar(bonsall2006$site)), sep = ""))
head(bonsall2006)
#view(bonsall2006) #we have 805, paper used 428 (only using fish with dna)

#remove NA values for EDAEXON28
#bonsall2006 <- bonsall2006[complete.cases(bonsall2006$EDAEXON78), ]
#bonsall2006 <- bonsall2006[complete.cases(bonsall2006$P7A09), ]

view(bonsall2006) #Gets to 419 fishes

########################################################################################################
###################################### CAMPBELL AND COURTENAY 2021 #####################################
################################## pectoral fin length and pelvic spine #####################################
########################################################################################################

#### ********
##### 1 Both Campbell and Courtenay data 
  courtcamp2021 <- read_excel("Courtney&Campbell/Courtenay_Campbell_MF2021_Counts_Calipers.xlsx")
  
  courtcamp2021b <- subset(courtcamp2021, select = c(specID, standard.length.mm,	left.pelvic.spine.length.mm,	left.pectoral.fin.insertion.mm))

  #Splitting to courtenay and campbell
  courtenay2021 <- courtcamp2021b[courtcamp2021b$specID %like% "CRY", ]

  campbell2021 <- courtcamp2021b[courtcamp2021b$specID %like% "CAMB", ]

##### 2 Digital Traits for Campbell
  campdig2021 <- read_excel("Courtney&Campbell/Basem.campbell.data.21.xlsx", sheet="digital.data")
  colnames(campdig2021)[colnames(campdig2021) == "2nd.dorsal.spine.length.pix"] ="sec.dorsal.spine.length.pix"
  
  # Calculate mm from pixels
  campdig2021b <- campdig2021 %>%
    mutate(body.depth = (body.depth.pix/scale.lateral.30mm.pix) * 30) %>%
    mutate(second.dorsal.spine.length.mm = (sec.dorsal.spine.length.pix/scale.lateral.30mm.pix) * 30) %>%
    mutate(dorsal.fin.length.mm = (dorsal.fin.length.pixels/scale.lateral.30mm.pix) * 30) %>%
    mutate(caudal.peduncle.depth.mm = (caudal.peduncle.depth.pixels/scale.lateral.30mm.pix) * 30)
  
  campdig2021c <- subset(campdig2021b, select = c(imageID, body.depth,	second.dorsal.spine.length.mm,	dorsal.fin.length.mm,	caudal.peduncle.depth.mm))
  colnames(campdig2021c)[colnames(campdig2021c) == "imageID"] ="specID"  
  campdig2021c$specID=gsub(".JPG","",campdig2021c$specID)

  

##### 3 Digital Traits for Courtenay
  courtdig2021 <- read_excel("Courtney&Campbell/edmond_courtney_data_2021.xlsx", sheet="digital.data")
  colnames(courtdig2021)[colnames(courtdig2021) == "2nd.dorsal.spine.length.pix"] ="sec.dorsal.spine.length.pix"
  
  # Calculate mm from pixels
  courtdig2021b <- courtdig2021 %>%
    mutate(body.depth = (body.depth.pix/scale.lateral.30mm.pix) * 10) %>%
    mutate(second.dorsal.spine.length.mm = (sec.dorsal.spine.length.pix/scale.lateral.30mm.pix) * 10) %>%
    mutate(dorsal.fin.length.mm = (dorsal.fin.length.pixels/scale.lateral.30mm.pix) * 10) %>%
    mutate(caudal.peduncle.depth.mm = (caudal.peduncle.depth.pixels/scale.lateral.30mm.pix) * 10)
  
  courtdig2021c <- subset(courtdig2021b, select = c(imageID, body.depth,	second.dorsal.spine.length.mm,	dorsal.fin.length.mm,	caudal.peduncle.depth.mm))
  colnames(courtdig2021c)[colnames(courtdig2021c) == "imageID"] ="specID"  
 

  ### Merging campbell
  campbell2021_full <- merge(campbell2021, campdig2021c, by = "specID", all.x = TRUE)
  campbell2021_full$distanceID <- paste(substr(campbell2021_full$specID, 9, 11))
  campbell2021_full <- campbell2021_full[!(campbell2021_full$distanceID%in% c("B1_", "B2_")), ]
  campbell2021_full$distance <-if_else(campbell2021_full$distanceID=='0.1',1.55,
                                      if_else(campbell2021_full$distanceID=='0.2',1.65,
                                      if_else(campbell2021_full$distanceID=='0.3',1.74,
                                      if_else(campbell2021_full$distanceID=='0.4',1.84,
                                      if_else(campbell2021_full$distanceID=='0.5',1.94,
                                      if_else(campbell2021_full$distanceID=='0.6',2,
                                      if_else(campbell2021_full$distanceID=='0.7',2.11,
                                      if_else(campbell2021_full$distanceID=='0.8',2.22,0))))))))
                                                                                               

   View(campbell2021_full)  
   
  ### Merging Courtenay
  courtenay2021_full <- merge(courtenay2021, courtdig2021c , by = "specID", all.x = TRUE)
  courtenay2021_full$distanceID <- as.numeric(paste(substr(courtenay2021_full$specID, 8, 9)))
  courtenay2021_full$distance <-if_else(courtenay2021_full$distanceID==1,1.96,
                                        if_else(courtenay2021_full$distanceID==2,1.86,
                                        if_else(courtenay2021_full$distanceID==3,1.68,
                                        if_else(courtenay2021_full$distanceID==4,1.32,
                                        if_else(courtenay2021_full$distanceID==5,1.07,
                                        if_else(courtenay2021_full$distanceID==6,0.71,
                                        if_else(courtenay2021_full$distanceID==7,0.55,
                                        if_else(courtenay2021_full$distanceID==9,0.13,0))))))))
                                                                                                                                      
   View(courtenay2021_full)

   
### Trait Loop Call Setup

mytraityearloc<-c('caudped2006bons','caudped2020bons'
                  ,'pelspine2006bons','pelspine2020bons'
                  ,'secdorsspine2006bons','secdorsspine2020bons'
                  ,'dorsfin2006bons','dorsfin2020bons'
                  ,'pectfin2006bons','pectfin2020bons'
                  ,'caudped2021camp','pelspine2021camp','secdorsspine2021camp','dorsfin2021camp','pectfin2021camp'
                  ,'caudped2021cry','pelspine2021cry','secdorsspine2021cry','dorsfin2021cry','pectfin2021cry'
  
)

for (i in mytraityearloc)
{
  if (i == 'pectfin2020bons') {
    subset<- df2 %>% select(imageID, standard.length.mm, pectoral.fin.length.mm, distance)
    
  } else if (i == 'dorsfin2020bons') {
    subset<- df2 %>% select(imageID, standard.length.mm, dorsal.fin.length.mm, distance)
  } else if (i == 'caudped2020bons') {
    subset<- df2 %>% select(imageID, standard.length.mm, caudal.peduncle.depth.mm, distance)
  } else if (i == 'pelspine2020bons') {
    subset<- df2 %>% select(imageID, standard.length.mm, left.pelvicspine.length.mm, distance)
  } else if (i == 'secdorsspine2020bons') {
    subset<- df2 %>% select(imageID, standard.length.mm, second.dorsal.spine.length.mm, distance)
  } else if (i == 'pectfin2006bons') {
    subset<- bonsall2006 %>% select(individual, body.length, pectoral.fin.length, distance)
  } else if (i == 'dorsfin2006bons') {
    subset<- bonsall2006 %>% select(individual, body.length, dorsal.fin, distance)
  } else if (i == 'caudped2006bons') {
    subset<- bonsall2006 %>% select(individual, body.length, caudal.peduncle.height, distance)
  } else if (i == 'pelspine2006bons') {
    subset<- bonsall2006 %>% select(individual, body.length, pelvic.spine, distance)
  } else if (i == 'secdorsspine2006bons') {
    subset<- bonsall2006 %>% select(individual, body.length, second.dorsal.spine, distance)
  } 
    else if (i == 'pectfin2021cry') {
    subset<- courtenay2021_full %>% select(specID, standard.length.mm, left.pectoral.fin.insertion.mm, distance)
  } else if (i == 'dorsfin2021cry') {
      subset<- courtenay2021_full %>% select(specID, standard.length.mm, dorsal.fin.length.mm, distance)
  } else if (i == 'caudped2021cry') {
    subset<- courtenay2021_full %>% select(specID, standard.length.mm, caudal.peduncle.depth.mm, distance)
  } else if (i == 'pelspine2021cry') {
    subset<- courtenay2021_full %>% select(specID, standard.length.mm, left.pelvic.spine.length.mm, distance)
  }  else if (i == 'secdorsspine2021cry') {
    subset<- courtenay2021_full %>% select(specID, standard.length.mm, second.dorsal.spine.length.mm, distance)
  } 
    else if (i == 'pectfin2021camp') {
    subset<- campbell2021_full %>% select(specID, standard.length.mm, left.pectoral.fin.insertion.mm, distance)
  } else if (i == 'dorsfin2021camp') {
    subset<- campbell2021_full %>% select(specID, standard.length.mm, dorsal.fin.length.mm, distance)
  } else if (i == 'caudped2021camp') {
    subset<- campbell2021_full %>% select(specID, standard.length.mm, caudal.peduncle.depth.mm, distance)
  } else if (i == 'pelspine2021camp') {
    subset<- campbell2021_full %>% select(specID, standard.length.mm, left.pelvic.spine.length.mm, distance)
  }  else if (i == 'secdorsspine2021camp') {
    subset<- campbell2021_full %>% select(specID, standard.length.mm, second.dorsal.spine.length.mm, distance)
  }   


  colnames(subset)[2] ="body.length"  
  colnames(subset)[3] ="trait"
  
  
  # Remove 'X' in trait column
  if (i == 'pectfin2020bons' || i == 'secdorsspine2020bons') {
    subset <- subset[!grepl('X',subset$trait),]
  }
  # Set standard length and trait as numerical values, not characters
  subset$body.length <- as.numeric(subset$body.length)
  subset$trait <- as.numeric(subset$trait)
  
  # Remove NA's from the dataset
  subset <-subset[!(is.na(subset$body.length)), ]
  subset <-subset[!(is.na(subset$trait)), ]
  
  #Title Variable setup for graphs:
  if (i %in% c('pectfin2020bons','dorsfin2020bons','caudped2020bons','pelspine2020bons','secdorsspine2020bons','pectfin2006bons', 'dorsfin2006bons','caudped2006bons','pelspine2006bons','secdorsspine2006bons')) {
    mylocation='Bonsall Creek'
  } else if (i %in% c('pectfin2021cry', 'dorsfin2021cry','caudped2021cry','pelspine2021cry','secdorsspine2021cry')){
    mylocation='Courtenay River' 
  } else if (i %in% c('pectfin2021camp', 'dorsfin2021camp','caudped2021camp','pelspine2021camp','secdorsspine2021camp')){
    mylocation='Campbell River'         
  }
                                       
                                       
  
  
  if (i %in% c('pectfin2020bons','dorsfin2020bons','caudped2020bons','pelspine2020bons','secdorsspine2020bons')) {
    myyear='2020'
  } else if (i %in% c('pectfin2006bons', 'dorsfin2006bons','caudped2006bons','pelspine2006bons','secdorsspine2006bons')) {
    myyear='2006'
  } else if (i %in% c('pectfin2021cry', 'dorsfin2021cry','caudped2021cry','pelspine2021cry','secdorsspine2021cry',
                      'pectfin2021camp', 'dorsfin2021camp','caudped2021camp','pelspine2021camp','secdorsspine2021camp')) {
    myyear='2021'
  } 
  if (i %in% c('pectfin2020bons','pectfin2006bons','pectfin2021cry','pectfin2021camp')) {
    myfulltrait='Pectoral Fin Length'
  } else if (i %in% c( 'dorsfin2020bons','dorsfin2006bons','dorsfin2021cry','dorsfin2021camp')) {
    myfulltrait='Dorsal Fin Length'
  } else if (i %in% c('caudped2020bons', 'caudped2006bons','caudped2021cry','caudped2021camp')) {
    myfulltrait='Caudal Penducle Depth'
  } else if (i %in% c('pelspine2020bons','pelspine2006bons','pelspine2021cry','pelspine2021camp')) {
    myfulltrait='Pelvic Spine Length'
  } else if (i %in% c( 'secdorsspine2020bons','secdorsspine2006bons','secdorsspine2021cry','secdorsspine2021camp')) {
    myfulltrait='Second Dorsal Spine Length'
  }
  
 
  # Standard length and trait regressed before outliers removed
  ###plot(subset$body.length,subset$trait)+title(paste(mylocation, myyear,'\n Standard Length vs',myfulltrait,'\n With Outliers'))
  
  # 3 standard deviations calculation and medians for each variable
  trait.stdev <- 3 * sd(subset$trait)
  length.stdev <- 3 * sd(subset$body.length)
  median.length <- median(subset$body.length)
  median.trait <- median(subset$trait)
  
  # Upper and lower bounds for values (to get rid of outliers)
  upper.trait <- median.trait + trait.stdev
  lower.trait <- median.trait - trait.stdev
  if (lower.trait < 0) {lower.trait = 0}
  
  upper.length <- median.length + length.stdev
  lower.length <- median.length - length.stdev
 
  # Remove outliers that are 3 standard deviations away from the median
  subset <-subset[subset$body.length > lower.length, ]
  subset <-subset[subset$body.length < upper.length, ]
  subset <-subset[subset$trait > lower.trait, ]
  subset <-subset[subset$trait < upper.trait, ]
  
  # Model with trait ~ standard length to get residuals
  trait.model <- lm(trait ~ body.length, data =subset)
  trait.residuals <- trait.model$residuals
  
  # Standard length and trait regressed AFTER outliers removed
  ### plot(subset$body.length,subset$trait)+title(paste(mylocation, myyear,'\n Standard Length vs',myfulltrait,'\n With No Outliers'))
  
  
  ###abline(trait.model, col = "red")
  
  # Add residuals to data frame
  subset$residuals <- trait.residuals
  
  subset$distance <-as.numeric(subset$distance)
  
  # SPLINE Plot 
  plot(subset$distance,subset$residuals,xlab = "Distance from Estuary", ylab = "Residual")+title(paste(myfulltrait,'Spline\n' ,mylocation, myyear))
  spline2= smooth.spline(subset$distance,subset$residuals,df=5) 
  lines(spline2,col="blue",lwd=4)
  
    
  ### Model Plots
  if (i %in% c('pectfin2006bons', 'dorsfin2006bons', 'pelspine2006bons', 'secdorsspine2006bons','secdorsspine2020bons' )) {
     
    #Trying nls models
    print(paste(mylocation,'-', myyear,'-',myfulltrait))
    fit<- nls(residuals ~SSlogis(distance,Asym,xmid,scal), data=subset)
    print(summary(fit))
    
    plot(subset$distance,subset$residuals,xlab = "Distance from Estuary", ylab = "Residual")+title(paste(myfulltrait,'Model\n' ,mylocation, myyear))
    lines(subset$distance,predict(fit),col="red",lwd=4)
    if (i == 'pectfin2006bons') {
      #points(3.4787,-0.42,col="red", lwd=8)
      points(3.5383 ,-0.38,col="red", lwd=8)
     
    } else if (i == 'pectfin2020bons') {
      
      points(2.21917 ,0.17,col="red", lwd=8)
      
    } else if (i == 'dorsfin2006bons') {
      #points(2.69753,-0.22,col="red", lwd=8)
      points(2.21346 ,0.2,col="red", lwd=8)
    } else if (i == 'pelspine2006bons') {
      #points(2.73680,-0.34,col="red", lwd=8)
      points(2.67697,-0.25,col="red", lwd=8)
    } else if (i == 'secdorsspine2006bons') {
      #points(2.66065,-0.19,col="red", lwd=8)
      points(2.15755,0.19,col="red", lwd=8)
    } else if (i == 'secdorsspine2020bons') {
      #points(1.9779 ,0.21,col="red", lwd=8)
      points(1.9779 ,0.21,col="red", lwd=8)
    }
    
  }
  ### Saving second dorsal spline 2006 and 2020 datasets for ANOVA below
  if ( i %in% c('secdorsspine2006bons')) {
    data2006 <- subset
    data2006$dumA <- 1
    data2006$dumB <- 0
  } else if ( i %in% c('secdorsspine2020bons')) {
    data2020 <- subset
    data2020$dumA <- 0
    data2020$dumB <- 1
  }
  
}

#### Comparing Parameters by ANOVA
    fit1 <- nls(residuals ~ SSlogis(distance, Asym, xmid, scal), data = data2006, control = nls.control(maxiter = 100))
    summary(fit1)
    
    fit2 <- nls(residuals ~ SSlogis(distance, Asym, xmid, scal), data = data2020, control = nls.control(maxiter = 100))
    summary(fit2)
    
    # renaming a column in data2020 so the column names match
    data2020$individual<- data2020$imageID
    data2020 = subset(data2020, select = -c(imageID) )
    
    data <- rbind(data2006, data2020)
    
    
    ### FULL MODEL ###
    LL3modA <- function(x,th1A,th1B,th2A,th2B,th3A,th3B) {
      th1 <- th1A*data$dumA+th1B*data$dumB
      th2 <- th2A*data$dumA+th2B*data$dumB
      th3 <- th3A*data$dumA+th3B*data$dumB
      th1/(1+exp((th2-x)/th3))
    }
    
    resA <- nls(residuals ~ LL3modA(distance,th1A,th1B,th2A,th2B,th3A,th3B),
                data=data,
                start = list(th1A=summary(fit1)$coefficients[,1][1], th1B=summary(fit2)$coefficients[,1][1],
                             th2A=summary(fit1)$coefficients[,1][2], th2B= summary(fit2)$coefficients[,1][2],
                             th3A=summary(fit1)$coefficients[,1][3],th3B=summary(fit2)$coefficients[,1][3]))
    
    summary(resA)
    
    ## CHECK FOR SAME CENTER ##
    
    LL3modB <- function(x,th1A,th1B,th2,th3A, th3B) {
      th1 <- th1A*data$dumA+th1B*data$dumB
      th3 <- th3A*data$dumA+th3B*data$dumB
      th1/(1+exp((th2-x)/th3))
    }
    resB <- nls(residuals ~ LL3modB(distance,th1A,th1B,th2,th3A, th3B),
                data=data,
                start = list(th1A=summary(fit1)$coefficients[,1][1], th1B=summary(fit2)$coefficients[,1][1],
                             th2 = (summary(fit1)$coefficients[,1][2]+summary(fit2)$coefficients[,1][2])/2,
                             th3A=summary(fit1)$coefficients[,1][3],th3B=summary(fit2)$coefficients[,1][3]))
    summary(resB)
    anova(resB, resA)
    
    
    ## CHECK FOR SAME SLOPE ##
    
    LL3modC <- function(x,th1A,th1B,th2A,th2B,th3) {
      th1 <- th1A*data$dumA+th1B*data$dumB
      th2 <- th2A*data$dumA+th2B*data$dumB
      th1/(1+exp((th2-x)/th3))
    }
    resC <- nls(residuals ~ LL3modC(distance,th1A,th1B,th2A,th2B,th3),
                data=data,
                start = list(th1A=summary(fit1)$coefficients[,1][1], th1B=summary(fit2)$coefficients[,1][1],
                             th2A=summary(fit1)$coefficients[,1][2], th2B= summary(fit2)$coefficients[,1][2],
                             th3=(summary(fit1)$coefficients[,1][3]+ summary(fit2)$coefficients[,1][3])/2))
    summary(resC)
    anova(resC, resA)
    

#Notes:
#We have 805 fish observed in bonsall 2006, paper used 428 (only using fish with dna)
#So we need to do the same with 2020 data, or do those all have dna?






