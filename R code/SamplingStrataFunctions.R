############################
# SamplingStrata functions #
############################

library(SamplingStrata)

#------------------------------------------------------------------------
# Get data on Swiss municipalities
data(swissmunicipalities)


# Only first two regions
swissmunicipalities <- swissmunicipalities[swissmunicipalities$REG < 3,]
# add a unique identifier
swissmunicipalities$id <- c(1:nrow(swissmunicipalities))


#------------------------------------------------------------------------

# Build sampling frame

swissframe <- buildFrameDF(df = swissmunicipalities,
                           id = "id",
                           X = c("Surfacesbois",
                                 "Surfacescult"),
                           Y = c("Pop020",
                                 "Pop2040"),
                           domainvalue = "REG")


# Categorize the two stratification variables
swissframe$X1 <- var.bin(swissframe$X1, bins=15)
swissframe$X2 <- var.bin(swissframe$X2, bins=15)
table(swissframe$X1)
table(swissframe$X2)

summary(swissframe)

#------------------------------------------------------------------------

# Build atomic strata

swissstrata <- buildStrataDF(swissframe, progress = TRUE)
head(swissstrata)

# Define precision constraints

swisserrors <- as.data.frame(list(DOM=rep("DOM1",2),
                                  CV1=c(0.08,0.10),
                                  CV2=c(0.12,0.15),
                                  domainvalue=c(1:2)
                            ))

# swisserrors <- swisserrors[1,]


# check input data

checkInput(errors = swisserrors, 
           strata = swissstrata, 
           sampframe = swissframe)


#########################################################################

# Optimization I (on categorical stratification variables)

set.seed(123)
solution1 <- optimizeStrata(
  errors = swisserrors, 
  strata = swissstrata,
  iter = 50,
  pops = 10,
  writeFiles = TRUE,
  showPlot = TRUE,
  parallel = FALSE)
sum(ceiling(solution1$aggr_strata$SOLUZ))
expected_CV(solution1$aggr_strata)
swisserrors

# adjust sample size
# ... decreasing it
adjustedStrataLow <- adjustSize(size=150,
                             strata=solution1$aggr_strata,cens=NULL)
sum(adjustedStrataLow$SOLUZ)
expected_CV(adjustedStrataLow)


# ... increasing it
adjustedStrataHigh <- adjustSize(size=350,
                             strata=solution1$aggr_strata,cens=NULL)
sum(adjustedStrataHigh$SOLUZ)
expected_CV(adjustedStrataHigh)
swisserrors

# update the frame and the strata

newstrata <- updateStrata(swissstrata, 
                          solution1, 
                          writeFiles = TRUE)
head(newstrata,3)

strata_aggregation <- read.delim("strata_aggregation.txt")
head(strata_aggregation)

framenew <- updateFrame(swissframe, 
                        newstrata, 
                        writeFiles=FALSE)


# select the sample
sample <- selectSample(framenew, 
                       solution1$aggr_strata, 
                       writeFiles = FALSE)


# evaluation by simulation
set.seed(123)
eval <- evalSolution(framenew, 
                     solution1$aggr_strata, 
                     nsampl=50, 
                     writeFiles=TRUE,
                     progress=TRUE) 
eval$coeff_var
expected_CV(adjustedStrata)

#########################################################################

# Optimization II (on continuous stratification variables)


swissframe <- buildFrameDF(df = swissmunicipalities,
                           id = "id",
                           X = c("Surfacesbois",
                                 "Surfacescult"),
                           Y = c("Pop020",
                                 "Pop2040"),
                           domainvalue = "REG")
set.seed(123)
solution2 <- optimizeStrata2 (
  errors = swisserrors, 
  framesamp = swissframe,
  iter = 50,
  pops = 10,
  nStrata = 10,
  writeFiles = FALSE,
  showPlot = TRUE,
  parallel = FALSE
)

sum(round(solution2$aggr_strata$SOLUZ))
expected_CV(solution2$aggr_strata)
swisserrors

# evaluation

framenew <- solution2$framenew
table(framenew$DOMAINVALUE,framenew$LABEL)


strataStructure <- summaryStrata(solution2$framenew,
                                 solution2$aggr_strata,
                                 writeFiles=TRUE,
                                 progress=FALSE)
strataStructure


# plot strata
outstrata <- plotStrata2d(
  solution2$framenew, 
  solution2$aggr_strata,
  domain = 1, 
  vars = c("X1","X2"),
  labels =     c("Surfacesbois","Surfacescult")
)

outstrata


# selection of sample

samp <- selectSample(solution$framenew,solution$aggr_strata)

#########################################################################

# Models

data("swissmunicipalities")

hist(swissmunicipalities$Pop020)
hist(log(swissmunicipalities$Pop020))
hist(swissmunicipalities$Airbat)
hist(log(swissmunicipalities$Airbat))

#-----------------------------------
mun_samp <- swissmunicipalities[sample(1:nrow(swissmunicipalities),500),]

# fit linear model on Pop020
mod_Pop_020_Airbat <- lm(log(Pop020) ~ log(Airbat), data=mun_samp)
summary(mod_Pop_020_Airbat)


# fit linear model on Pop2040
mod_Pop_2040_Airbat <- lm(log(Pop2040) ~ log(Airbat), data=mun_samp)
summary(mod_Pop_2040_Airbat)

#-----------------------------------

# build model dataframe

model <- NULL
model$beta[1] <- mod_Pop_020_Airbat$coefficients[2]
model$sig2[1] <- summary(mod_Pop_020_Airbat)$sigma
model$type[1] <- "loglinear"
model$gamma[1] <- 0
model$beta[2] <- mod_Pop_2040_Airbat$coefficients[2]
model$sig2[2] <- summary(mod_Pop_2040_Airbat)$sigma
model$type[2] <- "loglinear"
model$gamma[2] <- 0
model <- as.data.frame(model)
model

#-----------------------------------

# RUN WITHOUT TAKING INTO ACCOUNT THE MODEL

# 
swissmunicipalities$id <- c(1:nrow(swissmunicipalities))
swissmunicipalities$dom <- 1

swisserrors <- as.data.frame(list(DOM="DOM1",
                                  CV1=0.08,
                                  CV2=0.12,
                                  domainvalue=1
))

swisserrors
swissframe <- buildFrameDF(df = swissmunicipalities,
                           id = "id",
                           X = c("Surfacesbois",
                                 "Surfacescult"),
                           Y = c("Airbat",
                                 "Airbat"),
                           domainvalue = "dom")
swissframe$X1 <- var.bin(swissframe$X1, bins=15)
swissframe$X2 <- var.bin(swissframe$X2, bins=15)

#-----------------------------------

swissstrata1 <- buildStrataDF(swissframe)

set.seed(123)
solution1 <- optimizeStrata(
  errors = swisserrors, 
  strata = swissstrata1,
  iter = 50,
  pops = 10,
  writeFiles = TRUE,
  showPlot = TRUE,
  parallel = FALSE)

sum(round(solution1$aggr_strata$SOLUZ))
expected_CV(solution1$aggr_strata)
swisserrors

# evaluation of precision on real target variables

newstrata <- updateStrata(swissstrata1,solution1)
framenew1 <- updateFrame(swissframe,newstrata)
framenew1 <- framenew1[order(framenew2$ID),]
swissmunicipalities <- swissmunicipalities[order(swissmunicipalities$id),]

frame_pop <- framenew1
frame_pop$Y1 <- swissmunicipalities$Airbat
frame_pop$Y2 <- swissmunicipalities$Pop020
frame_pop$Y3 <- swissmunicipalities$Pop2040
eval1 <- evalSolution(frame_pop, 
                     solution1$aggr_strata, 
                     nsampl=50, 
                     writeFiles=TRUE,
                     progress=TRUE) 
eval1$coeff_var

#-----------------------------------

# RUN TAKING INTO ACCOUNT THE MODEL

# build the atomic strata TAKING INTO ACCOUNT THE MODEL

swissstrata2 <- buildStrataDF(swissframe, model = model, progress = TRUE)

set.seed(123)
solution2 <- optimizeStrata(
  errors = swisserrors, 
  strata = swissstrata2,
  iter = 50,
  pops = 10,
  writeFiles = TRUE,
  showPlot = TRUE,
  parallel = FALSE)

sum(round(solution2$aggr_strata$SOLUZ))
expected_CV(solution2$aggr_strata)
swisserrors

newstrata <- updateStrata(swissstrata2,solution2)
framenew2 <- updateFrame(swissframe,newstrata)
framenew2 <- framenew2[order(framenew2$ID),]
swissmunicipalities <- swissmunicipalities[order(swissmunicipalities$id),]
# evaluation of precision on real target variables

frame_pop <- framenew2
frame_pop$Y1 <- swissmunicipalities$Airbat
frame_pop$Y2 <- swissmunicipalities$Pop020
frame_pop$Y3 <- swissmunicipalities$Pop2040
eval2 <- evalSolution(frame_pop, 
                     solution2$aggr_strata, 
                     nsampl=100, 
                     writeFiles=TRUE,
                     progress=FALSE) 
eval2$coeff_var
swisserrors
