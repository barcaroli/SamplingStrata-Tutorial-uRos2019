## ---- include = F--------------------------------------------------------
library(SamplingStrata)
data("swissmunicipalities")
data("swisserrors")
data("swissframe")
data("swissstrata")
# load("2. SamplingStrata functions.RData")


## ---- out.width = "5000px", echo = FALSE---------------------------------
knitr::include_graphics("images1/Flow.png")


## ---- eval = T-----------------------------------------------------------
library(SamplingStrata)
data(swissmunicipalities)
# Only three regions
swissmunicipalities <- swissmunicipalities[swissmunicipalities$REG < 4,]


## ---- eval = T-----------------------------------------------------------
swissmunicipalities$id <- c(1:nrow(swissmunicipalities))
swissframe <- buildFrameDF(df = swissmunicipalities,
                           id = "id",
                           X = c("POPTOT",
                                 "Surfacesbois",
                                 "Surfacescult",
                                 "Alp",
                                 "Airbat",
                                 "Airind"),
                           Y = c("Pop020",
                                 "Pop2040",
                                 "Pop4065",
                                 "Pop65P"),
                           domainvalue = "REG")


## ---- eval = T-----------------------------------------------------------
swissframe$X1 <- var.bin(swissmunicipalities$POPTOT, bins=18)
swissframe$X2 <- var.bin(swissmunicipalities$Surfacesbois, bins=3)
swissframe$X3 <- var.bin(swissmunicipalities$Surfacescult, bins=3)
swissframe$X4 <- var.bin(swissmunicipalities$Alp, bins=3)
swissframe$X5 <- var.bin(swissmunicipalities$Airbat, bins=3)
swissframe$X6 <- var.bin(swissmunicipalities$Airind, bins=3)


## ---- eval = T-----------------------------------------------------------
swissmunicipalities$id <- c(1:nrow(swissframe))
newframe <- buildFrameDF(df = swissmunicipalities,
                         id = "id",
                         X = "id",
                         Y = c("Pop020",
                               "Pop2040",
                               "Pop4065",
                               "Pop65P"),
                         domainvalue = "REG")


## ---- eval=T-------------------------------------------------------------
swissstrata <- buildStrataDF(swissframe, progress = TRUE)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
swisserrors <- as.data.frame(list(DOM=rep("DOM1",3),
                         CV1=rep(0.08,3),
                         CV2=rep(0.12,3),
                         CV3=rep(0.08,3),
                         CV4=rep(0.12,3),
                         domainvalue=c(1:3)
))
swisserrors


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
checkInput(errors = swisserrors, 
           strata = swissstrata, 
           sampframe = swissframe)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
cv <- swisserrors[1,]
cv


## ---- eval=TRUE,echo=TRUE------------------------------------------------
allocation <- bethel(swissstrata,cv,printa=T)
sum(allocation)
attributes(allocation)$outcv


## ---- eval = T-----------------------------------------------------------
solution <- optimizeStrata(
	errors = swisserrors, 
	strata = swissstrata,
	iter = 25,
	pops = 10,
	writeFiles = TRUE,
	showPlot = TRUE,
	parallel = FALSE)
sum(ceiling(solution$aggr_strata$SOLUZ))


## ---- eval=T, echo = FALSE-----------------------------------------------
sum(ceiling(solution$aggr_strata$SOLUZ))

## ---- out.width = "1000px", echo = FALSE---------------------------------
knitr::include_graphics("images1/Rplot_optimize.png")


## ---- eval=TRUE, echo=TRUE, warning=FALSE--------------------------------
solutionKmeans <- KmeansSolution(swissstrata,
                                   swisserrors,
                                   nstrata=NA,
                                   minnumstrat=2,
                                   maxclusters=NA,
                                   showPlot=TRUE)


## ---- eval=TRUE, echo=TRUE, warning=FALSE--------------------------------
head(solutionKmeans)


## ---- out.width = "300px", echo = FALSE----------------------------------
knitr::include_graphics("images1/Rplot_kmeans.png",dpi=NA)


## ---- eval=T-------------------------------------------------------------
solution_with_kmeans <- optimizeStrata(
	errors = swisserrors,
	strata = swissstrata,
	iter = 25,
	pops = 10,
	suggestions = solutionKmeans,
	writeFiles = TRUE,
	showPlot = TRUE,
	parallel = FALSE)
sum(ceiling(solution_with_kmeans$aggr_strata$SOLUZ))


## ---- eval=T , echo = FALSE----------------------------------------------
sum(ceiling(solution_with_kmeans$aggr_strata$SOLUZ))

## ---- out.width = "300px", echo = FALSE----------------------------------
knitr::include_graphics("images1/Rplot_optimize_kmeans.png")


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
adjustedStrata <- adjustSize(size=30,
                             strata=solution_with_kmeans$aggr_strata,cens=NULL)
sum(adjustedStrata$SOLUZ)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
adjustedStrata <- adjustSize(size=60,
                             strata=solution_with_kmeans$aggr_strata,cens=NULL)
sum(adjustedStrata$SOLUZ)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
newstrata <- updateStrata(swissstrata, 
                          solution_with_kmeans, 
                          writeFiles = TRUE)
head(newstrata,3)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
strata_aggregation <- read.delim("strata_aggregation.txt")
head(strata_aggregation)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
framenew <- updateFrame(swissframe, 
                        newstrata, 
                        writeFiles=FALSE)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
sample <- selectSample(framenew, 
                       solution_with_kmeans$aggr_strata, 
                       writeFiles = FALSE)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
eval <- evalSolution(framenew, 
                     solution_with_kmeans$aggr_strata, 
                     nsampl=50, 
                     writeFiles=TRUE,
                     progress=FALSE) 


## ---- eval=TRUE, echo=TRUE-----------------------------------------------
eval$coeff_var
swisserrors

