## ---- include = F--------------------------------------------------------
library(SamplingStrata)
data("swissmunicipalities")
data("swisserrors")
data("swissframe")
data("swissstrata")
# load("2. SamplingStrata functions.RData")


## ---- eval = T-----------------------------------------------------------
data("swissmunicipalities")
swissmunicipalities$id <- c(1:nrow(swissmunicipalities))
swissmunicipalities$dom <- 1
frame <- buildFrameDF(swissmunicipalities,
                      id = "id",
                      domainvalue = "REG",
                      X = c("Surfacesbois","Surfacescult"),
                      Y = c("Pop020", "Pop2040")
)
# choice of units to be selected in any case (census units)
framecens <- frame[frame$X1 > 2500 
                   | frame$X2 > 1200,]
# remaining units 
framesamp <- frame[!(frame$id %in% framecens$id),]



## ---- eval = T-----------------------------------------------------------
cv <- NULL
cv$DOM <- "DOM1"
cv$CV1 <- 0.1
cv$CV2 <- 0.1
cv <- as.data.frame(cv)
cv <- cv[rep(row.names(cv),7),]
cv$domainvalue <- c(1:7)
cv


## ---- eval = T-----------------------------------------------------------
solution <- optimizeStrata2 (
  errors = cv, 
  framesamp = framesamp,
  framecens = framecens, 
  strcens = TRUE, 
  alldomains = FALSE,
  dom = 4,
  iter = 25,
  pops = 20,
  nStrata = 5,
  writeFiles = FALSE,
  showPlot = FALSE,
  parallel = FALSE
)


## ---- eval = T, echo=TRUE------------------------------------------------
sum(round(solution$aggr_strata$SOLUZ))
expected_CV(solution$aggr_strata)


## ---- include=TRUE-------------------------------------------------------
framenew <- solution$framenew
table(framenew$LABEL)


## ---- eval = T,echo=TRUE-------------------------------------------------
strataStructure <- summaryStrata(solution$framenew,solution$aggr_strata,progress=FALSE)
strataStructure


## ---- eval = T,echo=TRUE-------------------------------------------------
outstrata <- plotStrata2d(
                  solution$framenew, 
                  solution$aggr_strata,
                  domain = 4, 
                  vars = c("X1","X2"),
                  labels =     c("Surfacesbois","Surfacescult")
                  )


## ---- eval = T,echo=TRUE-------------------------------------------------
outstrata


## ---- eval = T,echo=TRUE-------------------------------------------------
samp <- selectSample(solution$framenew,solution$aggr_strata)

