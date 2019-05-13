## ---- include = F--------------------------------------------------------
library(SamplingStrata)
data("swissmunicipalities")
swissmunicipalities$id <- c(1:nrow(swissmunicipalities))
swissmunicipalities$dom <- 1
frame <- buildFrameDF(swissmunicipalities,
                      id = "id",
                      domainvalue = "REG",
                      X = c("Surfacesbois","Surfacescult"),
                      Y = c("H00PTOT", "H00PTOT")
)


## ---- eval = T-----------------------------------------------------------
mod_Pop_020_H00PTOT <- lm(Pop020 ~ H00PTOT, data=swissmunicipalities)
summary(mod_Pop_020_H00PTOT)


## ---- eval = T-----------------------------------------------------------
mod_Pop_2040_H00PTOT <- lm(Pop2040 ~ H00PTOT, data=swissmunicipalities)
summary(mod_Pop_2040_H00PTOT)


## ---- eval = T-----------------------------------------------------------
model <- NULL
model$beta[1] <- mod_Pop_020_H00PTOT$coefficients[2]
model$sig2[1] <- summary(mod_Pop_020_H00PTOT)$sigma
model$type[1] <- "linear"
model$gamma[1] <- 0
model$beta[2] <- mod_Pop_2040_H00PTOT$coefficients[2]
model$sig2[2] <- summary(mod_Pop_2040_H00PTOT)$sigma
model$type[2] <- "linear"
model$gamma[2] <- 0
model <- as.data.frame(model)
model


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
  framesamp = frame, 
  model = model,
  alldomains = FALSE,
  dom = 4,
  iter = 25,
  pops = 20,
  nStrata = 5,
  writeFiles = FALSE,
  showPlot = TRUE,
  parallel = FALSE
)


## ---- eval = T, echo=TRUE------------------------------------------------
sum(round(solution$aggr_strata$SOLUZ))
expected_CV(solution$aggr_strata)

