FindOutlier <- function(x, t3 = 3, tH = 3, tb = 1.5){
  threeLims <- ThreeSigma(x, t = t3)
  HampLims <- Hampel(x, t = tH)
  boxLims <- BoxplotRule(x, t = tb)
  n <- length(x)
  nMiss <- length(which(is.na(x)))
  threeList <- ExtractDetails(x, threeLims$down, threeLims$up)
  HampList <- ExtractDetails(x, HampLims$down, HampLims$up)
  boxList <- ExtractDetails(x, boxLims$down, boxLims$up)
  sumFrame <- data.frame(method = "ThreeSigma", n = n,
                         nMiss = nMiss, nOut = threeList$nOut,
                         lowLim = threeList$lowLim,
                         upLim = threeList$upLim,
                         minNom = threeList$minNom,
                         maxNom = threeList$maxNom)
  upFrame <- data.frame(method = "Hampel", n = n,
                        nMiss = nMiss, nOut = HampList$nOut,
                        lowLim = HampList$lowLim,
                        upLim = HampList$upLim,
                        minNom = HampList$minNom,
                        maxNom = HampList$maxNom)
  sumFrame <- rbind.data.frame(sumFrame, upFrame)
  upFrame <- data.frame(method = "BoxplotRule", n = n,
                        nMiss = nMiss, nOut = boxList$nOut,
                        lowLim = boxList$lowLim,
                        upLim = boxList$upLim,
                        minNom = boxList$minNom,
                        maxNom = boxList$maxNom)
  sumFrame <- rbind.data.frame(sumFrame, upFrame)
  threeFrame <- data.frame(index = threeList$index,
                           values = threeList$values,
                           type = threeList$outClass)
  HampFrame <- data.frame(index = HampList$index,
                          values = HampList$values,
                          type = HampList$outClass)
  boxFrame <- data.frame(index = boxList$index,
                         values = boxList$values,
                         type = boxList$outClass)
  outList <- list(summary = sumFrame, threeSigma = threeFrame,
                  Hampel = HampFrame, boxplotRule = boxFrame)
  return(outList)
}
ThreeSigma <- function(x, t = 3){
  mu <- median(x, na.rm = TRUE)
  sig <- mad(x, na.rm = TRUE)
  if (sig == 0){
    message("Hampel identifer implosion: MAD scale estimate is zero")}
  up <- mu + t * sig
  down <- mu - t * sig
  out <- list(up = up, down = down)
  return(out)
}
Hampel <- function(x, t = 3){
  mu <- median(x, na.rm = TRUE)
  sig <- mad(x, na.rm = TRUE)
  if (sig == 0){
    message("Hampel identifer implosion: MAD scale estimate is zero")
    }
  up <- mu + t * sig
  down <- mu - t * sig
  out <- list(up = up, down = down)
  return(out)
}
BoxplotRule <- function(x, t = 1.5){
  xL <- quantile(x, na.rm = TRUE, probs = 0.25, names = FALSE)
  xU <- quantile(x, na.rm = TRUE, probs = 0.75, names = FALSE)
  Q <- xU - xL
  if (Q == 0){
    message("Boxplot rule implosion: interquartile distance is zero")}
  up <- xU + t * Q
  down <- xU - t * Q
  out <- list(up = up, down = down)
  return(out)
}
ExtractDetails <- function(x, down, up){
  outClass <- rep("N", length(x))
  indexLo <- which(x < down)
  indexHi <- which(x > up)
  outClass[indexLo] <- "L"
  outClass[indexHi] <- "U"
  index <- union(indexLo, indexHi)
  values <- x[index]
  outClass <- outClass[index]
  nOut <- length(index)
  maxNom <- max(x[which(x <= up)])
  minNom <- min(x[which(x >= down)])
  outList <- list(nOut = nOut, lowLim = down,
                  upLim = up, minNom = minNom,
                  maxNom = maxNom, index = index,
                  values = values,
                  outClass = outClass)
  return(outList)
}
library(MASS)
UScereal
fullsummary <- FindOutlier(UScereal$fibre)
fullsummary$summary
fullsummary$threeSigma
fullsummary$Hampel
fullsummary$boxplotRule
