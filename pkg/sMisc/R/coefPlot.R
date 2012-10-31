defaultInterval <- function(fm)
{
  cbind(coef(fm), confint(fm))
}

waldInterval <- function(fm, use.summary = TRUE)
{
  if(!use.summary)
  {
    sd <- sqrt(diag(vcov(fm)))
    cbind(coef(fm), coef(fm) - 1.96 * sd, coef(fm) + 1.96 * sd)    
  }
  else
  {
    s <- summary(fm)$coef
    cbind(s[,1], s[,1] - 1.96 * s[,2], s[,1] + 1.96 * s[,2])  
  }
}

coefPlot <- function(..., var.names = NULL, log.y = FALSE, exclude = NULL, extractor.funcs = NULL)
{
  # make list with model objects:
  model.list <- match.call(expand.dots = FALSE)$...
  model.names <- as.character(model.list)
  # define extractors:
  if(is.null(extractor.funcs))
  {
    extractor.funcs <- list()
    for(i in 1:length(model.list))
      extractor.funcs[[i]] <- defaultInterval
    names(extractor.funcs) <- model.names
  }
  # generate vector with var.names to be considered:
  if(is.null(var.names)) 
  {
    var.names <- lapply(model.list, function(fm) names(do.call(coef, list(fm))))
    var.names <- unique(unlist(var.names))
  }
  var.names <- var.names[!var.names %in% exclude]
  # make an array containing the coefficients along with their interval
  coefs <- array(dim = c(length(var.names), 3, length(model.list)), dimnames = list(var.names, c("Estimate", "Lower", "Upper"), model.names))
  for(i in 1:length(model.list))
  {
    m <- do.call(extractor.funcs[[i]], list(model.list[[i]]))
    m <- m[rownames(m) %in% var.names,]
    row.names <- rownames(m)
    coefs[row.names, , i] <- m
  }
  plot(0, type = "l", ylim = range(coefs, 0, na.rm = TRUE), xlim = c(0.5, length(var.names) + 0.5), xaxt = "n", ylab = "Estimated coefficient", xlab = "")
  abline(h = 0, lty = "dashed")
  axis(side = 1, at = 1:length(var.names), labels = var.names, las = 2)
  o <- seq(-0.5, 0.5, length.out = length(model.list) + 2)[-c(1, length(model.list) + 2)] * 0.8
  for(i in 1:length(var.names))
  {
    for(j in 1:length(model.list))
    {
      triple <- coefs[i, ,j]
      if(!sum(is.na(coefs[c(2,3)])))
      {
        arrows(i + o[j], triple[2], i + o[j], triple[3], angle = 90, code = 3, length = 0.1, col = j + 1)
      }
      points(i + o[j], triple[1], col = j + 1)
    }
  }
  invisible(coefs)
}
