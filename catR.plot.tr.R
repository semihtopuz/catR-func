## catR'ın plot çıktıları plot fonksiyonuyla türkçeleşitirilemediği için bu fonksiyon yazılmıştır.


tr.cat.est <- function (x, type = "all", deciles = "theta", save.plot = FALSE, 
                        save.options = c("", "plot", "pdf"), res = 300, ...) 
{
  type <- switch(type, all = "all", trueEst = "trueEst", expRate = "expRate", 
                 cumExpRate = "cumExpRate", cumNumberItems = "cumNumberItems", 
                 expRatePara = "expRatePara", condBias = "condBias", condRMSE = "condRMSE", 
                 numberItems = "numberItems", sError = "sError", condThr = "condThr")
  if (is.null(x$responsesMatrix)) 
    simulation <- FALSE
  else simulation <- TRUE
  if (length(x$model) == 0) 
    x$model <- "dicho"
  if (deciles != "theta" & deciles != "deciles") 
    stop("'deciles' must be either 'theta' or 'deciles'", 
         call. = FALSE)
  if (is.null(type)) 
    stop("invalid 'type' argument", call. = FALSE)
  if ((type == "cumNumberItems" | type == "condThr" | type == 
       "numberItems") & (length(x$stop$rule) == 1 & sum(x$stop$rule == 
                                                        "length") == 1)) 
    stop("mismatch between 'type' value and 'length' stopping rule", 
         call. = FALSE)
  if (type == "expRatePara" & (x$model == "PCM" | x$model == 
                               "NRM")) 
    stop("mismatch between 'expRatePara' type value and 'PCM' or 'NRM' model", 
         call. = FALSE)
  internalCAT <- function() {
    if (deciles == "theta") 
      xline <- x$condTheta
    else xline <- 1:10
    if (simulation == TRUE) {
      xAccuracy <- "Atanmış Theta"
      yAccuracy <- "BOBUT Kestirilen Theta"
      if (deciles == "deciles") 
        thetasDeciles <- "Atamış Theta"
      else thetasDeciles <- "Kestirilen Theta"
    }
    else {
      xAccuracy <- "Gerçek Theta"
      yAccuracy <- "Kestirilen Theta"
      if (deciles == "deciles") 
        thetasDeciles <- "Gerçek Theta"
      else thetasDeciles <- "Gerçek Theta"
    }
    plot.trueEst <- function(x, ...) {
      plot(x$thetas, x$estimatedThetas, main = "Doğruluk", 
           xlab = xAccuracy, ylab = yAccuracy)
      abline(lm(x$estimatedThetas ~ x$thetas), col = "red")
    }
    plot.expRate <- function(x, ...) {
      plot(sort(x$exposureRates, decreasing = TRUE), type = "l", 
           main = "Açığa Çıkma Oranı", xlab = "Sıralanmış Maddeler", 
           ylab = "Madde Açığa Çıkma Oranı")
    }
    plot.cumExpRate <- function(x, ...) {
      plot(cumsum(sort(x$exposureRates, decreasing = TRUE))/x$testLength, 
           type = "l", main = "Birikimli Açığa Çıkma Oranı", 
           xlab = "Sıralanmış Maddeler", ylab = "Birikimli Madde Açığa Çıkma Oranı")
    }
    plot.cumNumberItems <- function(x, ...) {
      respondents <- length(x$numberItems)
      plot(seq(1, respondents, 1) * 100/respondents, sort(x$numberItems, 
                                                          decreasing = FALSE), type = "l", main = "Test Uzunluğu", 
           xlab = "Sınamaların Yüzdesi", ylab = "Test Uzunluğu")
    }
    plot.expRatePara <- function(x, ...) {
      plot(x$itemBank[, 1], x$exposureRates, main = "Exposure and a Parameter", 
           xlab = "Ayırım Parametresi", pch = 20, 
           ylab = "Madde Açığa Çıkma Oranı")
    }
    plot.condBias <- function(x, ...) {
      plot(xline, x$condBias, type = "o", main = "Koşullu Yanlılık", 
           xlab = thetasDeciles, ylab = "Yanlılık")
    }
    plot.condRMSE <- function(x, ...) {
      plot(xline, x$condRMSE, type = "o", main = "Koşullu RMSE", 
           xlab = thetasDeciles, ylab = "RMSE")
    }
    plot.numberItems <- function(x, ...) {
      plot(xline, x$condnItems, type = "o", main = "Koşullu Test Uzunluğu", 
           xlab = thetasDeciles, ylab = "Test Uzunluğu")
    }
    plot.sError <- function(x, ...) {
      plot(xline, x$condSE, type = "o", main = "Koşullu Standart Hata", 
           xlab = thetasDeciles, ylab = "Standart Hata")
    }
    plot.condThr <- function(x, ...) {
      plot(xline, x$condthrOK, type = "o", main = "Durdurma Kuralının Karşılanması", 
           xlab = thetasDeciles, ylab = "Oran")
    }
    if (type == "all") {
      if (sum(x$stop$rule == "precision") == 1 | sum(x$stop$rule == 
                                                     "classification") == 1) {
        par(mfrow = c(3, 3))
        plot.trueEst(x)
        plot.condBias(x)
        plot.condRMSE(x)
        plot.expRate(x)
        plot.condThr(x)
        plot.cumNumberItems(x)
        if (x$model != "PCM" & x$model != "NRM") 
          plot.expRatePara(x)
        plot.numberItems(x)
        plot.sError(x)
        par(mfrow = c(1, 1))
      }
      else {
        par(mfrow = c(2, 3))
        plot.trueEst(x)
        plot.condBias(x)
        plot.condRMSE(x)
        plot.expRate(x)
        plot.cumExpRate(x)
        if (x$model != "PCM" | x$model != "NRM") 
          plot.expRatePara(x)
        par(mfrow = c(1, 1))
      }
    }
    if (type == "trueEst") 
      plot.trueEst(x)
    if (type == "expRate") 
      plot.expRate(x)
    if (type == "cumExpRate") 
      plot.cumExpRate(x)
    if (type == "cumNumberItems") 
      plot.cumNumberItems(x)
    if (type == "expRatePara") 
      plot.expRatePara(x)
    if (type == "condBias") 
      plot.condBias(x)
    if (type == "condRMSE") 
      plot.condRMSE(x)
    if (type == "numberItems") 
      plot.numberItems(x)
    if (type == "sError") 
      plot.sError(x)
    if (type == "condThr") 
      plot.condThr(x)
  }
  internalCAT()
  if (save.plot) {
    plotype <- NULL
    if (save.options[3] == "pdf") 
      plotype <- 1
    if (save.options[3] == "jpeg") 
      plotype <- 2
    if (is.null(plotype)) 
      cat("Invalid plot type (should be either 'pdf' or 'jpeg').", 
          "\n", "The plot was not captured!", "\n")
    else {
      if (save.options[1] == "") 
        wd <- paste(getwd(), "/", sep = "")
      else wd <- save.options[1]
      nameFile <- paste(wd, save.options[2], switch(plotype, 
                                                    `1` = ".pdf", `2` = ".jpg"), sep = "")
      if (plotype == 1) {
        if (type == "all" & x$stop$rule == "precision") {
          {
            pdf(file = nameFile, width = 15, height = 15)
            internalCAT()
          }
          dev.off()
        }
        if (type == "all" & x$stop$rule == "classification") {
          {
            pdf(file = nameFile, width = 15, height = 15)
            internalCAT()
          }
          dev.off()
        }
        if (type == "all" & x$stop$rule == "length") {
          {
            pdf(file = nameFile, width = 10, height = 5)
            internalCAT()
          }
          dev.off()
        }
        if (type != "all") {
          {
            pdf(file = nameFile)
            internalCAT()
          }
          dev.off()
        }
      }
      if (plotype == 2) {
        if (type == "all" & x$stop$rule == "precision") {
          {
            jpeg(filename = nameFile, width = 24, height = 24, 
                 units = "cm", res = res)
            internalCAT()
          }
          dev.off()
        }
        if (type == "all" & x$stop$rule == "classification") {
          {
            jpeg(filename = nameFile, width = 24, height = 24, 
                 units = "cm", res = res)
            internalCAT()
          }
          dev.off()
        }
        if (type == "all" & x$stop$rule == "length") {
          {
            jpeg(filename = nameFile, width = 24, height = 16, 
                 units = "cm", res = res)
            internalCAT()
          }
          dev.off()
        }
        if (type != "all") {
          {
            jpeg(filename = nameFile)
            internalCAT()
          }
          dev.off()
        }
      }
      cat("The plot was captured and saved into", "\n", 
          " '", nameFile, "'", "\n", "\n", sep = "")
    }
  }
  else cat("The plot was not captured!", "\n", sep = "")
}
