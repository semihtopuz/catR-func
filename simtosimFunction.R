library(catR)

# parametreler mirt'den csv olarak çekilir

pars <- coef(model2, simplify=T)

parameters <- as.data.frame(pars$items)
write.csv(parameters, "par.csv")

# catR için hazırlanır
itemBank <- read.csv("par.csv")
itemBank <- itemBank[,-1] 
colnames(itemBank) <- c("a", "b", "c", "d")  

##### bu fonksiyon theta düzeylerini ve yanıt örüntülerini tekrar tekrar oluşturduğu için 
# daha sağlıklı bir kestirime izin verecektir. 

# times: kaç simülasyon yapılacağına karar verir default=1
# thetatimes: kaç theta oluşturulacağına karar verir 
# th: başlangıç theta düzeyi
# leng: durdurma için test uzunluğu
# sel: başlangıç ve ilerlemede madde seçim yöntemi (fonksiyon içerisinden ikisi ayrı ayrı düzenlenebilir.)
# estimate: test ve son kestirimde yöntem (fonksiyon içerisinden ikisi ayrı ayrı düzenlenebilir.)
# se: durdurma için standart hata


#fonksiyon yalnızca "mean cor.", "mean RMSE", "mean bias", "mean length" vermektedir. Geliştirilebilir.

simtosim <- function(times=1, thetatimes, th=0, leng, sel="MFI", estimate="BM", se=0.4){
    library(catR)
cors <- c()
bias <- c()
RMSE <- c()
testLength <- c()
parallel::makeCluster(parallel::detectCores())
for (i in 1:times){
  
  # theta will repeat 100 time
  theta <- rnorm(thetatimes, mean = 0, sd = 1)
  
  
  # response will repeat 100 time
  response <- genPattern(theta,
                            itemBank,
                            D = 1.702)
  
  #starting rule will be constant
  start <- list(theta = th, startSelect = sel)
  
  
  # this will be constant
  test <- list(method = estimate, itemSelect = sel)
  
  # this will be constant
  stop <- list(rule = c("length","precision"),
               thr = c(leng,se))
  
  # final will be constant 
  final <- list(method = estimate)
  
  #this will repeat 100 time

  ex  <- simulateRespondents(theta, itemBank, response,
                                                 start = start, test = test, 
                                                 stop = stop, final = final)
  cors[i] <- ex$correlation
  bias[i] <- ex$bias
  RMSE[i] <- ex$RMSE
  testLength[i] <- ex$testLength
}
mcors <- mean(cors)
mRMSE <- mean(RMSE)
mbias <- mean(bias)
mlength <- mean(testLength)
simtosimres <- cbind(c("mean cor.", "mean RMSE", "mean bias", "mean length"), 
                             round(c(mcors, mRMSE, mbias, mlength),5))
rm(mcors,mRMSE,mbias,mlength)
as.data.frame(simtosimres)
}
res <- simtosim(times = 2, thetatimes = 350,leng = 35)
