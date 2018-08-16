predict_skellam <- function(mod,data){
    
    mu1 = coef(mod)[[1]]+(coef(mod)[[2]]*data[1,]$V1)+(coef(mod)[[3]]*data[1,]$V2)+(coef(mod)[[4]]*data[1,]$V3)+(coef(mod)[[5]]*data[1,]$V4)
    mu2 = coef(mod)[[6]]+(coef(mod)[[7]]*data[1,]$V1)+(coef(mod)[[8]]*data[1,]$V2)+(coef(mod)[[9]]*data[1,]$V3)+(coef(mod)[[10]]*data[1,]$V4)
    predict_skellam = data.frame(mu1=exp(mu1),mu2=exp(mu2))
    predict_skellam
    
}