#function to caclulate slopes bttween each data point
ken_slopes<-function (x, y) 
{
  #cx = !xcen
  #cy = !ycen
  
  slopes = unlist(lapply(seq(along = x), function(i, y, x) ((y[i] - 
                                                               y[1:i])/(x[i] - x[1:i])), y, x ))
  
  #return(range(slopes[is.finite(slopes)]))
  return(slopes[is.finite(slopes)])
}

#function to return pairwise slopes, VarS, rank_slope_zero, and z1a_zero (a la McBride for calculating likelihood of trend)
#for datasets with nonDetects, will impute random values underneath the detection limit
#imuted values are based on the distribution of the entire dataset
#imputation is repeated [iter] times, default=1000
#function returns average slopes from those imputation permutations
#returns average rank_slope zero and the calculate zero (the center of the z-normalized slope distribution)
#this functino works fine if there are not censored data (can set ycen to FALSE )
imp_function<-function(x,y,ycen,iteration_threshold=0.2,iter=1000){
  VarS<-calc_VarS(x,y) # calculate S Variance
  
  if((length(which(ycen))/length(y))>iteration_threshold&length(unique(y))!=1){
    #only do iterations if more than XX% non-detects
    if((length(which(ycen))/length(ycen))>0.7){
      m=mean(y)
      s=sd(y)
    } else {y_fit<-NADA2::cfit(y,ycen,Cdf=F,printstat = F)
    m <- y_fit$KMmean
    s <- y_fit$KMsd
    }
    location <- log(m^2 / sqrt(s^2 + m^2))
    shape <- sqrt(log(1 + (s^2 / m^2)))
    ymat=matrix(y,iter,length(y),byrow=T)
    yc<-y[ycen]
    #ynew<-t(replicate(iter,yc*runif(length(yc),0.0001,0.99999)))
    ynew<-t(replicate(iter,EnvStats::rlnormTrunc(length(yc),meanlog = location,sdlog= shape,max=yc)))
    ymat[,ycen]<-ynew
    
    ymat_slopes<-apply(ymat,1,function(yi) ken_slopes(x=x,y=yi)) 
    
    rank_slope_zeros<-apply(ymat_slopes,2,function(slope_col) approx(x=sort(slope_col),y=1:length(slope_col),xout = 0)$y)
    rank_slope_zero<-mean(rank_slope_zeros)
    z1a_zero<-(rank_slope_zero*2-nrow(ymat_slopes))/-sqrt(VarS)
    
    
    ave_slopes<-ymat_slopes%>%
      rowMeans() %>%
      sort()
  } else{
    ave_slopes<-sort(ken_slopes(x,y))
    if(length(unique(ave_slopes))==1){rank_slope_zero=length(ave_slopes)/2} else{
      rank_slope_zero<-approx(x=ave_slopes,y=1:length(ave_slopes),xout=0)$y}
    z1a_zero<-(rank_slope_zero*2-length(ave_slopes))/-sqrt(VarS)
  }
  return(list(ave_slopes=ave_slopes,rank_slope_zero=rank_slope_zero,z1a_zero=z1a_zero,VarS=VarS))
}
calc_VarS<-function(x,y){
  tempData=data.frame(x,y)
  x_ties<-tempData %>%
    group_by(x) %>%
    summarise(n=n()) %>%
    filter(n>1)
  
  y_ties<-tempData %>%
    group_by(y) %>%
    summarise(n=n()) %>%
    filter(n>1)
  n_samp<-nrow(tempData)
  
  VarS=n_samp*(n_samp-1)*(2*n_samp+5)/18-
    sum(unlist(lapply(x_ties$n,function(x) x*(x-1)*(2*x+5))))/18-
    sum(unlist(lapply(y_ties$n,function(x) x*(x-1)*(2*x+5))))/18+
    sum(unlist(lapply(x_ties$n,function(x) x*(x-1)*(x-2))))*sum(unlist(lapply(y_ties$n,function(x) x*(x-1)*(x-2))))/
    (9*n_samp*(n_samp-1)*(n_samp-2))+
    sum(unlist(lapply(x_ties$n,function(x) x*(x-1))))*sum(unlist(lapply(y_ties$n,function(x) x*(x-1))))/
    (2*n_samp*(n_samp-1))
  return(VarS)
}
