WaterfallPlot <- function(YieldChange, 
                          title=NULL,
                          t1='Treated', 
                          t2='Control', 
                          assessment='Yield', 
                          alpha=0.05, 
                          error = 'bootstrap', 
                          basis='Difference',
                          baseplot=FALSE,
                          PooledSD=NA,
                          Groups="#999999") {
 cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", 
                "#F0E442", "#CC79A7", "#000000", "#734f80", "#2b5a74", "#004f39", 
                "#787221", "#003959", "#6aaf00", "#663cd3")
 
  grey = cbPalette[1]
  yellow <- cbPalette[7]
  blue <- cbPalette[5]
  vermillion <- cbPalette[6]
 
  WinRate.CI.text = ''
  Win.CI = NULL
  
  difference.ylab = paste(assessment,':', t1, '-' , t2)
  if(basis=='Percent') {
    YieldChange =  100 *  YieldChange / ((Mean1 + Mean2) / 2)
    difference.ylab = paste('Percent Difference, (',
                            t1, '-' , t2,
                            ')/mean(',
                            t1, ',', t2,
                            ')',
                            sep = '')
  }
  
  na.yield <- is.na(YieldChange)
  YieldChange <- YieldChange[!na.yield]
  if(length(Groups)>=length(na.yield)) {
    Groups <-  Groups[!na.yield]
  }
  
  if(is.null(title)) {
    hth.main = paste('Head to Head, ', t1, 'vs' , t2,'over',length(YieldChange),'Trials')
  } else {
    hth.main <- title
  }
  
  line1 <- max(YieldChange) * .9
  line2 <- max(YieldChange) * .8
  line3 <- max(YieldChange) * .7
  line4 <- max(YieldChange) * .6
  
  o <- order(YieldChange,
             decreasing = TRUE,
             na.last = NA)
  YieldChange <- YieldChange[o]
  
  if(length(Groups)==length(YieldChange)) {
    Groups <- Groups[o]
  }
  GroupsIdx <- as.numeric(as.factor(Groups))
  cols <- cbPalette[GroupsIdx]
  
  # get range for normal dist
  Mean = mean(YieldChange)
  Variance = var(YieldChange)
  if(is.na(PooledSD)) {
    StdDev <- sd(YieldChange)
  } else {
    StdDev <- PooledSD
  }
  
  quant <- seq(1/(10*length(YieldChange)),1-1/(10*length(YieldChange)),length.out =length(YieldChange))
  norm.sample <- qnorm(quant,mean=Mean,sd=StdDev)
  ylim <- c(min(c(YieldChange,norm.sample)),max(c(YieldChange,norm.sample)))
  df.bar <- barplot(
    YieldChange,
    col = cols,
    border = cols,
    space = 0.5,
    main = hth.main,
    ylab = difference.ylab,
    ylim=ylim,
    xaxt='n'
  )
  
  UniqueGroups <- unique(Groups)
  if(length(UniqueGroups)>1) {
    ncols <- ceiling(length(UniqueGroups)/6)
    lcols <-  cbPalette[as.numeric(as.factor(UniqueGroups))]
    legend('bottomleft',legend=UniqueGroups,ncol=ncols,pch=16,col=lcols)
  }
  
  offset = quantile(df.bar,0.25)
  offset2 = quantile(df.bar,0.05)
  
  Median = median(YieldChange)
  
  if(!is.na(PooledSD)) {
    StdDev <- PooledSD
  }
  StdErr <- StdDev/sqrt(length(YieldChange))
  
  Average.text <-
    paste('Mean =', format(Mean, 2))
  CI <-
    Mean + qnorm(1 - alpha / 2) * c(-StdErr, StdErr)
  
  Average.text <- paste(Average.text,', CI=(', format(CI[1], 2), ',', format(CI[2], 2),')',sep='')
  SD.text <- paste('SD =', format(StdDev, 2))
  
  WinRate <-
    100 * sum(YieldChange > 0) / length(YieldChange)
  WinRate.text <- paste('Win Rate =', format(WinRate, 2))
  text(offset, line1, Average.text, adj = c(0, 0))
  if(baseplot) {
    text(offset, line3, WinRate.text, adj = c(0, 0))
    return(0)
  }
  
  n <- length(YieldChange)
  
  if (error == 'ecdf') {
    Win.CI <-
      WinRate + c(-100, 100) * ((1 / sqrt(
        2 * length(YieldChange)
      ) * log(2 / 0.1)))
    Win.CI[Win.CI > 100] <- 100
    Win.CI[Win.CI < 0] <- 0
    WinRate.CI.text <- paste('eCDF CI =',format(Win.CI[1],2),'-',format(Win.CI[2],2))
    #WinRate.CI.text <- 'Win Rate CI (eCDF) ='
  } else if(error == 'normal') {
    se.p <- 100*sqrt(((WinRate/100)*(1-WinRate/100))/length(YieldChange))
    # WinRate <- 100*(1-pnorm(0,Mean,StdDev))
    # WinRate.text <- paste('Win Rate (approx) =',format(WinRate,2))
    Win.CI <- WinRate + qnorm(1-alpha/2)*c(-se.p,se.p)
    WinRate.CI.text <- paste('Normal Approx. CI =',format(Win.CI[1],2),'-',format(Win.CI[2],2))
    
    #WinRate.text <- 'Win Rate (approx) ='
  } else if (error=='nonpar') {
    boot.samples <- 1000
    boots <- rep(0, 1000)
    
    for (i in 1:boot.samples) {
      obs <- sample(YieldChange, replace = TRUE)
      boots[i] <- 100 * sum(obs > 0) / n 
    }
    Win.CI <- quantile(boots,c(alpha/2,1-alpha/2))
    WinRate.CI.text <- paste('Bootstrap percentile CI =',format(Win.CI[1],2),'-',format(Win.CI[2],2))
  } else if(error=='par') {
    boot.samples <- 1000
    boots <- rep(0, 1000)
    WinRate <- 100*(1-pnorm(0,Mean,StdDev))
    WinRate.text <- paste('Win Rate (approx) =',format(WinRate,2))
    for (i in 1:boot.samples) {
      obs <- sample(YieldChange, replace = TRUE)
      sample.mean <- mean(obs)
      sample.sd <- sd(obs)
      boots[i] <- 100*(1 - pnorm(0, sample.mean, sample.sd))-WinRate
    }
    boot.se <- sd(boots)
    Win.CI <- WinRate + qnorm(1 - alpha / 2) * c(-boot.se, boot.se)
    WinRate.CI.text <-
      paste(
        'Parameteric Boostrap CI  =',
        format(Win.CI[1], 2),
        '-',
        format(Win.CI[2], 2)
      )
  }
  text(offset, line3, WinRate.text, adj = c(0, 0))
  text(offset, line4, WinRate.CI.text, adj = c(0, 0))
  
  y0 = max(YieldChange) * .1
  
  bar.width <- max(df.bar)-min(df.bar)
  
  if(!is.null(Win.CI)) {
    lower <- min(df.bar)+bar.width*(Win.CI[1]/100)
    if(lower<min(df.bar)) lower = min(df.bar)
    upper <- min(df.bar)+bar.width*(Win.CI[2]/100)
    if(upper>max(df.bar)) upper = max(df.bar)
    segments(x0=upper, y0=y0, y1 = -y0, col = vermillion,lwd=2)
    segments(x0=lower, y0=y0, y1 = -y0, col = vermillion,lwd=2)
  }
  
  mid <- min(df.bar)+bar.width*(WinRate/100)
  fifty <- (max(df.bar)+min(df.bar))/2
  segments(x0=mid, y0=2*y0, y1 = -2*y0, col = blue,lwd=3)
  segments(x0=fifty, y0=y0*.7, y1 = -y0*.7, col = yellow,lwd=3)
  
  
  sw <- shapiro.test(YieldChange)
  text(offset, line2, paste('Shapiro-Wilks p-value =',format(sw$p.value,2)), adj = c(0, 0))
  

  x <- bar.width*quant
  norm.sample <- sort(norm.sample,decreasing = TRUE)
  lines(x,norm.sample ,col=cbPalette[2])
  
}