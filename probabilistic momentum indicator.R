require(quantmod)
require(PerformanceAnalytics)
require(quantmod)

#load data 
factors <- c("IEF","HYG","USO","UUP","DBA","IWM","GLD","EEM")
benchmark <- c("SPY")

getSymbols(factors,src = "yahoo")
getSymbols(benchmark, src = "yahoo")

#function for the specific t.test I will rollapply. 
tstatFunc <- function(x) {
        t.test(x,alt = "greater", conf.level = .95)[1]
}

#Create Probabilities.  
retlist <- list()
for(factor in factors) {
        tmp <- Return.calculate(Ad(get(factor)))
        spy <- Return.calculate(Ad(get("SPY")))
        spyTmp <- spy[index(tmp)]
        diffRets <- tmp-spyTmp 
        tstatTmp <- as.numeric(rollapply(diffRets,100,tstatFunc))
        tmpDF <- na.omit(cbind(diffRets,tstatTmp))
        colnames(tmpDF) <- c("TMP","TSTAT")
        tmpDF$PROB <- pt(tmpDF$TSTAT,99)
        retlist[[factor]] <- tmpDF$PROB
}

#combine into one dataframe. 
retDF <- na.omit(do.call(cbind,retlist)) 
colnames(retDF) <- c("IEF","HYG","USO","UUP","DBA","IWM","GLD","EEM")

#add rows to get daily total values. 
PMI <- rowMeans(retDF)
PMI <- xts(PMI, order.by = index(retDF))

#chart the series 
chartSeries(SPY,theme = chartTheme('white'), TA = NULL, subset = "2015-08::" )
addTA(PMI)

###############################################################################
#####################  Create indicator returns graph 

require(quantmod)

###This is the part of the code that....
prepare.indicator <- function(close, indicator, roc.n, normalize=FALSE, func=mean) {
        ##This calculates returns for the series.  it is equivalent to Return.calculate. 
        rets <- ROC(close, type="discrete", n=roc.n)
        
        
        #This looks at the argument above and if it is FALSE it is skipped if it is TRUE it is 
        #that means that the return figure is for multiple days say the return over 8 days.  This function says
        #what is this 8 day return expressed daily.  So what return compounded will for 8 days will give me this 8 day return. 
        if(normalize) {
                # Normalize the returns to daily
                rets <- ((1 + rets) ^ (1/roc.n)) - 1
        }
        
        #here we are merging the datasets together using common lables (here it would be the timestamp). read instructions
        #for merge if you don't remember what the arg "all" does. 
        #here you are lagging the INDICATOR, not the returns.  This matches the indicator with the return value. 
        
        mm <- merge(na.exclude(lag(indicator, k=roc.n)), na.exclude(rets), all=F)
        #here you grab the indicator columns and grab is a simple vector. 
        dd <- as.numeric(mm[,1])
        
        # Map the indicator values into the tenth intervals
        
        #here takes numbers and rounds up. so 
        ee <- ceiling(dd*10)
        #takes care of the case if the DVI is 0.  If it's zero, you assume it's 1, which is the lowest interval
        #remember, that above you take the DVI which, though expressed as a percent, when multiplied by 
        #10 gives you a vector of values between 0-10.  Since you round up, even a .01 will round to 1. 
        #it's just in the small cases when the DVI is 0 thatyou need to account for. 
        ee <- ifelse(ee == 0, 1, ee)
        
        # Create the factors
        #this takes the above vector of DVI values and it maps it to ee.  so for 1 in ee we get factor .1 
        #for 2 we get factor .2, and so on.  it's amazing to me how there is some matching principle at work here 
        #like it says the order of of the sequence can be made from smallest to greatest, just like the order 
        #of ee, so match by smallest to largest.  
        ff <- factor(ee, labels=as.character(seq(0.1, .6, 0.1)))
        
        # Split the returns according to the factors
        #here we are taking the factors we developed in ee and grabing the corresponding values for our return strea
        #and this gives us a list.  each factor will be an list object, with a vector of returns that match that factor
        gg <- split(as.numeric(mm[,2]), ff)
        
        #here we say go through each list object and find the mean of the corresonding elements in that object.  
        #you get a list that is returned. func is defined in the function args above.  In this case it calculates the mean.   
        yy <- sapply(gg, func)
        
        #and here is what is returned
        return(list(raw.res=gg, res=yy, rets=rets))
}

PMI.analysis <- function(close, lags=c(5), normalize=FALSE, file.path, do.plot=TRUE, width=800, height=1200, func=mean) {
        # Redirect the plot if necessary
        if(do.plot && !missing(file.path)) {
                png(filename=file.path, width=width, height=height, units='px', pointsize=12, bg='white')
        }
        
        if(length(lags) %% 2 == 0) {
                par(mfrow=c(length(lags) / 2, 2))
        } else {
                par(mfrow=c(length(lags), 1))
        }
        
        ind = PMI
        res = list()
        raw.res = list()
        rets = list()
        
        for(ll in lags) {
                xx <- prepare.indicator(close, ind, roc.n=ll, normalize=normalize, func=func)
                yy <- xx$res
                
                barplot(
                        yy,
                        ylim=c(-max(abs(yy)), max(abs(yy))),
                        col=ifelse(yy<0, "darkblue", "red"),
                        main=paste(as.character(ll), "-day returns", sep=""),
                        xlab="PMI level",
                        ylab="Expected return")
                
                res[[as.character(ll)]] = xx$res
                raw.res[[as.character(ll)]] = xx$raw.res
                rets[[as.character(ll)]] = xx$rets
                # return(list(gg=gg, ff=ff, ee=ee, dd=dd))
        }
        
        # Restore the plot output
        if(do.plot && !missing(file.path)) {
                dev.off();
        }
        
        return(list(res=res, raw.res=raw.res, rets=rets))
}

####################################
##################### Throwing in the LW Indicator 

#LW <- function(x) {
        y <- get("SPY")
        colnames(y) <- c("Open","High","Low","Close","Volume","Adj")
        #get Open-Close and calc an 7 period average
        omc <- y$Open - y$Close
        testOMC <- rollapply(omc,42,mean)
        #get High-low and calc a 7 period average
        hml <- y$High - y$Low
        testHML <- rollapply(hml,42,mean)
        #calc proxy 
        proxy <- na.omit((testOMC/testHML)*50+50)
        #return(proxy)
        #index proxy to PMI 
        proxy <- proxy[index(PMI)]
        adjPMI <- PMI * proxy 
        dviAjdPMI <- DVI(adjPMI)[,3]
        
        
#}

###this calls the analyses from above. 
require(quantmod)
getSymbols("SPY", from="1900-01-01")
aa = PMI.analysis(Cl(SPY["/2009"]), lags=seq(5,12), normalize=T, file.path="DPP2.mean.png", func=mean)



