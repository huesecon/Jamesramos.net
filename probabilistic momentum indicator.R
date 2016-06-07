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
        tstatTmp <- as.numeric(rollapply(diffRets,99,tstatFunc))
        tmpDF <- na.omit(cbind(diffRets,tstatTmp))
        colnames(tmpDF) <- c("TMP","TSTAT")
        tmpDF$PROB <- pt(tmpDF$TSTAT,100)
        retlist[[factor]] <- tmpDF$PROB
}

#combine into one dataframe. 
retDF <- na.omit(do.call(cbind,retlist)) 
colnames(retDF) <- c("IEF","HYG","USO","UUP","DBA","IWM","GLD","EEM")

#add rows to get daily total values. 
ind <- rowSums(retDF)
ind <- xts(ind, order.by = index(retDF))

#chart the series 
chartSeries(SPY,theme = chartTheme('white'), TA = NULL, subset = "2008-08::" )
addTA(ind)
