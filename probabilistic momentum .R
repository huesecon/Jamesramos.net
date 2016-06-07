require(quantmod)
require(PerformanceAnalytics)
require(quantmod)

#load data 
factors <- c("IEF","HYG","USO","UUP","DBA","IWM","GLD","EEM")
benchmark <- c("SPY")
tickers <- c("MMM","ABT","ACN","ATVI","ADBE","AAP","AES","AET","AMG","AFL",
             "A","GAS","APD","ARG","AKAM","AA","ALXN","AGN","ADS","ALL","GOOG","MO",
             "AMZN","AEE","AAL","AEP","AXP","AIG","AMT","AMP","ABC","AME","AMGN","APH","APC","ADI",
             "ANTM","AON","APA","AIV","AAPL","AMAT","ADM","AIZ","T","ADSK","ADP","AN","AZO","AVGO","AVB",
             "AVY","BHI","BLL","BAC","BCR","BAX","BBT","BDX","BBBY","BRK-B","BBY","BIIB","BLK","HRB",
             "BA","BWA","BXP","BSX","BMY","BF-B","CHRW","CA","CVC","COG","CAM","CPB","COF","CAH","KMX","CCL",
             "CAT","CBG","CBS","CELG","CNP","CTL","CERN","CF","SCHW","CHK","CVX","CMG","CB","CHD","CI","XEC",
             "CINF","CTAS","CSCO","C","CTXS","CME","CMS","COH","CCE","CTSH","CL","CMCSA","CMA","CAG",
             "CXO","COP","CNX","ED","STZ","GLW","COST","CCI","CSX","CMI","CVS","DHI","DHR","DRI","DVA","DE",
             "DAL","XRAY","DVN","DO","DFS","DISCA","DISCK","DG","DLTR","D","DOV","DOW","DPS","DTE","DD","DUK","DNB",
             "ETFC","EMN","ETN","EBAY","ECL","EIX","EW","EA","EMC","EMR","ENDP","ESV","ETR","EOG","EQT","EFX","EQIX",
             "EQR","ESS","EL","ES","EXC","EXPE","EXPD","ESRX","EXR","XOM","FFIV","FAST","FRT","FDX","FIS","FITB",
             "FSLR","FE","FISV","FLIR","FLS","FLR","FMC","FTI","F","BEN","FCX","FTR","GME","GPS","GRMN","GD","GE","GGP",
             "GIS","GPC","GILD","GS","GT","GWW","HAL","HBI","HOG","HAR","HRS","HIG","HAS","HCP","HP","HSIC",
             "HES","HD","HON","HRL","HST","HPQ","HUM","HBAN","ITW","ILMN","IR","INTC","ICE","IBM","IP","IPG","IFF",
             "INTU","ISRG","IVZ","IRM","JBHT","JEC","JNJ","JCI","JPM","JNPR","KSU","K","GMCR","KEY","KMB","KIM",
             "KLAC","KSS","KR","LB","LLL","LH","LRCX","LM","LEG","LEN","LUK","LVLT","LLY","LNC","LLTC","LMT","L",
             "LOW","LYB","MTB","MAC","M","MRO","MAR","MMC","MLM","MAS","MA","MAT","MKC","MCD","MHFI","MCK",
             "MJN","MDT","MRK","MET","MCHP","MU","MSFT","MHK","TAP","MDLZ","MON","MNST","MCO","MS","MSI","MUR",
             "MYL","NDAQ","NOV","NTAP","NFLX","NWL","NFX","NEM","NEE","NKE","NI","NBL","JWN",
             "NSC","NTRS","NOC","NRG","NUE","NVDA","ORLY","OXY","OMC","OKE","ORCL","OI","PCAR","PH","PDCO","PAYX",
             "PNR","PBCT","POM","PEP","PKI","PRGO","PFE","PCG","PM","PNW","PXD","PBI","PNC","RL","PPG","PPL","PX","PCLN",
             "PFG","PG","PGR","PLD","PRU","PEG","PSA","PHM","PVH","QCOM","PWR","DGX","RRC","RTN","O","RHT","REGN","RF",
             "RSG","RAI","RHI","ROK","COL","ROP","ROST","RCL","R","CRM","SNDK","SCG","SLB","SNI","STX","SEE","SRE","SHW","SIG",
             "SPG","SWKS","SLG","SJM","SNA","SO","LUV","SWN","SE","STJ","SWK","SPLS","SBUX","HOT","STT","SRCL","SYK","STI","SYMC",
             "SYY","TROW","TGT","TEL","TE","TGNA","THC","TDC","TSO","TXN","TXT","BK","CLX","KO","HSY","MOS","TRV","DIS",
             "TMO","TIF","TWC","TWX","TJX","TMK","TSS","TSCO","RIG","FOX","TYC","TSN","USB","UA","UNP","UAL","UNH","UPS",
             "URI","UTX","UHS","UNM","URBN","VFC","VLO","VAR","VTR","VRSN","VRSK","VZ","VRTX","VIAB","V","VNO","VMC","WMT",
             "WBA","WM","WAT","WFC","HCN","WDC","WU","WY","WHR","WFM","WMB","WEC","WYN","WYNN","XEL","XRX","XLNX",
             "XYL","YHOO","YUM","ZBH","ZION","ASH","UIS","TEX","MTG","DDS","IILG","SSP","NCR","MXIM","SANM","NAV","LPX",
             "AVP","JCP","X","SHLD")
getSymbols(factors,src = "yahoo")
getSymbols(tickers,src = "yahoo")
getSymbols(benchmark, src = "yahoo")

tstatFunc <- function(x) {
        t.test(x,alt = "greater", conf.level = .95)[1]
}

#create returns 
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


retDF <- na.omit(do.call(cbind,retlist)) 
colnames(retDF) <- c("IEF","HYG","USO","UUP","DBA","IWM","GLD","EEM")
chartSeries(SPY,theme = chartTheme('white'), TA = NULL, subset = "2008-08::" )
addTA(ind)

ind <- rowSums(retDF)
ind <- xts(ind, order.by = index(retDF))


reglist <- list()
for(ticker in tickers) {
tmp <- Ad(get(ticker))
name <- gsub(".Adjusted","",names(tmp))
tmp20 <- Next(ROC(tmp,type = "discrete", n = 21), k = 21)
tmp20 <- xts(tmp20[index(retDF)])
colnames(tmp20)[1] <- name 
regDF <- na.omit(cbind(tmp20,retDF))
reg <- lm(regDF[,1]~regDF$IEF+regDF$HYG+regDF$USO+regDF$UUP+regDF$DBA+regDF$IWM+regDF$GLD+regDF$EEM)

rSQD<- summary(reg)$adj.r.squared
intercept <- coef(summary(reg))[1,1]
iefCOEF <- coef(summary(reg))[2,1]
hygCOEF <- coef(summary(reg))[3,1]
usoCOEF <- coef(summary(reg))[4,1]
uupCOEF <- coef(summary(reg))[5,1]
dbaCOEF <- coef(summary(reg))[6,1]
iwmCOEF <- coef(summary(reg))[7,1]
gldCOEF <- coef(summary(reg))[8,1]
eemCOEF <- coef(summary(reg))[9,1]

returnDF <- t(cbind(rSQD,intercept, iefCOEF,hygCOEF,usoCOEF,uupCOEF,dbaCOEF,iwmCOEF,gldCOEF,eemCOEF))
reglist[[ticker]] <- returnDF
}

reglist <- na.omit(do.call(cbind,reglist))
colnames(reglist) <- c("MMM","ABT","ACN","ATVI","ADBE","AAP","AES","AET","AMG","AFL",
                                  "A","GAS","APD","ARG","AKAM","AA","ALXN","AGN","ADS","ALL","GOOG","MO",
                                  "AMZN","AEE","AAL","AEP","AXP","AIG","AMT","AMP","ABC","AME","AMGN","APH","APC","ADI",
                                  "ANTM","AON","APA","AIV","AAPL","AMAT","ADM","AIZ","T","ADSK","ADP","AN","AZO","AVGO","AVB",
                                  "AVY","BHI","BLL","BAC","BCR","BAX","BBT","BDX","BBBY","BRK-B","BBY","BIIB","BLK","HRB",
                                  "BA","BWA","BXP","BSX","BMY","BF-B","CHRW","CA","CVC","COG","CAM","CPB","COF","CAH","KMX","CCL",
                                  "CAT","CBG","CBS","CELG","CNP","CTL","CERN","CF","SCHW","CHK","CVX","CMG","CB","CHD","CI","XEC",
                                  "CINF","CTAS","CSCO","C","CTXS","CME","CMS","COH","CCE","CTSH","CL","CMCSA","CMA","CAG",
                                  "CXO","COP","CNX","ED","STZ","GLW","COST","CCI","CSX","CMI","CVS","DHI","DHR","DRI","DVA","DE",
                                  "DAL","XRAY","DVN","DO","DFS","DISCA","DISCK","DG","DLTR","D","DOV","DOW","DPS","DTE","DD","DUK","DNB",
                                  "ETFC","EMN","ETN","EBAY","ECL","EIX","EW","EA","EMC","EMR","ENDP","ESV","ETR","EOG","EQT","EFX","EQIX",
                                  "EQR","ESS","EL","ES","EXC","EXPE","EXPD","ESRX","EXR","XOM","FFIV","FAST","FRT","FDX","FIS","FITB",
                                  "FSLR","FE","FISV","FLIR","FLS","FLR","FMC","FTI","F","BEN","FCX","FTR","GME","GPS","GRMN","GD","GE","GGP",
                                  "GIS","GPC","GILD","GS","GT","GWW","HAL","HBI","HOG","HAR","HRS","HIG","HAS","HCP","HP","HSIC",
                                  "HES","HD","HON","HRL","HST","HPQ","HUM","HBAN","ITW","ILMN","IR","INTC","ICE","IBM","IP","IPG","IFF",
                                  "INTU","ISRG","IVZ","IRM","JBHT","JEC","JNJ","JCI","JPM","JNPR","KSU","K","GMCR","KEY","KMB","KIM",
                                  "KLAC","KSS","KR","LB","LLL","LH","LRCX","LM","LEG","LEN","LUK","LVLT","LLY","LNC","LLTC","LMT","L",
                                  "LOW","LYB","MTB","MAC","M","MRO","MAR","MMC","MLM","MAS","MA","MAT","MKC","MCD","MHFI","MCK",
                                  "MJN","MDT","MRK","MET","MCHP","MU","MSFT","MHK","TAP","MDLZ","MON","MNST","MCO","MS","MSI","MUR",
                                  "MYL","NDAQ","NOV","NTAP","NFLX","NWL","NFX","NEM","NEE","NKE","NI","NBL","JWN",
                                  "NSC","NTRS","NOC","NRG","NUE","NVDA","ORLY","OXY","OMC","OKE","ORCL","OI","PCAR","PH","PDCO","PAYX",
                                  "PNR","PBCT","POM","PEP","PKI","PRGO","PFE","PCG","PM","PNW","PXD","PBI","PNC","RL","PPG","PPL","PX","PCLN",
                                  "PFG","PG","PGR","PLD","PRU","PEG","PSA","PHM","PVH","QCOM","PWR","DGX","RRC","RTN","O","RHT","REGN","RF",
                                  "RSG","RAI","RHI","ROK","COL","ROP","ROST","RCL","R","CRM","SNDK","SCG","SLB","SNI","STX","SEE","SRE","SHW","SIG",
                                  "SPG","SWKS","SLG","SJM","SNA","SO","LUV","SWN","SE","STJ","SWK","SPLS","SBUX","HOT","STT","SRCL","SYK","STI","SYMC",
                                  "SYY","TROW","TGT","TEL","TE","TGNA","THC","TDC","TSO","TXN","TXT","BK","CLX","KO","HSY","MOS","TRV","DIS",
                                  "TMO","TIF","TWC","TWX","TJX","TMK","TSS","TSCO","RIG","FOX","TYC","TSN","USB","UA","UNP","UAL","UNH","UPS",
                                  "URI","UTX","UHS","UNM","URBN","VFC","VLO","VAR","VTR","VRSN","VRSK","VZ","VRTX","VIAB","V","VNO","VMC","WMT",
                                  "WBA","WM","WAT","WFC","HCN","WDC","WU","WY","WHR","WFM","WMB","WEC","WYN","WYNN","XEL","XRX","XLNX",
                                  "XYL","YHOO","YUM","ZBH","ZION","ASH","UIS","TEX","MTG","DDS","IILG","SSP","NCR","MXIM","SANM","NAV","LPX",
                                  "AVP","JCP","X","SHLD")




