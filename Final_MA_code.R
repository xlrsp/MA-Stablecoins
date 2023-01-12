# Set Working Directory
getwd()
setwd(dir = "C:/Users/Rapha/Dropbox/HSG/Master/MA/Data work/Data Yahoo 12-11-22")
set.seed(123)

# Required packages
library(stringr)
library(data.table)
library(rlang)
library(plyr)
library(dplyr)
library(tseries)
library(ggplot2)
library(ggpubr)
library(car)
library(FSA)
library(nparcomp)
library(ggstatsplot)
library(crypto2)
library(geckor)
library(stargazer)
library(lfe)
library(plm)
library(plot3D)
library(rgl)
library(lmtest)

# Excel files for each coin were downloaded manually from yahoo.com to the working directory and then imported to R:

  # Retrieve all filenames
  list.filenames <- list.files(path="C:/Users/Rapha/Dropbox/HSG/Master/MA/Data work/Data Yahoo 12-11-22", pattern="*.csv")

  # Create an empty list that will serve as a container to receive the incoming files
  list.data<-list()

  # Create a loop to read in data
  for (i in 1:length(list.filenames))
    {
     list.data[[i]]<-read.csv(list.filenames[i])
    }

##################################################################################################################
##################################          DATA PREPARATION 1: YAHOO        #####################################
##################################################################################################################

# Add the ticker names to the list by removing the "-USD.csv" ending
names(list.data)<-str_remove_all(list.filenames, "-USD.csv")

# Safe a vector of the ticker names for later use
TICKERS <- names(list.data)

# Combine all files into one dataframe, with additional column specifying the coin name
all_data <- rbindlist(list.data, idcol = TRUE)
colnames(all_data) <- c("Ticker", "Date", "Open", "High", "Low", "Close", "Adj.Close","Volume")

# Assign adequate data type
all_data$Date = as.Date(all_data$Date, "%Y-%m-%d")
all_data$Open = as.numeric(all_data$Open)
all_data$High = as.numeric(all_data$High)
all_data$Low = as.numeric(all_data$Low)
all_data$Close = as.numeric(all_data$Close)
all_data$Adj.Close = as.numeric(all_data$Adj.Close)
all_data$Volume = as.numeric(all_data$Volume)

# Design-type classification
all_data <- all_data %>%
  mutate(Design = case_when(
    Ticker %in% c("BUSD", "GUSD", "HUSD", "TUSD", "USDC", "USDK", "USDT",  "USDP") ~ "Tokenized",
    Ticker %in% c("DAI", "EOSDT", "LUSD", "MIM", "MUSD", "OUSD", "RSV", "SUSD", "USDX", "VAI", "ALUSD", "FRAX", "CUSD", "USDS",  "USDP1", "FEI") ~ "Onchain",
    Ticker %in% c("USTC", "SBD", "USDN", "TOR") ~ "Algorithmic",
  ))

# Calculate daily close-to-close returns and GK measures. 
data_base <- ddply(all_data, .(Ticker), summarize, 
                   Date = Date,
                   Close = Close,    
                   Return = c(NA,diff(log(Close))),
                   GK = sqrt((0.5 * (log(High/Low)^2) - (2*log(2)-1)*(log(Close/Open)^2))),
                   Volume = Volume,
                   Design = Design
)

# Limit data to Sept 30th 2022.
data_base <- data_base[data_base$Date <="2022-09-30",] 

# Remove NA's (i.e. first observation day for each ticker. Final base data for analysis.)

  # Show which datapoints will be excluded
  missing_values <- complete.cases(data_base)
  data_base$missing <- missing_values
  missing_datapoints_check <- ddply(data_base, .(Ticker), summarize,n_missing = sum(missing=="FALSE"))
  missing_datapoints_check

  # Safe file: write.csv(data_base[data_base$missing =="FALSE",], "missing_datapoints", row.names = F)

  # Exlude NAs
  data_base <- data_base[complete.cases(data_base), ] 

##################################################################################################################
#########  DATA PREPARATION 2: Load CMC/CG data for Marketcap (only relevant as of panel regression)   ########### 
##################################################################################################################

# Show list of all cryptocurrencies in CMC
all_crypto <- crypto_list(only_active = FALSE)
all_crypto

# Show info on available data
coin_info <- crypto_info(all_crypto, limit=3, finalWait=FALSE)
coin_info

# Download historic data for coins in scope from CMC
ALUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="alchemix-usd",], finalWait=FALSE)
BUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="binance-usd",], finalWait=FALSE)
CUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="celo-dollar",], finalWait=FALSE)
DAI <- crypto_history(coin_list = all_crypto[all_crypto$slug=="multi-collateral-dai",], finalWait=FALSE)
EOSDT <- crypto_history(coin_list = all_crypto[all_crypto$slug=="eosdt",], finalWait=FALSE)
FEI <- crypto_history(coin_list = all_crypto[all_crypto$slug=="fei-usd",], finalWait=FALSE)
FRAX <- crypto_history(coin_list = all_crypto[all_crypto$slug=="frax",], finalWait=FALSE)
GUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="gemini-dollar",], finalWait=FALSE)
HUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="husd",], finalWait=FALSE)
LUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="liquity-usd",], finalWait=FALSE)
MIM <- crypto_history(coin_list = all_crypto[all_crypto$slug=="magic-internet-money",], finalWait=FALSE)
MUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="mstable-usd",], finalWait=FALSE)
OUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="origin-dollar",], finalWait=FALSE)
RSV <- crypto_history(coin_list = all_crypto[all_crypto$slug=="reserve",], finalWait=FALSE)
SBD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="steem-dollars",], finalWait=FALSE)
SUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="susd",], finalWait=FALSE)
TOR <- crypto_history(coin_list = all_crypto[all_crypto$slug=="tor",], finalWait=FALSE)
TUSD <- crypto_history(coin_list = all_crypto[all_crypto$slug=="trueusd",], finalWait=FALSE)
USDC <- crypto_history(coin_list = all_crypto[all_crypto$slug=="usd-coin",], finalWait=FALSE)
USDK <- crypto_history(coin_list = all_crypto[all_crypto$slug=="usdk",], finalWait=FALSE)
USDN <- crypto_history(coin_list = all_crypto[all_crypto$slug=="neutrino-usd",], finalWait=FALSE)
USDP <- crypto_history(coin_list = all_crypto[all_crypto$slug=="paxos-standard",], finalWait=FALSE)
USDS <- crypto_history(coin_list = all_crypto[all_crypto$slug=="stableusd",], finalWait=FALSE)
USDT <- crypto_history(coin_list = all_crypto[all_crypto$slug=="tether",], finalWait=FALSE)
USDX <- crypto_history(coin_list = all_crypto[all_crypto$slug=="usdx-kava",], finalWait=FALSE)
USTC <- crypto_history(coin_list = all_crypto[all_crypto$slug=="terrausd",], finalWait=FALSE)
VAI <- crypto_history(coin_list = all_crypto[all_crypto$slug=="vai",], finalWait=FALSE)

# Filter and rename relevant data
N <- c("Ticker","Name", "MC", "Date")
ALUSD <- data.frame(ALUSD$symbol, ALUSD$name, ALUSD$market_cap, ALUSD$time_open); names(ALUSD) <- N 
BUSD <- data.frame(BUSD$symbol, BUSD$name, BUSD$market_cap, BUSD$time_open); names(BUSD) <- N 
CUSD <- data.frame(CUSD$symbol, CUSD$name, CUSD$market_cap, CUSD$time_open); names(CUSD) <- N
DAI <- data.frame(DAI$symbol, DAI$name, DAI$market_cap, DAI$time_open); names(DAI) <- N
EOSDT <- data.frame(EOSDT$symbol, EOSDT$name, EOSDT$market_cap, EOSDT$time_open); names(EOSDT) <- N
FEI <- data.frame(FEI$symbol, FEI$name, FEI$market_cap, FEI$time_open); names(FEI) <- N
FRAX <- data.frame(FRAX$symbol, FRAX$name, FRAX$market_cap, FRAX$time_open); names(FRAX) <- N
GUSD <- data.frame(GUSD$symbol, GUSD$name, GUSD$market_cap, GUSD$time_open); names(GUSD) <- N
HUSD <- data.frame(HUSD$symbol, HUSD$name, HUSD$market_cap, HUSD$time_open); names(HUSD) <- N
LUSD <- data.frame(LUSD$symbol, LUSD$name, LUSD$market_cap, LUSD$time_open); names(LUSD) <- N
MIM <- data.frame(MIM$symbol, MIM$name, MIM$market_cap, MIM$time_open); names(MIM) <- N
MUSD <- data.frame(MUSD$symbol, MUSD$name, MUSD$market_cap, MUSD$time_open); names(MUSD) <- N
OUSD <- data.frame(OUSD$symbol, OUSD$name, OUSD$market_cap, OUSD$time_open); names(OUSD) <- N
RSV <- data.frame(RSV$symbol, RSV$name, RSV$market_cap, RSV$time_open); names(RSV) <- N
SBD <- data.frame(SBD$symbol, SBD$name, SBD$market_cap, SBD$time_open); names(SBD) <- N
SUSD <- data.frame(SUSD$symbol, SUSD$name, SUSD$market_cap, SUSD$time_open); names(SUSD) <- N
TOR <- data.frame(TOR$symbol, TOR$name, TOR$market_cap, TOR$time_open); names(TOR) <- N
TUSD <- data.frame(TUSD$symbol, TUSD$name, TUSD$market_cap, TUSD$time_open); names(TUSD) <- N
USDC <- data.frame(USDC$symbol, USDC$name, USDC$market_cap, USDC$time_open); names(USDC) <- N
USDK <- data.frame(USDK$symbol, USDK$name, USDK$market_cap, USDK$time_open); names(USDK) <- N
USDN <- data.frame(USDN$symbol, USDN$name, USDN$market_cap, USDN$time_open); names(USDN) <- N
USDP <- data.frame(USDP$symbol, USDP$name, USDP$market_cap, USDP$time_open); names(USDP) <- N
USDS <- data.frame(USDS$symbol, USDS$name, USDS$market_cap, USDS$time_open); names(USDS) <- N
USDT <- data.frame(USDT$symbol, USDT$name, USDT$market_cap, USDT$time_open); names(USDT) <- N
USDX <- data.frame(USDX$symbol, USDX$name, USDX$market_cap, USDX$time_open); names(USDX) <- N
USTC <- data.frame(USTC$symbol, USTC$name, USTC$market_cap, USTC$time_open); names(USTC) <- N
VAI <- data.frame(VAI$symbol, VAI$name, VAI$market_cap, VAI$time_open); names(VAI) <- N

# Remove missing values (early dates when CMC did not yet report MC data but which is available on GK)
DAI <- DAI[DAI$Date>"2020-01-20",]
MUSD <- MUSD[MUSD$Date>"2020-10-27",]
OUSD <- OUSD[OUSD$Date>"2020-10-19",]

# Retrieve Coingeko MC data for which CMC is unable to report any MC
ALUSD <- coin_history_range(coin_id = "alchemix-usd", vs_currency = "usd", from = as.POSIXct("2000-01-01"), to = as.POSIXct("2022-10-01"))
TOR <- coin_history_range(coin_id = "tor", vs_currency = "usd", from = as.POSIXct("2000-01-01"), to = as.POSIXct("2022-10-01"))
MIM <- coin_history_range(coin_id = "magic-internet-money", vs_currency = "usd", from = as.POSIXct("2000-01-01"), to = as.POSIXct("2022-10-01"))
RSV <- coin_history_range(coin_id = "reserve", vs_currency = "usd", from = as.POSIXct("2000-01-01"), to = as.POSIXct("2022-10-01"))

#! wait 1 minute to avoid call failure due to too many requests (RFC 6585)!

# Retrieve Coingeko data for missing MC data in (early) days in CMC
DAI_CG <- coin_history_range(coin_id = "dai", vs_currency = "usd", from = as.POSIXct("2000-01-01"), to = as.POSIXct("2020-01-20"))
MUSD_CG <- coin_history_range(coin_id = "musd", vs_currency = "usd", from = as.POSIXct("2000-01-01"), to = as.POSIXct("2020-10-27"))
OUSD_CG <- coin_history_range(coin_id = "origin-dollar", vs_currency = "usd", from = as.POSIXct("2000-01-01"), to = as.POSIXct("2020-10-19"))

# Format Coingeko data to match CMC data
n <- c("Date", "MC", "Ticker", "Name")
ALUSD <- data.frame(ALUSD$timestamp, ALUSD$market_cap); ALUSD$Ticker <- "ALUSD"; ALUSD$Name <- "Alchemix USD"; names(ALUSD) <- n
TOR <- data.frame(TOR$timestamp, TOR$market_cap); TOR$Ticker <- "TOR"; TOR$Name <- "TOR"; names(TOR) <- n
MIM <- data.frame(MIM$timestamp, MIM$market_cap); MIM$Ticker <- "MIM"; MIM$Name <- "Magic Internet Money"; names(MIM) <- n
RSV <- data.frame(RSV$timestamp, RSV$market_cap); RSV$Ticker <- "RSV"; RSV$Name <- "Reserve"; names(RSV) <- n
DAI_CG <- data.frame(DAI_CG$timestamp, DAI_CG$market_cap); DAI_CG$Ticker <- "DAI"; DAI_CG$Name <- "Dai"; names(DAI_CG) <- n
MUSD_CG <- data.frame(MUSD_CG$timestamp, MUSD_CG$market_cap); MUSD_CG$Ticker <- "MUSD"; MUSD_CG$Name <- "mStable USD"; names(MUSD_CG) <- n
OUSD_CG <- data.frame(OUSD_CG$timestamp, OUSD_CG$market_cap); OUSD_CG$Ticker <- "OUSD"; OUSD_CG$Name <- "Origin Dollar"; names(OUSD_CG) <- n

# Merge additional data from CG with CMC
DAI <- rbind(DAI_CG,DAI)
MUSD <- rbind(MUSD_CG,MUSD)
OUSD <- rbind(OUSD_CG,OUSD)

# Bind all data together
CMC_data <- rbind(ALUSD,BUSD,CUSD,DAI,EOSDT,FEI,FRAX,GUSD,HUSD,LUSD,MIM,MUSD,OUSD,
                  RSV,SBD,SUSD,TOR,TUSD,USDC,USDK,USDN,USDP,USDS,USDT,USDX,USTC,VAI)  

# Replace MC = 0 with NA (for the first x days after listing usually no MC reported)
CMC_data$MC[which(CMC_data$MC==0)] <- NA #removes all 0 in MC but checked manually, only affects first x days

# Merge MC data with existing data
data_base_2 <- merge(data_base,CMC_data,by=c("Date","Ticker"))
data_base_2$Volume[which(data_base$Volume<=1)] <- 1 #Volume will later be log transformed, thus vol<1 is assigned vol=1

# Safe for future use
#write.csv(CMC_data, "CMC_data", row.names = T)
#write.csv(data_base, "data_base", row.names = T)

##################################################################################################################
###########################################      DESCRIPTIVES      ############################################### 
##################################################################################################################

# Check stationarity
stationarity_stats <- ddply(data_base, .(Ticker), summarize,
                            Statistic = adf.test(Return)$statistic,
                            pValue = adf.test(Return)$p.value
)
stationarity_stats

#safe file: write.csv(stationarity_stats, "Augmented Dickey-Fuller Test", row.names = F)

# Return plots

  # For Results section

    # CC Return Tokenized
    ggplot(data=data_base[data_base$Design =="Tokenized",], aes(x = Date, y = Return, color = Ticker, group = format(Date, "%Y-%m", fill = "Ticker"))) +
    geom_line()  +   theme_test() +  scale_x_date(limits = as.Date(c("2017-06-01","2022-09-30"))) + ylim(-0.5,0.5) + theme_bw(base_size=9)
    # CC Return Onchain
    ggplot(data=data_base[data_base$Design =="Onchain",], aes(x = Date, y = Return, color = Ticker, group = format(Date, "%Y-%m"))) +
    geom_line()  +   theme_test() + scale_x_date(limits = as.Date(c("2017-06-01","2022-09-30"))) + ylim(-0.5,0.5) + theme_bw(base_size=9) + theme(legend.key.size = unit(0.3,"cm"), legend.spacing = unit(0.02,"cm"))
    # CC Return Algorithmic
    ggplot(data=data_base[data_base$Design =="Algorithmic",], aes(x = Date, y = Return, color = Ticker, group = format(Date, "%Y-%m"))) +
    geom_line()  +   theme_test() + scale_x_date(limits = as.Date(c("2017-06-01","2022-09-30")))+ ylim(-0.5,0.5)+ theme_bw(base_size=9)

  # For Discussion section

    #CC Return Tokenized
    ggplot(data=data_base[data_base$Design =="Tokenized",], aes(x = Date, y = Return, color = Ticker, group = format(Date, "%Y-%m", fill = "Ticker"))) +
    geom_line()  +   theme_test() +  scale_x_date(limits = as.Date(c("2021-01-01","2022-07-01"))) + ylim(-0.06,0.06) + theme_bw(base_size=9)
    #CC Return Algorithmic
    ggplot(data=data_base[data_base$Design =="Algorithmic",], aes(x = Date, y = Return, color = Ticker, group = format(Date, "%Y-%m"))) +
    geom_line()  +   theme_test() + scale_x_date(limits = as.Date(c("2021-01-01","20212-07-01")))+ ylim(-0.5,0.5)+ theme_bw(base_size=9)

# Check and safe acf plots of each ticker with a loop function

  # Iterate over the tickers saved in vector TICKER
  for (ticker in TICKERS) {
  # Check if the "acf_outputs" folder exists and create it if it doesn't exist
  if (!dir.exists("acf_outputs")) {
    dir.create("acf_outputs")
  }
  
  # Open a JPEG file for writing
  jpeg(filename = paste0("acf_outputs/", ticker, ".jpg"))
  
  # Apply the acf() function to a ticker
  acf(data_base[data_base$Ticker == ticker,]$Return, type = "correlation", main = ticker)
  
  # Close the JPEG file
  dev.off()
}

# Create function to calculate AC-corrected SD of returns and other variables for a specified data(sub)set
calculate_variables <- function(data) {
  ddply(data, .(Ticker), dplyr::summarize, 
        obs = length(Return),
        Start_Date = min(Date),
        End_Date = max(Date),
        last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", plot = FALSE,lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                               tail(which(abs(acf(Return, type = "correlation", plot = FALSE, lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
        nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
        neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                       (1+2*if (nc == 0) {0} else if (nc == 1) {acf(Return, type = "correlation", plot = FALSE, lag.max = length(Return))$acf[1]^2} else {
                         sum(acf(Return, type = "correlation", plot = FALSE, lag.max = length(Return))$acf[1:nc]^2)})),
        sd_return_AC_corrected =  sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2)),
        Vol = mean(Volume, na.rm=TRUE),
        MC = if("MC" %in% names(data)) mean(MC, na.rm=TRUE) else NA,
        Design=unique(Design))
}

# Apply the calculate_variables function to the data_base
data_overview <- calculate_variables(data_base)

# Filter for relevant data and transform SD measure
data_overview <- ddply(data_overview, .(Ticker), summarize, 
                       Obs = obs,
                       Start_Date = Start_Date,
                       End_Date = End_Date,
                       sd_return_AC_corrected_nt = sd_return_AC_corrected, #"_nt"=non-transformed
                       sd_return_AC_corrected = -(1/sd_return_AC_corrected), 
                       Volume = Vol,
                       Design=unique(Design)
)

# Show overview ordered by SD
data_overview[order(data_overview[,5] ),]

#Safe file: write.csv(data_overview[order(data_overview[,5] ),], "data_overview", row.names = F)

# Boxplot
ggboxplot(data_overview, x = "Design", y = "sd_return_AC_corrected_nt", 
          color = "Design", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#99FF99"),
          order = c("Tokenized", "Onchain", "Algorithmic"),
          ylab = "SD", 
          #ylim = c(0,0.15),
) + theme(legend.position = "none") + theme_bw(base_size=9)


##################################################################################################################
########################################      ANOVA VS KRUKSAL       #############################################
##################################################################################################################

                                          #############################
##########################################   CONDITIONS FOR ANOVA MET?  ##########################################
                                          #############################

#a) Normality?
shapiro.test(data_overview$sd_return_AC_corrected_nt)       #p-value = 7.032e-07 / non-normal
ks.test(data_overview$sd_return_AC_corrected_nt, "pnorm")   #p-value = 8.33e-07 / non-normal

#after transformation
shapiro.test(data_overview$sd_return_AC_corrected)       #p-value = 0.008529 / non-normal
ks.test(data_overview$sd_return_AC_corrected, "pnorm")   #p-value < 2.2e-16 / non-normal

#b) Homogenity? 
fligner.test(sd_return_AC_corrected_nt ~ Design, data = data_overview)  #p-value = 0.002409 / non-homogenous
leveneTest(sd_return_AC_corrected_nt ~ Design, data = data_overview)    #Pr(>F) = 0.002072 / non-homogenous

#after transformation 
fligner.test(sd_return_AC_corrected ~ Design, data = data_overview) #p-value = 0.8866 / homogenous
leveneTest(sd_return_AC_corrected ~ Design, data = data_overview)   #Pr(>F) = 0.8518 / homogenous

                                          #############################
############################################   NON-PARAMETRIC TEST?  #############################################
                                          #############################

# Kruskal-Wallis test for differences between groups
kruskal.test(sd_return_AC_corrected ~ Design, data = data_overview) #p-value = 0.005241

# Exact p-values (#of observations table)
obs_per_group <- data.frame(
  c("Onchain", "Tokenized", "Algorithmic"),
  c(sum(data_overview$Design=="Onchain"),
    sum(data_overview$Design=="Tokenized"),
    sum(data_overview$Design=="Algorithmic"))
) ; colnames(obs_per_group) <- c("Design", "#obs")
obs_per_group

print("Exact p-values for 15-8-4 observations: 10%:H>4.558598, 5%:H>5.764947 1%:H>8.288757")

##################################################################################################################
########################################       ROBUSTNES TESTS       #############################################
##################################################################################################################

                                          #############################
###########################################        BOOTSTRAP        ##############################################
                                          #############################

# Create a separate dataframe to not mix-up data
data_boot <- data.frame(data_overview$Design, data_overview$sd_return_AC_corrected) 
names(data_boot) <- c("Design", "SD_b")

# Deducting group mean for each sample
grpA = data_boot$SD_b[data_boot$Design=="Algorithmic"]-mean(data_boot$SD_b[data_boot$Design=="Algorithmic"])
grpO = data_boot$SD_b[data_boot$Design=="Onchain"]-mean(data_boot$SD_b[data_boot$Design=="Onchain"])
grpT = data_boot$SD_b[data_boot$Design=="Tokenized"]-mean(data_boot$SD_b[data_boot$Design=="Tokenized"])

# Pool the mean-centered samples.
designeu <- c("A","A","A","A","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")

# Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=4, replace=T)
  groupO = sample(boot_data$SD, size=15, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview))$`F value`[1]), na.rm = TRUE)
p_bootvalue #0.0025
quantile(Fstar_real,1-0.05)

                                              #############################
##############################################   ALT. VOLATILITY ESTIMATOR  ######################################
                                              #############################

# Create function to calculate AC-corrected SD of daily GK's and other variables for a specified data(sub)set
calculate_variables_GK <- function(data) {
  ddply(data, .(Ticker), dplyr::summarize, 
                          obs = length(GK),
                          Start_Date = min(Date),
                          End_Date = max(Date),
                          last_sign_lag = ifelse(identical(tail(which(abs(acf(GK, type = "correlation", plot = FALSE,
                                                lag.max = length(GK))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(GK))),1),integer(0))==TRUE,0, #if there is no significant lag (tail function returns a vector of lenght 0), the last sign. lag is equal 0
                                                        tail(which(abs(acf(GK, type = "correlation", plot = FALSE,
                                                                           lag.max = length(GK))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(GK))),1)),
                          nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
                          neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                                         (1+2*if (nc == 0) {0} else if (nc == 1) {acf(GK, type = "correlation", plot = FALSE, lag.max = length(GK))$acf[1]^2} else {
                                           sum(acf(GK, type = "correlation", plot = FALSE, lag.max = length(GK))$acf[1:nc]^2)})),
                          sd_GK_AC_corrected =  sqrt((neff/(obs*(neff-1)))*sum((GK - mean(GK))^2)), 
                          Design=unique(Design)
)
}

# Apply the calculate_variables_GK function to the data_base
data_overview_GK <- calculate_variables_GK(data_base)

# Kruskal-Wallis test to check differences between groups
kruskal.test(sd_GK_AC_corrected ~ Design, data = data_overview_GK) #p-value = 0.0004411

# Exact p-values (#of observations table)
obs_per_group_GK <- data.frame(
  c("Onchain", "Tokenized", "Algorithmic"),
  c(sum(data_overview_GK$Design=="Onchain"),
    sum(data_overview_GK$Design=="Tokenized"),
    sum(data_overview_GK$Design=="Algorithmic"))
); colnames(obs_per_group_GK) <- c("Design", "#obs")
obs_per_group_GK

print("Exact p-values for 15-8-4 observations: 10%:H>4.558598, 5%:H>5.764947 1%:H>8.288757")

##################################################################################################################
############################################      POST-HOC TESTS:      ########################################### 
##################################################################################################################

#Info: p-values are adjusted for multiple comparisons

# Pairwise Wilcox rank sum test with Holm’s correction
pairwise.wilcox.test(data_overview$sd_return_AC_corrected, data_overview$Design, p.adjust.method = "holm") #OA-p-value=0.5304,TA-p-value=0.0323,TO-p-value=0.0057

# Dunn’s test, (Dunn 1964) with Benjamin and Hochberg correction
dunnTest(sd_return_AC_corrected~Design, data=data_overview, method = "bh") #OA-adj.p-value=0.55298033,TA-adj.p-value=0.01306787,TO-adj.p-value=0.01096315

# Nonparametric rank-based multiple contrast test

  # Define contrast matrix
  cmatrix <- rbind(c(-0.5, -0.5, 1), c(-1,1,0), c(-1,0,1), c(0,-1,1))
  # Apply Test as developped by Konietschke et al. (2015)
  nparcomp(sd_return_AC_corrected~Design, data=data_overview, asy.method="mult.t", plot.simci = TRUE, type="UserDefined", contrast.matrix = cmatrix)

##################################################################################################################
#######################################   SUMMARY: Boxplot+Kruskal+Dunn    #######################################
##################################################################################################################

ggbetweenstats(
  data = data_overview,
  x = Design,
  y = sd_return_AC_corrected,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  p.adjust.method = "BH",  
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  ylab="SD*"
)

##################################################################################################################
#############################               SLIDING WINDOW ANALYSIS             ################################## 
##################################################################################################################

                                           #############################
############################################    Window Size = 6 months   #########################################
                                           #############################

# Subset data to 6-month windows
window1 <- data_base[data_base$Date >= "2020-01-01" & data_base$Date <="2020-06-30",] 
window2 <- data_base[data_base$Date >= "2020-04-01" & data_base$Date <="2020-09-30",] 
window3 <- data_base[data_base$Date >= "2020-07-01" & data_base$Date <="2020-12-31",] 
window4 <- data_base[data_base$Date >= "2020-10-01" & data_base$Date <="2021-03-31",] 
window5 <- data_base[data_base$Date >= "2021-01-01" & data_base$Date <="2021-06-30",] 
window6 <- data_base[data_base$Date >= "2021-04-01" & data_base$Date <="2021-09-30",] 
window7 <- data_base[data_base$Date >= "2021-07-01" & data_base$Date <="2021-12-31",]
window8 <- data_base[data_base$Date >= "2021-10-01" & data_base$Date <="2022-03-31",]
window9 <- data_base[data_base$Date >= "2022-01-01" & data_base$Date <="2022-06-30",]
window10 <- data_base[data_base$Date >= "2022-04-01" & data_base$Date <="2022-09-30",]

# Create a list of the window variables. Create a list of window names.
windows_6m <- list(window1, window2, window3, window4, window5, window6, window7, window8, window9, window10)
window_names_6m <- c("Window 1", "Window 2","Window 3","Window 4","Window 5","Window 6","Window 7","Window 8", "Window 9","Window 10")

# Use the map() function to calculate the summary values for each window with the previousely created calculate_variables function
data_overview_6m <- purrr::map(windows_6m, calculate_variables)

# Data overviews ordered by the volatility measure
data_overview_6m = setNames(lapply(data_overview_6m, function(x) x[order(x[,6]), ]), window_names_6m)
data_overview_6m

# Test of homogeinity of variance after log transformation
fligner_results_6m <- setNames(purrr::map(data_overview_6m, function(x) fligner.test(log(sd_return_AC_corrected) ~ Design, data = x)), window_names_6m)
fligner_results_6m
levene_results_6m <- setNames(purrr::map(data_overview_6m, function(x) leveneTest(log(sd_return_AC_corrected) ~ Design, data = x)), window_names_6m)
levene_results_6m

# Kruskal-Tests Windows 1-10
p_values_6m <- vector("numeric", length = length(data_overview_6m))
statistics_6m <- vector("numeric", length = length(data_overview_6m))

for (i in 1:length(data_overview_6m)) {
  result <- kruskal.test(sd_return_AC_corrected ~ Design, data = data_overview_6m[[i]])
  p_values_6m[i] <- result$p.value
  statistics_6m[i] <- result$statistic
}
Kruskal_6m <- data.frame(
  window_names_6m,
  p_values_6m,
  statistics_6m
)
colnames(Kruskal_6m) <- c("Year", "p-value", "statistic")
Kruskal_6m

# Show number of observations per group
obs_per_group <- data.frame(
  c("Onchain", "Tokenized", "Algorithmic"),
  c(sum(data_overview_6m[[1]]$Design=="Onchain"),sum(data_overview_6m[[1]]$Design=="Tokenized"),sum(data_overview_6m[[1]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[2]]$Design=="Onchain"),sum(data_overview_6m[[2]]$Design=="Tokenized"),sum(data_overview_6m[[2]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[3]]$Design=="Onchain"),sum(data_overview_6m[[3]]$Design=="Tokenized"),sum(data_overview_6m[[3]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[4]]$Design=="Onchain"),sum(data_overview_6m[[4]]$Design=="Tokenized"),sum(data_overview_6m[[4]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[5]]$Design=="Onchain"),sum(data_overview_6m[[5]]$Design=="Tokenized"),sum(data_overview_6m[[5]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[6]]$Design=="Onchain"),sum(data_overview_6m[[6]]$Design=="Tokenized"),sum(data_overview_6m[[6]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[7]]$Design=="Onchain"),sum(data_overview_6m[[7]]$Design=="Tokenized"),sum(data_overview_6m[[7]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[8]]$Design=="Onchain"),sum(data_overview_6m[[8]]$Design=="Tokenized"),sum(data_overview_6m[[8]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[9]]$Design=="Onchain"),sum(data_overview_6m[[9]]$Design=="Tokenized"),sum(data_overview_6m[[9]]$Design=="Algorithmic")),
  c(sum(data_overview_6m[[10]]$Design=="Onchain"),sum(data_overview_6m[[10]]$Design=="Tokenized"),sum(data_overview_6m[[10]]$Design=="Algorithmic"))
); colnames(obs_per_group) <- c("Design","obs_w1","obs_w2","obs_w3","obs_w4","obs_w5","obs_w6", "obs_w7","obs_w8", "obs_w9","obs_w10")
obs_per_group

# Wilcox test
apply_WT <- function(data) {
  pairwise.wilcox.test(data$sd_return_AC_corrected, data$Design, p.adjust.method = "holm" )
} ;Wilcox_6m <- purrr::map(data_overview_6m,apply_WT)
Wilcox_6m

# Dunns test
apply_DT <- function(data) {
  dunnTest(data$sd_return_AC_corrected ~ data$Design, method = "bh")
}; Dunn_6m <- purrr::map(data_overview_6m,apply_DT)
Dunn_6m

# Non-parametric multiple contrast test (effect direction)

  # Apply nparcomp as developed by Konietschke et al. (2015)
  apply_MCT <- function(dataset) {
  nparcomp(sd_return_AC_corrected~Design, data=dataset, asy.method="mult.t", plot.simci = TRUE, type="UserDefined", contrast.matrix = cmatrix)$Analysis
  }; MCT_6m <- purrr::map(data_overview_6m,apply_MCT)
  MCT_6m

                                             #############################
############################################    Window Size = 12 months   #########################################
                                             #############################

# Subset data to 1-year windows
windowa <- data_base[data_base$Date >= "2020-01-01" & data_base$Date <="2020-12-31",] 
windowb <- data_base[data_base$Date >= "2020-04-01" & data_base$Date <="2021-03-31",] 
windowc <- data_base[data_base$Date >= "2020-07-01" & data_base$Date <="2021-06-30",] 
windowd <- data_base[data_base$Date >= "2020-10-01" & data_base$Date <="2021-09-30",] 
windowe <- data_base[data_base$Date >= "2021-01-01" & data_base$Date <="2021-12-31",] 
windowf <- data_base[data_base$Date >= "2021-04-01" & data_base$Date <="2022-03-31",] 
windowg <- data_base[data_base$Date >= "2021-07-01" & data_base$Date <="2022-06-30",]
windowh<- data_base[data_base$Date >= "2021-10-01" & data_base$Date <="2022-09-30",]

# Create a list of the window variables. Create a list of window names.
windows_1y <- list(windowa, windowb, windowc, windowd, windowe, windowf, windowg, windowh)
window_names_1y <- c("Window a", "Window b","Window c","Window d","Window e","Window f","Window g","Window h")

# Use the map() function to calculate the summary values for each window with the calculate_variables function
data_overview_1y <- purrr::map(windows_1y, calculate_variables)

# Data overviews ordered by the volatility measure
data_overview_1y = setNames(lapply(data_overview_1y, function(x) x[order(x[,6]), ]), window_names_1y)
data_overview_1y

# Test of homogeinity of variance after log transformation
fligner_results_1y <- setNames(purrr::map(data_overview_1y, function(x) fligner.test(log(sd_return_AC_corrected) ~ Design, data = x)), window_names_1y)
fligner_results_1y
levene_results_1y <- setNames(purrr::map(data_overview_1y, function(x) leveneTest(log(sd_return_AC_corrected) ~ Design, data = x)), window_names_1y)
levene_results_1y

# Kruskal-Tests Windows a-h
p_values_1y <- vector("numeric", length = length(data_overview_1y))
statistics_1y <- vector("numeric", length = length(data_overview_1y))

for (i in 1:length(data_overview_1y)) {
  result <- kruskal.test(sd_return_AC_corrected ~ Design, data = data_overview_1y[[i]])
  p_values_1y[i] <- result$p.value
  statistics_1y[i] <- result$statistic
}
Kruskal_1y <- data.frame(
  window_names_1y,
  p_values_1y,
  statistics_1y
)
colnames(Kruskal_1y) <- c("Year", "p-value", "statistic")
Kruskal_1y

# Show number of observations per group
obs_per_group_1y <- data.frame(
  c("Onchain", "Tokenized", "Algorithmic"),
  c(sum(data_overview_1y[[1]]$Design=="Onchain"),sum(data_overview_1y[[1]]$Design=="Tokenized"),sum(data_overview_1y[[1]]$Design=="Algorithmic")),
  c(sum(data_overview_1y[[2]]$Design=="Onchain"),sum(data_overview_1y[[2]]$Design=="Tokenized"),sum(data_overview_1y[[2]]$Design=="Algorithmic")),
  c(sum(data_overview_1y[[3]]$Design=="Onchain"),sum(data_overview_1y[[3]]$Design=="Tokenized"),sum(data_overview_1y[[3]]$Design=="Algorithmic")),
  c(sum(data_overview_1y[[4]]$Design=="Onchain"),sum(data_overview_1y[[4]]$Design=="Tokenized"),sum(data_overview_1y[[4]]$Design=="Algorithmic")),
  c(sum(data_overview_1y[[5]]$Design=="Onchain"),sum(data_overview_1y[[5]]$Design=="Tokenized"),sum(data_overview_1y[[5]]$Design=="Algorithmic")),
  c(sum(data_overview_1y[[6]]$Design=="Onchain"),sum(data_overview_1y[[6]]$Design=="Tokenized"),sum(data_overview_1y[[6]]$Design=="Algorithmic")),
  c(sum(data_overview_1y[[7]]$Design=="Onchain"),sum(data_overview_1y[[7]]$Design=="Tokenized"),sum(data_overview_1y[[7]]$Design=="Algorithmic")), 
  c(sum(data_overview_1y[[8]]$Design=="Onchain"),sum(data_overview_1y[[8]]$Design=="Tokenized"),sum(data_overview_1y[[8]]$Design=="Algorithmic"))
); colnames(obs_per_group_1y) <- c("Design","obs_wa","obs_wb","obs_wc","obs_wd","obs_we","obs_wf","obs_wg","obs_wh")
obs_per_group_1y

# Wilcox test
Wilcox_1y <- purrr::map(data_overview_1y,apply_WT)
Wilcox_1y

# Dunns test
Dunn_1y <- purrr::map(data_overview_1y,apply_DT)
Dunn_1y

# Non-parametric multiple contrast test (effect direction)

  # Apply nparcomp as developed by Konietschke et al. (2015)
  MCT_1y <- purrr::map(data_overview_1y,apply_MCT)
  MCT_1y


##################################################################################################################
#############################       Sliding Windows Analysis (ROBUSTNESS)        ################################# 
##################################################################################################################

                                           #############################
############################################   Window Size = 6 months  ###########################################
                                           #############################

# Use the map() function to calculate the summary values for each window with the calculate_variables_GK function
data_overview_6m_GK <- purrr::map(windows_6m, calculate_variables_GK)

# Data overviews ordered by the volatility measure
data_overview_6m_GK = setNames(lapply(data_overview_6m_GK, function(x) x[order(x[,6]), ]), window_names_6m)
data_overview_6m_GK

# Test of homogeinity of variance after log transformation
fligner_results_6m_GK <- setNames(purrr::map(data_overview_6m_GK, function(x) fligner.test(log(sd_GK_AC_corrected) ~ Design, data = x)), window_names_6m)
fligner_results_6m_GK
levene_results_6m_GK <- setNames(purrr::map(data_overview_6m_GK, function(x) leveneTest(log(sd_GK_AC_corrected) ~ Design, data = x)), window_names_6m)
levene_results_6m_GK

# Kruskal-Tests Windows 1-10
p_values_6m_GK <- vector("numeric", length = length(data_overview_6m_GK))
statistics_6m_GK <- vector("numeric", length = length(data_overview_6m_GK))

for (i in 1:length(data_overview_6m_GK)) {
  result <- kruskal.test(sd_GK_AC_corrected ~ Design, data = data_overview_6m_GK[[i]])
  p_values_6m_GK[i] <- result$p.value
  statistics_6m_GK[i] <- result$statistic
}
Kruskal_6m_GK <- data.frame(
  window_names_6m,
  p_values_6m_GK,
  statistics_6m_GK
)
colnames(Kruskal_6m_GK) <- c("Year", "p-value", "statistic")
Kruskal_6m_GK

# Show number of observations per group
obs_per_group <- data.frame(
  c("Onchain", "Tokenized", "Algorithmic"),
  c(sum(data_overview_6m_GK[[1]]$Design=="Onchain"),sum(data_overview_6m_GK[[1]]$Design=="Tokenized"),sum(data_overview_6m_GK[[1]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[2]]$Design=="Onchain"),sum(data_overview_6m_GK[[2]]$Design=="Tokenized"),sum(data_overview_6m_GK[[2]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[3]]$Design=="Onchain"),sum(data_overview_6m_GK[[3]]$Design=="Tokenized"),sum(data_overview_6m_GK[[3]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[4]]$Design=="Onchain"),sum(data_overview_6m_GK[[4]]$Design=="Tokenized"),sum(data_overview_6m_GK[[4]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[5]]$Design=="Onchain"),sum(data_overview_6m_GK[[5]]$Design=="Tokenized"),sum(data_overview_6m_GK[[5]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[6]]$Design=="Onchain"),sum(data_overview_6m_GK[[6]]$Design=="Tokenized"),sum(data_overview_6m_GK[[6]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[7]]$Design=="Onchain"),sum(data_overview_6m_GK[[7]]$Design=="Tokenized"),sum(data_overview_6m_GK[[7]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[8]]$Design=="Onchain"),sum(data_overview_6m_GK[[8]]$Design=="Tokenized"),sum(data_overview_6m_GK[[8]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[9]]$Design=="Onchain"),sum(data_overview_6m_GK[[9]]$Design=="Tokenized"),sum(data_overview_6m_GK[[9]]$Design=="Algorithmic")),
  c(sum(data_overview_6m_GK[[10]]$Design=="Onchain"),sum(data_overview_6m_GK[[10]]$Design=="Tokenized"),sum(data_overview_6m_GK[[10]]$Design=="Algorithmic"))
); colnames(obs_per_group) <- c("Design","obs_w1","obs_w2","obs_w3","obs_w4","obs_w5","obs_w6", "obs_w7","obs_w8", "obs_w9","obs_w10")
obs_per_group

                                            #############################
############################################   Window Size = 12 months   #########################################
                                            #############################

# Use the map() function to calculate the summary values for each window with the calculate_variables_GK function
data_overview_1y_GK <- purrr::map(windows_1y, calculate_variables_GK)

# Data overviews ordered by the volatility measure
data_overview_1y_GK = setNames(lapply(data_overview_1y_GK, function(x) x[order(x[,6]), ]), window_names_1y)
data_overview_1y_GK

# Test of homogeinity of variance after log transformation
fligner_results_1y_GK <- setNames(purrr::map(data_overview_1y_GK, function(x) fligner.test(log(sd_GK_AC_corrected) ~ Design, data = x)), window_names_1y)
fligner_results_1y_GK

# Kruskal-Tests Windows a-h
p_values_1y_GK <- vector("numeric", length = length(data_overview_1y_GK))
statistics_1y_GK <- vector("numeric", length = length(data_overview_1y_GK))

for (i in 1:length(data_overview_1y_GK)) {
  result <- kruskal.test(sd_GK_AC_corrected ~ Design, data = data_overview_1y_GK[[i]])
  p_values_1y_GK[i] <- result$p.value
  statistics_1y_GK[i] <- result$statistic
}
Kruskal_1y_GK <- data.frame(
  window_names_1y,
  p_values_1y_GK,
  statistics_1y_GK
)
colnames(Kruskal_1y_GK) <- c("Year", "p-value", "statistic")
Kruskal_1y_GK

# Show number of observations per group
obs_per_group_1y_GK <- data.frame(
  c("Onchain", "Tokenized", "Algorithmic"),
  c(sum(data_overview_1y_GK[[1]]$Design=="Onchain"),sum(data_overview_1y_GK[[1]]$Design=="Tokenized"),sum(data_overview_1y_GK[[1]]$Design=="Algorithmic")),
  c(sum(data_overview_1y_GK[[2]]$Design=="Onchain"),sum(data_overview_1y_GK[[2]]$Design=="Tokenized"),sum(data_overview_1y_GK[[2]]$Design=="Algorithmic")),
  c(sum(data_overview_1y_GK[[3]]$Design=="Onchain"),sum(data_overview_1y_GK[[3]]$Design=="Tokenized"),sum(data_overview_1y_GK[[3]]$Design=="Algorithmic")),
  c(sum(data_overview_1y_GK[[4]]$Design=="Onchain"),sum(data_overview_1y_GK[[4]]$Design=="Tokenized"),sum(data_overview_1y_GK[[4]]$Design=="Algorithmic")),
  c(sum(data_overview_1y_GK[[5]]$Design=="Onchain"),sum(data_overview_1y_GK[[5]]$Design=="Tokenized"),sum(data_overview_1y_GK[[5]]$Design=="Algorithmic")),
  c(sum(data_overview_1y_GK[[6]]$Design=="Onchain"),sum(data_overview_1y_GK[[6]]$Design=="Tokenized"),sum(data_overview_1y_GK[[6]]$Design=="Algorithmic")),
  c(sum(data_overview_1y_GK[[7]]$Design=="Onchain"),sum(data_overview_1y_GK[[7]]$Design=="Tokenized"),sum(data_overview_1y_GK[[7]]$Design=="Algorithmic")),
  c(sum(data_overview_1y_GK[[8]]$Design=="Onchain"),sum(data_overview_1y_GK[[8]]$Design=="Tokenized"),sum(data_overview_1y_GK[[8]]$Design=="Algorithmic"))
); colnames(obs_per_group_1y_GK) <- c("Design","obs_wa","obs_wb","obs_wc","obs_wd","obs_we","obs_wf","obs_wg","obs_wh")
obs_per_group_1y_GK

#################################################################################################################
#######################################       PANEL DATA REGRESSION       #######################################
#################################################################################################################

                                            #############################
############################################   Data prep - 1m windows   #########################################
                                            #############################

# Create windows
w1 <- data_base_2[data_base_2$Date >= "2020-01-01" & data_base_2$Date <="2020-01-31",] 
w2 <- data_base_2[data_base_2$Date >= "2020-02-01" & data_base_2$Date <="2020-02-29",] 
w3 <- data_base_2[data_base_2$Date >= "2020-03-01" & data_base_2$Date <="2020-03-31",] 
w4 <- data_base_2[data_base_2$Date >= "2020-04-01" & data_base_2$Date <="2020-04-30",] 
w5 <- data_base_2[data_base_2$Date >= "2020-05-01" & data_base_2$Date <="2020-05-31",] 
w6 <- data_base_2[data_base_2$Date >= "2020-06-01" & data_base_2$Date <="2020-06-30",] 
w7 <- data_base_2[data_base_2$Date >= "2020-07-01" & data_base_2$Date <="2020-07-31",]
w8 <- data_base_2[data_base_2$Date >= "2020-08-01" & data_base_2$Date <="2020-08-31",]
w9 <- data_base_2[data_base_2$Date >= "2020-09-01" & data_base_2$Date <="2020-09-30",]
w10 <- data_base_2[data_base_2$Date >= "2020-10-01" & data_base_2$Date <="2020-10-31",]
w11 <- data_base_2[data_base_2$Date >= "2020-11-01" & data_base_2$Date <="2020-11-30",]
w12 <- data_base_2[data_base_2$Date >= "2020-12-01" & data_base_2$Date <="2020-12-31",]
w13 <- data_base_2[data_base_2$Date >= "2021-01-01" & data_base_2$Date <="2021-01-31",] 
w14 <- data_base_2[data_base_2$Date >= "2021-02-01" & data_base_2$Date <="2021-02-28",] 
w15 <- data_base_2[data_base_2$Date >= "2021-03-01" & data_base_2$Date <="2021-03-31",] 
w16 <- data_base_2[data_base_2$Date >= "2021-04-01" & data_base_2$Date <="2021-04-30",] 
w17 <- data_base_2[data_base_2$Date >= "2021-05-01" & data_base_2$Date <="2021-05-31",] 
w18 <- data_base_2[data_base_2$Date >= "2021-06-01" & data_base_2$Date <="2021-06-30",] 
w19 <- data_base_2[data_base_2$Date >= "2021-07-01" & data_base_2$Date <="2021-07-31",]
w20 <- data_base_2[data_base_2$Date >= "2021-08-01" & data_base_2$Date <="2021-08-31",]
w21 <- data_base_2[data_base_2$Date >= "2021-09-01" & data_base_2$Date <="2021-09-30",]
w22 <- data_base_2[data_base_2$Date >= "2021-10-01" & data_base_2$Date <="2021-10-31",]
w23 <- data_base_2[data_base_2$Date >= "2021-11-01" & data_base_2$Date <="2021-11-30",]
w24 <- data_base_2[data_base_2$Date >= "2021-12-01" & data_base_2$Date <="2021-12-31",]
w25 <- data_base_2[data_base_2$Date >= "2022-01-01" & data_base_2$Date <="2022-01-31",] 
w26 <- data_base_2[data_base_2$Date >= "2022-02-01" & data_base_2$Date <="2022-02-28",] 
w27 <- data_base_2[data_base_2$Date >= "2022-03-01" & data_base_2$Date <="2022-03-31",] 
w28 <- data_base_2[data_base_2$Date >= "2022-04-01" & data_base_2$Date <="2022-04-30",] 
w29 <- data_base_2[data_base_2$Date >= "2022-05-01" & data_base_2$Date <="2022-05-31",] 
w30 <- data_base_2[data_base_2$Date >= "2022-06-01" & data_base_2$Date <="2022-06-30",] 
w31 <- data_base_2[data_base_2$Date >= "2022-07-01" & data_base_2$Date <="2022-07-31",]
w32 <- data_base_2[data_base_2$Date >= "2022-08-01" & data_base_2$Date <="2022-08-31",]
w33 <- data_base_2[data_base_2$Date >= "2022-09-01" & data_base_2$Date <="2022-09-30",]

# Create a list of the window variables. Create a list of window names.
windows_1m <- list(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20, 
                   w21, w22, w23, w24, w25, w26, w27, w28, w29, w30, w31, w32, w33)
window_names_1m <- c("Window 1", "Window 2", "Window 3", "Window 4", "Window 5", "Window 6", "Window 7", 
                     "Window 8", "Window 9", "Window 10", "Window 11", "Window 12", "Window 13", "Window 14", 
                     "Window 15", "Window 16", "Window 17", "Window 18", "Window 19", "Window 20", "Window 21", 
                     "Window 22", "Window 23", "Window 24", "Window 25", "Window 26", "Window 27", "Window 28", 
                     "Window 29", "Window 30", "Window 31", "Window 32", "Window 33")

# Use the map() function to calculate the summary values for each window with the calculate_variables function
data_overview_1m <- purrr::map(windows_1m, calculate_variables)

# Data overviews ordered by the volatility measure
data_overview_1m = setNames(lapply(data_overview_1m, function(x) x[order(x[,6]), ]), window_names_1m)

# Set up and format the panel data

  # Create an empty list to store the panel data frames
  panels_1m <- list()
  # Use a for loop to iterate over the elements of data_overview_6m
  for (i in 1:33) {
    # Create the current panel data frame
    panel <- ddply(data_overview_1m[[i]], .(Ticker), dplyr::summarize, 
                 Window = i,
                 Ticker = Ticker,
                 Volatility = sd_return_AC_corrected,
                 Volume = Vol,
                 MC = MC,
                 Design = Design)
    # Add the current panel data frame to the list
    panels_1m[[i]] <- panel
  }
  
  # Bind the panel data frames together
  panel_data_1m <- bind_rows(panels_1m)
  
  # Create function to calculate/assign relevant panel variables
  panel_vars <- function(data) { 
    ddply(data, .(Window),dplyr::summarize, 
          Window = Window,
          Volatility = log(Volatility),
          Ticker = Ticker,
          Volume = log(Volume),
          Marketcap = log(MC),
          NotAlgo = ifelse(Design=="Algorithmic",0,1),
          Design = Design,
          IB20 = case_when(Ticker %in% c("USDN", "MUSD","RSV","OUSD","CUSD","USDX","USTC","VAI","FRAX","FEI", "ALUSD","MIM","LUSD","USDS", "TOR") ~ "A",
                           Ticker %in% c("SBD", "USDT", "TUSD", "SUSD", "USDP", "USDP", "GUSD","USDC", "EOSDT", "USDK","BUSD","HUSD","DAI") ~ "B"),
          #W1=W1,W2=W2,W3=W3,W4=W4,W5=W5,W6=W6,W7=W7,W8=W8,W9=W9,W10=W10
    )
  }
  
  # Calculate/assign relevant panel variables
  panel_data_1m <- panel_vars(panel_data_1m)
  # Assing NA to inf values
  panel_data_1m$Volume[which(!is.finite(panel_data_1m$Volume))] <- NA
  panel_data_1m$Marketcap[which(!is.finite(panel_data_1m$Marketcap))] <- NA
  # Exlude NA's
  panel_data_1m <- panel_data_1m[complete.cases(panel_data_1m), ] 

                                            #############################
############################################   Data prep - 6m windows   #########################################
                                            #############################

# Recycle the windows from the sliding window analysis but use the data base which includes MC data instead

  # Windows
  window1 <- data_base_2[data_base_2$Date >= "2020-01-01" & data_base_2$Date <="2020-06-30",] 
  window2 <- data_base_2[data_base_2$Date >= "2020-04-01" & data_base_2$Date <="2020-09-30",] 
  window3 <- data_base_2[data_base_2$Date >= "2020-07-01" & data_base_2$Date <="2020-12-31",] 
  window4 <- data_base_2[data_base_2$Date >= "2020-10-01" & data_base_2$Date <="2021-03-31",] 
  window5 <- data_base_2[data_base_2$Date >= "2021-01-01" & data_base_2$Date <="2021-06-30",] 
  window6 <- data_base_2[data_base_2$Date >= "2021-04-01" & data_base_2$Date <="2021-09-30",] 
  window7 <- data_base_2[data_base_2$Date >= "2021-07-01" & data_base_2$Date <="2021-12-31",]
  window8 <- data_base_2[data_base_2$Date >= "2021-10-01" & data_base_2$Date <="2022-03-31",]
  window9 <- data_base_2[data_base_2$Date >= "2022-01-01" & data_base_2$Date <="2022-06-30",]
  window10 <- data_base_2[data_base_2$Date >= "2022-04-01" & data_base_2$Date <="2022-09-30",]

  # Create a list of the window variables. Create a list of window names.
  windows_6m <- list(window1, window2, window3, window4, window5, window6, window7, window8, window9, window10)
  window_names_6m <- c("Window 1", "Window 2","Window 3","Window 4","Window 5","Window 6","Window 7","Window 8", "Window 9","Window 10")

  # Use the map() function to calculate the summary values for each window with the calculate_variables function
  data_overview_6m <- purrr::map(windows_6m, calculate_variables)

  # Data overviews ordered by the volatility measure
  data_overview_6m = setNames(lapply(data_overview_6m, function(x) x[order(x[,6]), ]), window_names_6m)

# Set up and format the panel data

  # Create an empty list to store the panel data frames
  panels_6m <- list()

  # Use a for loop to iterate over the elements of data_overview_6m
  for (i in 1:10) {
    # Create the current panel data frame
    panel <- ddply(data_overview_6m[[i]], .(Ticker), dplyr::summarize, 
                 Window = i,
                 Ticker = Ticker,
                 Volatility = sd_return_AC_corrected,
                 Volume = Vol,
                 MC = MC,
                 Design = Design)
    # Add the current panel data frame to the list
    panels_6m[[i]] <- panel
  }

  # Bind the panel data frames together
  panel_data_6m <- bind_rows(panels_6m)
  # Summarize/Calculate relevant variables
  panel_data_6m <- panel_vars(panel_data_6m)
  # Assign NA to inf values
  panel_data_6m$Volume[which(!is.finite(panel_data_6m$Volume))] <- NA
  panel_data_6m$Marketcap[which(!is.finite(panel_data_6m$Marketcap))] <- NA
  # Exlude NA's
  panel_data_6m <- panel_data_6m[complete.cases(panel_data_6m), ] 

                                            #############################
############################################   Data prep - 12m windows   #########################################
                                            #############################

# Recycle the windows from the sliding window analysis but use the data base which includes MC data instead
  
  # Windows
  windowa <- data_base_2[data_base_2$Date >= "2020-01-01" & data_base_2$Date <="2020-12-31",] 
  windowb <- data_base_2[data_base_2$Date >= "2020-04-01" & data_base_2$Date <="2021-03-31",] 
  windowc <- data_base_2[data_base_2$Date >= "2020-07-01" & data_base_2$Date <="2021-06-30",] 
  windowd <- data_base_2[data_base_2$Date >= "2020-10-01" & data_base_2$Date <="2021-09-30",] 
  windowe <- data_base_2[data_base_2$Date >= "2021-01-01" & data_base_2$Date <="2021-12-31",] 
  windowf <- data_base_2[data_base_2$Date >= "2021-04-01" & data_base_2$Date <="2022-03-31",] 
  windowg <- data_base_2[data_base_2$Date >= "2021-07-01" & data_base_2$Date <="2022-06-30",]
  windowh <- data_base_2[data_base_2$Date >= "2021-10-01" & data_base_2$Date <="2022-09-30",]

  # Create a list of the window variables. Create a list of window names.
  windows_1y <- list(windowa, windowb, windowc, windowd, windowe, windowf, windowg, windowh)
  window_names_1y <- c("Window a", "Window b","Window c","Window d","Window e","Window f","Window g","Window h")

  # Use the map() function to calculate the summary values for each window with the calculate_variables function
  data_overview_1y <- purrr::map(windows_1y, calculate_variables)

  #Data overviews ordered by the volatility measure
  data_overview_1y = setNames(lapply(data_overview_1y, function(x) x[order(x[,6]), ]), window_names_1y)

# Set up / format the panel data

  # Create an empty list to store the panel data frames
  panels_1y <- list()

  # Use a for loop to iterate over the elements of data_overview_6m
  for (i in 1:8) {
    # Create the current panel data frame
    panel <- ddply(data_overview_1y[[i]], .(Ticker), dplyr::summarize, 
                 Window = i,
                 Ticker = Ticker,
                 Volatility = sd_return_AC_corrected,
                 Volume = Vol,
                 MC = MC,
                 Design = Design)
  
    # Add the current panel data frame to the list
    panels_1y[[i]] <- panel
  }

  # Bind the panel data frames together
  panel_data_1y <- bind_rows(panels_1y)
  # Prepare panel data
  panel_data_1y <- panel_vars(panel_data_1y)
  # Assing NA to inf values
  panel_data_1y$Volume[which(!is.finite(panel_data_1y$Volume))] <- NA
  panel_data_1y$Marketcap[which(!is.finite(panel_data_1y$Marketcap))] <- NA
  # Exlude NA's
  panel_data_1y <- panel_data_1y[complete.cases(panel_1y), ] 

  
                                          #############################
############################################   Check Assumptions   #########################################
                                          ############################# 
  
# Specify models  
  # Linear
  m1 <- felm(Volatility ~ Marketcap, data = panel_data_1m)
  # MC + Time FE
  m2 <- felm(Volatility ~ Marketcap | Window | 0 | Ticker, data = panel_data_1m)
  # MC + Design + Time FE
  m3 <- felm(Volatility ~ Marketcap + factor(Design) | Window | 0 | Ticker, data = panel_data_1m) 
  # MC + Design + Time FE + Entity FE (mulicolinearity!)
  m4 <- felm(Volatility ~ Marketcap + factor(Design) + factor(Ticker)| Window  | 0  | Ticker, data = panel_data_1m)
  # Design + Time FE
  m5 <- felm(Volatility ~ factor(Design) | Window  | 0  | Ticker, data = panel_data_1m) 
  # Sensitivity
  # MC + Design + add.independent + Time FE
  m6 <- felm(Volatility ~  Marketcap + factor(Design) + Volume + factor(IB20)| Window  | 0  | Ticker, data = panel_data_1m) 
  # MC + Volume + Time FE + Entity FE
  m7 <- felm(Volatility ~ Marketcap + Volume | Window + Ticker | 0  | Ticker, data = panel_data_1m) 
  # MC + alt.Design + alt.ind. + Time FE
  m8 <- felm(Volatility ~ Marketcap + NotAlgo + factor(IB20) | Window  | 0  | Ticker, data = panel_data_1m) 
  
#Normality
  
  # Shapiro
  shapiro.test(panel_data_1m$Volatility);shapiro.test(panel_data_1m$Marketcap);shapiro.test(panel_data_1m$Volume)
  shapiro.test(m1$residuals);shapiro.test(m2$residuals);shapiro.test(m3$residuals);shapiro.test(m4$residuals);
  shapiro.test(m5$residuals);shapiro.test(m6$residuals);shapiro.test(m7$residuals);shapiro.test(m8$residuals);
  # Shapirot tests reject normality but is highly sensitive to outliers, thus a visual inspection preferred:

  # Create functions to plot data
  normal.v.plot <- function(database,estimator,name) { ggplot(database, aes(x = estimator))+ labs(title=name)  + geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
                   stat_function(fun = dnorm, args = list(mean = mean(estimator), sd = sd(estimator)), colour="red")+theme(plot.title = element_text(hjust = 0.5))}
  normal.res.plot <- function(model_res,title) { hist(model_res, breaks=12, freq=FALSE,main=title, xlab='Residuals');
                   curve(dnorm(x, mean=mean(model_res), sd=sd(model_res)), col='red', lwd=2, add=TRUE); abline(v=0, col='red', lwd=2)}
  # Volatility
  normal.v.plot(panel_data_1m,panel_data_1m$Volatility,"Volatility")
  ggqqplot(panel_data_1m$Volatility)
  # Marketcap
  normal.v.plot(panel_data_1m,panel_data_1m$Marketcap,"Marketcap")
  ggqqplot(panel_data_1m$Marketcap)
  # Volume
  normal.v.plot(panel_data_1m,panel_data_1m$Volume,"Volume")
  ggqqplot(panel_data_1m$Volume)
  # Residuals
  normal.res.plot(m1$residuals,"m1")
  normal.res.plot(m2$residuals,"m2")
  normal.res.plot(m3$residuals,"m3")
  normal.res.plot(m4$residuals,"m4")
  normal.res.plot(m5$residuals,"m5")
  normal.res.plot(m6$residuals,"m6")
  normal.res.plot(m7$residuals,"m7")
  normal.res.plot(m8$residuals,"m8")
  
  # Based on the visual inspection, the distributions are deemed fairly normal approximated
    
# Heteroskedacity
p_models <- list(m1, m2, m3, m4, m5, m6, m7,m8)

results_bp <- data.frame(model = character(0), statistic = numeric(0), p.value = numeric(0))
for (i in 1:length(models)) {
  temp <- data.frame(model = paste0("m", i),
                     statistic = bptest(models[[i]])$statistic,
                     p.value = bptest(p_models[[i]])$p.value)
  results_bp <- rbind(results_bp, temp)
}; colnames(results_bp) <- c("Model", "Statistic", "p-value")

stargazer(as.matrix(results_bp), type = "html",title = "Breusch-Pagan Test Results",out = "Panel BP results.html",
          column.labels = c("Model", "Test Statistic", "p-value"), digits = 3, rownames=FALSE)

# Autocorrelation (bgtest as heteroskedasticity robust option)
results_ac <- data.frame(model = character(0), DW_statistic = numeric(0), DW_pvalue = numeric(0),
                         BG_statistic_order1 = numeric(0), BG_pvalue_order1 = numeric(0),
                         BG_statistic_order3 = numeric(0), BG_pvalue_order3 = numeric(0))
for (i in 1:length(p_models)) {
  dwtest <- durbinWatsonTest(summary(p_models[[i]])$residuals)
  DW_statistic = dwtest[1]
  DW_pvalue = dwtest[3]
  bgtest1 <- bgtest(p_models[[i]], order=1)
  BG_statistic_order1 = bgtest1$statistic
  BG_pvalue_order1 = bgtest1$p.value
  bgtest3 <- bgtest(p_models[[i]], order=3)
  BG_statistic_order3 = bgtest3$statistic
  BG_pvalue_order3 = bgtest3$p.value
  temp <- data.frame(model = paste0("m", i),
                     DW_statistic = DW_statistic, DW_pvalue = DW_pvalue,
                     BG_statistic_order1 = BG_statistic_order1, BG_pvalue_order1 = BG_pvalue_order1,
                     BG_statistic_order3 = BG_statistic_order3, BG_pvalue_order3 = BG_pvalue_order3)
  results_ac <- rbind(results_ac, temp)
}; colnames(results_ac) <- c("Model", "DW Statistic", "DW p-value", "BG Statistic (order 1)", "BG p-value (order 1)",
                            "BG Statistic (order 3)", "BG p-value (order 3)")

stargazer(results_ac, type = "text",summary = FALSE, title = "Test Results",out="Panel AC results.html",
          column.labels = c("Model", "DW Statistic", "DW p-value", "BG Statistic (order 1)", "BG p-value (order 1)",
          "BG Statistic (order 3)", "BG p-value (order 3)"), digits = 3, rownames = FALSE)


# Multicollinearity
vif(m1)   # n/a as only 1 independent var
vif(m2)   # n/a as only 1 independent var
vif(m3)   # not problematic, moderate multicollinearity
vif(m4)   # problematic but expected, high multicollinearity
vif(m5)   # n/a as only 1 independent var
vif(m6)   # somewhat problematic but only used for sensitivity analysis
vif(m7)   # not problematic, low multicollinearity
vif(m8)   # not problematic, low multicollinearity
vars <- c("Marketcap", "Volume", "Volatility"); cor_matrix <- cor(panel_data_1m[, vars]); cor_matrix  # Some correlation between Volume & Marketcap


# Due to autocorrelatin in the residuals of all models, and heteroskedacity in most models, clustered standard errors are used:


                                          #############################
############################################   Panel Data Models   #########################################
                                          #############################

# Create function to calculate and display the panel data  models
panel_model <- function(pdata){
# Regression Models  
  # Linear
  m1 <- felm(Volatility ~ Marketcap, data = pdata)
  # MC + Time FE
  m2 <- felm(Volatility ~ Marketcap | Window | 0 | Ticker, data = pdata)
  # MC + Design + Time FE
  m3 <- felm(Volatility ~ Marketcap + factor(Design) | Window | 0 | Ticker, data = pdata) 
  # MC + Design + Time FE + Entity FE (mulicolinearity!)
  m4 <- felm(Volatility ~ Marketcap + factor(Design) + factor(Ticker)| Window  | 0  | Ticker, data = pdata)
  # Design + Time FE
  m5 <- felm(Volatility ~ factor(Design) | Window  | 0  | Ticker, data = pdata) 
# Sensitivity
  # MC + Design + add.independent + Time FE
  m6 <- felm(Volatility ~  Marketcap + factor(Design) + Volume + factor(IB20)| Window  | 0  | Ticker, data = pdata) 
  # MC + Volume + Time FE + Entity FE
  m7 <- felm(Volatility ~ Marketcap + Volume | Window + Ticker | 0  | Ticker, data = pdata) 
  # MC + alt.Design + alt.ind. + Time FE
  m8 <- felm(Volatility ~ Marketcap + NotAlgo + factor(IB20) | Window  | 0  | Ticker, data = pdata) 

# Store the standard errors that are to be displayed separately.
vol_se <- list(sqrt(diag(vcovHC(m1, type = "HC1"))), # Uses HC1 (heteroskedastic-robust) errors
                  summary(m2)[["coefficients"]][,2], # From here onward, maintain the clustered errors as included in the models
                  summary(m3)[["coefficients"]][,2],
                  summary(m4)[["coefficients"]][,2],
                  summary(m5)[["coefficients"]][,2],
                  summary(m6)[["coefficients"]][,2],
                  summary(m7)[["coefficients"]][,2],
                  summary(m8)[["coefficients"]][,2])
#vol_se <- list(sqrt(diag(vcovHC(m1, type = "HC1"))), # Uses HC1 (heteroskedastic-robust) errors
 #              summary(m2)[["coefficients"]][,2], # From here onward, maintain the clustered errors as included in the models
   #            coeftest(m3, vcov = sandwich::vcovHAC(m3))[,2],
  #             coeftest(m4, vcov = sandwich::vcovHAC(m4))[,2],
   #            coeftest(m5, vcov = sandwich::vcovHAC(m5))[,2],
    #           coeftest(m6, vcov = sandwich::vcovHAC(m6))[,2],
     #          summary(m7)[["coefficients"]][,2],
      #         summary(m8)[["coefficients"]][,2])

# Use stargazer to create table
stargazer(m1,m2,m3,m4,m5,m6,m7,m8, 
          digits = 3,
          header = FALSE,
          font.size = "tiny",
          align = TRUE,
          no.space=TRUE, 
          table.layout ="=lt-a-s=n",
          type = "text", 
          out = "Panel_Analysis.html", 
          title = "Regression Analysis of the Effect of Marketcap on Stablecoin Volatility",
          dep.var.labels.include = FALSE,
          dep.var.caption = c("Volatility (log of SD)"),
          se = vol_se,
          model.numbers = TRUE,
          covariate.labels=c("Marketcap", "Onchain", "Tokenized", "Volume", "NotAlgo", "IB20"),
          omit = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10", "Constant", "Window", 
                   "BUSD", "GUSD", "HUSD", "TUSD", "USDC", "USDK", "USDT",  "USDP","DAI", "EOSDT", "LUSD", "MIM", 
                   "MUSD", "OUSD", "RSV", "SUSD", "USDX", "VAI", "ALUSD", "FRAX", "CUSD", "USDS",  "USDP1", "FEI",
                   "USTC", "SBD", "USDN", "TOR"),
          star.char = c("+", "*", "**"),
          star.cutoffs = c(.1, .05, .01),
          omit.stat = c("rsq", "ser"),
          add.lines=list(
            c("", "", "", "", "", "", "", ""),
            c("Windows", "1-x", "1-x", "1-x", "1-x", "1-x", "1-x", "1-x","1-x"),
            c("Entity FE?", "no", "no","no","yes","no","no","yes","no"),
            c("Time FE?", "no","yes","yes","yes","yes","yes","yes","yes"),
            c("Clustered SE?", "no","yes","yes","yes","yes","yes","yes","yes"),
            c("", "","","","","","",""),
            c("", "","","","","","","")),
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
          notes = c("These regressions were estimated using panel data for 27 Stablecoins over 33 monthly windows.."))
}

#3D plots

scatter3D(panel_data_1m$Volume, panel_data_1m$Marketcap,panel_data_1m$Volatility, phi = 0, bty ="g",
            type = "h", ticktype = "detailed", pch = 19, cex = 0.8, clab="Volatility",
            xlab="Volume",ylab="Marketcap",zlab="Volatility")
  
scatter3d(x = panel_data_1m$Volume, y = panel_data_1m$Marketcap, z = panel_data_1m$Volatility, 
            groups = factor(panel_data_1m$Design),  grid = FALSE, surface = FALSE,
            ellipsoid = TRUE, axis.scales = FALSE, axis.col = c("black", "black", "black"),
            surface.col = c("#999999", "#E69F00", "#56B4E9"),
            xlab="Volume",ylab="Marketcap",zlab="Volatility")
            #save: rgl.snapshot(filename = "3d_plot.png")
  
# Main Analysis: 1-month data
panel_model(panel_data_1m)

# Sensitivity Analysis: 6-month and 1-year data
panel_model(panel_data_6m)
panel_model(panel_data_1y)


#################################################################################################################
###########################################       EVENT STUDY       #############################################
#################################################################################################################

# Get data and set start date
data_base_3 <- data_base_2
#data_base_3$TE <- abs(1-data_base_3$Close)
data_base_3 <- data_base_3[data_base_3$Date>="2022-02-20",]

# Limit post period size depending on model

  # Base model (10d post period)
  data_base_3 <- data_base_3[data_base_3$Date<="2022-05-25",]

  # 1m post period
  data_base_3 <- data_base_3[data_base_3$Date<="2022-06-13",]

  # 3 m post period
  data_base_3 <- data_base_3[data_base_3$Date<="2022-08-13",]

# Create function to assign variables
event_data <- function(data) {ddply(data, .(Date), dplyr::summarize,
                                    Design = Design,
                                    GK_nt = GK,
                                    GK = log(GK),
                                    #Return = Return,
                                    MC = log(MC),
                                    #TE = TE,
                                    Pre = ifelse(Date<"2022-05-09"&Date>="2022-04-30",1,0),
                                    Event_d0 = ifelse(Date=="2022-05-09",1,0),
                                    Event_d1 = ifelse(Date=="2022-05-10",1,0),
                                    Event_d2 = ifelse(Date=="2022-05-11",1,0),
                                    Event_d3 = ifelse(Date=="2022-05-12",1,0),
                                    Event_d4 = ifelse(Date=="2022-05-13",1,0),
                                    Event_d5 = ifelse(Date=="2022-05-14",1,0),
                                    Post = ifelse(Date>"2022-05-14"&Date<="2022-05-25",1,0),
                                    Post_1m = ifelse(Date>"2022-05-12"&Date<="2022-06-13",1,0),
                                    Post_3m = ifelse(Date>"2022-05-12"&Date<="2022-08-13",1,0),
                                    Ticker = Ticker
                                    #Post_1mm = ifelse(Date>"2022-05-20"&Date<="2022-06-20",1,0) ,
                                    #Post_2mm =ifelse(Date>"2022-06-20"&Date<="2022-07-20",1,0),
                                    #Post_3mm =ifelse(Date>"2022-07-20"&Date<="2022-08-20",1,0),
                                    #Post_4mm =ifelse(Date>"2022-08-20"&Date<="2022-09-20",1,0)
)}

# Calculate variables and subset data by Design
event_data_all <- event_data(data_base_3)
event_data_tokenized <- event_data(data_base_3[data_base_3$Design=="Tokenized",])
event_data_onchain <- event_data(data_base_3[data_base_3$Design=="Onchain",])
event_data_algo <- event_data(data_base_3[data_base_3$Design=="Algorithmic",])
  
  # Check completeness
  table(event_data_all$Ticker)

# Apply models

  # Base model (10d post period)
  eventstudy_all <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3 + Event_d4+Event_d5+ Post + MC, data = event_data_all)
  eventstudy_tokenized <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  +Event_d4+Event_d5+ Post+ MC, data = event_data_tokenized)
  eventstudy_onchain <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2  + Event_d3  + Event_d4+Event_d5+Post+ MC, data = event_data_onchain)
  eventstudy_algo <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  + Event_d4+Event_d5+Post+ MC, data = event_data_algo)

  # 1 month post period
  eventstudy_all <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3 +Event_d4+Event_d5+ Post_1m, data = event_data_all)
  eventstudy_tokenized <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  + Event_d4+Event_d5+Post_1m, data = event_data_tokenized)
  eventstudy_onchain <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2  + Event_d3  +Event_d4+Event_d5+ Post_1m, data = event_data_onchain)
  eventstudy_algo <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  + Event_d4+Event_d5+Post_1m, data = event_data_algo)

  # 3 months post period
  eventstudy_all <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3 + Event_d4+Event_d5+Post_3m, data = event_data_all)
  eventstudy_tokenized <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  + Event_d4+Event_d5+Post_3m, data = event_data_tokenized)
  eventstudy_onchain <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2  + Event_d3  + Event_d4+Event_d5+Post_3m, data = event_data_onchain)
  eventstudy_algo <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  +Event_d4+Event_d5+ Post_3m, data = event_data_algo)

# Prepare for stargazer table (shorten model name)
e1 <- eventstudy_all; e2 <- eventstudy_tokenized; e3 <- eventstudy_onchain; e4 <- eventstudy_algo

# Check assumptions and chose robust standard errors 

  # Normality
    
  # Shapiro
  shapiro.test(event_data_all$GK); shapiro.test(event_data_tokenized$GK) 
  shapiro.test(event_data_onchain$GK); shapiro.test(event_data_algo$GK) 
  # Shapirot tests reject normality but is highly sensitive to outliers, thus a visual inspection preferred:
    # GK All
    normal.v.plot(event_data_all, event_data_all$GK, "GK All")
    ggqqplot(event_data_all$GK)
    # GK Tokenized
    normal.v.plot(event_data_tokenized, event_data_tokenized$GK, "GK Tokenized")
    ggqqplot(event_data_tokenized$GK)  
    # GK Onchain
    normal.v.plot(event_data_onchain, event_data_onchain$GK,"GK Onchain")
    ggqqplot(event_data_onchain$GK)
    # GK Algo
    normal.v.plot(event_data_algo, event_data_algo$GK, "GK Algorithmic")
    ggqqplot(event_data_algo$GK)
    # Residuals
    normal.res.plot(e1$residuals, "Residuals All")
    normal.res.plot(e2$residuals, "Residuals Tokenized")
    normal.res.plot(e3$residuals, "Residuals Onchain")
    normal.res.plot(e4$residuals, "Residuals Algorithmic")
    
  # Based on the visual inspection, the distributions are deemed fairly normal approximated
 
  # Testing fo Heteroskedacity with the Breusch Pagan test
  results_bp_e <- data.frame(model = character(), test_stat = numeric(), p_value = numeric())
  results_bp_e[1,] <- c("e1", bptest(e1)$statistic, bptest(e1)$p.value)
  results_bp_e[2,] <- c("e2", bptest(e2)$statistic, bptest(e2)$p.value)
  results_bp_e[3,] <- c("e3", bptest(e3)$statistic, bptest(e3)$p.value)
  results_bp_e[4,] <- c("e4", bptest(e4)$statistic, bptest(e4)$p.value)
  
  stargazer(results_bp_e, summary=FALSE,type = "text", column.labels = c("Model", "Test Statistic", "p-Value"),
            out="BP Results E.html", align = TRUE, digits = 3, rownames=FALSE)
  
  
  # Testing fo Autocorrelation in residuals with the Durbin Watson Test resp. Breusch-Godfrey (heteroskedacity robust)
  results_ac_e <- data.frame(model = character(), DW_test_stat = numeric(), DW_p_value = numeric(),BG_test_stat_order1 = numeric(), 
                             BG_p_value_order1 = numeric(),BG_test_stat_order3 = numeric(), BG_p_value_order3 = numeric())
  results_ac_e[1,] <- c("e1",  durbinWatsonTest(e1)$dw, durbinWatsonTest(e1)$p,bgtest(e1, order=1)$statistic, bgtest(e1, order=1)$p.value,bgtest(e1, order=3)$statistic, bgtest(e1, order=3)$p.value)
  results_ac_e[2,] <- c("e2",  durbinWatsonTest(e2)$dw, durbinWatsonTest(e2)$p,bgtest(e2, order=1)$statistic, bgtest(e2, order=1)$p.value,bgtest(e2, order=3)$statistic, bgtest(e2, order=3)$p.value)
  results_ac_e[3,] <- c("e3",  durbinWatsonTest(e3)$dw, durbinWatsonTest(e3)$p,bgtest(e3, order=1)$statistic, bgtest(e3, order=1)$p.value,bgtest(e3, order=3)$statistic, bgtest(e3, order=3)$p.value)
  results_ac_e[4,] <- c("e4",  durbinWatsonTest(e4)$dw, durbinWatsonTest(e4)$p,bgtest(e4, order=1)$statistic, bgtest(e4, order=1)$p.value,bgtest(e4, order=3)$statistic, bgtest(e4, order=3)$p.value)
  
  stargazer(results_ac_e,summary=FALSE, type = "html", column.labels = c("Model", "DW Test Statistic", "DW p-Value", "BG Test Statistic (Order 1)", "BG p-Value (Order 1)",
                              "BG Test Statistic (Order 3)", "BG p-Value (Order 3)"), align = TRUE, digits=3, rownames=FALSE, out="AC Test Results E.html")
  
 
  # Testing for multicollinearity with the VIF function
  vif(e1); vif(e2); vif(e3);vif(e4) # No or very low indication of multicollinearity
  
  # Constant Variance; Linearity
  plot(e1,3); plot(e1,1); 
  plot(e2,3); plot(e2,1);
  plot(e3,3); plot(e3,1)
  plot(e4,3); plot(e4,1)
  
  # assumptions interpret: https://godatadrive.com/blog/basic-guide-to-test-assumptions-of-linear-regression-in-r#:~:text=They%20are%3A,a%20mean%20value%20of%20zero
  

# Chose robust standard errors accordingly
# ev_se <- list(coeftest(e1, vcov=vcovHC(e1, cluster="Ticker"))[,2],# HAC standard errors (robust to heteroskedacity+autocorrelation)
#            coeftest(e2, vcov=vcovHC(e2, cluster="Ticker"))[,2],  # HAC standard errors (robust to heteroskedacity+autocorrelation)
#            sqrt(diag(vcovHC(e3, type = "HC1"))),                 # H1 standard errors (robust to heteroskedacity)
#            summary(e4)[["coefficients"]][,2])                    # Uncorrected standard errors

# Due to heteroskedacity and autocorrelation, HAC standard errors seem most appropiate
ev_se <- list(coeftest(e1, vcov=vcovHC(e1, cluster="Ticker"))[,2], 
              coeftest(e2, vcov=vcovHC(e2, cluster="Ticker"))[,2],
              coeftest(e3, vcov=vcovHC(e3, cluster="Ticker"))[,2],
              coeftest(e4, vcov=vcovHC(e4, cluster="Ticker"))[,2])

# Use stargazer to create summary table
stargazer(e1,e2,e3,e4, 
          digits = 5,
          header = FALSE,
          font.size = "tiny",
          align = TRUE,
          no.space=TRUE, 
          table.layout ="=lt-a-s=n",
          type = "text", 
          out = "Event_Study.html", 
          title = "Regression Analysis of the Effect of the Terra-Luna crash",
          dep.var.labels.include = FALSE,
          dep.var.caption = c("Volatility (GK)"),
          se = ev_se,
          model.numbers = TRUE,
          #covariate.labels=c("Pre 10d", "Event d+0", "Event d+1", "Event d+2", "Event d+3", "Event d+4", "Post 5d", "Intercept"),
          #omit = c(),
          star.char = c("+", "*", "**"),
          star.cutoffs = c(.1, .05, .01),
          omit.stat = c("rsq", "ser"),
          add.lines=list(
            #  c("", "", "", "", "", "", "", ""),
            c("Standard Errors:", "HAC", "HAC", "HAC", "HAC"))
          #  c("Entity FE?", "no", "no","no","yes","no","no","yes","no"),
          #  c("Time FE?", "no","yes","yes","yes","yes","yes","yes","yes"),
          #  c("Clustered SE?", "no","yes","yes","yes","yes","yes","yes","yes"),
          #  c("", "","","","","","",""),
          #  c("", "","","","","","","")),
          #column.labels = c("(1)", "(2)", "(3)", "(4)"),
          #notes = c("x")
)


################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################









# Konsistenztests
# F-Test: Fixed Effects vs. OLS/Pooled Model
# H0: Alle fixed time effects = 0
# H1: Nicht alle fixed time effects = 0 -> dann fixed effects model besser als PooledOLS
pFtest(m2, m1)
# ... mit den Kontrollvariablen LnEMPLOYEE, LnPREMIUMS, LEVERAGE und COMPANYTYPE
pFtest(reg.ke2.fixed, reg.ke2.pooledOLS)

# Hausman Test: Random Model vs. Fixed Model
# H0: Kein Modell ist inkonsistent
# H1: Ein Modell ist inkonsistent -> Fixed Effects Modell verwenden
phtest(reg.ke.random, reg.ke.fixed)
# ... mit den Kontrollvariablen leverage und companytype
phtest(reg.ke2.random, reg.ke2.fixed)

# Breusch Pagan Test:
# H0: keine signifikante Paneldaten-Effekte
# H1: signifikante Paneldaten-Effekte -> PooledOLS nicht verwenden
plmtest(m1, type = "bp")
# ... mit den Kontrollvariablen leverage und companytype
plmtest(reg.ke2.pooledOLS, type = "bp")































































d1 <- ddply(w1, .(Ticker), dplyr::summarize,                        {error}
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                  tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                      sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d2 <- ddply(w2, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d3 <- ddply(w3, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d4 <- ddply(w4, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d5 <- ddply(w5, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d6 <- ddply(w6, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d7 <- ddply(w7, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d8 <- ddply(w8, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d9 <- ddply(w9, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d10 <- ddply(w10, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d11 <- ddply(w11, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d12 <- ddply(w12, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d13 <- ddply(w13, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d14 <- ddply(w14, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d15 <- ddply(w15, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d16 <- ddply(w16, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d17 <- ddply(w17, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d18 <- ddply(w18, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d19 <- ddply(w19, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d20 <- ddply(w20, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d21 <- ddply(w21, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d22 <- ddply(w22, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d23 <- ddply(w23, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d24 <- ddply(w24, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d25 <- ddply(w25, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))

d26 <- ddply(w26, .(Ticker), dplyr::summarize, 
            obs = length(Return),
            last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                   tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
            nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
            neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                           (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                       sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
            sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
            Vol = mean(Volume, na.rm=TRUE),
            MC = mean(MC, na.rm=TRUE),
            Design=unique(Design))
d27 <- ddply(w27, .(Ticker), dplyr::summarize, 
             obs = length(Return),
             last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                    tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
             nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
             neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                            (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                        sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
             sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
             Vol = mean(Volume, na.rm=TRUE),
             MC = mean(MC, na.rm=TRUE),
             Design=unique(Design))

d28 <- ddply(w28, .(Ticker), dplyr::summarize, 
             obs = length(Return),
             last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                    tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
             nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
             neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                            (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                        sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
             sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
             Vol = mean(Volume, na.rm=TRUE),
             MC = mean(MC, na.rm=TRUE),
             Design=unique(Design))

d29 <- ddply(w29, .(Ticker), dplyr::summarize, 
             obs = length(Return),
             last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                    tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
             nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
             neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                            (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                        sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
             sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
             Vol = mean(Volume, na.rm=TRUE),
             MC = mean(MC, na.rm=TRUE),
             Design=unique(Design))

d30 <- ddply(w30, .(Ticker), dplyr::summarize, 
             obs = length(Return),
             last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                    tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
             nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
             neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                            (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                        sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
             sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
             Vol = mean(Volume, na.rm=TRUE),
             MC = mean(MC, na.rm=TRUE),
             Design=unique(Design))

d31 <- ddply(w31, .(Ticker), dplyr::summarize, 
             obs = length(Return),
             last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                    tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
             nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
             neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                            (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                        sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
             sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
             Vol = mean(Volume, na.rm=TRUE),
             MC = mean(MC, na.rm=TRUE),
             Design=unique(Design))

d32 <- ddply(w32, .(Ticker), dplyr::summarize, 
             obs = length(Return),
             last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                    tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
             nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
             neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                            (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                        sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
             sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
             Vol = mean(Volume, na.rm=TRUE),
             MC = mean(MC, na.rm=TRUE),
             Design=unique(Design))

d33 <- ddply(w33, .(Ticker), dplyr::summarize, 
             obs = length(Return),
             last_sign_lag = ifelse(identical(tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1), integer(0))==TRUE,0,
                                    tail(which(abs(acf(Return, type = "correlation", lag.max = length(Return))$acf)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(Return))),1)),                                                      
             nc = ifelse(last_sign_lag<=obs/4,last_sign_lag,round(obs/4, digits = 0)),
             neff = round((obs - 2*nc - 1 + nc*(nc+1)/obs) /
                            (1+2*ifelse(nc<=1,acf(Return, type = "correlation", lag.max = length(Return))$acf[1]^2,
                                        sum(acf(Return, type = "correlation", lag.max = length(Return))$acf[1:nc]^2))),digits = 0),
             sd_return_AC_corrected =  ifelse(nc==0, sqrt((1/(obs-1))*sum((Return - mean(Return))^2)),sqrt((neff/(obs*(neff-1)))*sum((Return - mean(Return))^2))),
             Vol = mean(Volume, na.rm=TRUE),
             MC = mean(MC, na.rm=TRUE),
             Design=unique(Design))


p1 <- ddply(d1, .(Ticker), dplyr::summarize, Window = 1, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p2 <- ddply(d2, .(Ticker), dplyr::summarize, Window = 2, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p3 <- ddply(d3, .(Ticker), dplyr::summarize, Window = 3, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p4 <- ddply(d4, .(Ticker), dplyr::summarize, Window = 4, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p5 <- ddply(d5, .(Ticker), dplyr::summarize, Window = 5, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p6 <- ddply(d6, .(Ticker), dplyr::summarize, Window = 6, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p7 <- ddply(d7, .(Ticker), dplyr::summarize, Window = 7, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p8 <- ddply(d8, .(Ticker), dplyr::summarize, Window = 8, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p9 <- ddply(d9, .(Ticker), dplyr::summarize, Window = 9, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p10 <- ddply(d10, .(Ticker), dplyr::summarize, Window = 10, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p11 <- ddply(d11, .(Ticker), dplyr::summarize, Window = 11, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p12 <- ddply(d12, .(Ticker), dplyr::summarize, Window = 12, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p13 <- ddply(d13, .(Ticker), dplyr::summarize, Window = 13, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p14 <- ddply(d14, .(Ticker), dplyr::summarize, Window = 14, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p15 <- ddply(d15, .(Ticker), dplyr::summarize, Window = 15, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p16 <- ddply(d16, .(Ticker), dplyr::summarize, Window = 16, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p17 <- ddply(d17, .(Ticker), dplyr::summarize, Window = 17, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p18 <- ddply(d18, .(Ticker), dplyr::summarize, Window = 18, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p19 <- ddply(d19, .(Ticker), dplyr::summarize, Window = 19, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p20 <- ddply(d20, .(Ticker), dplyr::summarize, Window = 20, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p21 <- ddply(d21, .(Ticker), dplyr::summarize, Window = 21, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p22 <- ddply(d22, .(Ticker), dplyr::summarize, Window = 22, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p23 <- ddply(d23, .(Ticker), dplyr::summarize, Window = 23, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p24 <- ddply(d24, .(Ticker), dplyr::summarize, Window = 24, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p25 <- ddply(d25, .(Ticker), dplyr::summarize, Window = 25, Ticker = Ticker, Volatility = sd_return_AC_corrected,
            Volume = Vol, Marketcap = MC, Design = Design)
p26 <- ddply(d26, .(Ticker), dplyr::summarize, Window = 26, Ticker = Ticker, Volatility = sd_return_AC_corrected,
             Volume = Vol, Marketcap = MC, Design = Design)
p27 <- ddply(d27, .(Ticker), dplyr::summarize, Window = 27, Ticker = Ticker, Volatility = sd_return_AC_corrected,
             Volume = Vol, Marketcap = MC, Design = Design)
p28 <- ddply(d28, .(Ticker), dplyr::summarize, Window = 28, Ticker = Ticker, Volatility = sd_return_AC_corrected,
             Volume = Vol, Marketcap = MC, Design = Design)
p29 <- ddply(d29, .(Ticker), dplyr::summarize, Window = 29, Ticker = Ticker, Volatility = sd_return_AC_corrected,
             Volume = Vol, Marketcap = MC, Design = Design)
p30 <- ddply(d30, .(Ticker), dplyr::summarize, Window = 30, Ticker = Ticker, Volatility = sd_return_AC_corrected,
             Volume = Vol, Marketcap = MC, Design = Design)
p31 <- ddply(d31, .(Ticker), dplyr::summarize, Window = 31, Ticker = Ticker, Volatility = sd_return_AC_corrected,
             Volume = Vol, Marketcap = MC, Design = Design)
p32 <- ddply(d32, .(Ticker), dplyr::summarize, Window = 32, Ticker = Ticker, Volatility = sd_return_AC_corrected,
             Volume = Vol, Marketcap = MC, Design = Design)
p33 <- ddply(d33, .(Ticker), dplyr::summarize, Window = 33, Ticker = Ticker, Volatility = sd_return_AC_corrected,
             Volume = Vol, Marketcap = MC, Design = Design)

panel_data_monthly <- rbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,p11, p12, p13, p14, p15, p16, p17, p18, p19,
                            p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31 ,p32, p33)

#prepare panel data
panel_data_monthly <- ddply(panel_data_monthly, .(Window),dplyr::summarize, 
                    Window = Window,
                    Volatility = log(Volatility),
                    Ticker = Ticker,
                    Volume = log(Volume),
                    Marketcap = log(Marketcap),
                    Onchain = ifelse(Design=="Onchain",1,0),
                    Algo = ifelse(Design=="Algorithmic",1,0),
                    Tokenized = ifelse(Design=="Tokenized",1,0),
                    NotAlgo = ifelse(Design=="Algorithmic",0,1),
                    Design = Design,
                    Inception = case_when(Ticker %in% c("SBD", "USDT") ~ 17,
                                          Ticker %in% c("TUSD", "SUSD", "USDP", "USDP", "GUSD","USDC") ~ 18,
                                          Ticker %in% c("EOSDT", "USDK","BUSD","HUSD","DAI") ~ 19,
                                          Ticker %in% c("USDN", "MUSD","RSV","OUSD","CUSD","USDX","USTC","VAI","FRAX") ~ 20,
                                          Ticker %in% c("FEI", "ALUSD","MIM","LUSD") ~ 21,
                                          Ticker %in% c("USDS", "TOR") ~ 22),
                    IB20 = case_when(Ticker %in% c("SBD", "USDT", "TUSD", "SUSD", "USDP", "USDP", "GUSD","USDC", "EOSDT", "USDK","BUSD","HUSD","DAI") ~ "A",
                                     Ticker %in% c("USDN", "MUSD","RSV","OUSD","CUSD","USDX","USTC","VAI","FRAX","FEI", "ALUSD","MIM","LUSD","USDS", "TOR") ~ "B"),
                    W1 = ifelse(Window==1,1,0),
                    W2 = ifelse(Window==2,1,0),
                    W3 = ifelse(Window==3,1,0),
                    W4 = ifelse(Window==4,1,0),
                    W5 = ifelse(Window==5,1,0),
                    W6 = ifelse(Window==6,1,0),
                    W7 = ifelse(Window==7,1,0),
                    W8 = ifelse(Window==8,1,0),
                    W9 = ifelse(Window==9,1,0),
                    W10 = ifelse(Window==10,1,0))

#assing NA to inf values
panel_data_monthly$Volume[which(!is.finite(panel_data_monthly$Volume))] <- NA
panel_data_monthly$Marketcap[which(!is.finite(panel_data_monthly$Marketcap))] <- NA
#exlude NAs
panel_data_monthly <- panel_data_monthly[complete.cases(panel_data_monthly), ] 

# Run all of the 7 different models in Stock & Watson Table 10.1 using the "felm" package.
#fatalities_mod1 <- lm(fatal_rate ~ Marketcap, data = Fatalities)

library(stargazer)
library(lfe)
library(plm)

#Methods can be used also for unbalanced but depend on software implementation // stockwatson p.351. 
#Clustering by ticker means, it corrects autocorrelation in each ticker but asumes the distint tickers are not correlated  // p.11: http://qed.econ.queensu.ca/pub/faculty/mackinnon/working-papers/qed_wp_1421.pdf 

#variable: existed before 2020, as of 2021, 2022

#as in CL3 but first Time fixed effect
m1 <- felm(Volatility ~ Marketcap, data = panel_data_monthly) #linear
m2 <- felm(Volatility ~ Marketcap | Window  | 0  | Ticker, data = panel_data_monthly) #time FE
m3 <- felm(Volatility ~ Marketcap + factor(Design) | Window  | 0  | Ticker, data = panel_data_monthly) #time FE + Design
m4 <- felm(Volatility ~ Marketcap + factor(Ticker) | Window  | 0  | Ticker, data = panel_data_monthly) #time FE + entity FE + Design
m5 <- felm(Volatility ~ Marketcap + factor(Design) + factor (Ticker)  + Volume +  factor(IB20) | Window  | 0  | Ticker, data = panel_data_monthly) #time FE + entitiy FE + Design + Volume

m6 <- felm(Volatility ~ Marketcap*factor(Design) + factor(Ticker)  | Window  | 0  | Ticker, data = panel_data_monthly) #Time FE + Entity FE without additional variables
m7 <- felm(Volatility ~ Marketcap + factor(Ticker) + NotAlgo +  factor(IB20) | Window   | 0  | Ticker, data = panel_data_monthly)

# Store the standard errors you wish to display separately.
# We can specify the standard errors in many different formats; f.e. for the first model, we use HC1 instead of HC0 (account for heteroscedasticity)
rob_se <- list(sqrt(diag(vcovHC(m1, type = "HC1"))), # Uses HC1, which is not clustered errors, but not basic OLS errors either (it's heteroskedastic-robust)
               summary(m2)[["coefficients"]][,2],
               summary(m3)[["coefficients"]][,2],
               summary(m4)[["coefficients"]][,2],
               summary(m5)[["coefficients"]][,2],
               summary(m6)[["coefficients"]][,2],
               summary(m7)[["coefficients"]][,2])

# Use "stargazer" to replicate the Table 10.1.
#coeftest(fatalities_mod1, vcov = vcovHC(fatalities_mod1, type="HC1"))
stargazer(m1,m2,m3,m4,m5,m6,m7, 
          digits = 3,
          header = FALSE,
          font.size = "tiny",
          align = TRUE,
          no.space=TRUE, 
          table.layout ="=lt-a-s=n",
          type = "html", # You can add latex here as well
          out = "Stablecoins_monthly.html", # Output file is html for simplicity
          title = "Regression Analysis of the Effect of Marketcap on Stablecoin Volatility",
          dep.var.labels.include = FALSE,
          dep.var.caption = c("Volatility (log of SD"),
          se = rob_se,
          model.numbers = FALSE,
          #covariate.labels=c("Marketcap", "Onchain", "Tokenized", "Volume", "Pre 2020 Coin"),
          omit = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10", "Constant", "Window", 
                   "BUSD", "GUSD", "HUSD", "TUSD", "USDC", "USDK", "USDT",  "USDP","DAI", "EOSDT", "LUSD", "MIM", 
                   "MUSD", "OUSD", "RSV", "SUSD", "USDX", "VAI", "ALUSD", "FRAX", "CUSD", "USDS",  "USDP1", "FEI",
                   "USTC", "SBD", "USDN", "TOR"),
          star.char = c("+", "*", "**"),
          star.cutoffs = c(.1, .05, .01),
          omit.stat = c("rsq", "ser"),
          add.lines=list(
            c("", "", "", "", "", "", "", ""),
            c("Windows", "1-10", "1-10", "1-10", "1-10", "1-10", "1-10", "1& <br> 108 only"),
            c("Ticker effects?", "no", "no","(no)","yes","yes","yes","yes"),
            c("Time effects?", "no","yes","yes","yes","yes","yes","yes"),
            c("Clustered standard errors?", "no","yes","yes","yes","yes","yes","yes"),
            c("", "","","","","","",""),
            c("", "","","","","","","")),
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
          notes = c("These regressions were estimated using panel data for 27 Stablecoins."))































































#Define weights to account for missing observations
weights <- data.frame(
  c("W1", "W2", "W3", "W4","W5","W6","W7","W8","W9","W10"),
  c(sum(panel_data$W1),
    sum(panel_data$W2),
    sum(panel_data$W3),
    sum(panel_data$W4),
    sum(panel_data$W5),
    sum(panel_data$W6),
    sum(panel_data$W7),
    sum(panel_data$W8),
    sum(panel_data$W9),
    sum(panel_data$W10))
)
colnames(weights) <- c("Window", "#obs")
weights


# Define vector of number of observations in each time period
num_obs <- c(12, 15, 21, 21, 21, 21, 24, 25, 26, 26)

# Initialize vector to hold weights for each time period
weights <- vector(length = sum(num_obs))

# Calculate weights for each time period
for (i in 1:length(num_obs)) {
  start <- sum(num_obs[1:(i - 1)]) + 1
  end <- sum(num_obs[1:i])
  weights[start:end] <- rep(10 / num_obs[i], num_obs[i])
}

# Print weights for each time period
for (i in 1:length(num_obs)) {
  start <- sum(num_obs[1:(i - 1)]) + 1
  end <- sum(num_obs[1:i])
  print(weights[start:end])
}

w_t1 <- rep(10 / 12, 12)
w_t2 <- rep(10 / 15, 15)
w_t3 <- rep(10 / 20, 20)
w_t4 <- rep(10 / 23, 23)
w_t5 <- rep(10 / 25, 25)
w_t6 <- rep(10 / 27, 27)
w_t7 <- rep(10 / 27, 27)
w_t8 <- rep(10 / 27, 27)
w_t9 <- rep(10 / 27, 27)
w_t10 <- rep(10 / 27, 27)


#as in CL3
m1 <- felm(Volatility ~ Marketcap, data = panel_data)
m2 <- felm(Volatility ~ Marketcap | Ticker  | 0  | Ticker, data = panel_data)
m3 <- felm(Volatility ~ Marketcap + W2+W3+W4+W5+W6+W7+W8+W9+W10 | Ticker  | 0  | Ticker, data = panel_data)
m4 <- felm(Volatility ~ Marketcap + W2+W3+W4+W5+W6+W7+W8+W9+W10 +  
             Onchain + Algo + Volume  | Ticker  | 0  | Ticker, data = panel_data)
#no clustering
m1 <- felm(Volatility ~ Marketcap, data = panel_data)
m2 <- felm(Volatility ~ Marketcap | Ticker , data = panel_data)
m3 <- felm(Volatility ~ Marketcap + W2+W3+W4+W5+W6+W7+W8+W9+W10 | Ticker, data = panel_data)
m4 <- felm(Volatility ~ Marketcap + W2+W3+W4+W5+W6+W7+W8+W9+W10 +  
             Onchain + Algo + Volume  | Ticker , data = panel_data)

#as in CL3 but first Time fixed effect
m1 <- felm(Volatility ~ Marketcap, data = panel_data)
m2 <- felm(Volatility ~ Marketcap + factor(Design) | Window  | 0  | Window, data = panel_data)
m3 <- felm(Volatility ~ Marketcap + factor(Ticker) | Window  | 0  | Window, data = panel_data)
m4 <- felm(Volatility ~ Marketcap + factor (Ticker) +  
             Onchain + Algo + Volume  | Ticker  | 0  | Ticker, data = panel_data)

# For adding additional explanatory variables, we need variables that are (1) measurable, i.e. we have the data, (2) change over time, (3) are correlated with the Marketcap.
# Recall that fixed effects are (1) not measurable, (2) do not change over time per group.
# What happens if we include a variable that does not change over time in a given Ticker but is measurable? (i.e. whether the state tends to vote Democratic or Republican)

m5 <- felm(Volatility ~ Marketcap + W2+W3+W4+W5+W6+W7+W8+W9+W10 +  
             Onchain + Algo  | Ticker  | 0  | Ticker, data = panel_data)
m6 <- felm(Volatility ~ Marketcap + W2+W3+W4+W5+W6+W7+W8+W9+W10 +  
             Not_Tokenized + Volume | Ticker  | 0  | Ticker, data = panel_data)
# the set of observations on all variables for 1982 and 1988
panel_data_w1_w10 <- panel_data[with(panel_data, Window == 1 | Window == 10), ]
m7 <- felm(Volatility ~ Marketcap + W1 +
             Onchain + Algo + Volume | Ticker  | 0  | Ticker, data = panel_data_w1_w10)


# Store the standard errors you wish to display separately.
# We can specify the standard errors in many different formats; f.e. for the first model, we use HC1 instead of HC0 (account for heteroscedasticity)
rob_se <- list(sqrt(diag(vcovHC(m1, type = "HC1"))), # Uses HC1, which is not clustered errors, but not basic OLS errors either (it's heteroskedastic-robust)
               summary(m2)[["coefficients"]][,2],
               summary(m3)[["coefficients"]][,2],
               summary(m4)[["coefficients"]][,2],
               summary(m5)[["coefficients"]][,2],
               summary(m6)[["coefficients"]][,2],
               summary(m7)[["coefficients"]][,2])

# Use "stargazer" to replicate the Table 10.1.
#coeftest(fatalities_mod1, vcov = vcovHC(fatalities_mod1, type="HC1"))
stargazer(m1,m2,m3,m4,m5,m6,m7, 
          digits = 3,
          header = FALSE,
          font.size = "tiny",
          align = TRUE,
          no.space=TRUE, 
          table.layout ="=lt-a-s=n",
          type = "html", # You can add latex here as well
          out = "Table_10_flm_Stablecoins.html", # Output file is html for simplicity
          title = "Regression Analysis of the Effect of Marketcap on Stablecoin Volatility",
          dep.var.labels.include = FALSE,
          dep.var.caption = c("Volatility (log of SD"),
          se = rob_se,
          model.numbers = FALSE,
          covariate.labels=c("Marketcap", "Onchain", "Algorithmic", "Volume"),
          omit = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10", "Constant", "Window", 
                   "BUSD", "GUSD", "HUSD", "TUSD", "USDC", "USDK", "USDT",  "USDP","DAI", "EOSDT", "LUSD", "MIM", 
                   "MUSD", "OUSD", "RSV", "SUSD", "USDX", "VAI", "ALUSD", "FRAX", "CUSD", "USDS",  "USDP1", "FEI",
                   "USTC", "SBD", "USDN", "TOR"),
          star.char = c("+", "*", "**"),
          star.cutoffs = c(.1, .05, .01),
          omit.stat = c("rsq", "ser"),
          add.lines=list(
            c("", "", "", "", "", "", "", ""),
            c("Windows", "1-10", "1-10", "1-10", "1-10", "1-10", "1-10", "1& <br> 108 only"),
            c("Ticker effects?", "no", "yes","yes","yes","yes","yes","yes"),
            c("Time effects?", "no","no","yes","yes","yes","yes","yes"),
            c("Clustered standard errors?", "no","yes","yes","yes","yes","yes","yes"),
            c("", "","","","","","",""),
            c("", "","","","","","","")),
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
          notes = c("These regressions were estimated using panel data for 27 Stablecoins."))



"factor(Ticker)EOSDT","EOSDT",
"factor(Ticker)FEI","factor(Ticker)FRAX","factor(Ticker)GUSD","factor(Ticker)HUSD","factor(Ticker)LUSD",	
"factor(Ticker)MIM","factor(Ticker)MUSD","factor(Ticker)OUSD","factor(Ticker)RSV","factor(Ticker)SBD",
"factor(Ticker)SUSD","factor(Ticker)TOR","factor(Ticker)TUSD","factor(Ticker)USDC","factor(Ticker)USDK",
"factor(Ticker)USDN","factor(Ticker)USDP","factor(Ticker)USDS","factor(Ticker)USDT","factor(Ticker)USDX",
"factor(Ticker)USTC","factor(Ticker)VAI")



##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

weights <- rep(table(panel_data$Volume), each=1)

#model
FE_mod_lm <- lm(formula = Volatility ~ Volume + Marketcap + factor(Design) + +factor(Window), data = panel_data, weights =weights)
summary(FE_mod_lm)

model <- 
  
  install.packages("lfe")
library(lfe)

install.packages("plm")
library(plm)

bla <- plm(formula = Volatility ~ Volume + Marketcap + Onchain + factor(Design), data = panel_data, effect = "twoways")
summary(bla)
bla <- update(bla,Volatility ~ Volume + Marketcap +  factor(Design))

#________
#https://medium.com/@manilwagle/how-to-deal-with-panel-data-practical-application-using-r-18ef95ae99c6
install.packages("stargazer")
library(stargazer)


panel_data_2 <- ddply(panel_data, .(Window),dplyr::summarize, 
                      Window = Window,
                      Volatility = Volatility,
                      Ticker = Ticker,
                      Volume = Volume,
                      Marketcap = ifelse(Marketcap==0,NA,Marketcap),
                      Onchain = Onchain,
                      Algo = Algo,
                      Design = Design,
                      W1 = ifelse(Window==1,1,0),
                      W2 = ifelse(Window==2,1,0),
                      W3 = ifelse(Window==3,1,0),
                      W4 = ifelse(Window==4,1,0),
                      W5 = ifelse(Window==5,1,0),
                      W6 = ifelse(Window==6,1,0),
                      W7 = ifelse(Window==7,1,0),
                      W8 = ifelse(Window==8,1,0),
                      W9 = ifelse(Window==9,1,0),
                      W10 = ifelse(Window==10,1,0)
)
panel_data_2 <- panel_data_2[complete.cases(panel_data_2), ]




#Pooled OLS with lm function
ols <- lm(Volatility~Volume+Marketcap+factor(Design), data=panel_data_3)
summary(ols)

#Pooled OLS with plm function (sould be the same)
pooled <- plm(Volatility~Volume+Marketcap+factor(Design)+
                factor(Window), data=panel_data_3, model="pooling",
              index=c("Ticker", "Window"))
pooled <- plm(Volatility~Volume+Marketcap,data=panel_data_3, model="pooling",
              index=c("Ticker", "Window"))

summary(pooled)

#in readable format
stargazer(pooled, type="text")

#Check homoskedacity
res<-residuals(ols)
yhat<-fitted(ols)
plot(panel_data_3$Volume, res,xlab="Volume",ylab="Residuals")
plot(yhat, res,xlab="Fitted Valued",ylab="Residuals")
shapiro.test(pooled$residuals)#bp test requires normality
bptest(pooled) #error is not homoscedastic

#Fixed effect model, assumes variations within a cross-section
fe <- plm(Volatility~Volume+Marketcap+factor(Design)+
            factor(Window), data=panel_data_3, model="within",
          index=c("Ticker", "Window"))

fe_within <- plm(Volatility~Volume+Marketcap+factor(Design), data=panel_data_3, model="within",
                 index=c("Window"))
summary(fe_within)

#Test to see if Fixed Effect Model is better than OLS
pFtest(fe,ols)
#we can reject the null hypothesis and proceed with fixed effectsmodel

#Random Effect model, includes the possiblity of between entity variation
re <- plm(Volatility~Volume+Marketcap+factor(Design)+W2+W3+W4+W5+W6+W7+W8+W9+W10
          , data=panel_data_3, model="random",
          index=c("Ticker", "Window"))
summary(re)

#FE or RE?
phtest(fe,re)
#FE is the better model

summary(fe)

## other test
plmtest(pooled,type=(c("bp")))
pcdtest(fe, test=c("lm"))
pcdtest(fe, test=c("cd"))
pbgtest(fe)
bptest(Volatility~Volume+Marketcap+factor(Design)+factor(Window), data=panel_data_3,
       studentize=F)

#Display results from ols model
results_fe <- matrix(nrow = 2, ncol = 14) 

results_fe[1,] <- coef(summary(ols))[, "Std. Error"][c(1:14)]
results_fe[2,] <- coef(summary(ols, robust=T))[, "Robust s.e"][c(1:14)]

colnames(results_fe) <- c("Intercept","Volume","Marketcap","Onchain","Tokenized","W2","W3","W4","W5","W6","W7","W8","W9","W10")
rownames(results_fe) <- c("Standard OLS", "Heteroskedasticity-robust")

stargazer(results, type = "html", out = "standarderrors.html", flip = TRUE, digits = NULL)

# p-values
results_fe_2 <- matrix(nrow = 2, ncol = 11) 

results_fe_2[1,] <- round(coef(summary(fe))[, "Pr(>|t|)"],4)[c(1:11)]
results_fe_2[2,] <- round(coef(summary(fe, robust = T))[, "Pr(>|t|)"],4)[c(1:11)]

colnames(results_fe_2) <- c("Intercept","Volume","Marketcap","Onchain","Tokenized","W2","W3","W4","W5","W6","W7","W8","W9","W10")
rownames(results_fe_2) <- c("Standard OLS", "Heteroscedasticity-robust")

stargazer(results_fe_2, type = "html", out = "pvalues.html", flip = TRUE, digits = NULL)


#######für von Taima
ols <- lm(Volatility~Volume+Marketcap+factor(Design), data=panel_data_3); summary(ols)

pooled <- plm(Volatility ~ Volume + Marketcap + factor(Design) , data=panel_data_3, model="pooling")

fe_fd <- plm(Volatility~Volume+Marketcap+factor(Design), data=panel_data_3, model="fd",
             index=c("Window","Ticker")); summary(fe_fd)

#time fixed effect
fe_within_time <- plm(Volatility~Volume + Marketcap + factor(Design), data=panel_data_3, model="within",
                      index=c("Window","Ticker"), effect = "time"); summary(fe_within_time)
fe_between_time <- plm(Volatility~Volume+Marketcap+factor(Design), data=panel_data_3, model="between",
                       index=c("Window","Ticker"), effect = "time"); summary(fe_between_time)
fe_random_time <- plm(Volatility~Volume+Marketcap, data=panel_data_3, model="random",
                      index=c("Window","Ticker"), effect = "time"); summary(fe_random_time)

#twoways effect
fe_within_two <- plm(Volatility~Volume + Marketcap + factor(Design), data=panel_data_3, model="within",
                     index=c("Window","Ticker"), effect = "twoways"); summary(fe_within_two)
fe_pooling_two <- plm(Volatility~Volume+Marketcap+factor(Design), data=panel_data_3, model="pooling",
                      index=c("Window","Ticker"), effect = "twoways"); summary(fe_between_two)
fe_random_two <- plm(Volatility~Volume+Marketcap, data=panel_data_3, model="random",
                     index=c("Window","Ticker"), effect = "twoways"); summary(fe_random_two)
#individual (entity fixed?)
fe_within_indv <- plm(Volatility~Volume + Marketcap + factor(Design), data=panel_data_3, model="within",
                      index=c("Window","Ticker"), effect = "individual"); summary(fe_within_indv)
fe_between_indv <- plm(Volatility~Volume+Marketcap+factor(Design), data=panel_data_3, model="between",
                       index=c("Window","Ticker"), effect = "individual"); summary(fe_between_indv)
fe_random_indv <- plm(Volatility~Volume+Marketcap, data=panel_data_3, model="random",
                      index=c("Window","Ticker"), effect = "individual"); summary(fe_random_indv)

#Test to see if Fixed Effect Model is better than OLS
pFtest(fe_within,ols)
pFtest(fe_between,ols)
#FE or RE?
phtest(fe_within,fe_random)

coeftest(fe_within, vcov. = vcovHC, type = "HC2")


#code taima: check ab 10.5 https://www.econometrics-with-r.org/rwpd.html
# Summary table with all the panel data models
# Standard errors of panel models in one list
reg.ke2.se <- list(sqrt(diag(vcov(ols, type="HC2"))),
                   sqrt(diag(vcov(fe_between, type="HC2"))),
                   sqrt(diag(vcov(fe_fd, type="HC2"))),
                   sqrt(diag(vcov(fe_within, type="HC2"))),
                   sqrt(diag(vcov(fe_random, type="HC2"))))
# Neue Namen für Table
m1 <- ols
m2 <- fe_between
m3 <- fe_fd
m4 <- fe_within
m5 <- fe_random
# Table erstellen mit Stargaze Package
stargazer(m1, m2, m3, m4, m5,
          digits = 3,
          type = "html",
          header = FALSE,
          se = reg.ke2.se,
          title = "Panel Data Analysis - All models",
          model.numbers = FALSE,
          column.labels = c("Pooled OLS", "Between", "Fd", "Within", "Random Effects"),
          out = "Panel Data Analysis - All models 3.html")






#______

#Method lab

## PROBLEM 3

# First, run the models
# OLS
fatalities_mod4_1<- felm(Volatility ~ Volume  + Marketcap + Window |  Ticker, data = panel_data_3) 
# Heteroscedasticity-robust
summary(fatalities_mod4_1, robust=T) # 0.221671
# clustered by group
fatalities_mod4_2 <- felm(Volatility ~ Volume  + Marketcap + Window  |  Ticker | 0  | Ticker, data = panel_data_3) 

# Store and display the results for standard errors
results <- matrix(nrow = 3, ncol = 11) 

results[1,] <- coef(summary(fatalities_mod4_1))[, "Std. Error"][c(1:11)]
results[2,] <- coef(summary(fatalities_mod4_1, robust=T))[, "Robust s.e"][c(1:11)]
results[3,] <- coef(summary(fatalities_mod4_2))[, "Cluster s.e."][c(1:11)]

colnames(results) <- c("Volume","Marketcap","W2","W3","W4","W5","W6","W7","W8","W9","W10")
rownames(results) <- c("Standard OLS", "Heteroskedasticity-robust", "Clustered by Design")

stargazer(results, type = "html", out = "standarderrors.html", flip = TRUE, digits = NULL)

# p-values
results_2 <- matrix(nrow = 3, ncol = 11) 

results_2[1,] <- round(coef(summary(fatalities_mod4_1))[, "Pr(>|t|)"],4)[c(1:11)]
results_2[2,] <- round(coef(summary(fatalities_mod4_1, robust = T))[, "Pr(>|t|)"],4)[c(1:11)]
results_2[3,] <- round(coef(summary(fatalities_mod4_2))[, "Pr(>|t|)"],4)[c(1:11)]

colnames(results_2) <- c("Volume","Marketcap","W2","W3","W4","W5","W6","W7","W8","W9","W10")
rownames(results_2) <- c("Standard OLS", "Heteroscedasticity-robust", "Clustered by state")

stargazer(results_2, type = "html", out = "pvalues.html", flip = TRUE, digits = NULL)

#______

# First, run the models
# OLS
fatalities_mod4_a<- felm(Volatility ~ Volume  + Marketcap + Design |  Window, data = panel_data_3) 
# Heteroscedasticity-robust
summary(fatalities_mod4_a, robust=T) # 0.221671
# clustered by group
fatalities_mod4_b <- felm(Volatility ~ Volume  + Marketcap + Design  |  Window | 0  | Window, data = panel_data_3) 

# Store and display the results for standard errors
results <- matrix(nrow = 3, ncol = 4) 

results[1,] <- coef(summary(fatalities_mod4_a))[, "Std. Error"][c(1:4)]
results[2,] <- coef(summary(fatalities_mod4_a, robust=T))[, "Robust s.e"][c(1:4)]
results[3,] <- coef(summary(fatalities_mod4_b))[, "Cluster s.e."][c(1:4)]

colnames(results) <- c("Volume","Marketcap","Onchain","Tokenized")
rownames(results) <- c("Standard OLS", "Heteroskedasticity-robust", "Clustered by Window")

stargazer(results, type = "html", out = "standarderrors.html", flip = TRUE, digits = NULL)

# p-values
results_2 <- matrix(nrow = 3, ncol = 4) 

results_2[1,] <- round(coef(summary(fatalities_mod4_a))[, "Pr(>|t|)"],4)[c(1:4)]
results_2[2,] <- round(coef(summary(fatalities_mod4_a, robust = T))[, "Pr(>|t|)"],4)[c(1:4)]
results_2[3,] <- round(coef(summary(fatalities_mod4_b))[, "Pr(>|t|)"],4)[c(1:4)]

colnames(results_2) <- c("Volume","Marketcap","Onchain","Tokenized")
rownames(results_2) <- c("Standard OLS", "Heteroscedasticity-robust", "Clustered by Window")

stargazer(results_2, type = "html", out = "pvalues.html", flip = TRUE, digits = NULL)

#________

fatalities_mod4_aa<- felm(Volatility ~ Volume  + Marketcap + Window  |  Ticker, data = panel_data_3) 
# Heteroscedasticity-robust
summary(fatalities_mod4_aa, robust=T) # 0.221671






install.packages("gplots")
library(gplots)
plotmeans(Volatility~Ticker , main = 'Heterogeneity Across Stablecoins', data = panel_data)
plotmeans(Volatility~Design , main = 'Heterogeneity Across Design', data = panel_data)
plotmeans(Volatility~Window , main = 'Heterogeneity Across Windows', data = panel_data)

ggplot() + geom_point(aes(panel_data$Volume, panel_data$Volatility),colour = 'red') + ggtitle('Volume VS Volatility') + 
  xlab('Volume') + ylab('Volatility') + xlim(0,1e+9)

ggplot() + geom_point(aes(panel_data$Marketcap, panel_data$Volatility),colour = 'red') + ggtitle('Marketcap Vs Volatility') + 
  xlab('Marketcap') + ylab('Volatility') + xlim(0,1e+9)

coplot(Volatility ~ Window|Ticker, type="b", data=panel_data)

ols <-plm(Volatility ~ Volume + Marketcap, data =panel_data, model = "pooling")
summary(ols)

FE_mod_lm <- lm(formula = Volatility ~ Volume + Marketcap + factor(Design), data = panel_data)
summary(FE_mod_lm)

fixed <- plm(Volatility ~ Volume + Marketcap, data=panel_data, model="within")
summary(fixed)

pFtest(fixed,ols)

random <- plm(Volatility ~ Volume+Marketcap,data =panel_data, index = c("Ticker","Window"),model ="random")
summary(random)

lmMod <- lm(Volatility ~ Volume+Marketcap+factor(Ticker)+factor(Window), data=panel_data)
plot(lmMod)

library(lmtest)
bptest(Volatility ~ Volume+Marketcap+factor(Ticker)+factor(Window), data=panel_data, studentize=F)


install.packages("caret")
library(caret)
distBCMod <- caret::BoxCoxTrans(panel_data$Volatility)
print(distBCMod)

yhat <- FE_mod_lm$fitted.values
yhat




install.packages("plm")
library(plm)

panel_data <- pdata.frame(panel_data, index=c("Ticker", "Window"))

pooling <- plm(Volatility ~ Volume + Marketcap, data=panel_data, model = "pooling")
summary(pooling)
between <- plm(Volatility ~ Volume + Marketcap, data=panel_data, model = "between")
summary(between)
firstdiff <- plm(Volatility ~ Volume + Marketcap, data=panel_data, model = "fd")
summary(firstdiff)
fixed <- plm(Volatility ~ Volume + Marketcap, data=panel_data, model = "within", effect = "twoways")
summary(fixed)


# Run pooled OLS 
wages_pooled.ols <- lm(Volatility ~ Volume   + Algo + Onchain + Window, data = panel_data)
wages_pooled <- plm(Volatility ~ Volume + as.factor(Tokenized) + as.factor(Onchain) + as.factor(Algo) + Window, data = panel_data, model="pooling")
summary(wages_pooled.ols)
summary(wages_pooled)
# Run random effects
wages_random <- plm(Volatility ~ educ + black + hisp + exper + expersq + married + union + year, data=data, model="random")
summary(wages_random)


##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################








#Bootstrap a
#Deducting group mean for each sample
grpA = data_overview_wa$sd_return_AC_corrected[data_overview_wa$Design=="Algorithmic"]-mean(data_overview_wa$sd_return_AC_corrected[data_overview_wa$Design=="Algorithmic"])#+mean(data_overview_wa$sd_return_AC_corrected)
grpO = data_overview_wa$sd_return_AC_corrected[data_overview_wa$Design=="Onchain"]-mean(data_overview_wa$sd_return_AC_corrected[data_overview_wa$Design=="Onchain"]) #+mean(data_overview_wa$sd_return_AC_corrected)
grpT = data_overview_wa$sd_return_AC_corrected[data_overview_wa$Design=="Tokenized"]-mean(data_overview_wa$sd_return_AC_corrected[data_overview_wa$Design=="Tokenized"]) #+mean(data_overview_wa$sd_return_AC_corrected)
#Pool the mean-centered samples.
designeu <- c("A","A","A","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")
#Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=3, replace=T)
  groupO = sample(boot_data$SD, size=6, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue_a = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview_wa))$`F value`[1]), na.rm = TRUE)
p_bootvalue_a
quantile(Fstar_real,.95)

#Bootstrap b
#Deducting group mean for each sample
grpA = data_overview_wb$sd_return_AC_corrected[data_overview_wb$Design=="Algorithmic"]-mean(data_overview_wb$sd_return_AC_corrected[data_overview_wb$Design=="Algorithmic"])#+mean(data_overview_wb$sd_return_AC_corrected)
grpO = data_overview_wb$sd_return_AC_corrected[data_overview_wb$Design=="Onchain"]-mean(data_overview_wb$sd_return_AC_corrected[data_overview_wb$Design=="Onchain"]) #+mean(data_overview_wb$sd_return_AC_corrected)
grpT = data_overview_wb$sd_return_AC_corrected[data_overview_wb$Design=="Tokenized"]-mean(data_overview_wb$sd_return_AC_corrected[data_overview_wb$Design=="Tokenized"]) #+mean(data_overview_wb$sd_return_AC_corrected)
#Pool the mean-centered samples.
designeu <- c("A","A","A","O","O","O","O","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")
#Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=3, replace=T)
  groupO = sample(boot_data$SD, size=10, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue_b = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview_wb))$`F value`[1]), na.rm = TRUE)
p_bootvalue_b
quantile(Fstar_real,.95)

#Bootstrap c
#Deducting group mean for each sample
grpA = data_overview_wc$sd_return_AC_corrected[data_overview_wc$Design=="Algorithmic"]-mean(data_overview_wc$sd_return_AC_corrected[data_overview_wc$Design=="Algorithmic"])#+mean(data_overview_wc$sd_return_AC_corrected)
grpO = data_overview_wc$sd_return_AC_corrected[data_overview_wc$Design=="Onchain"]-mean(data_overview_wc$sd_return_AC_corrected[data_overview_wc$Design=="Onchain"]) #+mean(data_overview_wc$sd_return_AC_corrected)
grpT = data_overview_wc$sd_return_AC_corrected[data_overview_wc$Design=="Tokenized"]-mean(data_overview_wc$sd_return_AC_corrected[data_overview_wc$Design=="Tokenized"]) #+mean(data_overview_wc$sd_return_AC_corrected)
#Pool the mean-centered samples.
designeu <- c("A","A","A","O","O","O","O","O","O","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")
#Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=3, replace=T)
  groupO = sample(boot_data$SD, size=12, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue_c = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview_wc))$`F value`[1]), na.rm = TRUE)
p_bootvalue_c
quantile(Fstar_real,.95)

#Bootstrap d
#Deducting group mean for each sample
grpA = data_overview_wd$sd_return_AC_corrected[data_overview_wd$Design=="Algorithmic"]-mean(data_overview_wd$sd_return_AC_corrected[data_overview_wd$Design=="Algorithmic"])#+mean(data_overview_wd$sd_return_AC_corrected)
grpO = data_overview_wd$sd_return_AC_corrected[data_overview_wd$Design=="Onchain"]-mean(data_overview_wd$sd_return_AC_corrected[data_overview_wd$Design=="Onchain"]) #+mean(data_overview_wd$sd_return_AC_corrected)
grpT = data_overview_wd$sd_return_AC_corrected[data_overview_wd$Design=="Tokenized"]-mean(data_overview_wd$sd_return_AC_corrected[data_overview_wd$Design=="Tokenized"]) #+mean(data_overview_wd$sd_return_AC_corrected)
#Pool the mean-centered samples.
designeu <- c("A","A","A","O","O","O","O","O","O","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")
#Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=3, replace=T)
  groupO = sample(boot_data$SD, size=12, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue_d = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview_wd))$`F value`[1]), na.rm = TRUE)
p_bootvalue_d
quantile(Fstar_real,.95)

#Bootstrap e
#Deducting group mean for each sample
grpA = data_overview_we$sd_return_AC_corrected[data_overview_we$Design=="Algorithmic"]-mean(data_overview_we$sd_return_AC_corrected[data_overview_we$Design=="Algorithmic"])#+mean(data_overview_we$sd_return_AC_corrected)
grpO = data_overview_we$sd_return_AC_corrected[data_overview_we$Design=="Onchain"]-mean(data_overview_we$sd_return_AC_corrected[data_overview_we$Design=="Onchain"]) #+mean(data_overview_we$sd_return_AC_corrected)
grpT = data_overview_we$sd_return_AC_corrected[data_overview_we$Design=="Tokenized"]-mean(data_overview_we$sd_return_AC_corrected[data_overview_we$Design=="Tokenized"]) #+mean(data_overview_we$sd_return_AC_corrected)
#Pool the mean-centered samples.
designeu <- c("A","A","A","O","O","O","O","O","O","O","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")
#Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=3, replace=T)
  groupO = sample(boot_data$SD, size=13, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue_e = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview_we))$`F value`[1]), na.rm = TRUE)
p_bootvalue_e
quantile(Fstar_real,.95)

#Bootstrap f
#Deducting group mean for each sample
grpA = data_overview_wf$sd_return_AC_corrected[data_overview_wf$Design=="Algorithmic"]-mean(data_overview_wf$sd_return_AC_corrected[data_overview_wf$Design=="Algorithmic"])#+mean(data_overview_wf$sd_return_AC_corrected)
grpO = data_overview_wf$sd_return_AC_corrected[data_overview_wf$Design=="Onchain"]-mean(data_overview_wf$sd_return_AC_corrected[data_overview_wf$Design=="Onchain"]) #+mean(data_overview_wf$sd_return_AC_corrected)
grpT = data_overview_wf$sd_return_AC_corrected[data_overview_wf$Design=="Tokenized"]-mean(data_overview_wf$sd_return_AC_corrected[data_overview_wf$Design=="Tokenized"]) #+mean(data_overview_wf$sd_return_AC_corrected)
#Pool the mean-centered samples.
designeu <- c("A","A","A","A","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")
#Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=4, replace=T)
  groupO = sample(boot_data$SD, size=15, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue_f = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview_wf))$`F value`[1]), na.rm = TRUE)
p_bootvalue_f
quantile(Fstar_real,.95)

#Bootstrap g
#Deducting group mean for each sample
grpA = data_overview_wg$sd_return_AC_corrected[data_overview_wg$Design=="Algorithmic"]-mean(data_overview_wg$sd_return_AC_corrected[data_overview_wg$Design=="Algorithmic"])#+mean(data_overview_wg$sd_return_AC_corrected)
grpO = data_overview_wg$sd_return_AC_corrected[data_overview_wg$Design=="Onchain"]-mean(data_overview_wg$sd_return_AC_corrected[data_overview_wg$Design=="Onchain"]) #+mean(data_overview_wg$sd_return_AC_corrected)
grpT = data_overview_wg$sd_return_AC_corrected[data_overview_wg$Design=="Tokenized"]-mean(data_overview_wg$sd_return_AC_corrected[data_overview_wg$Design=="Tokenized"]) #+mean(data_overview_wg$sd_return_AC_corrected)
#Pool the mean-centered samples.
designeu <- c("A","A","A","A","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")
#Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=4, replace=T)
  groupO = sample(boot_data$SD, size=15, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue_g = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview_wg))$`F value`[1]), na.rm = TRUE)
p_bootvalue_g
quantile(Fstar_real,.95)

#Bootstrap h
#Deducting group mean for each sample
grpA = data_overview_wh$sd_return_AC_corrected[data_overview_wh$Design=="Algorithmic"]-mean(data_overview_wh$sd_return_AC_corrected[data_overview_wh$Design=="Algorithmic"])#+mean(data_overview_wh$sd_return_AC_corrected)
grpO = data_overview_wh$sd_return_AC_corrected[data_overview_wh$Design=="Onchain"]-mean(data_overview_wh$sd_return_AC_corrected[data_overview_wh$Design=="Onchain"]) #+mean(data_overview_wh$sd_return_AC_corrected)
grpT = data_overview_wh$sd_return_AC_corrected[data_overview_wh$Design=="Tokenized"]-mean(data_overview_wh$sd_return_AC_corrected[data_overview_wh$Design=="Tokenized"]) #+mean(data_overview_wh$sd_return_AC_corrected)
#Pool the mean-centered samples.
designeu <- c("A","A","A","A","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","T","T","T","T","T","T","T","T")
boot_data <- data.frame(c(grpA,grpO,grpT), designeu)
names(boot_data) <- c("SD","Design")
#Resample, generate Fstatistic distribution, calculate pvalue
R=10000
Fstar_real = numeric(R)
set.seed(1.234)
for (i in 1:R){
  groupA = sample(boot_data$SD, size=4, replace=T)
  groupO = sample(boot_data$SD, size=15, replace=T)
  groupT = sample(boot_data$SD, size=8, replace=T)
  simSD =c(groupA,groupO,groupT)
  simdata=data.frame(simSD,designeu)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= anova((lm(simdata$simSD~simdata$designeu)))$`F value`[1]}
}
p_bootvalue_h = mean(Fstar_real>=abs(anova(lm(sd_return_AC_corrected ~ Design, data=data_overview_wh))$`F value`[1]), na.rm = TRUE)
p_bootvalue_h
quantile(Fstar_real,.95)































# PREPARATION

#applying log-transformation (as proposed here: https://www.graphpad.com/support/faq/what-to-do-when-data-fail-tests-for-homogeneity-of-variance/#:~:text=Often%20the%20best%20approach%20is,the%20trick%2C%20restoring%20equal%20variance.)
data_overview$SD_b <- log(data_overview$sd_return_AC_corrected) #might be ok normal

#check if base asumptions are now fullfilled (not mandatory)

#a) Normality?
shapiro.test(data_overview$SD_b)        
ks.test(data_overview$SD_b, "pnorm")

#shapiro is more appropiate for small sample sizes (<50) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6350423/#:~:text=The%20Shapiro%E2%80%93Wilk%20test%20is,taken%20from%20normal%20distributed%20population.

#result unclear, thus also check graphic:
qqnorm(data_overview$SD_b)
qqline(data_overview$SD_b)
h <- hist(data_overview$SD_b, breaks=10, main="Histogram of mean-adjusted log(GK) with normal curve")
xfit <- seq(min(data_overview$SD_b), max(data_overview$SD_b), length = 40) 
yfit <- dnorm(xfit, mean = mean(data_overview$SD_b), sd = sd(data_overview$SD_b)) 
yfit <- yfit * diff(h$mids[1:2]) * length(data_overview$SD_b) 
lines(xfit, yfit, col = "black", lwd = 2)

print("maybe normal distributed")

#b) Homogenity?
bartlett.test(SD_b ~ Design, data = data_overview) 
fligner.test(SD_b ~ Design, data = data_overview)
leveneTest(SD_b ~ Design, data = data_overview)

print("definitely homogenous")

# BOOTSTRAPPING

data_boot <- data.frame(data_overview$Design, data_overview$SD_b) #just creating a separate dataframe to not mix-up data
names(data_boot) <- c("Design", "SD_b")

#guide: https://www.datanovia.com/en/lessons/anova-in-r/
model <- lm(SD_b ~ Design, data = data_boot)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

ggqqplot(data_boot, "SD_b", facet.by = "Design")
shapiro.test((data_boot %>% filter(Design=="Onchain"))$SD_b)
shapiro.test((data_boot %>% filter(Design=="Tokenized"))$SD_b)
shapiro.test((data_boot %>% filter(Design=="Algorithmic"))$SD_b)

plot(model,1)
leveneTest(SD_b ~ Design, data=data_boot)

res.aov <- anova(lm(data_boot$SD_b ~ factor(data_boot$Design)))
res.aov

#bootstrap anova per https://rpubs.com/howelb/39050 -> this creates the empirical F-distribution, which serves as Test statistic of the bootstrap
#normal

meanstar = mean(data_boot$SD_b)
sdstar = sqrt(res.aov$`Mean Sq`[2])
simdesign = data_boot$Design
R=10000
Fstar = numeric(R)
for (i in 1:R){
  groupA = rnorm(4, mean=meanstar, sd=sdstar)
  groupO = rnorm(15, mean=meanstar, sd=sdstar)
  groupT = rnorm(8, mean=meanstar, sd=sdstar)
  simGKb =c(groupA,groupO,groupT)
  simdata=data.frame(simGKb,simdesign)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar[i]= oneway.test(simGKb~simdesign, var.equal = T, data=simdata)$statistic}
}
par(mfrow=c(1,1))
hist(Fstar, ylim=c(0,1), xlim=c(0, 8), prob=T, main = "Historgram of Empirical F-distribution")
x=seq(.25,6,.25)
points(x,y=df(x,2,24),type="b",col="red")

#bootstrap #GK_=strateur, joblost=design)
meanstar = with(data_boot,tapply(SD_b,Design,mean))
grpA = data_boot$SD_b[data_boot$Design=="Algorithmic"]-meanstar[1]
grpO = data_boot$SD_b[data_boot$Design=="Onchain"]-meanstar[2]
grpT = data_boot$SD_b[data_boot$Design=="Tokenized"]-meanstar[3]
simdesign = data_boot$Design
R=10000
Fstar_real = numeric(R)

for (i in 1:R){
  groupA = sample(grpA, size=4, replace=T)
  groupO = sample(grpO, size=15, replace=T)
  groupT = sample(grpT, size=8, replace=T)
  simGKb =c(groupA,groupO,groupT)
  simdata=data.frame(simGKb,simdesign)
  if(sd(groupA)==0 & sd(groupO)==0 & sd(groupT)==0) {Fstar_real[i]=NA}
  else{Fstar_real[i]= oneway.test(simGKb~simdesign, var.equal = T, data=simdata)$statistic}
}

par(mfrow=c(1,1))
hist(Fstar_real, ylim=c(0,1), xlim=c(0, 8), prob=T, main = "Historgram of Empirical F-distribution")
x=seq(.25,6,.25)
points(x,y=df(x,2,24),type="b",col="red")

print(realFstar<-oneway.test(SD_b~Design, var.equal=T, data=data_boot)$statistic)
mean(Fstar>=realFstar) #comparing against empirical F-distribution
qf(.95,2,24)
quantile(Fstar_real,.95)






#Plot of GK
  # GK All
  ggplot(data=data_base_3, aes(x = Date, y = GK, color = Ticker, group = format(Date, "%Y-%m", fill = "Ticker"))) +
  geom_line() + theme_test() + theme_bw(base_size=9)

  # GK Tokenized
  ggplot(data=data_base_3[data_base_3$Design =="Tokenized",], aes(x = Date, y = GK, color = Ticker, 
  group = format(Date, "%Y-%m", fill = "Ticker"))) +  geom_line()  +   theme_test()  + ylim(0,0.) + theme_bw(base_size=9)
  
  # GK Onchain
  ggplot(data=data_base_3[data_base_3$Design =="Onchain",], aes(x = Date, y = GK, color = Ticker, 
  group = format(Date, "%Y-%m", fill = "Ticker"))) +  geom_line()  +   theme_test()  + ylim(0,0.6) + theme_bw(base_size=9)

  # CC Return Algoritmic
  ggplot(data=data_base_3[data_base_3$Design =="Algorithmic",], aes(x = Date, y = GK, color = Ticker, 
  group = format(Date, "%Y-%m", fill = "Ticker"))) + geom_line() + theme_test() +  theme_bw(base_size=9)

  
library(psych)
  describe.by(data_base_3, data_base_3$Design)







# Ranking of MC
summary(lm(MC~Design, data_base_2))

a <- ddply(data_base_2, .(Ticker), dplyr::summarize,
     Ticker = unique(Ticker),
     MC = mean(MC, na.rm=TRUE),
     Design = unique(Design)
)
a[order(a[,2]),]























#Methods can be used also for unbalanced but depend on software implementation // stockwatson p.351. 
#Clustering by ticker means, it corrects autocorrelation in each ticker but asumes the distint tickers are not correlated  // p.11: http://qed.econ.queensu.ca/pub/faculty/mackinnon/working-papers/qed_wp_1421.pdf 
#FE s appropiate: file:///C:/Users/Rapha/Downloads/14505_paper_fay8TyZ8.pdf

# Fixed effect analysis

m1_6m <- felm(Volatility ~ Marketcap , data = panel_data_6m) #linear
m2_6m <- felm(Volatility ~ Marketcap| Window  | 0  | Ticker, data = panel_data_6m) # + time FE
m3_6m <- felm(Volatility ~ Marketcap + factor(Design) | Window  | 0  | Ticker, data = panel_data_6m) # + time FE + Design
m4_6m <- felm(Volatility ~ Marketcap + factor(Design) +  factor(Ticker) | Window  | 0  | Ticker, data = panel_data_6m) # + time FE + entity FE + Design
m5_6m <- felm(Volatility ~ Marketcap + factor(Design) + factor (Ticker) + Volume + factor(IB20) | Window  | 0  | Ticker, data = panel_data_6m) # + time FE + entitiy FE + Design + Volume + IB2020

m6_6m <- felm(Volatility ~ Marketcap + factor(Design) + factor(Ticker) +  factor(IB20)  | Window  | 0  | Ticker, data = panel_data_6m) # + Time FE + Entity FE without volume
m7_6m <- felm(Volatility ~ Marketcap + factor(Ticker) + NotAlgo + factor(IB20) | Window  | 0  | Ticker, data = panel_data_6m) # alternative dummy for design type

# Store the standard errors that are to be displayed separately.
vol_se_6m <- list(sqrt(diag(vcovHC(m1_6m, type = "HC1"))), # Uses HC1, which is not clustered errors, but not basic OLS errors either (it's heteroskedastic-robust)
                  summary(m2_6m)[["coefficients"]][,2],
                  summary(m3_6m)[["coefficients"]][,2],
                  summary(m4_6m)[["coefficients"]][,2],
                  summary(m5_6m)[["coefficients"]][,2],
                  summary(m6_6m)[["coefficients"]][,2],
                  summary(m7_6m)[["coefficients"]][,2])

# Use "stargazer" to replicate the Table 10.1.
#coeftest(fatalities_mod1, vcov = vcovHC(fatalities_mod1, type="HC1"))
stargazer(m1_6m,m2_6m,m3_6m,m4_6m,m5_6m,m6_6m,m7_6m, 
          digits = 3,
          header = FALSE,
          font.size = "tiny",
          align = TRUE,
          no.space=TRUE, 
          table.layout ="=lt-a-s=n",
          type = "text", # You can add latex here as well
          out = "Panel_Analysis_6m.html", # Output file is html for simplicity
          title = "Regression Analysis of the Effect of Marketcap on Stablecoin Volatility",
          dep.var.labels.include = FALSE,
          dep.var.caption = c("Volatility (log of SD"),
          se = vol_se_6m,
          model.numbers = FALSE,
          #covariate.labels=c("Volume", "Onchain", "Tokenized", "Marketcap", "IB20", "Turnover"),
          omit = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10", "Constant", "Window", 
                   "BUSD", "GUSD", "HUSD", "TUSD", "USDC", "USDK", "USDT",  "USDP","DAI", "EOSDT", "LUSD", "MIM", 
                   "MUSD", "OUSD", "RSV", "SUSD", "USDX", "VAI", "ALUSD", "FRAX", "CUSD", "USDS",  "USDP1", "FEI",
                   "USTC", "SBD", "USDN", "TOR"),
          star.char = c("+", "*", "**"),
          star.cutoffs = c(.1, .05, .01),
          omit.stat = c("rsq", "ser"),
          add.lines=list(
            c("", "", "", "", "", "", "", ""),
            c("Windows", "1-10", "1-10", "1-10", "1-10", "1-10", "1-10", "1-10"),
            c("Ticker effects?", "no", "no","(no)","yes","yes","yes","yes"),
            c("Time effects?", "no","yes","yes","yes","yes","yes","yes"),
            c("Clustered standard errors?", "no","yes","yes","yes","yes","yes","yes"),
            c("", "","","","","","",""),
            c("", "","","","","","","")),
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
          notes = c("These regressions were estimated using panel data for 27 Stablecoins."))


#Check assumptions
# https://rpubs.com/honkalimonka/UL1#:~:text=the%20wrong%20clusters.-,Conclusion,cross%2Dsectionally%20or%20across%20time.


#####################################
# Fixed effect analysis - 1y
#####################################

m1_1y <- felm(Volatility ~ Marketcap, data = panel_data_1y) #linear
m2_1y <- felm(Volatility ~ Marketcap| Window  | 0  | Ticker, data = panel_data_1y) # + time FE
m3_1y <- felm(Volatility ~ Marketcap + factor(Design) | Window  | 0  | Ticker, data = panel_data_1y) # + time FE + Design
m4_1y <- felm(Volatility ~ Marketcap + factor(Design) +  factor(Ticker) | Window  | 0  | Ticker, data = panel_data_1y) # + time FE + entity FE + Design
m5_1y <- felm(Volatility ~ Marketcap + factor(Design) + factor (Ticker) + Volume + factor(IB20) | Window  | 0  | Ticker, data = panel_data_1y) # + time FE + entitiy FE + Design + Volume + IB2020

m6_1y <- felm(Volatility ~ Marketcap + factor(Design) + factor(Ticker) +  factor(IB20)  | Window  | 0  | Ticker, data = panel_data_1y) # + Time FE + Entity FE without volume
m7_1y <- felm(Volatility ~ Marketcap + factor(Ticker) + NotAlgo + factor(IB20) | Window  | 0  | Ticker, data = panel_data_1y) # alternative dummy for design type

# Store the standard errors that are to be displayed separately.
vol_se_1y <- list(sqrt(diag(vcovHC(m1_1y, type = "HC1"))), # Uses HC1, which is not clustered errors, but not basic OLS errors either (it's heteroskedastic-robust)
                  summary(m2_1y)[["coefficients"]][,2],
                  summary(m3_1y)[["coefficients"]][,2],
                  summary(m4_1y)[["coefficients"]][,2],
                  summary(m5_1y)[["coefficients"]][,2],
                  summary(m6_1y)[["coefficients"]][,2],
                  summary(m7_1y)[["coefficients"]][,2])

# Use "stargazer" to replicate the Table 10.1.
#coeftest(fatalities_mod1, vcov = vcovHC(fatalities_mod1, type="HC1"))
stargazer(m1_1y,m2_1y,m3_1y,m4_1y,m5_1y,m6_1y,m7_1y, 
          digits = 3,
          header = FALSE,
          font.size = "tiny",
          align = TRUE,
          no.space=TRUE, 
          table.layout ="=lt-a-s=n",
          type = "text", # You can add latex here as well
          out = "Panel_Analysis_1y.html", # Output file is html for simplicity
          title = "Regression Analysis of the Effect of Marketcap on Stablecoin Volatility",
          dep.var.labels.include = FALSE,
          dep.var.caption = c("Volatility (log of SD"),
          se = vol_se_1y,
          model.numbers = FALSE,
          #covariate.labels=c("Volume", "Onchain", "Tokenized", "Marketcap", "IB20", "Turnover"),
          omit = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10", "Constant", "Window", 
                   "BUSD", "GUSD", "HUSD", "TUSD", "USDC", "USDK", "USDT",  "USDP","DAI", "EOSDT", "LUSD", "MIM", 
                   "MUSD", "OUSD", "RSV", "SUSD", "USDX", "VAI", "ALUSD", "FRAX", "CUSD", "USDS",  "USDP1", "FEI",
                   "USTC", "SBD", "USDN", "TOR"),
          star.char = c("+", "*", "**"),
          star.cutoffs = c(.1, .05, .01),
          omit.stat = c("rsq", "ser"),
          add.lines=list(
            c("", "", "", "", "", "", "", ""),
            c("Windows", "1-10", "1-10", "1-10", "1-10", "1-10", "1-10", "1-10"),
            c("Ticker effects?", "no", "no","(no)","yes","yes","yes","yes"),
            c("Time effects?", "no","yes","yes","yes","yes","yes","yes"),
            c("Clustered standard errors?", "no","yes","yes","yes","yes","yes","yes"),
            c("", "","","","","","",""),
            c("", "","","","","","","")),
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
          notes = c("These regressions were estimated using panel data for 27 Stablecoins."))










################################################################################################################
################################################################################################################
################################################################################################################

#a) Normality?
shapiro.test(log(panel_data_1m$Volatility))       #p-value = 7.032e-07 / non-normal
shapiro.test(panel_data_1m$Marketcap) 
shapiro.test(panel_data_1m$Volume)
ks.test(panel_data_1m$Volume, "pnorm")   #p-value = 8.33e-07 / non-normal
plot(panel_data_1m$Volatility)

#Chatgpt
m3 <- felm(Volatility ~ Marketcap + factor(Design) | Window | 0 | Ticker, data = panel_data_1m)
m4 <- felm(Volatility ~ Marketcap + factor(Design) + factor(Ticker)| Window  | 0  | Ticker, data = panel_data_1m)
m5 <- felm(Volatility ~ factor(Design) | Window  | 0  | Ticker, data = panel_data_1m) 
m6 <- felm(Volatility ~  Marketcap + factor(Design) + Volume + factor(IB20)| Window  | 0  | Ticker, data = panel_data_1m) 

summary(m3)
summary(m4)
summary(m5)
summary(m6)

################################################################################################################
################################################################################################################
################################################################################################################
library(lmtest)
#stationarity
adf.test(panel_data_1m$Volatility)
adf.test(panel_data_1m$Marketcap)
adf.test(panel_data_1m$Volume)

model <- plm(Volatility ~ Marketcap + factor(Design) + Volume + factor(IB20), 
             data = panel_data_1m, index = c("Window", "Ticker"), effect = "individual", model = "within")
plmTest <- coeftest(model,vcov=vcovHC(model,type = "HC3", cluster="group"))

pcdtest(model, test=c("lm"))
pcdtest(model, test=c("cd"))
pbgtest(model)

################################################################################################################
################################################################################################################
################################################################################################################
library(sandwich)
m5_1m <- plm(Volatility ~ Marketcap + factor(Design) + Volume + factor(IB20), 
             data = panel_data_1m,
             index = c("Ticker", "Window"), 
             model = "within", 
             effect = "twoways")

m5_1m <- lm(Volatility ~ Marketcap + Window=1 + Window=3 + factor(Design) + Volume + factor(IB20)-1, data=panel_data_1m )

coeftest(m5_1m, vcov = vcovHC, type = "HC3")
coeftest(m5_1m, vcov=vcovHAC(m5_1m))
summary(m5_1m)


m5_1m <- felm(Volatility ~ Marketcap + factor(Design) + factor (Ticker) + Volume + factor(IB20) | Window  | 0  | 0, data = panel_data_1m) # + time FE + entitiy FE + Design + Volume + IB2020


################################################################################################################
################################################################################################################
################################################################################################################
# Fixed effect analysis
m1 <- felm(Volatility ~ Marketcap , data = panel_data_6m) #linear
m2 <- felm(Volatility ~ Marketcap | Window  | 0  | 0, data = panel_data_6m) # + time FE
m2 <- felm(Volatility ~ Marketcap | Window  | 0  | Ticker, data = panel_data_6m) # + time FE

m2.1 <- lm(Volatility ~ Marketcap + factor(Window) + factor(Ticker), data=panel_data_6m)
model <- plm(Volatility ~ Marketcap + factor(Design) + Volume + factor(IB20), data = panel_data_6m, 
             index = c("Window", "Ticker"), model = "within",effect="twoways")

summary(model)
summary(m2.1)
summary(m2.2)

coeftest(m2.1, vcov = vcovHC, type = "HC3")
coeftest(m2.1, vcov=vcovHAC(m2.1))


library(lme4)

fit <- lmer(Volatility ~ Marketcap + factor(Window) + factor(Ticker) + factor(Design) 
            + factor(IB20) + Volume + (1|Ticker), data=panel_data_1m)
summary(fit)
coeftest(fit)
################################################################################################################
##############################  Descriptive #############################################################################
################################################################################################################

#Plot vol/marketcap/volume
scatter3D(panel_data_1m$Volume, panel_data_1m$Marketcap,panel_data_1m$Volatility, phi = 0, bty ="g",
          type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.8)

# Full model w/o SE correction
model <- plm(Volatility ~ Marketcap + factor(Design) + Volume + factor(IB20), data = panel_data_1m, 
             index = c("Window", "Ticker"), model = "within",effect="twoways")

# cross-sectional dependence
pcdtest(model, test=c("lm"))
pcdtest(model, test=c("cd"))

# stationarity
gravity_ln.set <- plm.data(panel_data_1m, index=c("Ticker", "Window"))
adf.test(gravity_ln.set$Volatility)

# heteroskedasticity present - robust covariance matrix needed
bptest(Volatility ~ Marketcap + factor(Design) + Volume + factor(IB20) + factor(Ticker) + 
         factor(Window), data=panel_data_1m, studentize=F)

# serial correlation present
pbgtest(model)

cov2cor(vcovHC(model, type = "HC4", cluster="group"))
cov2cor(vcovCR(randomeff, gravity_ln$county,type = "CR2"))


# Cluster-robust variance estimator with clustering by county
t(sapply(c("CR0", "CR1", "CR2", "CR3", "CR1S", "CR1p"), 
         function(x) sqrt(diag(vcovCR(model, panel_data_1m$Ticker,type = x)))))

################################################################################################################
################################################################################################################
################################################################################################################

plotmeans(Volatility ~ Window, main="Heterogeneity across windows", data=panel_data_1m)
plotmeans(Volatility ~ Design, main="Heterogeneity across design", data=panel_data_1m)

################################################################################################################
################################################################################################################
################################################################################################################

x <- panel_data_1m$Volume
y <- panel_data_1m$Marketcap
z <- panel_data_1m$Volatility


plot(panel_data_1m$Volatility, panel_data_1m$Marketcap,
     pch = 19,
     col = factor(panel_data_1m$Design),
)

legend("topright",
       legend = levels(factor(panel_data_1m$Design)),
       pch = 19,
       col = factor(levels(factor(panel_data_1m$Design))))





################################################################################################################
################################################################################################################
################################################################################################################
scatter3D(x, y, z, phi = 0, bty ="g")





################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################




