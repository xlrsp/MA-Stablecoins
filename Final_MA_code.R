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
all_crypto <- crypto_list(only_active = FALSE, add_untracked = TRUE)
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

  # Apply nparcomp as developed by Konietschke et al. (2015) to all windows
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

  # Apply nparcomp as developed by Konietschke et al. (2015) to all windows
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
  panel_data_1y <- panel_data_1y[complete.cases(panel_data_1y), ] 

  
                                          #############################
############################################   Check Assumptions   #########################################
                                          ############################# 
  
# Specify models  
  # Linear
  m1 <- felm(Volatility ~ Marketcap| 0 | 0 | Ticker, data = panel_data_1m)
  # MC + Time FE
  m2 <- felm(Volatility ~ Marketcap | Window | 0 | Ticker, data = panel_data_1m)
  # MC + Design + Time FE
  m3 <- felm(Volatility ~ Marketcap + factor(Design) | Window | 0 | Ticker, data = panel_data_1m) 
  # MC + Design + Time FE + Entity FE (mulicolinearity!)
  m4 <- felm(Volatility ~ Marketcap | Window+Ticker  | 0  | Ticker, data = panel_data_1m)
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
for (i in 1:length(p_models)) {
  temp <- data.frame(model = paste0("m", i),
                     statistic = bptest(p_models[[i]])$statistic,
                     p.value = bptest(p_models[[i]])$p.value)
  results_bp <- rbind(results_bp, temp)
}; colnames(results_bp) <- c("Model", "Statistic", "p-value")

stargazer(as.matrix(results_bp), type = "text",title = "Breusch-Pagan Test Results",out = "Panel BP results.html",
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
vif(m4)   # n/a as only 1 independent var
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
  m1 <- felm(Volatility ~ Marketcap| 0 | 0 | Ticker+Window, data = pdata)
  # MC + Time FE
  m2 <- felm(Volatility ~ Marketcap | Window | 0 | Ticker+Window, data = pdata)
  # MC + Design + Time FE
  m3 <- felm(Volatility ~ Marketcap + factor(Design) | Window | 0 | Ticker+Window, data = pdata) 
  # MC + Design + Time FE + Entity FE (mulicolinearity!)
  m4 <- felm(Volatility ~ Marketcap  | Window+Ticker  | 0  | Ticker+Window, data = pdata)
  # Design + Time FE
  m5 <- felm(Volatility ~ factor(Design) | Window  | 0  | Ticker+Window, data = pdata) 
# Sensitivity
  # MC + Design + add.independent + Time FE
  m6 <- felm(Volatility ~  Marketcap + factor(Design) + Volume + factor(IB20)| Window  | 0  | Ticker+Window, data = pdata) 
  # MC + Volume + Time FE + Entity FE
  m7 <- felm(Volatility ~ Marketcap + Volume | Window + Ticker | 0  | Ticker+Window, data = pdata) 
  # MC + alt.Design + alt.ind. + Time FE
  m8 <- felm(Volatility ~ Marketcap + NotAlgo + factor(IB20) | Window  | 0  | Ticker+Window, data = pdata) 

# Store the standard errors that are to be displayed separately.
vol_se <- list(summary(m1)[["coefficients"]][,2], 
                  summary(m2)[["coefficients"]][,2], 
                  summary(m3)[["coefficients"]][,2],
                  summary(m4)[["coefficients"]][,2],
                  summary(m5)[["coefficients"]][,2],
                  summary(m6)[["coefficients"]][,2],
                  summary(m7)[["coefficients"]][,2],
                  summary(m8)[["coefficients"]][,2])

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
          star.char = c("*", "**", "***"),
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
                                    GK = log(GK)-Abase,
                                    #Return = Return, #variable not needed
                                    MC = log(MC),
                                    #TE = TE, #variable not needed
                                    Pre = ifelse(Date<"2022-05-09"&Date>="2022-04-30",1,0),
                                    Event_d0 = ifelse(Date=="2022-05-09",1,0),
                                    Event_d1 = ifelse(Date=="2022-05-10",1,0),
                                    Event_d2 = ifelse(Date=="2022-05-11",1,0),
                                    Event_d3 = ifelse(Date=="2022-05-12",1,0),
                                    Event_d4 = ifelse(Date=="2022-05-13",1,0),
                                    Event_d5 = ifelse(Date=="2022-05-14",1,0),
                                    Event_d1_5 = ifelse(Date>="2022-05-09"&Date<="2022-05-14",1,0),
                                    Post = ifelse(Date>"2022-05-14"&Date<="2022-05-25",1,0),
                                    Post_1m = ifelse(Date>"2022-05-12"&Date<="2022-06-13",1,0),
                                    Post_3m = ifelse(Date>"2022-05-12"&Date<="2022-08-13",1,0),
                                    Ticker = Ticker
                                    #Post_1mm = ifelse(Date>"2022-05-20"&Date<="2022-06-20",1,0) ,  #variable not needed
                                    #Post_2mm =ifelse(Date>"2022-06-20"&Date<="2022-07-20",1,0),    #variable not needed
                                    #Post_3mm =ifelse(Date>"2022-07-20"&Date<="2022-08-20",1,0),    #variable not needed
                                    #Post_4mm =ifelse(Date>"2022-08-20"&Date<="2022-09-20",1,0)     #variable not needed
)}

# Calculate variables and subset data by Design
event_data_all <- event_data(data_base_3)
event_data_tokenized <- event_data(data_base_3[data_base_3$Design=="Tokenized",])
event_data_onchain <- event_data(data_base_3[data_base_3$Design=="Onchain",])
event_data_algo <- event_data(data_base_3[data_base_3$Design=="Algorithmic",])
  
  # Check completeness
  table(event_data_all$Ticker)
  Abase <- mean(event_data_all[event_data_all$Date<"2022-04-30",]$GK_nt)
  
# Apply models

  # Base model (10d post period)
  eventstudy_all <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3 + Event_d4+Event_d5+ Post, data = event_data_all)
  eventstudy_tokenized <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  +Event_d4+Event_d5+ Post, data = event_data_tokenized)
  eventstudy_onchain <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2  + Event_d3  + Event_d4+Event_d5+Post, data = event_data_onchain)
  eventstudy_algo <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  + Event_d4+Event_d5+Post, data = event_data_algo)
  
  # Summarized/average event effect
  eventstudy_all_sum <- lm(GK ~ Pre + Event_d1_5 + Post, data = event_data_all)
  eventstudy_tokenized_sum <- lm(GK ~ Pre + Event_d1_5 + Post, data = event_data_tokenized)
  eventstudy_onchain_sum <- lm(GK ~ Pre + Event_d1_5 + Post, data = event_data_onchain)  
  eventstudy_algo_sum <- lm(GK ~ Pre + Event_d1_5 + Post, data = event_data_algo)
  
  # 1 month post period (models not included)
  # eventstudy_all <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3 +Event_d4+Event_d5+ Post_1m, data = event_data_all)
  # eventstudy_tokenized <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  + Event_d4+Event_d5+Post_1m, data = event_data_tokenized)
  # eventstudy_onchain <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2  + Event_d3  +Event_d4+Event_d5+ Post_1m, data = event_data_onchain)
  # eventstudy_algo <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  + Event_d4+Event_d5+Post_1m, data = event_data_algo)

  # 3 months post period (models not included)
  # eventstudy_all <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3 + Event_d4+Event_d5+Post_3m, data = event_data_all)
  # eventstudy_tokenized <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  + Event_d4+Event_d5+Post_3m, data = event_data_tokenized)
  # eventstudy_onchain <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2  + Event_d3  + Event_d4+Event_d5+Post_3m, data = event_data_onchain)
  # eventstudy_algo <- lm(GK ~ Pre + Event_d0 + Event_d1 + Event_d2 + Event_d3  +Event_d4+Event_d5+ Post_3m, data = event_data_algo)

# Prepare for stargazer table (shorten model name). 
# Note: Perform the following steps once for standard model and once for the summarized/av. model 
e1 <- eventstudy_all; e2 <- eventstudy_tokenized; e3 <- eventstudy_onchain; e4 <- eventstudy_algo # Base model
e1 <- eventstudy_all_sum; e2 <- eventstudy_tokenized_sum; e3 <- eventstudy_onchain_sum; e4 <- eventstudy_algo_sum # Av. effect

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
  
# Due to heteroskedacity and autocorrelation, HAC standard errors seem most appropiate

#clustered
ev_se <- list(coeftest(e1, vcov=vcovHC(e1, cluster="Ticker"))[,2], 
              coeftest(e2, vcov=vcovHC(e2, cluster="Ticker"))[,2],
              coeftest(e3, vcov=vcovHC(e3, cluster="Ticker"))[,2],
              coeftest(e4, vcov=vcovHC(e4, cluster="Ticker"))[,2])
#HAC
ev_se <- list(coeftest(e1, vcov=vcovHAC(e1, type = "HC1", cluster = "Ticker"))[,2], 
              coeftest(e2, vcov=vcovHAC(e2, type = "HC1", cluster = "Ticker"))[,2],
              coeftest(e3, vcov=vcovHAC(e3, type = "HC1", cluster = "Ticker"))[,2],
              coeftest(e4, vcov=vcovHAC(e4, type = "HC1", cluster = "Ticker"))[,2])

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
          dep.var.caption = c("Volatility (logGK)"),
          se = ev_se,
          model.numbers = TRUE,
          #covariate.labels=c("Pre 10d", "Event d+0", "Event d+1", "Event d+2", "Event d+3", "Event d+4", "Post 5d", "Intercept"),
          #omit = c(),
          star.char = c("*", "**", "***"),
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
