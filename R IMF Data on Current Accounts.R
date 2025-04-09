#
# Replicating KJ Forbes & Warnock 2012, 2020 paper on GICFs
# https://mitmgmtfaculty.mit.edu/kjforbes/research/
# GICFs == gross int'l capital flows
# Started: 1/2/2022
#

source("C:/R/libraries.R")
library(haven)   # lib to read *.dta files (STATA)
library(readxl);library(writexl);library(xlsx);library(readxl) 

yr <- year(Sys.Date()) # current year

# IMF's IFS data set -----------------------------------------------------------+

# Codes:
# NGDP_R_SA_XDC  - GDP Qtly, real, SA-adj'd, in national money of account
# BCA_BP6_USD    - C/A standard presentation
# BCAXF_BP6_USD  - C/A analytical presentation
# BGS_BP6_USD    - Goods & Services Balance
#
# Source #1: 
# https://www.elibrary.imf.org/display/book/9781484329801/9781484329801.xml
# Source #2:
# https://data.imf.org/regular.aspx?key=61545851

#A. Current Account* BCAXF_BP6_USD 
#Goods, credit (exports) BXG_BP6_USD
#Goods, debit (imports) BMG_BP6_USD
#Balance on goods BG_BP6_USD
#Services, credit (exports) BXS_BP6_USD
#Services, debit (imports) BMS_BP6_USD
#Balance on Goods & Services BGS_BP6_USD 
#Primary income: credit BXIP_BP6_USD
#Primary income: debit BMIP_BP6_USD
#Balance on goods, services & primary income BTGSI_BP6_USD
#Secondary income: credit BXISXF_BP6_USD 
#Secondary income: debit BMIS_BP6_USD

# Downloading data from IMF via JSON
u.xml <- paste0("http://dataservices.imf.org/REST/SDMX_XML.svc/CompactData/IFS/Q..",
                #"BXISXF_BP6_USD+BMIS_BP6_USD+BXIP_BP6_USD+BMIP_BP6_USD",
                #"BFOADG_BP6_USD+BFOADG_BP6_USD+BFOLG_BP6_USD+BFOLG_BP6_USD",
                #"BFOA_BP6_USD+BFOLXF_BP6_USD+BFPA_BP6_USD+BFPAE_BP6_USD+BFPAD_BP6_USD+BFPLXF_BP6_USD+BFPLEXF_BP6_USD+BFPLDXF_BP6_USD",
                #"BFDA_BP6_USD+BFDAE_BP6_USD+BFDAD_BP6_USD+BFDLXF_BP6_USD+BFDLEXF_BP6_USD+BFDLDXF_BP6_USD",
                "BXG_BP6_USD+BMG_BP6_USD+BXS_BP6_USD+BMS_BP6_USD+BXIP_BP6_USD+BMIP_BP6_USD+BXIPCE_BP6_USD+BMIPCE_BP6_USD+BXIPI_BP6_USD+BMIPI_BP6_USD+BXISXF_BP6_USD+BMIS_BP6_USD",
                "BGS_BP6_USD+BCAXF_BP6_USD+NGDP_R_SA_XDC+PWPI_IX+PPPI_IX",
                ".?startPeriod=1940&endPeriod=",yr) # ...through current year
u.xml
data <- xmlTreeParse(file=u.xml, useInternalNode=TRUE)
xl   <- xmlToList(data)
df <- data.frame()
for (i in 1:length(xl$DataSet)) {
  area <- 
    data.frame(FREQ        = xl$DataSet[i]$Series$.attrs[1],
               REF_AREA    = xl$DataSet[i]$Series$.attrs[2],
               INDICATOR   = xl$DataSet[i]$Series$.attrs[3],
               UNIT_MULT   = xl$DataSet[i]$Series$.attrs[4],
               TIME_FORMAT = xl$DataSet[i]$Series$.attrs[5])
  obs <- data.frame()
  for (j in 1:(length(xl$DataSet[i]$Series)-1)) {
    obs <- rbind(obs, cbind(area,
                       data.frame(TIME_PERIOD = xl$DataSet[i]$Series[j]$Obs[1],
                                  OBS_VALUE   = as.numeric(xl$DataSet[i]$Series[j]$Obs[2]))))
  }
  df <- rbind(df, obs)
}
head(df); tail(df); rownames(df) <- NULL
df <- df[df$REF_AREA!="1C_355",]

# Number of countries/regions in the data set
c <- df %>% group_by(REF_AREA) %>% summarise(n=n()); c; #View(c)
df %>% group_by(INDICATOR) %>% summarise(n=n())
df[df$REF_AREA=="GH",] %>% group_by(INDICATOR) %>% summarise(n=n())

df$DATE <- as.Date(paste0(substr(df$TIME_PERIOD,1,4),"-",
                          3 * as.numeric(substr(df$TIME_PERIOD,7,7)),"-01"))

t <- df[df$REF_AREA=="UA",]; d1 <- min(t$DATE); d2 <- max(t$DATE)
t %>%
  ggplot(aes(x=DATE, y=OBS_VALUE, group=INDICATOR, color=INDICATOR)) + 
  geom_line() + labs(x = "", y = "") +
  labs(title = t$REF_AREA, subtitle = paste("From",format(d1,"%b-%Y"),"through",format(d2,"%b-%Y")))

# --- TRADE BALANCE ------------------------------------------------------------+

c <- df[df$INDICATOR=="BGS_BP6_USD",] %>% group_by(REF_AREA) %>% summarise(n=n())
t <- df[df$REF_AREA %in% c[c$n>99,]$REF_AREA & 
          df$DATE>=as.Date("1970-01-01") & df$INDICATOR=="BGS_BP6_USD",]
# BCAXF_BP6_USD ; BGS_BP6_USD
d1 <- min(t$DATE); d2 <- max(t$DATE)
t$OBS_VALUE2 <- with(t, ifelse(OBS_VALUE>0,1,0))
n <- nrow(t %>% group_by(REF_AREA) %>% summarise(n=n()))

t %>%
  ggplot(aes(x=DATE, y=REF_AREA)) + 
  geom_tile(aes(fill=OBS_VALUE2)) +
  scale_fill_gradient(low = "#FF3300", high = "#33CC33") +
  theme(legend.position="none") +
  labs(x="",y=paste(n,"Countries"),
       title = paste("IMF's Data on Trade Balances:",n,"Countries"),
       subtitle = paste0("Quarterly history from ",quarters(d1),"-",year(d1),
                        " through ",quarters(d2),"-",year(d2)))

# --- CURRENT ACCOUNT BALANCE --------------------------------------------------+

t <- df[df$INDICATOR=="BCAXF_BP6_USD",]
c <- t[t$INDICATOR=="BCAXF_BP6_USD",] %>% group_by(REF_AREA) %>% summarise(n=n())
t <- t[t$REF_AREA %in% c[c$n>99,]$REF_AREA & t$DATE>=as.Date("1970-01-01"),]
# BCAXF_BP6_USD ; BGS_BP6_USD
d1 <- min(t$DATE); d2 <- max(t$DATE)
t$OBS_VALUE2 <- with(t, ifelse(OBS_VALUE>0,1,0))
n <- nrow(t %>% group_by(REF_AREA) %>% summarise(n=n()))

g <-
  t %>%
  ggplot(aes(x=DATE, y=REF_AREA)) + 
  geom_tile(aes(fill=OBS_VALUE2)) +
  scale_fill_gradient(low = "#FF3300", high = "#33CC33") +
  theme(legend.position="none") + 
  labs(x="",y="",
       title = paste("IMF's Data on Current Accounts:",n,"Countries"),
       subtitle = paste0("Quarterly history from ",quarters(d1),"-",year(d1),
                         " through ",quarters(d2),"-",year(d2))); g
p <- "C:/UMKC/My Working Papers/Dissertation/Data/"
p.ca <- g
ggsave(filename = paste0(p,"CAs_IMF.png"), plot = p.ca, 
       height = 4*8/3, width = 5 * 2.025766871)

t.ca <- t %>% 
  group_by(REF_AREA) %>% 
  summarise(n_qtrs=n(), 
            n_yrs=round(as.numeric(max(DATE) - min(DATE)) / 365), 
            data_since=min(DATE), data_latest=max(DATE))

list.countries <- c

# --- IMF: GRA Purchases and Repurchases ---------------------------------------+
#
# IMF Glossary: https://www.imf.org/external/np/fin/tad/Docs/Glossary.pdf
#
# GRA Purchases = are loans (purchases) disbursed to members under facilities 
#                 of the IMF General Resources Account (GRA).  
# GRA Repurchases = are principal repayments (repurchases) by members
# PRGT Disbursements = are concessional loans disbursed to members under 
# the Poverty Reduction and Growth Trust (PRGT).
# 
# Sourec of data: https://www.imf.org/external/np/fin/tad/query.aspx
#
# Volumes are denominated in SDR

p <- "C:/UMKC/My Working Papers/Dissertation/Data/"
f <- paste0(p,"FLOWS.csv");
#readxl::read_xls(path=paste0(p,"FLOWS.xls"), sheet="FLOWS", skip=9, col_names=TRUE)
#read.xlsx(f, sheetIndex=1,startRow = 10)
df.imf <- read.csv(file=f,header=TRUE,skip=9)
c <- df.imf %>% group_by(Member.Code,Member) %>% summarise (n=n())

imf.2lcods <- read.csv(paste0(p,"Country 2-letter codes.csv"))
imf.3lcods <- read.csv(paste0(p,"Country 3-letter codes (short version).csv"))
nrow(imf.2lcods);nrow(imf.3lcods)

# List of countries: <IMF 3-letter code table> joined by <IMF loans table> 
colnames(c) <- c("ISO_Code_3L","Country_Name","IMF_Obs_N")
colnames(imf.2lcods) <- c("ISO_Code_2L","Country_Name")
colnames(imf.3lcods) <- c("IMF_Code","ISO_Code_3L","Country_Name","Money_of_acc")
imf.3lcods.loans <- left_join(imf.3lcods, c, by=c("ISO_Code_3L"))
colnames(imf.3lcods.loans) <- c("IMF_Code","ISO_Code_3L","Country_Name",
                                "Money_of_acc","Country_Name2","IMF_Obs_N")
imf.3lcods.loans <- left_join(imf.3lcods.loans, imf.2lcods, by=c("Country_Name"))
head(imf.3lcods.loans)
print("*** NUMBER of countries that needs data cleaning ! ***") 
nrow(imf.3lcods.loans[is.na(imf.3lcods.loans$ISO_Code_2L)==TRUE,])

df.imf$Transaction.Value.Date     <- as.Date(df.imf$Transaction.Value.Date, "%m/%d/%Y")
df.imf$Original.Disbursement.Date <- as.Date(df.imf$Original.Disbursement.Date, "%m/%d/%Y")
df.imf$Original.Arrangement.Date  <- as.Date(df.imf$Original.Arrangement.Date, "%m/%d/%Y")
df.imf$quarter <- quarter(df.imf$Transaction.Value.Date)
df.imf$year    <- year(df.imf$Transaction.Value.Date)
df.imf$Amount  <- as.numeric(gsub(",","",df.imf$Amount))

head(df.imf)
df.imf %>% group_by(Flow.Type) %>% summarise(n=n())
z <-
  df.imf[df.imf$Flow.Type!="GRA Repurchases",] %>% 
  group_by(ISO_Code_3L=Member.Code, YR=year, QT=quarter) %>% 
  summarise(Vlm_SDR=sum(Amount))

z <- left_join(z, imf.3lcods.loans[,c("ISO_Code_3L","ISO_Code_2L")], by=c("ISO_Code_3L"))
z$DATE <- as.Date(paste0(z$YR,"-",z$QT*3,"-01"))
z$Vlm_SDR2 <- with(z, ifelse(Vlm_SDR>0,TRUE,FALSE))
  
# Data set on IMF loans
z <- z[z$ISO_Code_2L %in% 
           list.countries[list.countries$n>99,]$REF_AREA & 
           z$DATE>=as.Date("1970-01-01"),]
nrow(z)  
# The heatmap plot with two data sets: df1 & df2
g <-
  z %>%
  ggplot(aes(x=DATE, y=ISO_Code_2L)) + 
  geom_tile(aes(fill=Vlm_SDR2)) +
  scale_fill_gradient(low = "#FF3300", high = "#33CC33") +
  theme(legend.position="none") + 
  labs(x="",y="",
       title = paste("IMF's Data on Current Accounts:",n,"Countries"),
       subtitle = paste0("Quarterly history from ",quarters(d1),"-",year(d1),
                         " through ",quarters(d2),"-",year(d2))); g
p.imf.loans <- g

ggarrange(p.ca, p.imf.loans, ncol = 2, nrow = 1, align = "h", heights = c(3,1.5))

ggsave(filename = paste0(p,"CAs_IMFloans.png"), plot = last_plot(), 
       height = 4*4/3, width = 4 * 2.025766871)

#
# Solution on layering two data sets was borrowed from 
# https://stackoverflow.com/questions/42641106/add-a-second-geom-tile-layer-in-ggplot
#

tt <- t[,c("REF_AREA","DATE","OBS_VALUE2")];  colnames(tt) <- c("ISO_Code_2L","DATE","CA_BAL")
zz <- z[,c("ISO_Code_2L","DATE","Vlm_SDR2")]; colnames(zz) <- c("ISO_Code_2L","DATE","IMF_LOAN")
df <- left_join(tt, zz, by = c("ISO_Code_2L","DATE"))
df[is.na(df$IMF_LOAN)==TRUE,]$IMF_LOAN <- FALSE

d1 <- min(df$DATE); d2 <- max(df$DATE)
n <- nrow(df %>% group_by(ISO_Code_2L) %>% summarise(n=n()))

g <-
  df %>%
  ggplot(aes(x=DATE, y=ISO_Code_2L)) + 
  geom_raster(aes(fill=CA_BAL)) +
  geom_tile(aes(colour=IMF_LOAN), fill = '#00000000', size = .6) + 
  scale_fill_gradient(low = "#FF3300", high = "#33CC33", name="Current account \nbalances",
                      limits=c(0, 1), breaks=c(0,1), labels=c("Deficits","Surpluses")) +
  scale_color_manual(values = c('#00000000', 'blue'), name="Borrowing from \nthe IMF") +
  theme(legend.title=element_text(size=10, face="bold"),
        plot.caption = element_text(hjust=0, size=8)) + 
  labs(x="",y=paste(n,"Countries"),
       title = paste("Current Account Balances & Borrowings from the IMF:",n,"Countries"),
       subtitle = paste0("Quarterly history from ",quarters(d1),"-",year(d1),
                         " through ",quarters(d2),"-",year(d2)),
       caption = "Source: IMF. Note: data on the borrowings from the IMF is available starting from January 1983."); g

ggsave(filename = paste0(p,"CAs_IMF_loans.png"), plot=g, 
       height = 4*8/3, width = 5 * 2.025766871)

# --- China Int'l Finance ------------------------------------------------------+
#
# Source: 
# https://docs.aiddata.org/ad4/datasets/AidDatas_Global_Chinese_Development_Finance_Dataset_Version_2_0.zip
#

f <- "https://docs.aiddata.org/ad4/datasets/AidDatas_Global_Chinese_Development_Finance_Dataset_Version_2_0.zip"
#f <- "https://docs.aiddata.org/ad4/datasets/AidDatas_Global_Chinese_Development_Finance_Dataset_Version_3_0.zip"
f.xlsx <- "AidDatasGlobalChineseDevelopmentFinanceDataset_v2.0.xlsx"

curl::curl_download(url = f, destfile = paste0(p,"tmp.zip"))
unzip(zipfile = paste0(p,"tmp.zip"), exdir = substr(p,1,nchar(p)-1))
f <- paste0(p,"AidDatas_Global_Chinese_Development_Finance_Dataset_Version_2_0/",f.xlsx)

ch <- readxl::read_xlsx(f, sheet = "Global_CDF2.0")
ch %>% group_by(ch$`Financier Country`) %>% summarise(n=n())
ch %>% group_by(ch$Recipient) %>% summarise(n=n(),vlm=sum(ch$`Amount (Nominal)`))
ch %>% group_by(ch$`Funding Agencies`) %>% summarise(n=n())
ch$YR <- with(ch, 
  ifelse(is.na(ch$`Implementation Start Year`),ch$`Commitment Year`,ch$`Implementation Start Year`))
ch$QT <- with(ch, 
  ifelse(is.na(ch$`Actual Implementation Start Date (MM/DD/YYYY)`),
                         4,quarter(ch$`Actual Implementation Start Date (MM/DD/YYYY)`)))

ch[is.na(ch$`Amount (Nominal)`)==TRUE,]$`Amount (Nominal)` <- 0
cc <- ch %>% group_by(Country_Name=ch$Recipient, YR, QT) %>% 
  summarise(vlm_USD=sum(ch$`Amount (Nominal)`))
cc$DATE <- as.Date(paste0(cc$YR,"-",cc$QT,"-01"))
cc$CHINA_LOAN <- TRUE

cc <- left_join(cc, imf.3lcods.loans[,c("Country_Name","ISO_Code_2L")], by=c("Country_Name"))
cc %>% group_by(ISO_Code_2L) %>% summarise(n=n())

#tt <- t[,c("REF_AREA","DATE","OBS_VALUE2")];  colnames(tt) <- c("ISO_Code_2L","DATE","CA_BAL")
#cc <- cc[,c("ISO_Code_2L","DATE","Vlm_USD")]; colnames(zz) <- c("ISO_Code_2L","DATE","IMF_LOAN")
df <- left_join(tt, cc[,c(4:7)], by = c("ISO_Code_2L","DATE")); 
df[is.na(df$CHINA_LOAN)==TRUE,]$CHINA_LOAN <- FALSE
df <- df[,c(1:3,5)]

d1 <- min(df$DATE); d2 <- max(df$DATE)
n <- nrow(df %>% group_by(ISO_Code_2L) %>% summarise(n=n()))

g <-
  df %>%
  ggplot(aes(x=DATE, y=ISO_Code_2L)) + 
  geom_raster(aes(fill=CA_BAL)) +
  geom_tile(aes(colour=CHINA_LOAN), fill = '#00000000', size = .6) + 
  scale_fill_gradient(low = "#FF3300", high = "#33CC33", name="Current account \nbalances",
                      limits=c(0, 1), breaks=c(0,1), labels=c("Deficits","Surpluses")) +
  scale_color_manual(values = c('#00000000', 'blue'), name="Borrowing from \nChina") +
  theme(legend.title=element_text(size=10, face="bold"),
        plot.caption = element_text(hjust=0, size=8)) + 
  labs(x="",y=paste(n,"Countries"),
       title = paste("Current Account Balances & Borrowings from China:",n,"Countries"),
       subtitle = paste0("Quarterly history from ",quarters(d1),"-",year(d1),
                         " through ",quarters(d2),"-",year(d2)),
       caption = "Source: IMF, AidData. Note: Data on borrowings from China is from AidData's Global Chinese Development Finance Dataset, Version 2.0."); g

ggsave(filename = paste0(p,"CAs_China_loans.png"), plot=g, 
       height = 4*8/3, width = 5 * 2.025766871)

# --- GDP, Qtly, real, SA-adj'd ------------------------------------------------+

c <- df[df$INDICATOR=="NGDP_R_SA_XDC",] %>% group_by(REF_AREA) %>% summarise(n=n())
t <- df[df$REF_AREA %in% c[c$n>99,]$REF_AREA & 
          df$DATE>=as.Date("1970-01-01") & df$INDICATOR=="NGDP_R_SA_XDC",]
# BCAXF_BP6_USD ; BGS_BP6_USD
d1 <- min(t$DATE); d2 <- max(t$DATE)
t$OBS_VALUE2 <- with(t, ifelse(OBS_VALUE>0,1,0))
n <- nrow(t %>% group_by(REF_AREA) %>% summarise(n=n()))

t %>%
  ggplot(aes(x=DATE, y=REF_AREA)) + 
  geom_tile(aes(fill=OBS_VALUE2)) +
  scale_fill_gradient(low = "#FF3300", high = "#33CC33") +
  theme(legend.position="none") +
  labs(x="",y=paste(n,"Countries"),
       title = paste("IMF's Data on Trade Balances:",n,"Countries"),
       subtitle = paste0("Quarterly history from ",quarters(d1),"-",year(d1),
                         " through ",quarters(d2),"-",year(d2)))

