pacman::p_load('readr', 'tidyverse', 'dplyr', 'zoo', 'stringr', 'ggplot2', 'plyr',
               'tidyverse', 'lubridate', 'patchwork', 'hrbrthemes', 'graphics', 'stats', 'lubridate',
               'forecast', 'hrbrthemes', 'patchwork', 'gridExtra')
rm(list = ls())

# import and create column for domestic value
totalassets <- read_csv("Desktop/Personal Projects/totalassets.csv")[-1] %>% 
  mutate(domestic = as.character(as.numeric(value) - as.numeric(foreign)))

# renaming columns
colnames(totalassets) <- c('item', 'foreign', 'total', 'bank', 'date', 'domestic')



totalassets <- pivot_longer(totalassets, c(foreign, total, domestic), names_to = 'currency', values_to = 'value')

totalassets$item2 <- str_replace_all(totalassets$item, c("Non-Mortgage Loans, less allowance for impairment/expected credit losses" = 
                                                              "Non-Mortgage Loans, less allowance for expected credit losses",
                                                            "Non-Mortgage Loans, less allowance for impairment" = 
                                                              "Non-Mortgage Loans, less allowance for expected credit losses",
                                                            "Securities issues or guaranteed by Canada/Canadian Province/Canadian Municipal or School Corporation, less allowance for expected credit losses where applicable" = 
                                                              "Securities issues or guaranteed by Canada/Canadian Province/Canadian Municipal or School Corporation",
                                                            "Deposits with regulated financial institutions, less allowance for impairment/expected credit losses" = 
                                                              "Deposits with regulated financial institutions, less allowance for expected credit losses",
                                                            "Deposits with regulated financial institutions, less allowances for impairment" = 
                                                              "Deposits with regulated financial institutions, less allowance for expected credit losses",
                                                            "Mortgages, less allowance for impairment/expected credit losses" = 
                                                              "Mortgages, less allowance for expected credit losses",
                                                            "Mortgages, less allowance for impairment" = 
                                                              "Mortgages, less allowance for expected credit losses",
                                                            "Other securities, less allowance for impairment/expected credit losses where applicable" = 
                                                              "Other securities, less allowance for expected credit losses where applicable",
                                                            "Other securities, less allowance for impairment" = 
                                                              "Other securities, less allowance for expected credit losses where applicable",
                                                            "Customers' liability under acceptances, less allowance for impairment/expected credit losses" =
                                                              "Customers' liability under acceptances, less allowance for expected credit losses",
                                                            "Customers' liability under acceptances, less allowances for impairment" = 
                                                              "Customers' liability under acceptances, less allowance for expected credit losses",
                                                            "Other assets, less allowance for expected credit losses where applicable" = 
                                                              "Other assets"
                                                            ))

totalassets <- mutate(totalassets, Date = as.Date(date, format = "%B %e, %Y"))
totalassets <- mutate(totalassets, value = as.numeric(value))[,-3]

wide_ass <- pivot_wider(totalassets[-1], values_from = value, names_from = item2)
colnames(wide_ass)

# subcategorizing items that have the same name
wide_ass$`Of  (A) Secured by residential property`
wide_ass$`(vi) To individuals for non-business purposes: Of  (A) Secured by residential property` <- sapply(wide_ass$`Of  (A) Secured by residential property`, "[[", 2)
wide_ass$`(viii) To individuals and others for business purposes: Of  (A) Secured by residential property` <- sapply(wide_ass$`Of  (A) Secured by residential property`, "[[", 1)

wide_ass$`which:  (B) Secured by other than residential property`
wide_ass$`(vi) To individuals for non-business purposes: which:  (B) Secured by other than residential property` <- sapply(wide_ass$`which:  (B) Secured by other than residential property`, "[[", 2)
wide_ass$`(viii) To individuals and others for business purposes: which:  (B) Secured by other than residential property` <- sapply(wide_ass$`which:  (B) Secured by other than residential property`, "[[", 1)

# deleting origingal columns
wide_ass <- wide_ass[ , -which(names(wide_ass) %in% c("which:  (B) Secured by other than residential property","Of  (A) Secured by residential property"))]

# creating aggregates

wide_ass <- wide_ass %>% 
  mutate_at(-c(1,2,3), unlist)

# calculating aggregate asset position
wide_ass$`1. Cash and cash equivalent` <- transmute(wide_ass,
                                                    `1. Cash and cash equivalent` = `(a) Gold, bank notes, deposits with Bank of Canada, cheques and other items in transit` +
                                                      `(b) Deposits with regulated financial institutions, less allowance for expected credit losses`)


wide_ass$`(a) Securities issues or guaranteed by Canada/Canadian Province/Canadian Municipal or School Corporation` <- transmute(wide_ass,
                                                    `(a) Securities issues or guaranteed by Canada/Canadian Province/Canadian Municipal or School Corporation` = 
                                                      `(i) Treasury Bills and other short term paper` +
                                                      `(ii)Other securities`)

wide_ass$`(b) Other securities, less allowance for expected credit losses where applicable` <- transmute(wide_ass,`(b) Other securities, less allowance for expected credit losses where applicable` = 
                                                                                                                                   `(i) Debt` +
                                                                                                                                   `(ii) Shares`)
wide_ass$`2. Securities` <- transmute(wide_ass,`2. Securities` = `(a) Securities issues or guaranteed by Canada/Canadian Province/Canadian Municipal or School Corporation` + 
                                        `(b) Other securities, less allowance for expected credit losses where applicable`)


wide_ass$`(vi) To individuals for non-business purposes` <- transmute(wide_ass, `(vi) To individuals for non-business purposes` = `(vi) To individuals for non-business purposes: Of  (A) Secured by residential property` +
                           `(vi) To individuals for non-business purposes: which:  (B) Secured by other than residential property`)

wide_ass$`(viii) To individuals and others for business purposes` <- transmute(wide_ass, `(viii) To individuals and others for business purposes` = `(viii) To individuals and others for business purposes: Of  (A) Secured by residential property` +
                                                                        `(viii) To individuals and others for business purposes: which:  (B) Secured by other than residential property`)


wide_ass$`(a) Non-Mortgage Loans, less allowance for expected credit losses` <- transmute(wide_ass, `(a) Non-Mortgage Loans, less allowance for expected credit losses` = 
                                                                                            `(i) Call and other short loans to investment dealers and brokers, secured` +
                                                                                            `(ii) To regulated financial institutions` +
                                                                                            `(iii) To Canadian federal government, provinces, municipal or school corporations` +
                                                                                            `(iv) To foreign governments` +
                                                                                            `(v) Lease receivables` +
                                                                                            `(vi) To individuals for non-business purposes` +
                                                                                            `(vii) Reverse repurchase agreements` +
                                                                                            `(viii) To individuals and others for business purposes`
                                                                                            )

wide_ass$`(i) Residential` <- transmute(wide_ass, `(i) Residential` = `(A) Insured` + 
                                          `(B) Of which: NHA MBS pooled and unsold` +
                                          `(C) Uninsured` +
                                          `(D) Reverse Mortgages`)

wide_ass$`(b) Mortgages, less allowance for expected credit losses` <- transmute(wide_ass, `(b) Mortgages, less allowance for expected credit losses` = `(i) Residential` + `(ii) Non-residential`)

wide_ass$`3. Loans` <- transmute(wide_ass, `3. Loans` = `(a) Non-Mortgage Loans, less allowance for expected credit losses` + 
                                   `(b) Mortgages, less allowance for expected credit losses`)

wide_ass$`(e) Intangibles` <- transmute(wide_ass, `(e) Intangibles` = `(i) with definite lives` + `(ii) with indefinite lives`)
wide_ass$`6. Other assets` <- transmute(wide_ass, `6. Other assets` = `(a) Insurance-related assets` +
                           `(b) Accrued interest` +
                           `(c) Prepaid and deferred charges` +
                           `(d) Goodwill` +
                           `(e) Intangibles` +
                           `(f) Deferred tax assets` +
                           `(g) Derivatives related amounts` +
                           `(h) Due from Head Office and related Canadian regulated Financial Institutions` +
                           `(i) Interests in associates and joint ventures` +
                           `(j) Other`)
wide_ass <- wide_ass %>% 
  mutate_at(-c(1,2,3), unlist)

totalassets <- pivot_longer(wide_ass, names_to = 'item', values_to = 'amount', cols = -c(Date, bank, currency))

# plot any value, currency, bank group
ggplot(filter(totalassets, currency == 'domestic' 
              & item == '3. Loans' 
              & (bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE), 
              aes(x = Date, y = amount, col = bank)) + 
  geom_line()



#little banks
littlebois <- filter(totalassets, currency == 'total' 
                     & (!bank %in% c('ADS Canadian Bank', 'Canadian Tire Bank',
                                               'Digital Commerce Bank','Vancity Community Investment Bank',
                                               'B2B Bank', 'Duo Bank of Canada',
                                               'Exchange Bank of Canada', 'General Bank of Canada',
                                               "President's Choice Bank", 'Rogers Bank', 'Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 
                                               'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 
                                               'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == TRUE) 

# D-SIBs
bigbois <- filter(totalassets, currency == 'domestic' 
                     & (bank %in% c('Bank of Montreal', 
                                    'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 
                                    'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == TRUE) 

sum(filter(bigbois, Date == as.Date('2022-03-31', format = "%Y-%m-%d") 
       & item == 'Total  Assets')$amount)/1000000000
sum(filter(littlebois, Date == as.Date('2022-03-31', format = "%Y-%m-%d") 
           & item == 'Total  Assets')$amount)/1000000000


# DSB activation
DSB <- as.Date(levels(as.factor(bigbois$Date)))

dsb <- matrix(NA, length(DSB), 1)

for (i in 0:length(DSB)) {
  if(isTRUE(as.Date(DSB[i]) < as.Date("25/06/2018", format = "%d/%m/%Y"))) {
    dsb[i] <- 0
  } else if (isTRUE(as.Date(DSB[i]) < as.Date("12/12/2018", format = "%d/%m/%Y"))){
    dsb[i] <- 1.5
  } else if (isTRUE(as.Date(DSB[i]) < as.Date("04/06/2019", format = "%d/%m/%Y"))){
    dsb[i] <- 1.75
  } else if (isTRUE(as.Date(DSB[i]) < as.Date("10/12/2019", format = "%d/%m/%Y"))){
    dsb[i] <- 2
  } else if (isTRUE(as.Date(DSB[i]) < as.Date("13/03/2020", format = "%d/%m/%Y"))){
    dsb[i] <- 2.25
  } else if (isTRUE(as.Date(DSB[i]) < as.Date("17/06/2021", format = "%d/%m/%Y"))){
    dsb[i] <- 1
  } else {
    dsb[i] <- 2.5
  }
} 

test <- data.frame(Date = DSB, dsb = dsb)

comparison <- merge(filter(littlebois, item == '3. Loans') %>% 
        group_by(Date) %>% 
        dplyr::summarise(sum(amount)),
      filter(bigbois, item == '3. Loans') %>% 
        group_by(Date) %>% 
        dplyr::summarise(sum(amount)), by = 'Date')
comparison <- merge(comparison, test, by = 'Date')

colnames(comparison) <- c('Date', 'Non-D-SIB', 'D-SIBs', 'DSB')
colnames(wide_ass)

# remove trend for plotting purposes

comparison$`D-SIBs`/comparison$`Non-D-SIB`

trendcomparison <- comparison %>% 
  mutate(DSIBtrend = ma(`D-SIBs`, order = 4)) %>% 
  mutate(nonDSIBtrend = ma(`Non-D-SIB`, order = 4))

plot(x = trendcomparison$Date, y = trendcomparison$DSB, type = 'l', 
     main = 'Domestic Stability Buffer',
     xlab = 'Date', ylab = 'Asset Growth')
legend(x = 17250, 2.5, legend=c("DSB Level", "D-SIB", 'Non D-SIB'),
       col=c("black", "blue", "red"), lty=1:2, cex=0.8)
par(new = TRUE)
plot(comparison$Date, trendcomparison$DSIBtrend, col = 'blue', type = 'l',
     xlab = '', ylab = '')
par(new = TRUE)
plot(trendcomparison$Date, trendcomparison$nonDSIBtrend, type = 'l', col = 'red',
     xlab = '', ylab = '')

# Compare Growth and Size of D-SIB vs Non-DSIBs

pivot_longer(comparison, cols = c('Non-D-SIB', 'D-SIBs'), names_to = 'Size', values_to = 'Value') %>% 
  ddply("Size",transform,
        Growth=c(NA,exp(diff(log(Value)))-1)) %>%
  ggplot(aes(x = Date, y = Growth, col = Size)) +
  geom_col()

# More plots
plot(x = comparison$Date, y = comparison$DSB, type = 'l', 
     main = 'Domestic Stability Buffer',
     xlab = 'Date', ylab = 'Capital Buffer (% Tier 1 Assets)')
par(new = TRUE)
plot(comparison$Date, comparison$`Non-D-SIB`, type = 'l', col = 'blue',
     xlab = '', ylab = '')

plot(x = comparison$Date, y = comparison$DSB, type = 'l', 
     main = 'Domestic Stability Buffer',
     xlab = 'Date', ylab = 'Capital Buffer (% Tier 1 Assets)')
par(new = TRUE)
plot(comparison$Date, comparison$`D-SIB`, type = 'l', col = 'blue',
     xlab = '', ylab = '')

# total value comparison

plot(x = comparison$Date, y = comparison$DSB, type = 'l', 
     main = 'Domestic Stability Buffer',
     xlab = 'Date', ylab = 'Asset Growth')
legend(x = 17250, 2.5, legend=c("DSB Level", "D-SIB", 'Non D-SIB'),
       col=c("black", "blue", "red"), lty=1:2, cex=0.8)
par(new = TRUE)
plot(comparison$Date, comparison$`D-SIB`, col = 'blue', type = 'l',
     xlab = '', ylab = '')
par(new = TRUE)
plot(comparison$Date, comparison$`Non-D-SIB`, type = 'l', col = 'red',
     xlab = '', ylab = '')

# growth
growthcomparison <- trendcomparison %>% 
  mutate(DSIBgrowth = c(NA,exp(diff(log(DSIBtrend)))-1)) %>% 
  mutate(nonDSIBgrowth = c(NA,exp(diff(log(nonDSIBtrend)))-1))

plot(x = growthcomparison$Date, y = growthcomparison$DSB, type = 'l', 
     main = 'Domestic Stability Buffer',
     xlab = 'Date', ylab = 'Asset Growth')
legend(x = 17250, 2.5, legend=c("DSB Level", "D-SIB", 'Non D-SIB'),
       col=c("black", "blue", "red"), lty=1:2, cex=0.8)

par(new = TRUE)
plot(growthcomparison$Date, growthcomparison$DSIBgrowth, type = 'l', col = 'blue',
     xlab = '', ylab = '')
par(new = TRUE)
plot(growthcomparison$Date, growthcomparison$nonDSIBgrowth, type = 'l', col = 'red',
     xlab = '', ylab = '')


# two y axes attempt
dsibColor <- "brown2"
nondsibColor <- "blue3"

coeff <- 1
ggplot(growthcomparison, aes(x = Date)) +
  
  geom_line(aes(y = (exp(DSIBgrowth)-1)*100 / coeff), color = dsibColor, size = 0.4) +
  geom_line(aes(y = ((exp(nonDSIBgrowth)-1)*100)), color = nondsibColor, size  = 0.4) + 
  geom_line(aes(y = DSB), size  = 0.4) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Non-D-SIBs",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="D-SIB")) + 
  theme(
    legend.title = element_text(
      family = "Calibri",
      colour = "brown",
      face = "bold",
      size = 12),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", colour = "black"),
    axis.title.y = element_text(colour = nondsibColor, size=10), 
    axis.title.y.right = element_text(colour = dsibColor, size=10),
    plot.title = element_text(
      size = rel(1.2), lineheight = .9, face = "bold", colour = "brown"
    )
    ) +
  ggtitle("Total Lending Growth")
  
totalassets <- totalassets %>% 
  mutate_at(-c(1,2,3,4), ~replace(., is.na(.), 0)) %>% 
  mutate_at(-c(1,2,3,4), as.numeric)

widedom <- filter(wide_ass, currency == 'foreign')[,-2]

# converting to percentage of balance sheet
widedom <- cbind(Date = widedom$date, bank = widedom$bank, mutate_each(widedom[,-(1:2)], funs(. / `Total  Assets`)))

# converting to long format

domestic <- widedom %>% 
  pivot_longer(cols = c(-'Date', -'bank'), names_to = 'item', values_to = 'amount')

colnames(wide_ass)

#### More plots #####

# Macro-prudential Environment
recessions.trim = read.table(textConnection(
  "Peak, Trough
2018-01-01, 2020-03-01
2021-12-01, 2022-06-06"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

### Non-Mortgage Lending ###
# small domestic banks
ggplot(filter(domestic, item == '(a) Non-Mortgage Loans, less allowance for expected credit losses' 
              & (bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

# large domestic banks
ggplot(filter(domestic, item == '(a) Non-Mortgage Loans, less allowance for expected credit losses' 
              & (!bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

### Residential Mortgage Lending ####
# small domestic banks
ggplot(filter(domestic, item == '(i) Residential' 
              & (bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

# large domestic banks
ggplot(filter(domestic, item == '(i) Residential' 
              & (!bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)


### Cash and Cash Equivalent ####
# small domestic banks
ggplot(filter(domestic, item == '1. Cash and cash equivalent' 
              & (bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

# large domestic banks
ggplot(filter(domestic, item == '1. Cash and cash equivalent' 
              & (!bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

### Insured mortgages ###
# small domestic banks
ggplot(filter(domestic, item == '(A) Insured' 
              & (bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

# large domestic banks
ggplot(filter(domestic, item == '(A) Insured' 
              & (!bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

### Uninsured Mortgages ###
# small domestic banks
ggplot(filter(domestic, item == '(C) Uninsured' 
              & (bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

# large domestic banks
ggplot(filter(domestic, item == '(C) Uninsured' 
              & (!bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

### Of which MBS pooled ###
# small domestic banks
ggplot(filter(domestic, item == '(B) Of which: NHA MBS pooled and unsold' 
              & (bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

# large domestic banks
ggplot(filter(domestic, item == '(B) Of which: NHA MBS pooled and unsold' 
              & (!bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = Date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)


wide_ass <- wide_ass %>% 
  mutate_at(-c(1,2,3))

# creating dummy variables
wide_ass <- wide_ass %>% mutate(DSIB = ifelse(bank %in% c( 
  'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 
  'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 
  'The Toronto-Dominion Bank') == TRUE, 1, 0))

wide_ass$buffer = buffer

wide_ass$did <- wide_ass$buffer*wide_ass$DSIB

model <- lm(`3. Loans` ~ DSIB + buffer + did, data = wide_ass)

summary(model)

# - ads bank, - b2b bank, - canadian tire bank, -digital commerce bank, -duo bank,
# - exchange bank, -general bank of canada, -presidents choice bank,
# - rogers bank, -vancity

smallbanks <- filter(wide_ass, !bank %in% c('ADS Canadian Bank', 'Canadian Tire Bank',
                               'Digital Commerce Bank','Vancity Community Investment Bank',
                               'B2B Bank', 'Duo Bank of Canada',
                               'Exchange Bank of Canada', 'General Bank of Canada',
                               "President's Choice Bank", 'Rogers Bank', 'Total Domestic Banks', 'Total All Banks', 'Bank of Montreal', 
                              'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 
                              'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank'))

bigbanks <- filter(wide_ass, bank %in% c('Bank of Montreal', 
                                                        'Canadian Imperial Bank of Commerce', 'National Bank of Canada', 
                                                        'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank'))










