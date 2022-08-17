##### Brock Kelly -  Impact of Domestic Stability Buffer Essay #####

## Importing libraries and data ###

# Libraries
pacman::p_load('readr', 'tidyverse', 'dplyr', 'zoo', 'stringr', 'ggplot2', 'plyr',
               'tidyverse', 'lubridate', 'patchwork', 'hrbrthemes', 'graphics', 
               'stats', 'lubridate', 'dplyr', 'ggthemes', 'stargazer')
rm(list = ls())

# import and create column for domestic value
totalassets <- read_csv("Desktop/Personal Projects/totalassets.csv")[-1] %>% 
  mutate(domestic = as.character(as.numeric(value) - as.numeric(foreign)))

# renaming columns
colnames(totalassets) <- c('item', 'foreign', 'total', 'bank', 'date', 'domestic')

length(levels(as.factor(totalassets$bank)))
## Cleaning Data ##

# Some item names have changed in the balance sheets over time
# convert to long format
totalassets <- pivot_longer(totalassets, c(foreign, total, domestic), names_to = 'currency', values_to = 'value')

totalassets$item <- str_replace_all(totalassets$item, c("Non-Mortgage Loans, less allowance for impairment/expected credit losses" = 
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

# Formatting date variable and converting values to numeric
totalassets <- mutate(totalassets, Date = as.Date(date, format = "%B %e, %Y")) 
totalassets <- mutate(totalassets, value = as.numeric(value))[,-3]

## Wide Data Frame ##
wide_ass <- pivot_wider(totalassets, values_from = value, names_from = item)
colnames(wide_ass)

# some subcategories have the same names so we have to rename them
wide_ass$`Of  (A) Secured by residential property`
wide_ass$`(vi) To individuals for non-business purposes: Of  (A) Secured by residential property` <- sapply(wide_ass$`Of  (A) Secured by residential property`, "[[", 2)
wide_ass$`(viii) To individuals and others for business purposes: Of  (A) Secured by residential property` <- sapply(wide_ass$`Of  (A) Secured by residential property`, "[[", 1)

wide_ass$`which:  (B) Secured by other than residential property`
wide_ass$`(vi) To individuals for non-business purposes: which:  (B) Secured by other than residential property` <- sapply(wide_ass$`which:  (B) Secured by other than residential property`, "[[", 2)
wide_ass$`(viii) To individuals and others for business purposes: which:  (B) Secured by other than residential property` <- sapply(wide_ass$`which:  (B) Secured by other than residential property`, "[[", 1)

# deleting origingal columns
wide_ass <- wide_ass[ , -which(names(wide_ass) %in% c("which:  (B) Secured by other than residential property","Of  (A) Secured by residential property"))]

# aggregate values ie. Cash, total loans, total securities are not defined so we have to create them
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

# renaming columns
colnames(wide_ass)
colnames(wide_ass) <- c('bank',
                        'currency',
                        'date',
                        'totalcash',
                        'gold_deposits_cheques',
                        'financialinst_deposits',
                        'totalsecurities',
                        'govtsecurities',
                        'tbills',
                        'othergovtsecurities',
                        'othersecurities',
                        'debtsecurities',
                        'shares',
                        'totalloans',
                        'nonmortgageloans',
                        'call_shortloans_invest_dealers_brokers',
                        'loansto_financialinst',
                        'loansto_fed_prov_govern',
                        'loansto_foreigngov',
                        'lease_receivables',
                        'loansto_indiv_nonbusiness',
                        'reverserepo',
                        'loansto_indiv_business',
                        'mortgageloans',
                        'mortgageloans_residential',
                        'mortgageloans_residential_insured',
                        'mortgageloans_residential_mbspooled',
                        'mortgageloans_residential_uninsured',
                        'mortgageloans_residential_reverse',
                        'mortgageloans_nonresidential',
                        'customer_liabilityunderacceptances',
                        'land_buildings_equip',
                        'totalotherassets',
                        'insuranceassets',
                        'accruedinterest',
                        'prepaid_deferredcharges',
                        'goodwill',
                        'intangibles',
                        'intangibles_definite',
                        'intangibles_indefinite',
                        'deferred_tax',
                        'derivative_related',
                        'dueto_headoffice_regulatedfinancialinst',
                        'interestin_associate_jointventures',
                        'other',
                        'totalassets',
                        'loansto_indiv_nonbusiness_propsecured',
                        'loansto_indiv_business_propsecured',
                        'loansto_indiv_nonbusiness_othersecured',
                        'loansto_indiv_business_othersecured')

## Long Data Frame ##
long_ass <- pivot_longer(wide_ass, names_to = 'item', values_to = 'amount', cols = -c(date, bank, currency))

## As percentage of total assets ##
wide_ass$total_assets <- wide_ass$totalassets
wide_ass <- filter(wide_ass, !bank %in% c('Duo Bank of Canada', 'Total All Banks', 'Total Domestic Banks'))
wide_ass <- select(wide_ass, -c(totalassets))

# converting to percentage of balance sheet
perc_wide <- cbind(date = wide_ass$date, 
                   bank = wide_ass$bank, 
                   currency = wide_ass$currency,
                   mutate_all(wide_ass[,-(1:3)], funs(. / total_assets)))


# converting to long format
perc_long <- perc_wide %>% 
  pivot_longer(cols = c(-'date', -'bank',-'currency'), names_to = 'item', values_to = 'amount')

## As percentage of total assets - growth rate ##

# function for growth
logG = function(x) c(NA, diff(log(x)))

# growth
wide_growth <- wide_ass %>%
  arrange(bank, currency, date) %>% 
  dplyr::group_by(bank, currency) %>% 
  dplyr::mutate_at(vars(-c(1,2,3)), logG)

long_growth <- wide_growth %>% 
  pivot_longer(cols = c(-'date', -'bank',-'currency'), names_to = 'item', values_to = 'amount')

## Bank-level controls ##

# MKTTOBOOK
# Size
# Earnings Volatility
# Distance to Capital Requirement
# Size of retail deposit business

## DSB Level and D-SIB Indicator ##

# DSB activation
DSB <- long_ass$date
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

long_ass$dsb <- dsb

# creating DSIB indicator
long_ass <- long_ass %>% mutate(DSIB = ifelse(bank %in% c( 
  'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 
  'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 
  'The Toronto-Dominion Bank') == TRUE, 1, 0))

# Macro-prudential Environment - tightening
recessions.trim = read.table(textConnection(
  "Peak, Trough
2018-01-01, 2020-03-01
2021-12-01, 2022-06-06"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

wide_ass <- filter(wide_ass, !bank %in% c('Duo Bank of Canada', 'Total Domestic Banks', 'Total All Banks'))

# plotting
ggplot(filter(long_growth, item == 'mortgageloans_residential' & currency == 'foreign'
                & (!bank %in% c('Total Domestic Banks', 'Total All Banks', 'Bank of Montreal',
                               'Canadian Imperial Bank of Commerce', 'National Bank of Canada',
                               'Royal Bank of Canada', 'The Bank of Nova Scotia', 'The Toronto-Dominion Bank')) == FALSE)) + 
  geom_line(aes(x = date, y = amount, col = bank)) + 
  theme_bw() +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)


#### Clean Liability Data ####

totalliabilities <- read_csv("Desktop/Personal Projects/totalliabilities.csv")[-1] %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(domestic = as.character(as.numeric(value) - as.numeric(foreign)))

colnames(totalliabilities) <- c('item', 'foreign', 'total', 'bank', 'date', 'domestic')

## Convert to long format ##
totalliabilities <- pivot_longer(totalliabilities, c(foreign, total, domestic), names_to = 'currency', values_to = 'value') %>% 
  mutate(date = as.Date(date, format = "%B %e, %Y")) %>%   # Formatting date variable
  mutate(value = as.numeric(value))                       # and converting values to numeric
## Wide Data Frame ##
wide_liab <- pivot_wider(totalliabilities, values_from = value, names_from = item)
colnames(wide_liab)

ncol(wide_liab)


# renaming columns
colnames(wide_liab)
colnames(wide_liab) <- c('bank',
                         'date',
                         'currency',
                         'demanddep',
                         'demanddep_federalprovincial',
                         'demanddep_municipalschoolcorp',
                         'demanddep_depositinstit',
                         'demanddep_individ',
                         'demanddep_individ_taxsheltered',
                         'demanddep_individ_other',
                         'demanddep_other',
                         'fixedtermdep',
                         'fixedtermdep_indiv_taxsheltered',
                         'fixedtermdep_other',
                         'cheques_intransit',
                         'bocadvances',
                         'acceptances',
                         'other',
                         'other_subsidiaryliab',
                         'other_subsidiaryliab_callshortloans',
                        'other_insurancerelated',
                         'other_accrued_interest',
                         'other_mortgages_loanspayable',
                         'other_incometax',
                         'other_incometax_current',
                         'other_incometax_deferred',
                         'other_obligations_borrowedsec',
                         'other_obligations_repoagreementassets',
                         'other_deferred_income',
                         'other_derivative_related',
                         'other_duetoheadoffcice_regfininstit',
                         'other_other',
                         'subordinateddebt',
                         'shequity',
                         'shequity_prefshares',
                         'shequity_commonshares',
                         'shequity_contribsurplus',
                         'shequity_retainedearnings',
                         'noncontrol_interes',
                         'accumulatedother_comp_income',
                         'totalliabilities')

colnames(wide_liab)

# subcategorizing items that have the same name
wide_liab$demanddep_federalprovincial
wide_liab$fixedtermdep_federalprovincial <- sapply(wide_liab$demanddep_federalprovincial, "[[", 2)
wide_liab$demanddep_federalprovincial <- sapply(wide_liab$demanddep_federalprovincial, "[[", 1)


wide_liab$demanddep_municipalschoolcorp
wide_liab$fixedtermdep_municipalschoolcorp <- sapply(wide_liab$demanddep_municipalschoolcorp, "[[", 2)
wide_liab$demanddep_municipalschoolcorp <- sapply(wide_liab$demanddep_municipalschoolcorp, "[[", 1)

wide_liab$demanddep_depositinstit
wide_liab$fixedtermdep_depositinstit <- sapply(wide_liab$demanddep_depositinstit, "[[", 2)
wide_liab$demanddep_depositinstit <- sapply(wide_liab$demanddep_depositinstit, "[[", 1)

wide_liab$demanddep_individ
wide_liab$fixedtermdep_individ <- sapply(wide_liab$demanddep_individ, "[[", 2)
wide_liab$demanddep_individ <- sapply(wide_liab$demanddep_individ, "[[", 1)

wide_liab$demanddep_individ_other
wide_liab$other_subsidiaryliab_other <- sapply(wide_liab$demanddep_individ_other, "[[", 3)
wide_liab$fixedtermdep_individ_other <- sapply(wide_liab$demanddep_individ_other, "[[", 2)
wide_liab$demanddep_individ_other <- sapply(wide_liab$demanddep_individ_other, "[[", 1)

## Creating aggregates ##

wide_liab <- wide_liab %>% 
  mutate_at(-c(1,2,3), unlist) %>% 
  mutate_at(-c(1,2,3), as.numeric)

wide_liab$demanddep_individ <- wide_liab$demanddep_individ_taxsheltered + 
  wide_liab$demanddep_individ_other

wide_liab$demanddep <- wide_liab$demanddep_municipalschoolcorp +
  wide_liab$demanddep_federalprovincial +
  wide_liab$demanddep_depositinstit +
  wide_liab$demanddep_individ +
  wide_liab$demanddep_other

wide_liab$fixedtermdep_individ <- wide_liab$fixedtermdep_indiv_taxsheltered +
  wide_liab$fixedtermdep_individ_other

wide_liab$fixedtermdep <- wide_liab$fixedtermdep_municipalschoolcorp +
  wide_liab$fixedtermdep_federalprovincial +
  wide_liab$fixedtermdep_depositinstit +
  wide_liab$fixedtermdep_individ +
  wide_liab$fixedtermdep_other

wide_liab$other_incometax <- wide_liab$other_incometax_current + 
  wide_liab$other_incometax_deferred

wide_liab$other_subsidiaryliab <-  wide_liab$other_subsidiaryliab_callshortloans +
  wide_liab$other_subsidiaryliab_other

wide_liab$other <- wide_liab$other_accrued_interest +
  wide_liab$other_subsidiaryliab +
  wide_liab$other_insurancerelated +
  wide_liab$other_accrued_interest +
  wide_liab$other_mortgages_loanspayable +
  wide_liab$other_incometax +
  wide_liab$other_obligations_borrowedsec +
  wide_liab$other_deferred_income +
  wide_liab$other_derivative_related +
  wide_liab$other_duetoheadoffcice_regfininstit +
  wide_liab$other_other

## Long Data Frame ##
long_liab <- pivot_longer(wide_liab, names_to = 'item', values_to = 'amount', cols = -c(date, bank, currency))
  
## Initial Proximity to capital requirements ##

## Size of off-Balance sheet assets ##

## BCAR Capital Components Data ##

totalcapital <- read_csv("Desktop/Personal Projects/totalcapital.csv")[-1] %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(date = as.Date(as.yearqtr(date, format = "Q%q - %Y"))) %>% 
  filter(date == as.Date('2018-01-01'))
  
colnames(totalcapital) <- c('item', 'value', 'bank', 'date')

totalcapital <- totalcapital %>% mutate(DSIB = ifelse(bank %in% c( 
  'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 
  'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 
  'The Toronto-Dominion Bank') == TRUE, 1, 0))

ggplot(filter(totalcapital, item == 'Common equity Tier 1 (CET1)',
              !bank %in% c('Exchange Bank of Canada')), aes(x = bank, y = value, fill = DSIB)) +
         geom_col(width = 0.5) +
  theme_economist_white()

## Wide Data Frame ##
wide_cap <- pivot_wider(totalcapital, values_from = value, names_from = item)
colnames(wide_cap) <- make.names(colnames(wide_cap), unique=TRUE)
colnames(wide_cap)

ggplot(filter(totalcapital, item == 'Leverage Ratio'),
              aes(y = value, x = bank)) +
  geom_col()

# fixing repeated column names
wide_cap$Tier.1
wide_cap$tier1capratio <- sapply(wide_cap$Tier.1, "[[", 2)
wide_cap$Tier.1 <- sapply(wide_cap$Tier.1, "[[", 1)

wide_cap$Placeholder
wide_cap$Tier1cap_placeholder <- sapply(wide_cap$Placeholder, "[[", 3)
wide_cap$Additional_tier1cap_placeholder <- sapply(wide_cap$Placeholder, "[[", 2)
wide_cap$CET1_placeholder <- sapply(wide_cap$Placeholder, "[[", 1)

wide_cap$Adjustment.for.floor
wide_cap$RWAprephase_totalcap_Adjustment.for.floor <- sapply(wide_cap$Adjustment.for.floor, "[[", 3)
wide_cap$RWAprephase_tier1cap_Adjustment.for.floor <- sapply(wide_cap$Adjustment.for.floor, "[[", 2)
wide_cap$RWAprephase_CET1_Adjustment.for.floor <- sapply(wide_cap$Adjustment.for.floor, "[[", 1)

wide_cap$Leverage.Ratio <- sapply(wide_cap$Leverage.Ratio, "[[", 2)
wide_cap <- subset(wide_cap, select = -c(Placeholder, X0, Adjustment.for.floor)) %>% 
  mutate_at(vars(-c(1,2,3)), as.numeric)

## Long Data Frame #
long_cap <- pivot_longer(wide_cap, names_to = 'item', values_to = 'amount', cols = -c(date, bank))

## Indicator for initial level of capitalization ##

# degree of capitalization
WELLCAP <- wide_cap$Total.capital
wellcap <- matrix(NA, length(WELLCAP), 1)

for (i in 0:length(WELLCAP)) {
  if(isTRUE(WELLCAP[i] < 8 & WELLCAP[i] > 0)) {
    wellcap[i] <- 1
  } else if (isTRUE(WELLCAP[i] > 8 & WELLCAP[i] < 10.5)) {
    wellcap[i] <- 2
  } else if (isTRUE(WELLCAP[i] > 10.5 & WELLCAP[i] <= 13)) {
    wellcap[i] <- 3
  } else if (isTRUE(WELLCAP[i] > 13 & WELLCAP[i] < 15.5)) {
    wellcap[i] <- 4
  } else if(isTRUE(WELLCAP[i] > 15.5)) {
    wellcap[i] <- 5
  }
}

wide_cap$wellcap <- wellcap

long_cap <- pivot_longer(wide_cap, names_to = 'item', values_to = 'amount', cols = -c(date, bank))
long_cap <- long_cap %>% mutate(DSIB = ifelse(bank %in% c( 
  'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 
  'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 
  'The Toronto-Dominion Bank') == TRUE, 1, 0))

ggplot(filter(long_cap, item == 'Total.capital'),
       aes(y = bank, x = amount, fill = DSIB)) +
  geom_col()

## Regressions ##

## Bank level controls ##
# initial capitalization level
initcap <- data.frame(bank = wide_cap$bank, cap = wide_cap$Common.equity.Tier.1..CET1., wellcap = wellcap)
initcap$wellcap <-  initcap$wellcap-2

wide_growth <- filter(wide_growth, !bank %in% c('Duo Bank of Canada', 'Total All Banks', 'Total Domestic Banks'))

nwellcap <- matrix(NA, length(wide_ass$bank))

for(i in (1:length(wide_ass$bank))) {
  nwellcap[i] <- filter(initcap, bank == wide_ass$bank[i])[,2]
}
rm(initcap)

wide_growth$wellcap <- as.vector(nwellcap)

# size
wide_growth$size <- log(wide_ass$total_assets)

# size of retail deposit business
wide_liab <- filter(wide_liab, !bank %in% c('Duo Bank of Canada', 'Total All Banks', 'Total Domestic Banks'))

wide_growth$retaildeposit <- wide_liab$demanddep/wide_ass$total_assets

length(wide_liab$demanddep)
length(wide_ass$totalcash)

ggplot((filter(wide_growth, date == as.Date('2018-05-31', format = '%Y-%m-%d')& currency == 'domestic')), aes(y = retaildeposit, x = bank)) +
  geom_col()

# earnings volatility

## Time fixed effects
wide_growth$time <- as.factor(wide_growth$date)
wide_growth$quarter <- as.factor(quarter(wide_growth$date))
##  DSB Buffer
# DSB activation
DSB <- wide_growth$date
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

wide_growth$DSB <- as.vector(dsb)
wide_growth$trueDSB <- as.vector(lag(wide_growth$DSB, n = 1L))

# Big Six Dummy
wide_growth <- wide_growth %>% mutate(DSIB = ifelse(bank %in% c( 
  'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 
  'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 
  'The Toronto-Dominion Bank') == TRUE, 1, 0))

# Big Six Dummy
perc_wide <- perc_wide %>% mutate(DSIB = ifelse(bank %in% c( 
  'Bank of Montreal', 'Canadian Imperial Bank of Commerce', 
  'National Bank of Canada', 'Royal Bank of Canada', 'The Bank of Nova Scotia', 
  'The Toronto-Dominion Bank') == TRUE, 1, 0))

### Interest rate effect ###

library(fredr)

int_rate <- fredr(
  series_id = "IRSTCB01CAM156N",
  observation_start = as.Date("2017-03-31"),
  observation_end = as.Date("2022-04-30")
)$value


wide_growth$int_rate <- rep_len(int_rate, length.out=nrow(wide_growth))
wide_growth$trueint_rate <- lag(wide_growth$int_rate, n = 1L)
levels(as.factor(wide_growth$bank))
# DID Dummy

wide_growth$did <- wide_growth$DSB*wide_growth$DSIB

wide_ass <- wide_ass %>% 
  mutate_at(-c(1,2,3), as.numeric)

is.na(wide_growth)<-sapply(wide_growth, is.infinite)
wide_growth[is.na(wide_growth)] <- 0

## Descriptive Analysis ##
## Total Assets - foreign vs domestic ##

# Want to look at the impact of the the DSB on total foreign vs domestic assets ##
foreignconc <- filter(long_ass, currency == 'domestic' & item == 'totalassets') 
foreignconc$amount <- filter(long_ass, item == 'totalassets' & currency == 'foreign')$amount / filter(long_ass, item == 'totalassets' & currency == 'total')$amount


filter(foreignconc, (!bank %in% c('Canadian Imperial Bank of Commerce', 'National Bank of Canada', 
                                  'Royal Bank of Canada', 'The Bank of Nova Scotia', 
                                  'The Toronto-Dominion Bank')) == TRUE) %>% 
  ggplot() +
  geom_line(aes(y = amount, x = date, col = bank)) +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

# Market Structure in 2017
dat2017 <- filter(perc_wide, date == as.Date('2020-04-30', format = '%Y-%m-%d') & currency == 'total' & (!bank %in% c('Total All Banks', 'Total Domestic Banks')))


# Business Lending
ggplot(dat2017, aes(y = bank, x = loansto_indiv_business, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle('Business Lending (% of Total Assets)')
colnames(dat2017)
# Individual Lending
ggplot(dat2017, aes(y = bank, x = loansto_indiv_nonbusiness, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle('Loans to Individuals for Non-Business Purposes (% of Total Assets)')
# Mortgage Lending
ggplot(dat2017, aes(y = bank, x = mortgageloans, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle('Mortgage Loans (% of Total Assets)')

# Mortgage Lending - Residential
ggplot(dat2017, aes(y = bank, x = mortgageloans_residential, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')

# Mortgage Lending - Residential, Insured
ggplot(dat2017, aes(y = bank, x = mortgageloans_residential_insured, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')

# Mortgage Lending - Residential, Uninsured
ggplot(dat2017, aes(y = bank, x = mortgageloans_residential_uninsured, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')

# Mortgage Lending - Residential, Uninsured
ggplot(dat2017, aes(y = bank, x = mortgageloans_residential_uninsured, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')

# Mortgage Lending - Residential, Reverse
ggplot(dat2017, aes(y = bank, x = mortgageloans_residential_reverse, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')
colnames(dat2017)

# Mortgage Lending - Non-residential
ggplot(dat2017, aes(y = bank, x = mortgageloans_nonresidential, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')

# Land, buildings, equip
ggplot(dat2017, aes(y = bank, x = land_buildings_equip, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')

# Land, buildings, equip
ggplot(dat2017, aes(y = bank, x = land_buildings_equip, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')

# Derivative related
ggplot(dat2017, aes(y = bank, x = derivative_related, fill = DSIB)) + 
  geom_col() +
  theme_bw(base_size = 8) +
  ggtitle(' Residential Mortgage Loans (% of Total Assets)')


dat2017 %>% 
  pivot_longer(-c(bank, date, currency, DSIB), names_to = 'item', values_to = 'value') %>%
  filter(item %in% c('mortgageloans_residential_insured', 'mortgageloans_residential_uninsured')) %>% 
  ggplot() + 
  geom_col(aes(y = bank, x = value, fill = item)) +
  theme_bw(base_size = 8) +
  ggtitle(' Mortgage Lending (% of Total Assets)') +
  xlab('Value') +
  ylab('Bank') +
  scale_fill_discrete(name = "Asset", labels = c("Insured",
                                                 "Uninsured"))
  
# Foreign Lending
dat2017 %>% 
  pivot_longer(-c(bank, date, currency, DSIB), 
               names_to = 'item', values_to = 'value') %>%
  filter(item %in% c('mortgageloans', 'nonmortgageloans', 
                     'totalsecurities', 'totalcash', 
                     'totalotherassets')) %>% 
  ggplot() + 
  geom_col(aes(y = bank, x = value, fill = item)) +
  theme_bw(base_size = 8) +
  ggtitle(' Major Asset Classes (% of Total Assets)') +
  xlab('Value') +
  ylab('Bank') +
  scale_fill_discrete(name = "Asset", labels = c("Mortgage Loans",
                                                 "Non-mortgage Loans", 
                                                 "Cash or Cash Equivalent",
                                                 "Other Assets",
                                                 "Securities"))

  
#### Regressions ####

# Initial Basic Specification

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'domestic') , paste(colnames(wide_growth)[i+3], '~ DSIB + DSB + DSIB:DSB'))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect * -0.001386 ***
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # positive effect - 0.017 *
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)


# Inital Basic Specification + Bank-specific fixed effects

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'domestic') , paste(colnames(wide_growth)[i+3], '~ DSIB + DSB + DSIB:DSB + wellcap + size + retaildeposit + int_rate'))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect * -0.001385 ***, positive effect from DSIB
summary(model_loansto_indiv_business) 
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans) 
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # positive effect - 0.017 *
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Inital Basic Specification + Bank-specific fixed effects + time fixed effects

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'domestic') , paste(colnames(wide_growth)[i+3], '~ DSIB + DSB + DSIB:DSB + wellcap + size + retaildeposit + int_rate + time'))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect * -0.00137 ***
summary(model_loansto_indiv_business) 
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans) 
summary(model_loansto_indiv_nonbusiness) 
summary(model_loansto_indiv_nonbusiness_othersecured) 
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # positive effect - 0.0173 *
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Inital Basic Specification + Bank-specific fixed effects + time fixed effects + Lagged int_rate + Lagged DSB

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'domestic') , paste(colnames(wide_growth)[i+3], '~ DSIB + trueDSB + DSIB:trueDSB + wellcap + size + retaildeposit + trueint_rate + time '))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect * -0.00137 ***
summary(model_loansto_indiv_business) 
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured) # positive effect 0.01804 .
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured) # strong positive effect from wellcap
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # positive effect - 0.0173 *
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Initial Basic Specification + Bank-specific fixed effects + time fixed effects + Lagged int_rate(2) + Lagged DSB(2)

wide_growth$trueDSB <- lag(wide_growth$DSB, n = 2L)
wide_growth$trueint_rate <- lag(wide_growth$int_rate, n = 1L)

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'domestic') , paste(colnames(wide_growth)[i+3], '~ DSIB + trueDSB + DSIB:trueDSB + wellcap + size + retaildeposit + trueint_rate + time '))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques) # strong positive effect - 0.052
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect * -0.0012 **
summary(model_loansto_indiv_business) 
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured) # positive effect 0.01904 .
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # positive effect - 0.0173 *
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Initial Basic Specification + Bank-specific fixed effects + time fixed effects + Lagged int_rate(1) + Lagged DSB(3)

wide_growth$trueDSB <- lag(wide_growth$DSB, n = 1L)
wide_growth$trueint_rate1 <- lag(wide_growth$int_rate, n = 1L)
wide_growth$trueint_rate2 <- lag(wide_growth$int_rate, n = 2L)
wide_growth$trueint_rate3 <- lag(wide_growth$int_rate, n = 3L)

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'domestic') , paste(colnames(wide_growth)[i+3], '~ DSIB + trueDSB + DSIB:trueDSB + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques) # strong positive effect - 0.052
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect * -0.0012 **
summary(model_loansto_indiv_business) 
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured) # positive effect 0.01904 .
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured) # strong positive effect from wellcap
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets) # strong positive effect
summary(model_totalsecurities)

# New specification - using a tightening and loosening cycle indicator variable instead of the DSB
# Creating an indicator variable for tightening and loosening cycles
spec_2 <- wide_growth
ENV <- spec_2$date
env <- matrix(NA, length(ENV), 1)
for (i in 0:length(ENV)) {
  if(isTRUE(as.Date(ENV[i]) < as.Date("25/06/2018", format = "%d/%m/%Y"))) {
    env[i] <- 0
  } else if (isTRUE(as.Date(ENV[i]) < as.Date("01/03/2020", format = "%d/%m/%Y"))){
    env[i] <- 1
  } else if (isTRUE(as.Date(ENV[i]) < as.Date("17/07/2021", format = "%d/%m/%Y"))){
    env[i] <- 0
  } else if (isTRUE(as.Date(ENV[i]) > as.Date("17/07/2021", format = "%d/%m/%Y"))){
    env[i] <- 1
  }
}

spec_2$ENV <- env
spec_2$trueENV <- lag(spec_2$ENV, n = 1L)

for (i in (1:(ncol(spec_2)-3))) {
  model <- lm(data = filter(spec_2, currency == 'domestic') , paste(colnames(spec_2)[i+3], '~ DSIB + ENV + DSIB:ENV + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_2)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques) # negative effect -0.08, .
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect -1.439
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)



# New specification - limited time-span
## Focus only on year before and after implementation - environmental indicator

spec_3 <- filter(spec_2, (date > as.Date("25/06/2017", format = "%d/%m/%Y")) 
                 & (date < as.Date("25/06/2019", format = "%d/%m/%Y")))

for (i in (1:(ncol(spec_3)-3))) {
  model <- lm(data = filter(spec_3, currency == 'domestic') , paste(colnames(spec_3)[i+3], '~ DSIB + ENV + DSIB:ENV + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_3)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness) # negative effect - 0 .0109 .
summary(model_loansto_indiv_nonbusiness_othersecured) # negative effect -0.0127 .
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

## Focus only on year before and after implementation - DSB indicator

spec_4 <- filter(spec_2, (date > as.Date("25/06/2017", format = "%d/%m/%Y")) 
                 & (date < as.Date("25/06/2019", format = "%d/%m/%Y")))

for (i in (1:(ncol(spec_4)-3))) {
  model <- lm(data = filter(spec_4, currency == 'domestic') , paste(colnames(spec_4)[i+3], '~ DSIB + ENV + DSIB:ENV + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_4)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # negative effect -0.003791 *
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness) # negative effect -0.0109 .
summary(model_loansto_indiv_nonbusiness_othersecured) # negative effect 0.01273 . 
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Reintroduction in 2021 effects

spec_7 <- filter(spec_2, (date > as.Date("01/01/2020", format = "%d/%m/%Y")) &
                   (date < as.Date("01/01/2022", format = "%d/%m/%Y")))

for (i in (1:(ncol(spec_7)-3))) {
  model <- lm(data = filter(spec_7, currency == 'domestic') , paste(colnames(spec_7)[i+3], '~ DSIB + ENV + DSIB:ENV + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_7)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect -0.07 .
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques) # negative effect
summary(model_govtsecurities)
summary(model_land_buildings_equip) # strong negative effect -0.048 .
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # strong positive effect 0.142 .
summary(model_loansto_foreigngov)
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential) # almost significant negative effect -0.007964
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured) # negative effect - 0.012
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash) # negative effect
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Reducing sample size of banks to those with large retail deposit franchises

wide_growth$bank <- as.factor(wide_growth$bank)

highretail <- wide_growth %>% 
  group_by(bank) %>%
  dplyr::summarise(mean = mean(retaildeposit)) %>% 
  filter(mean > 0.2)

wide_growth$trueDSB <- lag(wide_growth$DSB, n = 2L)
wide_growth$ENV <- env
spec_8 <- filter(wide_growth, bank %in% highretail$bank)

for (i in (1:(ncol(spec_8)-3))) {
  model <- lm(data = filter(spec_8, currency == 'domestic') , paste(colnames(spec_8)[i+3], '~ DSIB + trueDSB + DSIB:trueDSB + size + retaildeposit + trueint_rate1'))
  assign(paste0("model_", colnames(spec_8)[i+3]), model)
}

summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness) # negative effect -0.08 *
summary(model_loansto_indiv_nonbusiness_othersecured) # negative effect -0.009 *
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential) # negative effect -0.0025 **
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured) # negative effect -0.00281 **, positive effect from wellcap:DSIB (if added)
summary(model_nonmortgageloans)

stargazer(model_mortgageloans_residential,
          model_mortgageloans_residential_insured,
          model_mortgageloans_residential_uninsured, title = 'Residential Mortgage Loans')


# Robustness check on limited sample test
# initial introduction effects

rob_check_2 <- filter(spec_8, (date > as.Date("25/06/2017", format = "%d/%m/%Y")) 
                 & (date < as.Date("25/06/2019", format = "%d/%m/%Y")))

for (i in (1:(ncol(rob_check_2)-3))) {
  model <- lm(data = filter(rob_check_2, currency == 'domestic') , paste(colnames(rob_check_2)[i+3], '~ DSIB + ENV + DSIB:ENV + size + retaildeposit + trueint_rate1 + trueint_rate2 + time'))
  assign(paste0("model_", colnames(rob_check_2)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # positive effect 0.022 .
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip)
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov)
summary(model_loansto_indiv_business) # negative effect -0.00485  .
summary(model_loansto_indiv_business_othersecured) # negative effect - 0.014 .
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans) # negative effect - 0.006 .
summary(model_loansto_indiv_nonbusiness) # negative effect -0.0255 **
summary(model_loansto_indiv_nonbusiness_othersecured) # negative effect -0.0252 **
summary(model_loansto_indiv_nonbusiness_propsecured) # negative effect -0.0189 *
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential) # negative effect - 0.0079 .
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured) # almost significant negative effect - 0.0085
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# reintroduction effects
rob_check_3 <- filter(spec_8, (date > as.Date("01/01/2020", format = "%d/%m/%Y")) &
                   (date < as.Date("01/01/2022", format = "%d/%m/%Y")))

for (i in (1:(ncol(rob_check_3)-3))) {
  model <- lm(data = filter(rob_check_3, currency == 'domestic') , paste(colnames(rob_check_3)[i+3], '~ DSIB + ENV + DSIB:ENV + size + retaildeposit + trueint_rate1 + trueint_rate2 + time'))
  assign(paste0("model_", colnames(rob_check_3)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # strong positive effect -0.07 .
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # strong negative effect -0.048 .
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov)
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans) # negative effect -0.01 .
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential) # almost significant negative effect -0.006
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured) # negative effect -0.011 *
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # positive effect 0.05 .
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

#### Regressions - Foreign Lending ####

# Inital Basic Specification

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'foreign') , paste(colnames(wide_growth)[i+3], '~ DSIB + DSB + DSIB:DSB'))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}
summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # strong  positive effect 0.027 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect - 0.0304 *
summary(model_loansto_foreigngov) # positive effect 0.012 *
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured) # negative effect -0.02231 *
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured) # positive effect 0.012 .
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)


# Inital Basic Specification + Bank-specific fixed effects

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'foreign') , paste(colnames(wide_growth)[i+3], '~ DSIB + DSB + DSIB:DSB + wellcap + size + retaildeposit + int_rate'))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # strong positive effect 0.027 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect -0.0302
summary(model_loansto_foreigngov) # positive effect 0.0121 *
summary(model_loansto_indiv_business) 
summary(model_loansto_indiv_business_othersecured) 
summary(model_loansto_indiv_business_propsecured) # negative effect -0.0217 *
summary(model_totalloans) 
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured) # postive effect 0.01595 .
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Inital Basic Specification + Bank-specific fixed effects + time fixed effects

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'foreign') , paste(colnames(wide_growth)[i+3], '~ DSIB + DSB + DSIB:DSB + wellcap + size + retaildeposit + int_rate + time'))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) 
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # strong positive effect 0.0273***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect -0.0303 *
summary(model_loansto_foreigngov) # positive effect 0.0127 *
summary(model_loansto_indiv_business) 
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured) # negative effect -0.0216 *
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured) # positive effect 0.0159 .
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured) # strong positive effect from wellcap
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # positive effect - 0.0173 *
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Inital Basic Specification + Bank-specific fixed effects + time fixed effects + Lagged int_rate + Lagged DSB

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'foreign') , paste(colnames(wide_growth)[i+3], '~ DSIB + trueDSB + DSIB:trueDSB + wellcap + size + retaildeposit + trueint_rate + time '))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # positive effect - 0.0273 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect -0.028 *
summary(model_loansto_foreigngov) # positive effect 0.01063 .
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured) # negative effect -0.02674 **
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans) # negative effect -0.0036 ***
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential) # negative effect -0.00418 ***
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured) # negative effect -0.00335 **
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # negative effect - 0.016 ***
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Initial Basic Specification + Bank-specific fixed effects + time fixed effects + Lagged int_rate(1) + Lagged DSB(2)

wide_growth$trueDSB <- lag(wide_growth$DSB, n = 2L)
wide_growth$trueint_rate <- lag(wide_growth$int_rate, n = 1L)

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'foreign') , paste(colnames(wide_growth)[i+3], '~ DSIB + trueDSB + DSIB:trueDSB + wellcap + size + retaildeposit + trueint_rate + time '))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers) # positive effect 0.0197 .
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques) 
summary(model_govtsecurities)
summary(model_land_buildings_equip) # positive 0.02294 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect -0.028 *
summary(model_loansto_foreigngov) # positive effect 0.01063 .
summary(model_loansto_indiv_business) 
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured) # negative effect -0.0267 **
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans) # negative effect -0.00362 ***
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential) # negative effect -0.004185 ***
summary(model_mortgageloans_residential_insured) # strong positive effect from wellcap
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured) # negative effect -0.003346 **
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # negative effect - 0.016 **
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Initial Basic Specification + Bank-specific fixed effects + time fixed effects + Lagged int_rate(1) + Lagged DSB(3)

wide_growth$trueDSB <- lag(wide_growth$DSB, n = 1L)
wide_growth$trueint_rate1 <- lag(wide_growth$int_rate, n = 1L)
wide_growth$trueint_rate2 <- lag(wide_growth$int_rate, n = 2L)
wide_growth$trueint_rate3 <- lag(wide_growth$int_rate, n = 3L)

for (i in (1:(ncol(wide_growth)-3))) {
  model <- lm(data = filter(wide_growth, currency == 'foreign') , paste(colnames(wide_growth)[i+3], '~ DSIB + trueDSB + DSIB:trueDSB + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(wide_growth)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques) 
summary(model_govtsecurities)
summary(model_land_buildings_equip) # positive 0.027 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect -0.025 *
summary(model_loansto_foreigngov) # positive effect 0.015 **
summary(model_loansto_indiv_business)  # negative effect-0.0062 .
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured) # negative effect -0.0206 *
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured) # positive effect 0.0148 .
summary(model_mortgageloans) # negative effect - almost significant
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential) # negative effect -0.002065 .
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # negative effect - 0.026 ***
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# New specification - using a tightening and loosening cycle indicator variable instead of the DSB
# Creating an indicator variable for tightening and loosening cycles
spec_2 <- wide_growth
ENV <- spec_2$date
env <- matrix(NA, length(ENV), 1)
for (i in 0:length(ENV)) {
  if(isTRUE(as.Date(ENV[i]) < as.Date("25/06/2018", format = "%d/%m/%Y"))) {
    env[i] <- 0
  } else if (isTRUE(as.Date(ENV[i]) < as.Date("01/03/2020", format = "%d/%m/%Y"))){
    env[i] <- 1
  } else if (isTRUE(as.Date(ENV[i]) < as.Date("01/12/2021", format = "%d/%m/%Y"))){
    env[i] <- 0
  } else if (isTRUE(as.Date(ENV[i]) > as.Date("01/12/2021", format = "%d/%m/%Y"))){
    env[i] <- 1
  }
}

spec_2$ENV <- env
spec_2$trueENV <- lag(spec_2$ENV, n = 1L)

for (i in (1:(ncol(spec_2)-3))) {
  model <- lm(data = filter(spec_2, currency == 'foreign') , paste(colnames(spec_2)[i+3], '~ DSIB + ENV + DSIB:ENV + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_2)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques) # negative effect -0.039 .
summary(model_govtsecurities)
summary(model_land_buildings_equip) # strong positive effect 0.04825 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect -0.039 .
summary(model_loansto_foreigngov) # positive effect 0.0238 *
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans) # positive effect 0.004 *
summary(model_mortgageloans_nonresidential) # positive effect 0.01823 ***
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse) # positive effect 0.00363 .
summary(model_mortgageloans_residential_uninsured) # positive effect 0.0363 .
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # negative effect -0.02 *
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)


# New specification - limited time-span
## Focus only on year before and after implementation - environmental indicator

spec_3 <- filter(spec_2, (date > as.Date("25/06/2017", format = "%d/%m/%Y")) 
                 & (date < as.Date("25/06/2019", format = "%d/%m/%Y")))

for (i in (1:(ncol(spec_3)-3))) {
  model <- lm(data = filter(spec_3, currency == 'foreign') , paste(colnames(spec_3)[i+3], '~ DSIB + ENV + DSIB:ENV + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_3)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # again, strong positive 0.05 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect -0.095 *
summary(model_loansto_foreigngov)
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness) # negative effect - 0 .0109 .
summary(model_loansto_indiv_nonbusiness_othersecured) # negative effect -0.0127 .
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential) # positive 0.0252 ***
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

## Focus only on year before and after implementation - DSB indicator

spec_4 <- filter(spec_2, (date > as.Date("25/06/2017", format = "%d/%m/%Y")) 
                 & (date < as.Date("25/06/2019", format = "%d/%m/%Y")))

for (i in (1:(ncol(spec_4)-3))) {
  model <- lm(data = filter(spec_4, currency == 'foreign') , paste(colnames(spec_4)[i+3], '~ DSIB + ENV + DSIB:ENV + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_4)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # again, strong positive 0.05 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst) # negative effect -0.095 *
summary(model_loansto_foreigngov)
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans)
summary(model_mortgageloans_nonresidential) # positive 0.0252 ***
summary(model_mortgageloans_residential)
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares)
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)


# Reintroduction in 2021 effects

spec_7 <- filter(spec_2, (date > as.Date("01/01/2020", format = "%d/%m/%Y")) &
                   (date < as.Date("01/01/2022", format = "%d/%m/%Y")))

for (i in (1:(ncol(spec_7)-3))) {
  model <- lm(data = filter(spec_7, currency == 'foreign') , paste(colnames(spec_7)[i+3], '~ DSIB + ENV + DSIB:ENV + wellcap + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_7)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # almost significant positive effect
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov) # positive effect 0.05 **
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured) # positive effect 0.012 **
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans) # 0.01347 ***
summary(model_mortgageloans_nonresidential) # positive 0.0539 ***
summary(model_mortgageloans_residential) # positive 0.01185 ***
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured) # 0.01283 ***
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # almost significant negative
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

# Reducing sample size of banks to those with large retail deposit franchises

wide_growth$bank <- as.factor(wide_growth$bank)

highretail <- wide_growth %>% 
  group_by(bank) %>%
  dplyr::summarise(mean = mean(retaildeposit)) %>% 
  filter(mean > 0.2)

wide_growth$trueDSB <- lag(wide_growth$DSB, n = 1L)
spec_8 <- filter(wide_growth, bank %in% highretail$bank)

for (i in (1:(ncol(spec_8)-3))) {
  model <- lm(data = filter(spec_8, currency == 'foreign') , paste(colnames(spec_8)[i+3], '~ DSIB + trueDSB + DSIB:trueDSB + size + retaildeposit + trueint_rate1 + trueint_rate2 + time '))
  assign(paste0("model_", colnames(spec_8)[i+3]), model)
}

summary(model_call_shortloans_invest_dealers_brokers)
summary(model_debtsecurities)
summary(model_derivative_related)
summary(model_dueto_headoffice_regulatedfinancialinst)
summary(model_financialinst_deposits)
summary(model_gold_deposits_cheques)
summary(model_govtsecurities)
summary(model_land_buildings_equip) # positive effect 0.03131 ***
summary(model_loansto_fed_prov_govern)
summary(model_loansto_financialinst)
summary(model_loansto_foreigngov)
summary(model_loansto_indiv_business)
summary(model_loansto_indiv_business_othersecured)
summary(model_loansto_indiv_business_propsecured)
summary(model_totalloans)
summary(model_loansto_indiv_nonbusiness)
summary(model_loansto_indiv_nonbusiness_othersecured)
summary(model_loansto_indiv_nonbusiness_propsecured)
summary(model_mortgageloans) # almost significant negative
summary(model_mortgageloans_nonresidential)
summary(model_mortgageloans_residential) # negative 0.0035 .
summary(model_mortgageloans_residential_insured)
summary(model_mortgageloans_residential_mbspooled)
summary(model_mortgageloans_residential_reverse)
summary(model_mortgageloans_residential_uninsured)
summary(model_nonmortgageloans)
summary(model_other)
summary(model_othergovtsecurities)
summary(model_othersecurities)
summary(model_retaildeposit)
summary(model_reverserepo)
summary(model_shares) # negative -0.027 **
summary(model_tbills)
summary(model_totalcash)
summary(model_totalloans)
summary(model_totalotherassets)
summary(model_totalsecurities)

