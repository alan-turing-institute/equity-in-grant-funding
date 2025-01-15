library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(cowplot)
library(lme4)
library(MASS)
library(ordinal)
library(lmtest)
library(gtsummary)

################ Loading datasheets #################

grant <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Grants")
reviewer <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Reviewers")
investigator <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Investigators")

fellow <- investigator %>% filter(Role == 'Fellow')
fellow_PI <- investigator %>% filter(Role %in% c('Principal Investigator', 'Fellow'))
PI_coI <- investigator %>% filter(Role != 'Fellow')

lfeatures <- read.table('../tables/Score_and_LingFeatures.AllSections.tsv', sep = '\t', header = TRUE)[, -3]

merged <- merge(grant, fellow_PI, by = 'AnonPROJ', suffixes = c('.proj', '.inv'))
merged <- merged %>% group_by(AnonPROJ) %>% mutate(PI_num = n()) %>% filter(PI_num == 1) %>%
    merge(reviewer, by = 'AnonPROJ', suffixes = c('.inv', '.rev')) %>%
    dplyr::select(!c(PI_num, ConclusionsOnProposalDescription)) %>%
    filter(!is.na(OverallAssessmentScore) & OverallAssessmentScore %% 1 == 0) %>%
    left_join(distinct(lfeatures), by = c('AnonPROJ', 'AnonREV'))
colnames(merged)[66] <- "Sentiment.stanza"

format_table <- function(m) {
    t <- tbl_regression(m, estimate_fun = label_style_sigfig(digits = 3), intercept = TRUE) %>%
        bold_p()
    if(class(m) == 'lm')
        return(t %>% add_glance_table(include = c(r.squared, logLik, nobs)))
    if(class(m) == 'clm')
        return(t %>% add_glance_table(include = c(logLik, nobs)))
}
                         
################ Model 1: score (as numerical) on features of the comments ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'Agentic', 'Ability', 'Achievement',
           'Research', 'StandoutAdj', 'Masculine', 'Feminine', 'TotalWordCount',
           'TotalSentences', 'WordsPerSentence', 'Gender.inv', 'Gender.rev', 'EthnicityBinary.inv', 'EthnicityBinary.rev')]
d[, 2:12] <- as.data.frame(apply(d[, 2:12], 2, scale))

m1.0 <- lm(OverallAssessmentScore ~ Sentiment.stanza, data = d)
summary(m1.0)

m1.1 <- lm(OverallAssessmentScore ~ Sentiment.stanza + Ability, data = d)
summary(m1.1)

m1.2 <- lm(OverallAssessmentScore ~ Sentiment.stanza + Agentic, data = d)
summary(m1.2) 

m1.3 <- lm(OverallAssessmentScore ~ Sentiment.stanza + Achievement, data = d)
summary(m1.3)

m1.4 <- lm(OverallAssessmentScore ~ Sentiment.stanza + Research, data = d)
summary(m1.4) 

m1.5 <- lm(OverallAssessmentScore ~ Sentiment.stanza + StandoutAdj, data = d)
summary(m1.5) 

m1.6 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount, data = d)
summary(m1.6) 

m1.7 <- lm(OverallAssessmentScore ~ Sentiment.stanza + WordsPerSentence, data = d)
summary(m1.7) 

m1.8 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalSentences, data = d)
summary(m1.8) 

m1.9 <- lm(OverallAssessmentScore ~ Sentiment.stanza + Masculine, data = d)
summary(m1.9) 

m1.10 <- lm(OverallAssessmentScore ~ Sentiment.stanza + Feminine, data = d)
summary(m1.10)

m1.11 <- lm(OverallAssessmentScore ~ Sentiment.stanza + Feminine + Masculine, data = d)
summary(m1.11)

m1.12 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Agentic, data = d)
summary(m1.12)

m1.13 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + StandoutAdj, data = d)
summary(m1.13)

m1.14 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine, data = d)
summary(m1.14)

m1.15 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + Ability + Achievement + Research + StandoutAdj, data = d)
summary(m1.15) 
## included in report

m1.15a <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Ability + Achievement + Research + StandoutAdj, data = d)
summary(m1.15a)

m1.16 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj, data = d)
summary(m1.16) 

lrtest(m1.16, m1.15)

d <- d %>% filter(!(Gender.inv %in% c('Unknown', 'Not Disclosed')) & !(Gender.rev %in% c('Unknown', 'Not Disclosed')) &
                  !(EthnicityBinary.inv %in% c('Unknown', 'Not Disclosed')) & !(EthnicityBinary.rev %in% c('Unknown', 'Not Disclosed')) &
                  !is.na(Gender.inv) & !is.na(Gender.rev) & !is.na(EthnicityBinary.inv) & !is.na(EthnicityBinary.rev))

m1.16a <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj, data = d)
summary(m1.16a) 

m1.17 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj + Gender.inv + EthnicityBinary.inv, data = d)
summary(m1.17)

lrtest(m1.16a, m1.17)

m1.18 <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj + Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev, data = d)
summary(m1.18)
## include in report; need to rename
m1.18_rename <- lm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj + ApplicantSex * ReviewerSex + ApplicantEthnicityBinary * ReviewerEthnicityBinary, data = d %>% rename('ApplicantSex' = 'Gender.inv', 'ReviewerSex' = 'Gender.rev', 'ApplicantEthnicityBinary' = 'EthnicityBinary.inv', 'ReviewerEthnicityBinary' = 'EthnicityBinary.rev'))

lrtest(m1.17, m1.18)

save(list = ls(pattern = "m1\\.*"), file = "models/m1.raw_scores.RData")

################ Model 1a: score (as ordinal) on features of the comments ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'Agentic', 'Ability', 'Achievement',
           'Research', 'StandoutAdj', 'Masculine', 'Feminine', 'TotalWordCount',
           'TotalSentences', 'WordsPerSentence', 'Gender.inv', 'Gender.rev', 'EthnicityBinary.inv', 'EthnicityBinary.rev')]
d[, 2:10] <- as.data.frame(apply(d[, 2:10], 2, scale))
d$OverallAssessmentScore <- as.ordered(d$OverallAssessmentScore)

m1a.0 <- clm(OverallAssessmentScore ~ Sentiment.stanza, data = d)
summary(m1a.0)

m1a.1 <- clm(OverallAssessmentScore ~ Sentiment.stanza + Ability, data = d)
summary(m1a.1)

m1a.2 <- clm(OverallAssessmentScore ~ Sentiment.stanza + Agentic, data = d)
summary(m1a.2) 

m1a.3 <- clm(OverallAssessmentScore ~ Sentiment.stanza + Achievement, data = d)
summary(m1a.3)

m1a.4 <- clm(OverallAssessmentScore ~ Sentiment.stanza + Research, data = d)
summary(m1a.4) 

m1a.5 <- clm(OverallAssessmentScore ~ Sentiment.stanza + StandoutAdj, data = d)
summary(m1a.5) 

m1a.6 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount, data = d)
summary(m1a.6) 

m1a.7 <- clm(OverallAssessmentScore ~ Sentiment.stanza + WordsPerSentence, data = d)
summary(m1a.7) 

m1a.8 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalSentences, data = d)
summary(m1a.8) 

m1a.9 <- clm(OverallAssessmentScore ~ Sentiment.stanza + Masculine, data = d)
summary(m1a.9) 

m1a.10 <- clm(OverallAssessmentScore ~ Sentiment.stanza + Feminine, data = d)
summary(m1a.10)

m1a.11 <- clm(OverallAssessmentScore ~ Sentiment.stanza + Feminine + Masculine, data = d)
summary(m1a.11)

m1a.12 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Agentic, data = d)
summary(m1a.12)

m1a.13 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + StandoutAdj, data = d)
summary(m1a.13)

m1a.14 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine, data = d)
summary(m1a.14)

m1a.15 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + Ability + Achievement + Research + StandoutAdj, data = d)
summary(m1a.15) 

m1a.15a <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Ability + Achievement + Research + StandoutAdj, data = d)
summary(m1a.15a)
## included in report

m1a.16 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj, data = d)
summary(m1a.16)

lrtest(m1a.16, m1a.15)

d <- d %>% filter(!(Gender.inv %in% c('Unknown', 'Not Disclosed')) & !(Gender.rev %in% c('Unknown', 'Not Disclosed')) &
                  !(EthnicityBinary.inv %in% c('Unknown', 'Not Disclosed')) & !(EthnicityBinary.rev %in% c('Unknown', 'Not Disclosed')) &
                  !is.na(Gender.inv) & !is.na(Gender.rev) & !is.na(EthnicityBinary.inv) & !is.na(EthnicityBinary.rev))

m1a.16a <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj, data = d)
summary(m1a.16a) 

m1a.17 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj + Gender.inv + EthnicityBinary.inv, data = d)
summary(m1a.17)

lrtest(m1a.16a, m1a.17)

m1a.18 <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj + Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev, data = d)
summary(m1a.18)

## include in report; need to rename
m1a.18_rename <- clm(OverallAssessmentScore ~ Sentiment.stanza + TotalWordCount + Feminine + Masculine + Agentic + StandoutAdj + ApplicantSex * ReviewerSex + ApplicantEthnicityBinary * ReviewerEthnicityBinary, data = d %>% rename('ApplicantSex' = 'Gender.inv', 'ReviewerSex' = 'Gender.rev', 'ApplicantEthnicityBinary' = 'EthnicityBinary.inv', 'ReviewerEthnicityBinary' = 'EthnicityBinary.rev'))
format_table(m1a.18_rename)

lrtest(m1a.17, m1a.18)

save(list = ls(pattern = "m1a\\.*"), file = "models/m1a.ordinal_scores.RData")

################ Model 2: score (as numerical) on reviewer characteristics ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'AnonREV', 'Country',
                'ReviewerSource', 'MultiDisc', 'OverallQualityConfidence', 'Gender.rev',
                'AgeRange.rev', 'DisabilityInformationGrouped.rev', 'EthnicOriginGrouped.rev',
                'EthnicityBinary.rev', 'NationalityBinary.rev', 'AnonTHM', 'AnonPROJ')]
d[d == 'Unknown'] <- NA
factor_cols <- c('AnonREV', 'Country', 'ReviewerSource', 'MultiDisc',
                 'Gender.rev', 'AgeRange.rev', 'DisabilityInformationGrouped.rev',
                 'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                 'AnonTHM', 'AnonPROJ')
d[, factor_cols] <- lapply(d[, factor_cols], factor)

d_filter <- d %>% filter(Gender.rev != 'Not Disclosed' & EthnicityBinary.rev != 'Not Disclosed')

m2.0 <- lm(OverallAssessmentScore ~ Gender.rev*EthnicityBinary.rev, data = d_filter)
summary(m2.0)

m2.1 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev, data = d_filter)
summary(m2.1)

m2.2 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicOriginGrouped.rev, data = d_filter)
summary(m2.2)

m2.3 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + AgeRange.rev, data = d_filter)
summary(m2.3)

d_filter <- d_filter %>% mutate(AgeGroup.rev = ifelse(AgeRange.rev %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.rev %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.rev), NA, '56+'))))

m2.4 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + AgeGroup.rev, data = d_filter)
summary(m2.4)

m2.5 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country, data = d_filter)
summary(m2.5)

m2.6 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + AgeGroup.rev + Country, data = d_filter)

m2.7 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + NationalityBinary.rev, data = d_filter)
summary(m2.7)

m2.7a <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + NationalityBinary.rev, data = d_filter[d_filter$NationalityBinary.rev != 'Not Disclosed', ])
summary(m2.7a)

m2.7b <- lm(OverallAssessmentScore ~ Gender.rev + NationalityBinary.rev, data = d_filter[d_filter$NationalityBinary.rev != 'Not Disclosed', ])
summary(m2.7b)

m2.8 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + NationalityBinary.rev, data = d_filter)
summary(m2.8)

m2.8a <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + NationalityBinary.rev, data = d_filter[d_filter$NationalityBinary.rev != 'Not Disclosed', ])
summary(m2.8a)

table(d_filter[, c('Country', 'NationalityBinary.rev')])

m2.9 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + DisabilityInformationGrouped.rev, data = d_filter)
summary(m2.9)

m2.9a <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + DisabilityInformationGrouped.rev, data = d_filter[d_filter$DisabilityInformationGrouped.rev != 'Not disclosed', ])
summary(m2.9a)

m2.10 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + ReviewerSource, data = d_filter)
summary(m2.10)

m2.11 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + MultiDisc, data = d_filter)
summary(m2.11)

m2.12 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + ReviewerSource + MultiDisc, data = d_filter)
summary(m2.12)

m2.13 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + MultiDisc, data = d_filter)
summary(m2.13)

m2.14 <- lm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + MultiDisc + AgeGroup.rev, data = d_filter)
summary(m2.14)

## Random effects

m2.15 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + MultiDisc + (1|AnonTHM), data = d_filter)
summary(m2.15)

m2.16 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonTHM), data = d_filter)
summary(m2.16)

m2.17 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonREV), data = d_filter)
summary(m2.17)

m2.18 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + MultiDisc + (1|AnonREV), data = d_filter)
summary(m2.18)

m2.19 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonPROJ), data = d_filter)
summary(m2.19)

m2.20 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonPROJ) + (1|AnonTHM), data = d_filter)
summary(m2.20)

m2.21 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonPROJ) + (1|AnonREV), data = d_filter)
summary(m2.21)

m2.22 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonPROJ) + (1|AnonREV) + (1|AnonTHM), data = d_filter)
summary(m2.22)

m2.23 <- lmer(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonREV) + (1|AnonTHM), data = d_filter)
summary(m2.23)

save(list = ls(pattern = "m2\\.*"), file = "models/m2.raw_scores.RData")

################ Model 2a: score (as ordinal) on reviewer characteristics ################

library(MASS)

## Try out Box-Cox transformation first

res <- boxcox(lm(merged$OverallAssessmentScore ~ 1))
lambda <- res$x[which.max(res$y)]
merged <- merged %>% mutate(Score_BoxCox = (OverallAssessmentScore ^ lambda - 1)/lambda)
hist(merged$Score_BoxCox) ## Hmm still not looking normal

## Try ordinal regression

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'AnonREV', 'Country',
                'ReviewerSource', 'MultiDisc', 'OverallQualityConfidence', 'Gender.rev',
                'AgeRange.rev', 'DisabilityInformationGrouped.rev', 'EthnicOriginGrouped.rev',
                'EthnicityBinary.rev', 'NationalityBinary.rev', 'AnonTHM', 'AnonPROJ')]
d[d == 'Unknown'] <- NA
factor_cols <- c('AnonREV', 'Country', 'ReviewerSource', 'MultiDisc',
                 'Gender.rev', 'AgeRange.rev', 'DisabilityInformationGrouped.rev',
                 'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                 'AnonTHM', 'AnonPROJ')
d[, factor_cols] <- lapply(d[, factor_cols], factor)
d$OverallAssessmentScore <- as.ordered(d$OverallAssessmentScore)

d_filter <- d %>% filter(Gender.rev != 'Not Disclosed' & EthnicityBinary.rev != 'Not Disclosed')

m2a.0 <- clm(OverallAssessmentScore ~ Gender.rev*EthnicityBinary.rev, data = d_filter)
summary(m2a.0)

m2a.1 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev, data = d_filter)
summary(m2a.1)

m2a.2 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicOriginGrouped.rev, data = d_filter)
summary(m2a.2)

m2a.3 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + AgeRange.rev, data = d_filter)
summary(m2a.3)

d_filter <- d_filter %>% mutate(AgeGroup.rev = ifelse(AgeRange.rev %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.rev %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.rev), NA, '56+'))))

m2a.4 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + AgeGroup.rev, data = d_filter)
summary(m2a.4)

m2a.5 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country, data = d_filter)
summary(m2a.5)

m2a.6 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + AgeGroup.rev + Country, data = d_filter)

m2a.7 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + NationalityBinary.rev, data = d_filter)
summary(m2a.7)

m2a.7a <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + NationalityBinary.rev, data = d_filter[d_filter$NationalityBinary.rev != 'Not Disclosed', ])
summary(m2a.7a)

m2a.7b <- clm(OverallAssessmentScore ~ Gender.rev + NationalityBinary.rev, data = d_filter[d_filter$NationalityBinary.rev != 'Not Disclosed', ])
summary(m2a.7b)

m2a.8 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + NationalityBinary.rev, data = d_filter)
summary(m2a.8)

m2a.8a <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + NationalityBinary.rev, data = d_filter[d_filter$NationalityBinary.rev != 'Not Disclosed', ])
summary(m2a.8a)

table(d_filter[, c('Country', 'NationalityBinary.rev')])

m2a.9 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + DisabilityInformationGrouped.rev, data = d_filter)
summary(m2a.9)

m2a.9a <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + DisabilityInformationGrouped.rev, data = d_filter[d_filter$DisabilityInformationGrouped.rev != 'Not disclosed', ])
summary(m2a.9a)

m2a.10 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + ReviewerSource, data = d_filter)
summary(m2a.10)

m2a.11 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + MultiDisc, data = d_filter)
summary(m2a.11)

m2a.12 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + ReviewerSource + MultiDisc, data = d_filter)
summary(m2a.12)

m2a.13 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + MultiDisc, data = d_filter)
summary(m2a.13)

m2a.14 <- clm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + MultiDisc + AgeGroup.rev, data = d_filter)
summary(m2a.14)

## Random effects

m2a.15 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + MultiDisc + (1|AnonTHM), data = d_filter, Hess = TRUE)
summary(m2a.15)

m2a.16 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonTHM), data = d_filter, Hess = TRUE)
summary(m2a.16)

m2a.17 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonREV), data = d_filter, Hess = TRUE)
summary(m2a.17)

m2a.18 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + MultiDisc + (1|AnonREV), data = d_filter, Hess = TRUE)
summary(m2a.18)

## m2a.19 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonPROJ), data = d_filter, Hess = TRUE)
## summary(m2a.19)

## When including AnonPROJ, the run time is not feasible

## m2a.20 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonPROJ) + (1|AnonTHM), data = d_filter, Hess = TRUE)
## summary(m2a.20)

## m2a.21 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonPROJ) + (1|AnonREV), data = d_filter, Hess = TRUE)
## summary(m2a.21)

## m2a.22 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonPROJ) + (1|AnonREV) + (1|AnonTHM), data = d_filter, Hess = TRUE)
## summary(m2a.22)

m2a.23 <- clmm(OverallAssessmentScore ~ Gender.rev + EthnicityBinary.rev + Country + ReviewerSource + (1|AnonREV) + (1|AnonTHM), data = d_filter, Hess = TRUE)
summary(m2a.23)

save(list = ls(pattern = "m2a\\.*"), file = "models/m2a.ordinal_scores.RData")

################ Model 3: score (as numerical) on applicant characteristics ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'AnonINV', 'Country.inv',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'DisabilityInformationGrouped.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'AnonTHM', 'AnonPROJ',
                'GrantCategory')]
d[d == 'Unknown'] <- NA
factor_cols <- c('AnonINV', 'Country.inv', 'Gender.inv', 'AgeRange.inv',
                 'DisabilityInformationGrouped.inv', 'EthnicOriginGrouped.inv',
                 'EthnicityBinary.inv', 'NationalityBinary.inv',
                 'AnonTHM', 'AnonPROJ')
d[, factor_cols] <- lapply(d[, factor_cols], factor)

d_filter <- d %>% filter(Gender.inv != 'Not Disclosed' & EthnicityBinary.inv != 'Not Disclosed')

m3.0 <- lm(OverallAssessmentScore ~ Gender.inv * EthnicityBinary.inv, data = d_filter)
summary(m3.0)

m3.1 <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv, data = d_filter)
summary(m3.1)

m3.2 <- lm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv, data = d_filter)
summary(m3.2)

m3.3 <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeRange.inv, data = d_filter)
summary(m2.3)

m3.3a <- lm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + AgeRange.inv, data = d_filter)
summary(m3.3a)

d_filter <- d_filter %>% mutate(AgeGroup.inv = ifelse(AgeRange.inv %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.inv %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.inv), NA, '56+'))))

## d_filter <- d_filter %>% mutate(AgeGroup2.inv = ifelse(AgeRange.inv %in% c('26-35', '36-45'), '26-45',
##                                                 ifelse(AgeRange.inv %in% c('46-55', '56-65'), '46-65', as.character(AgeRange.inv))))
## d_filter$AgeGroup2.inv <- as.factor(d_filter$AgeGroup2.inv)

m3.4 <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeGroup.inv, data = d_filter)
summary(m3.4)

m3.4a <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeGroup.inv, data = d_filter[d_filter$GrantCategory == 'Research Grant', ])
summary(m3.4a)

m3.4b <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeGroup.inv, data = d_filter[d_filter$GrantCategory == 'Fellowship', ])
summary(m3.4b)

## Not looking into Country.inv as very few applicants not UK based

m3.5 <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + NationalityBinary.inv, data = d_filter)
summary(m3.5)

m3.5a <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + NationalityBinary.inv, data = d_filter[d_filter$NationalityBinary.inv != 'Not Disclosed', ])
summary(m3.5a)

m3.5b <- lm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + NationalityBinary.inv, data = d_filter[d_filter$NationalityBinary.inv != 'Not Disclosed', ])
summary(m3.5b)

m3.6 <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + DisabilityInformationGrouped.inv, data = d_filter)
summary(m3.6)

m3.6a <- lm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + DisabilityInformationGrouped.inv, data = d_filter)
summary(m3.6a)

m3.6b <- lm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + DisabilityInformationGrouped.inv, data = d_filter[d_filter$DisabilityInformationGrouped.inv != 'Not disclosed', ])
summary(m3.6b)

m3.7 <- lm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + Role, data = d_filter)
summary(m3.7)
## Same as fellowship vs. grant

## Random effects

m3.8 <- lmer(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + (1|AnonTHM), data = d_filter)
summary(m3.8)

m3.8a <- lmer(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + (1|AnonTHM), data = d_filter)
summary(m3.8a)

m3.9 <- lmer(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeGroup.inv + (1|AnonTHM), data = d_filter)
summary(m3.9)

save(list = ls(pattern = "m3\\.*"), file = "models/m3.raw_scores.RData")

################ Model 3a: score (as ordinal) on applicant characteristics ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'AnonINV', 'Country.inv',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'DisabilityInformationGrouped.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'AnonTHM', 'AnonPROJ',
                'GrantCategory')]
d[d == 'Unknown'] <- NA
factor_cols <- c('AnonINV', 'Country.inv', 'Gender.inv', 'AgeRange.inv',
                 'DisabilityInformationGrouped.inv', 'EthnicOriginGrouped.inv',
                 'EthnicityBinary.inv', 'NationalityBinary.inv',
                 'AnonTHM', 'AnonPROJ')
d[, factor_cols] <- lapply(d[, factor_cols], factor)

d$OverallAssessmentScore <- as.ordered(d$OverallAssessmentScore)

d_filter <- d %>% filter(Gender.inv != 'Not Disclosed' & EthnicityBinary.inv != 'Not Disclosed')

m3a.0 <- clm(OverallAssessmentScore ~ Gender.inv * EthnicityBinary.inv, data = d_filter)
summary(m3a.0)

m3a.1 <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv, data = d_filter)
summary(m3a.1)

m3a.2 <- clm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv, data = d_filter)
summary(m3a.2)

m3a.3 <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeRange.inv, data = d_filter)
summary(m3a.3)

m3a.3a <- clm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + AgeRange.inv, data = d_filter)
summary(m3a.3a)

d_filter <- d_filter %>% mutate(AgeGroup.inv = ifelse(AgeRange.inv %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.inv %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.inv), NA, '56+'))))

m3a.4 <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeGroup.inv, data = d_filter)
summary(m3a.4)

m3a.4a <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeGroup.inv, data = d_filter[d_filter$GrantCategory == 'Research Grant', ])
summary(m3a.4a)

m3a.4b <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeGroup.inv, data = d_filter[d_filter$GrantCategory == 'Fellowship', ])
summary(m3a.4b)

m3a.5 <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + NationalityBinary.inv, data = d_filter)
summary(m3a.5)

m3a.5a <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + NationalityBinary.inv, data = d_filter[d_filter$NationalityBinary.inv != 'Not Disclosed', ])
summary(m3a.5a)

m3a.5b <- clm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + NationalityBinary.inv, data = d_filter[d_filter$NationalityBinary.inv != 'Not Disclosed', ])
summary(m3a.5b)

m3a.6 <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + DisabilityInformationGrouped.inv, data = d_filter)
summary(m3a.6)

m3a.6a <- clm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + DisabilityInformationGrouped.inv, data = d_filter)
summary(m3a.6a)

m3a.6b <- clm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + DisabilityInformationGrouped.inv, data = d_filter[d_filter$DisabilityInformationGrouped.inv != 'Not disclosed', ])
summary(m3a.6b)

m3a.7 <- clm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + Role, data = d_filter)
summary(m3a.7)

## Random effects

m3a.8 <- clmm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + (1|AnonTHM), data = d_filter, Hess = TRUE)
summary(m3a.8)

m3a.8a <- clmm(OverallAssessmentScore ~ Gender.inv + EthnicOriginGrouped.inv + (1|AnonTHM), data = d_filter, Hess = TRUE)
summary(m3a.8a)

m3a.9 <- clmm(OverallAssessmentScore ~ Gender.inv + EthnicityBinary.inv + AgeGroup.inv + (1|AnonTHM), data = d_filter, Hess = TRUE)
summary(m3a.9)

save(list = ls(pattern = "m3a\\.*"), file = 'models/m3a.ordinal_scores.RData')

################ Model 4: score (as numeric) on both applicant and reviewer ################
################ characteristics, and other features about the funding call ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'AnonINV', 'AnonREV', 'Country.inv',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'Gender.rev', 'AgeRange.rev', 'Country',
                'ReviewerSource', 'MultiDisc', 'AnonOrgG.inv',
                'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                'AnonTHM', 'AnonPROJ', 'Outline', 'Driver', 'GrantCategory')]
d[d == 'Unknown'] <- NA

factor_cols <- c('AnonINV', 'AnonREV', 'Country.inv', 'Country', 'ReviewerSource', 'MultiDisc',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'Gender.rev', 'AgeRange.rev',
                'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                'AnonTHM', 'AnonPROJ', 'Outline', 'Driver', 'GrantCategory', 'AnonOrgG.inv')
d[, factor_cols] <- lapply(d[, factor_cols], factor)

d_filter <- d %>% filter(Gender.inv != 'Not Disclosed' & EthnicityBinary.inv != 'Not Disclosed' &
                         Gender.rev != 'Not Disclosed' & EthnicityBinary.rev != 'Not Disclosed')

d_filter <- d_filter %>% mutate(AgeGroup.inv = ifelse(AgeRange.inv %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.inv %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.inv), NA, '56+'))))

d_filter <- d_filter %>% mutate(AgeGroup.rev = ifelse(AgeRange.rev %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.rev %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.rev), NA, '56+'))))


m4.1 <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev, data = d_filter)
summary(m4.1)

m4.1a <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv + EthnicityBinary.rev, data = d_filter)
summary(m4.1a)

m4.1b <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv + EthnicOriginGrouped.rev, data = d_filter)
summary(m4.1b)

m4.1c <- lm(OverallAssessmentScore ~ Gender.inv * GrantCategory + Gender.rev + EthnicOriginGrouped.inv + EthnicOriginGrouped.rev, data = d_filter)
summary(m4.1c)

m4.1d <- lm(OverallAssessmentScore ~ Gender.inv * Driver + Gender.rev + EthnicOriginGrouped.inv + EthnicOriginGrouped.rev, data = d_filter)
summary(m4.1d)

m4.1e <- lm(OverallAssessmentScore ~ Gender.inv * Outline + Gender.rev + EthnicOriginGrouped.inv + EthnicOriginGrouped.rev, data = d_filter)
summary(m4.1e)

m4.2 <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource, data = d_filter)
summary(m4.2)

m4.3 <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country, data = d_filter)
summary(m4.3)

m4.4 <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.4)

m4.5 <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv, data = d_filter)
summary(m4.5)
## included in report but need to change Gender -> Sex
m4.5_rename <- lm(OverallAssessmentScore ~ ApplicantSex + ReviewerSex + ApplicantEthnicityBinary + ReviewerEthnicityBinary + ReviewerSource + Country + MultiDisc + ApplicantAge, data = d_filter %>% rename('ApplicantSex' = 'Gender.inv', 'ReviewerSex' = 'Gender.rev', 'ApplicantAge' = 'AgeGroup.inv', 'ApplicantEthnicityBinary' = 'EthnicityBinary.inv', 'ReviewerEthnicityBinary' = 'EthnicityBinary.rev'))
format_table(m4.5_rename)

m4.6 <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv + AgeGroup.rev, data = d_filter)
summary(m4.6)

m4.7 <- lm(OverallAssessmentScore ~ Gender.inv * Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv + AgeGroup.rev, data = d_filter)
summary(m4.7)

m4.7a <- lm(OverallAssessmentScore ~ Gender.inv * Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.7a)

m4.8 <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv + AgeGroup.rev, data = d_filter)
summary(m4.8)

m4.8a <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.8a)

m4.8b <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.8b)

m4.8c <- lm(OverallAssessmentScore ~ Gender.inv * Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.8c)
## included in report but need to change Gender -> Sex
m4.8c_rename <- lm(OverallAssessmentScore ~ ApplicantSex * ReviewerSex + ApplicantEthnicity * ReviewerEthnicity + ReviewerSource + Country + MultiDisc, data = d_filter %>% rename('ApplicantSex' = 'Gender.inv', 'ReviewerSex' = 'Gender.rev', 'ApplicantEthnicity' = 'EthnicOriginGrouped.inv', 'ReviewerEthnicity' = 'EthnicOriginGrouped.rev'))
format_table(m4.8c_rename)

m4.9 <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv * AgeGroup.rev, data = d_filter)
summary(m4.9)

m4.10 <- lm(OverallAssessmentScore ~ Gender.inv * EthnicityBinary.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.10)

m4.10a <- lm(OverallAssessmentScore ~ Gender.inv * EthnicOriginGrouped.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.10a)

m4.10b <- lm(OverallAssessmentScore ~ Gender.inv * EthnicOriginGrouped.inv + Gender.rev * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.10b)

m4.11 <- lm(OverallAssessmentScore ~ Gender.inv * AgeGroup.inv + EthnicOriginGrouped.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4.11)

m4.11a <- lm(OverallAssessmentScore ~ Gender.inv * AgeGroup.inv + EthnicOriginGrouped.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter[d_filter$GrantCategory == 'Fellowship', ])
summary(m4.11a)

m4.11b <- lm(OverallAssessmentScore ~ Gender.inv * AgeGroup.inv + EthnicOriginGrouped.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter[d_filter$GrantCategory == 'Research Grant', ])
summary(m4.11b)

m4.12 <- lm(OverallAssessmentScore ~ Gender.inv * Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver, data = d_filter)
summary(m4.12)

m4.12a <- lm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver, data = d_filter)
summary(m4.12a)

m4.13 <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver + (1|AnonTHM), data = d_filter)
summary(m4.13)

m4.14 <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver + (1|AnonTHM) + (1|AnonREV), data = d_filter)
summary(m4.14)

m4.14a <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver + (1|AnonREV), data = d_filter)
summary(m4.14a)

m4.15 <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver + (1|AnonTHM) + (1|AnonPROJ), data = d_filter)
summary(m4.15)

m4.16 <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver + (1|AnonREV) + (1|AnonPROJ), data = d_filter)
summary(m4.16)

m4.17 <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver + (1|AnonREV) + (1|AnonPROJ) + (1|AnonTHM), data = d_filter)
summary(m4.17)

m4.17a <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + Outline + Driver + (1|AnonREV) + (1|AnonPROJ) + (1|AnonTHM), data = d_filter)
summary(m4.17a)

m4.18 <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + Outline + Driver + (1|AnonOrgG.inv) + (1|AnonPROJ) + (1|AnonTHM), data = d_filter)
summary(m4.18)

m4.19 <- lmer(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + Outline + Driver + (1|AnonOrgG.inv) + (1|AnonPROJ) + (1|AnonTHM) + (1|AnonREV), data = d_filter)
summary(m4.19)

save(list = ls(pattern = "m4\\.*"), file = 'models/m4.raw_score.RData')

################ Model 4: score (as ordinal) on both applicant and reviewer ################
################ characteristics, and other features about the funding call ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'AnonINV', 'AnonREV', 'Country.inv',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'Gender.rev', 'AgeRange.rev', 'Country',
                'ReviewerSource', 'MultiDisc', 'AnonOrgG.inv',
                'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                'AnonTHM', 'AnonPROJ', 'Outline', 'Driver', 'GrantCategory')]
d[d == 'Unknown'] <- NA

factor_cols <- c('AnonINV', 'AnonREV', 'Country.inv', 'Country', 'ReviewerSource', 'MultiDisc',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'Gender.rev', 'AgeRange.rev',
                'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                'AnonTHM', 'AnonPROJ', 'Outline', 'Driver', 'GrantCategory', 'AnonOrgG.inv')
d[, factor_cols] <- lapply(d[, factor_cols], factor)

d_filter <- d %>% filter(Gender.inv != 'Not Disclosed' & EthnicityBinary.inv != 'Not Disclosed' &
                         Gender.rev != 'Not Disclosed' & EthnicityBinary.rev != 'Not Disclosed')

d_filter <- d_filter %>% mutate(AgeGroup.inv = ifelse(AgeRange.inv %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.inv %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.inv), NA, '56+'))))

d_filter <- d_filter %>% mutate(AgeGroup.rev = ifelse(AgeRange.rev %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.rev %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.rev), NA, '56+'))))
d_filter$OverallAssessmentScore <- as.ordered(d_filter$OverallAssessmentScore)

m4a.1 <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev, data = d_filter)
summary(m4a.1)

m4a.1a <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv + EthnicityBinary.rev, data = d_filter)
summary(m4a.1a)

m4a.1b <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv + EthnicOriginGrouped.rev, data = d_filter)
summary(m4a.1b)

m4a.1c <- clm(OverallAssessmentScore ~ Gender.inv * GrantCategory + Gender.rev + EthnicOriginGrouped.inv + EthnicOriginGrouped.rev, data = d_filter)
summary(m4a.1c)

m4a.1d <- clm(OverallAssessmentScore ~ Gender.inv * Driver + Gender.rev + EthnicOriginGrouped.inv + EthnicOriginGrouped.rev, data = d_filter)
summary(m4a.1d)

m4a.1e <- clm(OverallAssessmentScore ~ Gender.inv * Outline + Gender.rev + EthnicOriginGrouped.inv + EthnicOriginGrouped.rev, data = d_filter)
summary(m4a.1e)

m4a.2 <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource, data = d_filter)
summary(m4a.2)

m4a.3 <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country, data = d_filter)
summary(m4a.3)

m4a.4 <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.4)

m4a.5 <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv, data = d_filter)
summary(m4a.5)
## included in report
m4a.5_rename <- clm(OverallAssessmentScore ~ ApplicantSex + ReviewerSex + ApplicantEthnicityBinary + ReviewerEthnicityBinary + ReviewerSource + Country + MultiDisc + ApplicantAge, data = d_filter %>% rename('ApplicantSex' = 'Gender.inv', 'ReviewerSex' = 'Gender.rev', 'ApplicantAge' = 'AgeGroup.inv', 'ApplicantEthnicityBinary' = 'EthnicityBinary.inv', 'ReviewerEthnicityBinary' = 'EthnicityBinary.rev'))
format_table(m4a.5_rename)

m4a.6 <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv + AgeGroup.rev, data = d_filter)
summary(m4a.6)

m4a.7 <- clm(OverallAssessmentScore ~ Gender.inv * Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv + AgeGroup.rev, data = d_filter)
summary(m4a.7)

m4a.7a <- clm(OverallAssessmentScore ~ Gender.inv * Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.7a)

m4a.8 <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv + AgeGroup.rev, data = d_filter)
summary(m4a.8)

m4a.8a <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.8a)

m4a.8b <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.8b)

m4a.8c <- clm(OverallAssessmentScore ~ Gender.inv * Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.8c)
## included in report but need to change Gender -> Sex
m4a.8c_rename <- clm(OverallAssessmentScore ~ ApplicantSex * ReviewerSex + ApplicantEthnicity * ReviewerEthnicity + ReviewerSource + Country + MultiDisc, data = d_filter %>% rename('ApplicantSex' = 'Gender.inv', 'ReviewerSex' = 'Gender.rev', 'ApplicantEthnicity' = 'EthnicOriginGrouped.inv', 'ReviewerEthnicity' = 'EthnicOriginGrouped.rev'))
format_table(m4a.8c_rename)

m4a.9 <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc + AgeGroup.inv * AgeGroup.rev, data = d_filter)
summary(m4a.9)

m4a.10 <- clm(OverallAssessmentScore ~ Gender.inv * EthnicityBinary.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.10)

m4a.10a <- clm(OverallAssessmentScore ~ Gender.inv * EthnicOriginGrouped.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.10a)

m4a.10b <- clm(OverallAssessmentScore ~ Gender.inv * EthnicOriginGrouped.inv + Gender.rev * EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.10b)

m4a.11 <- clm(OverallAssessmentScore ~ Gender.inv * AgeGroup.inv + EthnicOriginGrouped.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter)
summary(m4a.11)

m4a.11a <- clm(OverallAssessmentScore ~ Gender.inv * AgeGroup.inv + EthnicOriginGrouped.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter[d_filter$GrantCategory == 'Fellowship', ])
summary(m4a.11a)

m4a.11b <- clm(OverallAssessmentScore ~ Gender.inv * AgeGroup.inv + EthnicOriginGrouped.inv + Gender.rev + EthnicityBinary.rev + ReviewerSource + Country + MultiDisc, data = d_filter[d_filter$GrantCategory == 'Research Grant', ])
summary(m4a.11b)

m4a.12 <- clm(OverallAssessmentScore ~ Gender.inv * Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver, data = d_filter)
summary(m4a.12)

m4a.12a <- clm(OverallAssessmentScore ~ Gender.inv + Gender.rev + EthnicOriginGrouped.inv * EthnicOriginGrouped.rev + ReviewerSource + Country + MultiDisc + Outline + Driver, data = d_filter)
summary(m4a.12a)

################ Model 5: Linguistic features on both applicant ##################
################ and reviewer characteristics and reviewer scores ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'AnonINV', 'AnonREV', 'Country.inv',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'Gender.rev', 'AgeRange.rev',
                'Country', 'ReviewerSource', 'MultiDisc', 'AnonOrgG.inv',
                'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                'AnonTHM', 'AnonPROJ', 'Outline', 'Driver', 'GrantCategory',
                'TotalWordCount', 'WordsPerSentence', 'Ability', 'Achievement', 'Agentic',
                'StandoutAdj', 'Research', 'Masculine', 'Feminine', 'NegEval.LIWC', 'PosEval.LIWC')]
d[d == 'Unknown'] <- NA

factor_cols <- c('AnonINV', 'AnonREV', 'Country.inv', 'Country', 'ReviewerSource', 'MultiDisc',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'Gender.rev', 'AgeRange.rev',
                'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                'AnonTHM', 'AnonPROJ', 'Outline', 'Driver', 'GrantCategory', 'AnonOrgG.inv')
d[, factor_cols] <- lapply(d[, factor_cols], factor)

to_scale_cols <- c('Ability', 'Achievement', 'Agentic',
                'StandoutAdj', 'Research', 'Masculine', 'Feminine', 'NegEval.LIWC', 'PosEval.LIWC',
                'Sentiment.stanza')
d[, to_scale_cols] <- lapply(d[, to_scale_cols], scale)

d_filter <- d %>% filter(Gender.inv != 'Not Disclosed' & EthnicityBinary.inv != 'Not Disclosed' &
                         Gender.rev != 'Not Disclosed' & EthnicityBinary.rev != 'Not Disclosed')

d_filter <- d_filter %>% mutate(AgeGroup.inv = ifelse(AgeRange.inv %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.inv %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.inv), NA, '56+'))))

d_filter <- d_filter %>% mutate(AgeGroup.rev = ifelse(AgeRange.rev %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.rev %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.rev), NA, '56+'))))

m5.1.0 <- lm(Ability ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m5.1.0)

m5.1.1 <- lm(Ability ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m5.1.1)

m5.1.2 <- lm(Ability ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.1.2)

m5.1.3 <- lm(Ability ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.1.3)


m5.2.0 <- lm(Achievement ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m5.2.0)

m5.2.1 <- lm(Achievement ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m5.2.1)

m5.2.2 <- lm(Achievement ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.2.2)

m5.2.3 <- lm(Achievement ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.2.3)

m5.3.0 <- lm(StandoutAdj ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m5.3.0)

m5.3.1 <- lm(StandoutAdj ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m5.3.1)

m5.3.2 <- lm(StandoutAdj ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.3.2)

m5.3.3 <- lm(StandoutAdj ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.3.3)

m5.4.0 <- lm(Agentic ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m5.4.0)

m5.4.1 <- lm(Agentic ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m5.4.1)

m5.4.2 <- lm(Agentic ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.4.2)

m5.4.3 <- lm(Agentic ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.4.3)

m5.5.0 <- lm(Feminine ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m5.5.0)

m5.5.1 <- lm(Feminine ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m5.5.1)

m5.5.2 <- lm(Feminine ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.5.2)

m5.5.3 <- lm(Feminine ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.5.3)

m5.6.0 <- lm(Masculine ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m5.6.0)

m5.6.1 <- lm(Masculine ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m5.6.1)

m5.6.2 <- lm(Masculine ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.6.2)

m5.6.3 <- lm(Masculine ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.6.3)

m5.7.0 <- lm(Research ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m5.7.0)

m5.7.1 <- lm(Research ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m5.7.1)

m5.7.2 <- lm(Research ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.7.2)

m5.7.3 <- lm(Research ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.7.3)

m5.8.0 <- lm(Sentiment.stanza ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m5.8.0)

m5.8.1 <- lm(Sentiment.stanza ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m5.8.1)

m5.8.2 <- lm(Sentiment.stanza ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.8.2)

m5.8.3 <- lm(Sentiment.stanza ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m5.8.3)

save(list = ls(pattern = "m5\\.*"), file = 'models/m5.RData')

################ Model 6: Linguistic features on both applicant and ##################
################ reviewer characteristics and reviewer scores (ordinal) ################

d <- merged[, c('OverallAssessmentScore', 'Sentiment.stanza', 'AnonINV', 'AnonREV', 'Country.inv',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'Gender.rev', 'AgeRange.rev',
                'Country', 'ReviewerSource', 'MultiDisc', 'AnonOrgG.inv',
                'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                'AnonTHM', 'AnonPROJ', 'Outline', 'Driver', 'GrantCategory',
                'TotalWordCount', 'WordsPerSentence', 'Ability', 'Achievement', 'Agentic',
                'StandoutAdj', 'Research', 'Masculine', 'Feminine', 'NegEval.LIWC', 'PosEval.LIWC')]
d[d == 'Unknown'] <- NA

factor_cols <- c('AnonINV', 'AnonREV', 'Country.inv', 'Country', 'ReviewerSource', 'MultiDisc',
                'Role', 'OriginalApplicant', 'Gender.inv', 'AgeRange.inv', 'EthnicOriginGrouped.inv',
                'EthnicityBinary.inv', 'NationalityBinary.inv', 'Gender.rev', 'AgeRange.rev',
                'EthnicOriginGrouped.rev', 'EthnicityBinary.rev', 'NationalityBinary.rev',
                'AnonTHM', 'AnonPROJ', 'Outline', 'Driver', 'GrantCategory', 'AnonOrgG.inv', 'OverallAssessmentScore')
d[, factor_cols] <- lapply(d[, factor_cols], factor)

to_scale_cols <- c('Ability', 'Achievement', 'Agentic',
                'StandoutAdj', 'Research', 'Masculine', 'Feminine', 'NegEval.LIWC', 'PosEval.LIWC',
                'Sentiment.stanza')
d[, to_scale_cols] <- lapply(d[, to_scale_cols], scale)

d_filter <- d %>% filter(Gender.inv != 'Not Disclosed' & EthnicityBinary.inv != 'Not Disclosed' &
                         Gender.rev != 'Not Disclosed' & EthnicityBinary.rev != 'Not Disclosed')

d_filter <- d_filter %>% mutate(AgeGroup.inv = ifelse(AgeRange.inv %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.inv %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.inv), NA, '56+'))))

d_filter <- d_filter %>% mutate(AgeGroup.rev = ifelse(AgeRange.rev %in% c('25 and under', '26-35'), '35 and under',
                                               ifelse(AgeRange.rev %in% c('36-45', '46-55'), '36-55',
                                               ifelse(is.na(AgeRange.rev), NA, '56+'))))

m6.1.0 <- lm(Ability ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m6.1.0)

m6.1.1 <- lm(Ability ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m6.1.1)

m6.1.2 <- lm(Ability ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.1.2)

m6.1.3 <- lm(Ability ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.1.3)


m6.2.0 <- lm(Achievement ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m6.2.0)

m6.2.1 <- lm(Achievement ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m6.2.1)

m6.2.2 <- lm(Achievement ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.2.2)

m6.2.3 <- lm(Achievement ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.2.3)

m6.3.0 <- lm(StandoutAdj ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m6.3.0)

m6.3.1 <- lm(StandoutAdj ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m6.3.1)

m6.3.2 <- lm(StandoutAdj ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.3.2)

m6.3.3 <- lm(StandoutAdj ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.3.3)

m6.4.0 <- lm(Agentic ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m6.4.0)

m6.4.1 <- lm(Agentic ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m6.4.1)

m6.4.2 <- lm(Agentic ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.4.2)

m6.4.3 <- lm(Agentic ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.4.3)

m6.5.0 <- lm(Feminine ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m6.5.0)

m6.5.1 <- lm(Feminine ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m6.5.1)

m6.5.2 <- lm(Feminine ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.5.2)

m6.5.3 <- lm(Feminine ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.5.3)

m6.6.0 <- lm(Masculine ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m6.6.0)

m6.6.1 <- lm(Masculine ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m6.6.1)

m6.6.2 <- lm(Masculine ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.6.2)

m6.6.3 <- lm(Masculine ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.6.3)

m6.7.0 <- lm(Research ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m6.7.0)

m6.7.1 <- lm(Research ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m6.7.1)

m6.7.2 <- lm(Research ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.7.2)

m6.7.3 <- lm(Research ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.7.3)

m6.8.0 <- lm(Sentiment.stanza ~ Gender.inv + OverallAssessmentScore, data = d_filter)
summary(m6.8.0)

m6.8.1 <- lm(Sentiment.stanza ~ Gender.inv + Gender.rev + OverallAssessmentScore, data = d_filter)
summary(m6.8.1)

m6.8.2 <- lm(Sentiment.stanza ~ Gender.inv + Gender.rev + EthnicityBinary.inv + EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.8.2)

m6.8.3 <- lm(Sentiment.stanza ~ Gender.inv * Gender.rev + EthnicityBinary.inv * EthnicityBinary.rev + OverallAssessmentScore, data = d_filter)
summary(m6.8.3)

save(list = ls(pattern = "m5\\.*"), file = 'models/m6.RData')

