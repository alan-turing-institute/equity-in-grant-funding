library(lme4)
library(gtsummary)
library(lmtest)
library(dplyr)

################ Loading & filtering data #################

ranking <- read.table('data/merged_ranking_table.tsv', sep = "\t", header = TRUE)

filter_list_size <- function(rank_df, min_list_size) {
    list <- rank_df %>% count(AnonLST, name = 'ListSize') %>%
        select('AnonLST', 'ListSize') %>%
        distinct()
    valid_list <- list %>% 
        filter(ListSize >= min_list_size)
    filtered_ranking <- rank_df %>% filter(AnonLST %in% valid_list$AnonLST)
    return(filtered_ranking)
}

format_table <- function(m) {
    t <- tbl_regression(m, estimate_fun = label_style_sigfig(digits = 3), intercept = TRUE) %>%
        bold_p()
    if(class(m) == 'lm')
        return(t %>% add_glance_table(include = c(r.squared, logLik, nobs)))
    if(class(m) == 'clm')
        return(t %>% add_glance_table(include = c(logLik, nobs)))
}

################ Model 1: Rank quantile on stats of reviewer scores  ################

filtered_ranking <- ranking %>%
    filter(!is.na(RankQuantile) & MeetingType %in% c('Proposal', 'Interview'))
## Sift and Outline meetings happen before the full proposal is submitted

m1.0 <- lm(RankQuantile ~ ScoreQuantile, data = filtered_ranking)
summary(m1.0)

m1.0a <- lm(RankQuantile ~ ScoreQuantile,
            data = filtered_ranking[filtered_ranking$GrantCategory == 'Research Grant', ])
summary(m1.0a)

m1.1 <- lm(RankQuantile ~ ScoreMean, data = filtered_ranking)
summary(m1.1)

m1.2 <- lm(RankQuantile ~ ScoreQuantile + ScoreSD, data = filtered_ranking)
summary(m1.2)

m1.3 <- lm(RankQuantile ~ ScoreQuantile + ScoreRange, data = filtered_ranking)
summary(m1.3)

m1.4 <- lm(RankQuantile ~ ScoreQuantile + ScoreMin, data = filtered_ranking)
summary(m1.4)

m1.5 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax, data = filtered_ranking)
summary(m1.5)

m1.6 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin, data = filtered_ranking)
summary(m1.6)
## included in report

m1.6a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin,
            data = filtered_ranking[filtered_ranking$GrantCategory == 'Research Grant', ])
summary(m1.6a)

m1.6b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin,
            data = filter_list_size(ranking, 10))
summary(m1.6b)

AIC(m1.0, m1.4, m1.5, m1.6)
BIC(m1.0, m1.4, m1.5, m1.6)

lrtest(m1.6, m1.0)

save(list = ls(pattern = "m1\\.*"), file = "models/m1.RData")

################ Model 2: Rank quantile on applicant characteristics, ################
################ controlling for stats about reviewer scores          ################

unknowns <- c(NA, 'Unknown', 'unknown')
filtered_ranking <- ranking %>%
    filter(!is.na(RankQuantile) & !(Gender %in% unknowns) & !(EthnicityBinary %in% unknowns) &
           MeetingType %in% c('Proposal', 'Interview')) %>%
    mutate(MeetingType = relevel(factor(MeetingType, ordered = FALSE), ref = 'Proposal'))
## interview <- filtered_ranking %>% filter(MeetingType = 

m2.1 <- lm(RankQuantile ~ ScoreQuantile + Gender, data = filtered_ranking)
summary(m2.1)

m2.1a <- lm(RankQuantile ~ ScoreQuantile + Gender, data = filtered_ranking[filtered_ranking$Gender != 'Not Disclosed', ])
summary(m2.1a)

m2.1b <- lm(RankQuantile ~ ScoreQuantile + Gender * MeetingType, data = filtered_ranking)
summary(m2.1b)

lrtest(m2.1b, m2.1)

m2.2 <- lm(RankQuantile ~ ScoreQuantile + EthnicityBinary, data = filtered_ranking)
summary(m2.2)

m2.2a <- lm(RankQuantile ~ ScoreQuantile + EthnicOriginGrouped, data = filtered_ranking)
summary(m2.2a)

m2.2b <- lm(RankQuantile ~ ScoreQuantile + EthnicityBinary * MeetingType, data = filtered_ranking)
summary(m2.2b)

m2.3 <- lm(RankQuantile ~ ScoreQuantile + Gender + EthnicityBinary, data = filtered_ranking)
summary(m2.3)

m2.3a <- lm(RankQuantile ~ ScoreQuantile + Gender + EthnicityBinary, data = filtered_ranking[filtered_ranking$Gender != 'Not Disclosed' & filtered_ranking$EthnicityBinary != 'Not Disclosed', ])
summary(m2.3a)

m2.3b <- lm(RankQuantile ~ ScoreQuantile + MeetingType + Gender + EthnicityBinary, data = filtered_ranking[filtered_ranking$Gender != 'Not Disclosed' & filtered_ranking$EthnicityBinary != 'Not Disclosed', ])
summary(m2.3b)

m2.3c <- lm(RankQuantile ~ ScoreQuantile + ListType + Gender + EthnicityBinary, data = filtered_ranking[filtered_ranking$Gender != 'Not Disclosed' & filtered_ranking$EthnicityBinary != 'Not Disclosed', ])
summary(m2.3c)

m2.3d <- lm(RankQuantile ~ ScoreQuantile + MeetingType * Gender + EthnicityBinary, data = filtered_ranking[filtered_ranking$Gender != 'Not Disclosed' & filtered_ranking$EthnicityBinary != 'Not Disclosed', ])
summary(m2.3d)

m2.3e <- lm(RankQuantile ~ ScoreQuantile + MeetingType * Gender + MeetingType * EthnicityBinary, data = filtered_ranking[filtered_ranking$Gender != 'Not Disclosed' & filtered_ranking$EthnicityBinary != 'Not Disclosed', ])
summary(m2.3e)

m2.3f <- lm(RankQuantile ~ ScoreQuantile + Gender * ScoreRange + EthnicityBinary * ScoreRange, data = filtered_ranking)
summary(m2.3f)

m2.3g <- lm(RankQuantile ~ ScoreQuantile + Gender * ScoreSD + EthnicityBinary * ScoreSD, data = filtered_ranking)
summary(m2.3g)

m2.3h <- lm(RankQuantile ~ ScoreQuantile + Gender * ScoreMin + EthnicityBinary * ScoreMin, data = filtered_ranking)
summary(m2.3h)

m2.4 <- lm(RankQuantile ~ ScoreQuantile + Gender * EthnicityBinary, data = filtered_ranking)
summary(m2.4)

m2.4a <- lm(RankQuantile ~ ScoreQuantile + Gender * EthnicityBinary, data = filtered_ranking[filtered_ranking$Gender != 'Not Disclosed' & filtered_ranking$EthnicityBinary != 'Not Disclosed', ])
summary(m2.4a)

m2.4b <- lm(RankQuantile ~ ScoreQuantile + Gender * EthnicityBinary + MeetingType, data = filtered_ranking)
summary(m2.4b)

m2.5 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary, data = filtered_ranking)
summary(m2.5)

m2.6 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicOriginGrouped, data = filtered_ranking)
summary(m2.6)

m2.7 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + MeetingType * Gender + EthnicityBinary, data = filtered_ranking)
summary(m2.7)

m2.7a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + MeetingType * Gender + MeetingType * EthnicityBinary, data = filtered_ranking)
summary(m2.7a)
## Interviews seem to put men at a disadvantage, but White people at an advantage

lrtest(m2.7, m2.7a)

m2.7b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + MeetingType * Gender + MeetingType * EthnicityBinary, data = filtered_ranking[filtered_ranking$Gender != 'Not Disclosed' & filtered_ranking$EthnicityBinary != 'Not Disclosed', ])
summary(m2.7b)

m2.8 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary + AgeRange, data = filtered_ranking)
summary(m2.8)

filtered_ranking <- filtered_ranking %>%
    mutate(AgeGroup = ifelse(AgeRange %in% c('25 and under', '26-35'), '35 and under',
                      ifelse(AgeRange %in% c('36-45', '46-55'), '36-55',
                      ifelse(is.na(AgeRange), NA, '56+'))))

m2.8a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary + AgeGroup, data = filtered_ranking)
summary(m2.8a)
## Older (56+) applicants have a disadvantage

m2.8b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * MeetingType + EthnicityBinary * MeetingType + AgeGroup, data = filtered_ranking)
summary(m2.8b)

m2.8c <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * MeetingType + EthnicityBinary * MeetingType + AgeGroup * MeetingType, data = filtered_ranking)
summary(m2.8c)

m2.9 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary + NationalityBinary, data = filtered_ranking)
summary(m2.9)

m2.9a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * MeetingType + EthnicityBinary * MeetingType + AgeGroup + NationalityBinary, data = filtered_ranking)
summary(m2.9a)

m2.9b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * MeetingType + EthnicityBinary * MeetingType + AgeGroup + NationalityBinary * MeetingType, data = filtered_ranking)
summary(m2.9b)
## Best model 

m2.10 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * MeetingType + EthnicityBinary * MeetingType + AgeGroup + NationalityBinary + DisabilityInformationGrouped, data = filtered_ranking)
summary(m2.10)

m2.10a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * MeetingType + EthnicityBinary * MeetingType + AgeGroup + NationalityBinary + DisabilityInformationGrouped * MeetingType, data = filtered_ranking)
summary(m2.10a)

m2.10b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * MeetingType + EthnicityBinary * MeetingType + AgeGroup + NationalityBinary * MeetingType + DisabilityInformationGrouped, data = filtered_ranking)
summary(m2.10b)

save(list = ls(pattern = "m2\\.*"), file = "models/m2.RData")

################ Model 2a & 2b: Separate Interviews and Proposals ################

unknowns <- c(NA, 'Unknown', 'unknown')

filtered_ranking <- ranking %>%
    filter(!is.na(RankQuantile) & !(Gender %in% unknowns) & !(EthnicityBinary %in% unknowns) &
           !(NationalityBinary %in% unknowns) &
           MeetingType %in% c('Proposal', 'Interview')) %>%
    mutate(AgeGroup = ifelse(AgeRange %in% c('25 and under', '26-35'), '35 and under',
                      ifelse(AgeRange %in% c('36-45', '46-55'), '36-55',
                      ifelse(is.na(AgeRange), NA, '56+'))))

interview <- filtered_ranking %>% filter(MeetingType == 'Interview') ## 2,500 rows
proposal <- filtered_ranking %>% filter(MeetingType == 'Proposal') ## 12,206 rows

m2a.9 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex + ApplicantEthnicityBinary + AgeGroup + NationalityBinary, data = proposal)
summary(m2a.9)

m2a.9_rename <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex + ApplicantEthnicityBinary + ApplicantAge + ApplicantNationality, data = proposal %>%    
            rename('ApplicantSex' = 'Gender', 'ApplicantEthnicityBinary' = 'EthnicityBinary', 'ApplicantAge' = 'AgeGroup', 'ApplicantNationality' = 'NationalityBinary'))
summary(m2a.9_rename)

m2b.9 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary + AgeGroup + NationalityBinary, data = interview)
summary(m2b.9)

m2b.9_rename <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex + ApplicantEthnicityBinary + ApplicantAge + ApplicantNationality, data = interview %>%
            rename('ApplicantSex' = 'Gender', 'ApplicantEthnicityBinary' = 'EthnicityBinary', 'ApplicantAge' = 'AgeGroup', 'ApplicantNationality' = 'NationalityBinary'))
summary(m2b.9_rename)
## included in report

save(list = ls(pattern = "m2*"), file = "models/m2.RData")

################ Model 3: Ranking quantiles on panel characteristics ################
################ Most only make sense in interaction with applicant  ################

unknowns <- c(NA, 'Unknown', 'unknown')

filtered_ranking <- ranking %>%
    filter(!is.na(RankQuantile) & !(Gender %in% unknowns) & !(EthnicityBinary %in% unknowns) &
           !(NationalityBinary %in% unknowns) &
           MeetingType %in% c('Proposal', 'Interview')) %>%
    mutate(AgeGroup = ifelse(AgeRange %in% c('25 and under', '26-35'), '35 and under',
                      ifelse(AgeRange %in% c('36-45', '46-55'), '36-55',
                      ifelse(is.na(AgeRange), NA, '56+'))),
           ChairAgeGroup = ifelse(ChairAgeRange %in% c('25 and under', '26-35'), '35 and under',
                      ifelse(ChairAgeRange %in% c('36-45', '46-55'), '36-55',
                      ifelse(is.na(ChairAgeRange), NA, '56+'))),
           FemalePresent = FemaleProportion > 0,
           EthnicMinorityPresent = NonWhiteProportion > 0)

interview <- filtered_ranking %>% filter(MeetingType == 'Interview') ## 2,500 rows
proposal <- filtered_ranking %>% filter(MeetingType == 'Proposal') ## 12,206 rows

m3.0a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = proposal)
summary(m3.0a)

m3.0b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = interview)
summary(m3.0b)

m3.1a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * ChairGender + EthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = proposal)
summary(m3.1a)

m3.1b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * ChairGender + EthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = interview)
summary(m3.1b)

m3.2a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * FemaleProportion + EthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = proposal)
summary(m3.2a)

m3.2b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * FemaleProportion + EthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = interview)
summary(m3.2b)

m3.3a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * FemalePresent + EthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = proposal)
summary(m3.3a)

m3.3b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * FemalePresent + EthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = interview)
summary(m3.3b)

m3.4a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary * ChairEthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = proposal)
summary(m3.4a)

m3.4b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary * ChairEthnicityBinary + AgeGroup + NationalityBinary + Conflict, data = interview)
summary(m3.4b)

m3.5a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary * NonWhiteProportion + AgeGroup + NationalityBinary + Conflict, data = proposal)
summary(m3.5a)

m3.5b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary * NonWhiteProportion + AgeGroup + NationalityBinary + Conflict, data = interview)
summary(m3.5b)

m3.6a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary * EthnicMinorityPresent + AgeGroup + NationalityBinary + Conflict, data = proposal)
summary(m3.6a)

m3.6b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender + EthnicityBinary * EthnicMinorityPresent + AgeGroup + NationalityBinary + Conflict, data = interview)
summary(m3.6b)

m3.7a <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * FemalePresent + EthnicityBinary * EthnicMinorityPresent + AgeGroup + NationalityBinary + Conflict, data = proposal)
summary(m3.7a)

m3.7a.1 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * FemalePresent + EthnicityBinary * EthnicMinorityPresent + AgeGroup + NationalityBinary + Conflict, data = proposal[(proposal$Gender != 'Not Disclosed') & (proposal$EthnicityBinary != 'Not Disclosed'), ])
summary(m3.7a.1)

m3.7a_rename <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex * FemalePanelistPresent + ApplicantEthnicity * EthnicMinorityPanelistPresent + ApplicantAge + ApplicantNationality + Conflict,
                   data = proposal %>% rename('ApplicantSex' = 'Gender', 'FemalePanelistPresent' = 'FemalePresent', 'ApplicantEthnicity' = 'EthnicityBinary', 'ApplicantAge' = 'AgeGroup',
                                              'EthnicMinorityPanelistPresent' = 'EthnicMinorityPresent', 'ApplicantNationality' = 'NationalityBinary'))
format_table(m3.7a_rename)


m3.7b <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * FemalePresent + EthnicityBinary * EthnicMinorityPresent + AgeGroup + NationalityBinary + Conflict, data = interview)
summary(m3.7b)

m3.7b.1 <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + Gender * FemalePresent + EthnicityBinary * EthnicMinorityPresent + AgeGroup + NationalityBinary + Conflict, data = interview[(interview$Gender != 'Not Disclosed') & (interview$EthnicityBinary != 'Not Disclosed'), ])
summary(m3.7b.1)

m3.7b_rename <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex * FemalePanelistPresent + ApplicantEthnicity * EthnicMinorityPanelistPresent + ApplicantAge + ApplicantNationality + Conflict,
                   data = interview %>% rename('ApplicantSex' = 'Gender', 'FemalePanelistPresent' = 'FemalePresent', 'ApplicantEthnicity' = 'EthnicityBinary', 'ApplicantAge' = 'AgeGroup',
                                              'EthnicMinorityPanelistPresent' = 'EthnicMinorityPresent', 'ApplicantNationality' = 'NationalityBinary'))
format_table(m3.7b_rename)

save(list = ls(pattern = "m3\\.*"), file = "models/m3.RData")

################ Model 4: Including language features ################

lfeatures <- read.table('~ruoyun.hui/Documents/stat_modelling/tables/Score_and_LingFeatures.AllSections.tsv', sep = '\t', header = TRUE)[, -3]
proj_lfeatures <- lfeatures %>%
    distinct() %>%
    group_by(AnonPROJ) %>%
    select(-AnonREV) %>%
    summarise_all(mean)

unknowns <- c(NA, 'Unknown', 'unknown')
filtered_ranking <- ranking %>%
    filter(!is.na(RankQuantile) & !(Gender %in% unknowns) & !(EthnicityBinary %in% unknowns) &
           !(NationalityBinary %in% unknowns) &
           MeetingType %in% c('Proposal', 'Interview')) %>%
    mutate(MeetingType = relevel(factor(MeetingType, ordered = FALSE), ref = 'Proposal'),
           ApplicantAge = ifelse(AgeRange %in% c('25 and under', '26-35'), '35 and under',
                          ifelse(AgeRange %in% c('36-45', '46-55'), '36-55',
                          ifelse(is.na(AgeRange), NA, '56+'))),
           PanelChairAge = ifelse(ChairAgeRange %in% c('25 and under', '26-35'), '35 and under',
                      ifelse(ChairAgeRange %in% c('36-45', '46-55'), '36-55',
                      ifelse(is.na(ChairAgeRange), NA, '56+'))),
           FemalePanelistPresent = FemaleProportion > 0,
           EthnicMinorityPanelistPresent = NonWhiteProportion > 0) %>%
    merge(proj_lfeatures, by = 'AnonPROJ', all.x = TRUE)

interview <- filtered_ranking %>% filter(MeetingType == 'Interview') ## 2,327 rows
proposal <- filtered_ranking %>% filter(MeetingType == 'Proposal') ## 11,364 rows

m4a.1_rename <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex + ApplicantEthnicityBinary + ApplicantAge + ApplicantNationality +
                 SentenceMean.stanza + TotalWordCount + Achievement + Agentic + Ability + Research + StandoutAdj + Masculine + Feminine, data = proposal %>%
            rename('ApplicantSex' = 'Gender', 'ApplicantEthnicityBinary' = 'EthnicityBinary', 'ApplicantNationality' = 'NationalityBinary'))
summary(m4a.1_rename)

m4b.1_rename <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex + ApplicantEthnicityBinary + ApplicantAge + ApplicantNationality +
                 SentenceMean.stanza + TotalWordCount + Achievement + Agentic + Ability + Research + StandoutAdj + Masculine + Feminine, data = interview %>%
            rename('ApplicantSex' = 'Gender', 'ApplicantEthnicityBinary' = 'EthnicityBinary', 'ApplicantNationality' = 'NationalityBinary'))
summary(m4b.1_rename)

m4a.2_rename <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex * FemalePanelistPresent + ApplicantEthnicityBinary * EthnicMinorityPanelistPresent + ApplicantAge + ApplicantNationality +
                 SentenceMean.stanza + TotalWordCount + Achievement + Agentic + Ability + Research + StandoutAdj + Masculine + Feminine, data = proposal %>%
            rename('ApplicantSex' = 'Gender', 'ApplicantEthnicityBinary' = 'EthnicityBinary', 'ApplicantNationality' = 'NationalityBinary'))
summary(m4a.2_rename)
format_table(m4a.2_rename)

m4b.2_rename <- lm(RankQuantile ~ ScoreQuantile + ScoreMax + ScoreMin + ApplicantSex * FemalePanelistPresent + ApplicantEthnicityBinary * EthnicMinorityPanelistPresent + ApplicantAge + ApplicantNationality +
                 SentenceMean.stanza + TotalWordCount + Achievement + Agentic + Ability + Research + StandoutAdj + Masculine + Feminine, data = interview %>%
            rename('ApplicantSex' = 'Gender', 'ApplicantEthnicityBinary' = 'EthnicityBinary', 'ApplicantNationality' = 'NationalityBinary'))
summary(m4b.2_rename)
format_table(m4b.2_rename)

save(list = ls(pattern = "m4\\.*"), file = "models/m4.RData")
