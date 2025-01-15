library(readxl)
library(reshape2)
library(dplyr)

################ Loading datasheets #################

grant <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Grants")
investigator <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Investigators")
fellow_PI <- investigator %>% filter(Role %in% c('Principal Investigator', 'Fellow')) %>%
    add_count(AnonPROJ) %>%
    filter(n == 1)

reviewers <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Reviewers")
reviewer_scores <- reviewers %>%
    select(AnonPROJ, AnonREV, OverallAssessmentScore) %>%
    group_by(AnonPROJ) %>%
    mutate(ScoreMean = mean(OverallAssessmentScore), ScoreSD = sd(OverallAssessmentScore),
           ScoreMax = max(OverallAssessmentScore), ScoreMin = min(OverallAssessmentScore),
           ScoreRange = max(OverallAssessmentScore) - min(OverallAssessmentScore)) %>%
    select(-c(AnonREV, OverallAssessmentScore)) %>%
    distinct()

meeting <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "GrantMeeting")
meeting <- meeting %>% filter(!is.na(RankOrder)) %>%
    group_by(AnonLST) %>% 
    mutate(BinaryOutcomeList = all(RankOrder %in% c(0, 1)))

## 474 out of 1831 lists has binary outcomes;
## at projects level this is 2449 out of 17607 rows

## But sometimes (e.g. LST-0903) there is a mixture of numerical ranking (1, 2, 3..)
## and 0 for failed applications. Need recoding the 0 to the buttom rank before
## calculating quantiles

meeting[meeting$RankOrder == 0, "RankOrder"] <- 999

meeting <- meeting %>%
    group_by(AnonLST) %>%
    mutate(RankQuantile = ifelse(BinaryOutcomeList, NA, percent_rank(RankOrder)),
           ProperRankList = n() == length(unique(RankOrder))) %>%
    group_by(AnonPROJ) %>%
    mutate(FinalMeeting = MeetingOrder == max(MeetingOrder))

## The same meeting can be final for some projects but not for others - 
## hence I don't think it's a good filtering criteria
## 14281 out of 17607 records are from lists properly ranked (with numerical orders
## and not binary (0, 1))

meeting <- meeting %>%
    merge(reviewer_scores, by = 'AnonPROJ', all.x = TRUE) %>%
    group_by(AnonLST) %>% mutate(ScoreQuantile = percent_rank(ScoreMean))

## Some applications went through multiple meetings. They are considered and ranked alongside
## a different collection of other applications each time. For now we retain the ranking from
## each meeting.

panel <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "MeetingParticipants")
panel$AgeRangeOrd <- ordered(panel$AgeRange, exclude = 'Unknown')

## Summarising panel-level features from individual-level ones

panel_features <- panel %>% filter(AttendedInd | AttendedStatus == 'Attendance Accepted') %>%
    group_by(AnonMTG) %>%
    mutate(PanelSize = n(),
           ChairGender = ifelse('Chair' %in% MeetingRole, Gender[MeetingRole == 'Chair'], NA),
           ChairEthnicityBinary = ifelse('Chair' %in% MeetingRole, EthnicityBinary[MeetingRole == 'Chair'], NA),
           ChairEthnicityDetailed = ifelse('Chair' %in% MeetingRole, EthnicOriginGrouped[MeetingRole == 'Chair'], NA),
           ChairAgeRange = ifelse('Chair' %in% MeetingRole, AgeRange[MeetingRole == 'Chair'], NA),
           ChairNationality = ifelse('Chair' %in% MeetingRole, NationalityBinary[MeetingRole == 'Chair'], NA),
           ChairDisability = ifelse('Chair' %in% MeetingRole, DisabilityInformationGrouped[MeetingRole == 'Chair'], NA)) %>%
    mutate(FemaleProportion = sum(Gender == 'Female') / (sum(Gender == 'Female') + sum(Gender == 'Male')),
           NonWhiteProportion = sum(EthnicityBinary == 'Ethnic minority (excluding white minority)') /
               (sum(EthnicityBinary == 'Ethnic minority (excluding white minotiry)') + sum(EthnicityBinary == 'White')),
           AgeRangeMode = names(sort(-table(AgeRangeOrd, useNA = 'no')))[1],
           MaxAgeRange = max(AgeRangeOrd, na.rm = TRUE),
           NonUKProportion = sum(NationalityBinary == 'Non UK') / (sum(NationalityBinary == 'Non UK') + sum(NationalityBinary == 'UK')),
           DisabilityProportion = sum(DisabilityInformationGrouped == 'Known disability') / (sum(DisabilityInformationGrouped == 'No known disability') + sum(DisabilityInformationGrouped == 'Known disability'))) %>%
    select(AnonMTG, PanelSize, ChairGender, ChairEthnicityBinary, ChairEthnicityDetailed, ChairAgeRange, ChairNationality,
           ChairDisability, FemaleProportion, NonWhiteProportion, AgeRangeMode, MaxAgeRange, NonUKProportion, DisabilityProportion) %>%
    distinct()

## The "TotalParticipants" column also counts participants that did not attend meeting
## Some participants have "AttendedInd" as False (0) but "AttendedStatus" as Attendance
## accepted -- assuming they still attended

ranking <- merge(meeting, fellow_PI, by = 'AnonPROJ', all.x = TRUE)
ranking <- merge(ranking, panel_features, by = 'AnonMTG', all.x = TRUE)
ranking <- merge(ranking, grant[, c('AnonPROJ', 'GrantCategory', 'Outline', 'Driver', 'AnonTHM', 'FYReceivedDate', 'FYDecisionDate', 'GrantOutcome')],
                 by = 'AnonPROJ', all.x = TRUE, suffixes = c('.meeting', '.grant'))

## 11.8% of the records have different themes (AnonTHM) in the Grants and GrantMeeting sheet?

write.table(ranking, file = 'data/merged_ranking_table.tsv', sep = '\t', quote = FALSE, row.names = FALSE)
