library(readxl)
library(dplyr)

grants <- read_excel("/data/PROJ2023.002-DATA001.xlsx", sheet = "Grants")
reviewers <- read_excel("/data/PROJ2023.002-DATA001.xlsx", sheet = "Reviewers")
investigators <- read_excel("/data/PROJ2023.002-DATA001.xlsx", sheet = "Investigators")
comments <- read.csv("/home/ruoyun.hui/ingressed_copy/DATA002_ReviewerCommentsMerged.Col1-5.tsv", sep = "\t")
comments$RowID <- 1:nrow(comments)

wc <- read.csv("~/Documents/liwc/liwc_output/DATA002_ReviewerComments_masculine-feminine.wc.csv")
wc2 <- read.csv("~/Documents/liwc/liwc_output/DATA002_ReviewerComments_granteval.wc.csv")
colnames(wc)[6] <- "FM.Dic"
colnames(wc2)[6] <- "Grant.Dic"

wc_all <- cbind(wc2[, c(1, 3, 4, 5:13)], wc[, c('FM.Dic', 'Masculine', 'Feminine')])

comments_wc <- merge(comments, wc_all, by.x = "RowID", by.y = "Row.ID", all.x = FALSE, all.y = TRUE)
comments_wc <- comments_wc[comments_wc$WC > 0, ]
conclusions_wc <- comments_wc %>% filter(NOTE_TYPE == 'Conclusions On Proposal')
applicant_wc <- comments_wc %>% filter(NOTE_TYPE == 'Applicant')
nonapplicant_wc <- comments_wc %>% filter((NOTE_TYPE != 'Applicant') & (NOTE_TYPE != 'Conclusions On Proposal'))
## write.table(comments_wc, 'comments_with_wc.tsv', sep = "\t", row.names = FALSE, quote = FALSE)
## write.table(conclusions_wc, 'ConclusionsOnProposal_wc.tsv', sep = "\t", row.names = FALSE, quote = FALSE)
## write.table(applicant_wc, 'Applicant_wc.tsv', sep = "\t", row.names = FALSE, quote = FALSE)

## Pool all comments in each review

merge_by_wc <- function(df) {
    per_review <- df %>% group_by(AnonPROJ, AnonREV) %>%
        summarise(TotalWordCount = sum(WC),
                  WordsPerSentence = weighted.mean(WPS, WC),
                  Grant.Dic = weighted.mean(Grant.Dic, WC),
                  FM.Dic = weighted.mean(FM.Dic, WC),
                  Ability = weighted.mean(Ability, WC),
                  Achievement = weighted.mean(Achievement, WC),
                  Agentic = weighted.mean(Agentic, WC),
                  NegEval = weighted.mean(NegEval, WC),
                  PosEval = weighted.mean(PosEval, WC),
                  Research = weighted.mean(Research, WC),
                  StandoutAdj = weighted.mean(StandoutAdj, WC),
                  Masculine = weighted.mean(Masculine, WC),
                  Feminine = weighted.mean(Feminine, WC))
    return(per_review)
}

all_wc_perreview <- merge_by_wc(comments_wc)
conclusions_wc_perreview <- merge_by_wc(conclusions_wc)
applicant_wc_perreview <- merge_by_wc(applicant_wc)
nonapplicant_wc_perreview <- merge_by_wc(nonapplicant_wc)

write.table(all_wc_perreview, 'allfields_wc.per_review.tsv', sep = "\t", row.names = FALSE, quote = FALSE)
write.table(conclusions_wc_perreview, 'conclusions_wc.per_review.tsv', sep = "\t", row.names = FALSE, quote = FALSE)
write.table(applicant_wc_perreview, 'applicant_wc.per_review.tsv', sep = "\t", row.names = FALSE, quote = FALSE)
write.table(nonapplicant_wc_perreview, 'nonapplicant_wc.per_review.tsv', sep = "\t", row.names = FALSE, quote = FALSE)


## per_review <- merge(per_review, grants[, c(1, 18)], by = 'AnonPROJ', all.x = TRUE, all.y = FALSE)
## colnames(reviewers)[c(3:5, 7, 14:19)] <- paste0("Reviewer.", colnames(reviewers)[c(3:5, 7, 14:19)])
## per_review <- merge(per_review, reviewers[, -c(9:12)], by = c('AnonPROJ', 'AnonREV'), all.x = TRUE, all.y = FALSE)

one_PI_fellow <- investigators %>% filter(Role %in% c("Principal Investigator", "Fellow")) %>%
    count(AnonPROJ) %>% filter(n == 1)

## Correlations with reviewer scores
reviewer_scores <- reviewers %>% select(AnonPROJ, AnonREV, OverallAssessmentScore)
all_wc_score <- inner_join(all_wc_perreview, reviewer_scores, by = c('AnonPROJ', 'AnonREV'))
conclusion_wc_score <- inner_join(conclusions_wc_perreview, reviewer_scores, by = c('AnonPROJ', 'AnonREV'))
applicant_wc_score <- inner_join(applicant_wc_perreview, reviewer_scores, by = c('AnonPROJ', 'AnonREV'))
nonapplicant_wc_score <- inner_join(nonapplicant_wc_perreview, reviewer_scores, by = c('AnonPROJ', 'AnonREV'))

score_corcoef <- data.frame(matrix(NA, nrow = 13, ncol = 4))
rownames(score_corcoef) <- colnames(all_wc_perreview)[3:15]
colnames(score_corcoef) <- c('all', 'conclusion', 'applicant', 'non-applicant')

for(i in 3:15) {
    score_corcoef[i - 2, 1] <- cor(all_wc_score$OverallAssessmentScore, all_wc_score[i])
    score_corcoef[i - 2, 2] <- cor(conclusion_wc_score$OverallAssessmentScore, conclusion_wc_score[i])
    score_corcoef[i - 2, 3] <- cor(applicant_wc_score$OverallAssessmentScore, applicant_wc_score[i])
    score_corcoef[i - 2, 4] <- cor(nonapplicant_wc_score$OverallAssessmentScore, nonapplicant_wc_score[i])
}

## one_PI <- investigators %>% filter(Role == "Principal Investigator") %>%
##     inner_join(one_PI_apps, by = 'AnonPROJ') %>%
##     select(-c("n", "Role"))
## )

## colnames(one_PI)[3:15] <- paste0("PI.", colnames(one_PI)[3:15])

## per_review <- merge(per_review, one_PI, by = 'AnonPROJ', all.x = TRUE, all.y = FALSE)

## write.table(per_review, "PerReview_PI_Reviewer_WC.tsv", quote = FALSE, row.names = FALSE, sep = "\t")


