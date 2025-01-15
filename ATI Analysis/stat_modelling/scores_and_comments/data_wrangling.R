library(readxl)
library(dplyr)

## Reading data and combine reviewer scores and linguistic features of comments
## (LIWC output and sentiment analysis) in one table

liwc_dir <- "~/Documents/liwc/"
sentiment_dir <- "~/Documents/sentiment/"
reviewers <- read_excel("/data/PROJ2023.002-DATA001.xlsx", sheet = "Reviewers")
reviewer_scores <- reviewers %>% select(AnonPROJ, AnonREV, OverallAssessmentScore)

all_wc <- read.table(paste0(liwc_dir, 'allfields_wc.per_review.tsv'), sep = "\t", header = TRUE)
conclusion_wc <- read.table(paste0(liwc_dir, 'conclusions_wc.per_review.tsv'), sep = "\t", header = TRUE)
applicant_wc <- read.table(paste0(liwc_dir, 'applicant_wc.per_review.tsv'), sep = "\t", header = TRUE)
project_wc <- read.table(paste0(liwc_dir, 'nonapplicant_wc.per_review.tsv'), sep = "\t", header = TRUE)

section_stanza <- read.table(paste0(sentiment_dir, 'ReviewerComments_perSection.stanza.tsv'), sep = '\t', header = TRUE)
section_stanza <- section_stanza %>% filter(num_sentences > 0)

all_stanza <- section_stanza %>%
    group_by(AnonPROJ, AnonREV) %>%
    summarise(total_sentences = sum(num_sentences), section_avg = sum(num_sentences * sentence_avg) / total_sentences)

conclusion_stanza <- section_stanza %>%
    filter(NOTE_TYPE == 'Conclusions On Proposal') %>%
    group_by(AnonPROJ, AnonREV) %>%
    summarise(total_sentences = sum(num_sentences), section_avg = sum(num_sentences * sentence_avg) / total_sentences)

applicant_stanza <- section_stanza %>%
    filter(NOTE_TYPE == 'Applicant') %>%
    group_by(AnonPROJ, AnonREV) %>%
    summarise(total_sentences = sum(num_sentences), section_avg = sum(num_sentences * sentence_avg) / total_sentences)

project_stanza <- section_stanza %>%
    filter(!(NOTE_TYPE %in% c('Applicant', 'Conclusions On Proposal'))) %>%
    group_by(AnonPROJ, AnonREV) %>%
    summarise(total_sentences = sum(num_sentences), section_avg = sum(num_sentences * sentence_avg) / total_sentences)

all_vader <- read.table(paste0(sentiment_dir, 'ReviewerComments_perReview.nltk_vader.tsv'), sep = "\t", header = TRUE)
conclusion_vader <- read.table(paste0(sentiment_dir, 'Conclusions_perReview.nltk_vader.tsv'), sep = "\t", header = TRUE)
applicant_vader <- read.table(paste0(sentiment_dir, 'Applicant_perReview.nltk_vader.tsv'), sep = "\t", header = TRUE)
project_vader <- read.table(paste0(sentiment_dir, 'NonApplicant_perReview.nltk_vader.tsv'), sep = "\t", header = TRUE)
                               
all <- reviewer_scores %>%
    left_join(all_wc, by = c('AnonPROJ', 'AnonREV')) %>%
    left_join(all_vader, by = c('AnonPROJ', 'AnonREV')) %>%
    left_join(all_stanza, by = c('AnonPROJ', 'AnonREV'))

conclusion <- reviewer_scores %>%
    left_join(conclusion_wc, by = c('AnonPROJ', 'AnonREV')) %>%
    left_join(conclusion_vader, by = c('AnonPROJ', 'AnonREV')) %>%
    left_join(conclusion_stanza, by = c('AnonPROJ', 'AnonREV'))

applicant <- reviewer_scores %>%
    left_join(applicant_wc, by = c('AnonPROJ', 'AnonREV')) %>%
    left_join(applicant_vader, by = c('AnonPROJ', 'AnonREV')) %>%
    left_join(applicant_stanza, by = c('AnonPROJ', 'AnonREV'))

project <- reviewer_scores %>%
    left_join(project_wc, by = c('AnonPROJ', 'AnonREV')) %>%
    left_join(project_vader, by = c('AnonPROJ', 'AnonREV')) %>%
    left_join(project_stanza, by = c('AnonPROJ', 'AnonREV'))

arrange_cols <- function(df) {
    colnames(df)[c(11, 12)] <- c('NegEval.LIWC', 'PosEval.LIWC')
    colnames(df)[18:ncol(df)] <- c('Neg.VADER', 'Neu.VADER', 'Pos.VADER', 'Compound.VADER', 'TotalSentences', 'SentenceMean.stanza')
    return(df[, c(1:5, 22, 8:10, 13:16, 11, 12, 18:21, 23)])
}

all <- arrange_cols(all)
conclusion <- arrange_cols(conclusion)
applicant <- arrange_cols(applicant)
project <- arrange_cols(project)

write.table(all, '../tables/Score_and_LingFeatures.AllSections.tsv', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(conclusion, '../tables/Score_and_LingFeatures.Conclusions.tsv', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(applicant, '../tables/Score_and_LingFeatures.Applicant.tsv', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(project, '../tables/Score_and_LingFeatures.Project.tsv', sep = '\t', row.names = FALSE, quote = FALSE)
