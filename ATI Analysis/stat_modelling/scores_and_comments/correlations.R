library(dplyr)
library(corrplot)
library(polycor)

## Calculate correlations between linguistic features, and between features and scores

all <- read.table('../tables/Score_and_LingFeatures.AllSections.tsv', sep = '\t', header = TRUE)
conclusion <- read.table('../tables/Score_and_LingFeatures.Conclusions.tsv', sep = '\t', header = TRUE)
applicant <- read.table('../tables/Score_and_LingFeatures.Applicant.tsv', sep = '\t', header = TRUE)
project <- read.table('../tables/Score_and_LingFeatures.Project.tsv', sep = '\t', header = TRUE)

plot_correlations <- function(df, save_mat = FALSE, mat_name = '') {
    cormat <- cor(df[, -c(1, 2)], use = 'pairwise.complete.obs')
    colnames(cormat)[1] <- 'ReviewerScore'
    colnames(cormat)[18] <- 'Sentiment.stanza'
    rownames(cormat) <- colnames(cormat)

    if(save_mat) {
        write.table(cormat, mat_name, sep = '\t', quote = FALSE)
    }
    par(mfrow = c(1, 2))
    corrplot(cormat, method = 'circle', type = 'upper', tl.col = 'black')
    corrplot(cormat, method = 'number', number.cex = 0.75, type = 'upper', tl.col = 'black')
}

pdf('plots/Score_and_LingFeatures.corr.all.pdf', width = 16.7, height = 7.05)
plot_correlations(all, TRUE, '../tables/Score_and_LingFeatures.corr.AllSections.tsv')
dev.off()

pdf('plots/Score_and_LingFeatures.corr.conclusion.pdf', width = 16.7, height = 7.05)
plot_correlations(conclusion, TRUE, '../tables/Score_and_LingFeatures.corr.Conclusion.tsv')
dev.off()

pdf('plots/Score_and_LingFeatures.corr.applicant.pdf', width = 16.7, height = 7.05)
plot_correlations(applicant, TRUE, '../tables/Score_and_LingFeatures.corr.Applicant.tsv')
dev.off()

pdf('plots/Score_and_LingFeatures.corr.project.pdf', width = 16.7, height = 7.05)
plot_correlations(project, TRUE, '../tables/Score_and_LingFeatures.corr.Project.tsv')
dev.off()

## Summarise the score's correlation with linguistic features 

get_score_cor <- function(suffix = '') {
    score_cor <- matrix(NA, nrow = ncol(all) - 3, ncol = 4)
    rownames(score_cor) <- colnames(all)[-c(1, 2, 3)]
    rownames(score_cor)[17] <- 'Sentiment.stanza'

    colnames(score_cor) <- c('All', 'Conclusion', 'Applicant', 'Project')

    for(i in 1:ncol(score_cor)) {
        score_cor[, i] <- cor(eval(parse(text = paste0(tolower(colnames(score_cor)[i]), suffix)))[, -c(1, 2)], use = "pairwise.complete.obs")[-1, 1]
    }
    return(score_cor)
}

pdf(file = "plots/Cor_with_Scores.pearson.pdf", height = 8.5, width = 8.5)
par(mfrow = c(1, 2))
score_cor <- get_score_cor()
corrplot(score_cor, method = 'circle', tl.col = 'black')
corrplot(score_cor, method = 'number', number.cex = 0.75, tl.col = 'black')
dev.off()

## Alternatively, if we treat the scores as ordinal not continuous variables...

score_polyserial <- score_cor
score_polyserial[, ] <- NA

for(i in 1:ncol(score_polyserial)) {
    d <- eval(parse(text = tolower(colnames(score_cor)[i])))
    d <- d %>% filter(OverallAssessmentScore %% 1 == 0)
    ordered_score <- as.ordered(pull(d, "OverallAssessmentScore"))
    for(j in 1:nrow(score_polyserial)) {
        score_polyserial[j, i] <- polyserial(pull(d, j + 3), ordered_score)
    }
}

pdf(file = "plots/Cor_with_Scores.polyserial.pdf", height = 8.5, width = 8.5)
par(mfrow = c(1, 2))
corrplot(score_polyserial, method = 'circle', tl.col = 'black')
corrplot(score_polyserial, method = 'number', number.cex = 0.75, tl.col = 'black')
dev.off()

## Compare fellowship vs research grant

grant <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Grants")

all.grant <- left_join(all, grant, by = 'AnonPROJ') %>%
    filter(GrantCategory == 'Research Grant') %>%
    select(c(1:20))
all.fellowship <- left_join(all, grant, by = 'AnonPROJ') %>%
    filter(GrantCategory == 'Fellowship') %>%
    select(c(1:20))

conclusion.grant <- left_join(conclusion, grant, by = 'AnonPROJ') %>%
    filter(GrantCategory == 'Research Grant') %>%
    select(c(1:20))
conclusion.fellowship <- left_join(conclusion, grant, by = 'AnonPROJ') %>%
    filter(GrantCategory == 'Fellowship') %>%
    select(c(1:20))

applicant.grant <- left_join(applicant, grant, by = 'AnonPROJ') %>%
    filter(GrantCategory == 'Research Grant') %>%
    select(c(1:20))
applicant.fellowship <- left_join(applicant, grant, by = 'AnonPROJ') %>%
    filter(GrantCategory == 'Fellowship') %>%
    select(c(1:20))

project.grant <- left_join(project, grant, by = 'AnonPROJ') %>%
    filter(GrantCategory == 'Research Grant') %>%
    select(c(1:20))
project.fellowship <- left_join(project, grant, by = 'AnonPROJ') %>%
    filter(GrantCategory == 'Fellowship') %>%
    select(c(1:20))

score_cor_fellowship <- get_score_cor(suffix = '.fellowship')
score_cor_grant <- get_score_cor(suffix = '.grant')

pdf(file = "plots/Cor_with_Scores.fellowship_vs_grant.pdf", height = 10, width = 10)
par(mfrow = c(1, 2))
corrplot(score_cor_fellowship, method = 'number', number.cex = 0.75, tl.col = 'black', main = 'Fellowship applications', mar = c(2, 0, 2, 0))
corrplot(score_cor_grant, method = 'number', number.cex = 0.75, tl.col = 'black', main = 'Research grant applications', mar = c(2, 0, 2, 0))
dev.off()
