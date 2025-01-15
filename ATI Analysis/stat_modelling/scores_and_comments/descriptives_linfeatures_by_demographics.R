library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(cowplot)

################ Loading datasheets #################

grant <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Grants")
reviewer <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Reviewers")
investigator <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Investigators")

fellow <- investigator %>% filter(Role == 'Fellow')
fellow_PI <- investigator %>% filter(Role %in% c('Principal Investigator', 'Fellow'))
PI_coI <- investigator %>% filter(Role != 'Fellow')

lfeatures <- read.table('../tables/Score_and_LingFeatures.AllSections.tsv', sep = '\t', header = TRUE)

merged <- merge(grant, fellow_PI, by = 'AnonPROJ', suffixes = c('.proj', '.inv'))
merged <- merged %>% group_by(AnonPROJ) %>% mutate(PI_num = n()) %>% filter(PI_num == 1)
merged <- merge(merged, reviewer, by = 'AnonPROJ', suffixes = c('.inv', '.rev'))
merged <- merged %>% select(!c(PI_num, ConclusionsOnProposalDescription)) %>%
    filter(!is.na(OverallAssessmentScore))
merged <- left_join(merged, distinct(lfeatures), by = c('AnonPROJ', 'AnonREV'))
                    
## Function that plots the distribution of the scores by any variable

comments_by_feature <- function(df, comment_var, feature, facet_var) {
    plot_df <- df %>%
        select(!!sym(comment_var), !!sym(facet_var), !!sym(feature))
    uniq_features <- sort(unique(df[, feature]))
    legend_labels <- c()
    for(i in uniq_features) {
        legend_labels <- c(legend_labels, paste0(i, " (n=", sum(df[, feature] == i), ")"))
    }
    p <- ggplot(plot_df, aes(x = !!sym(comment_var), fill = !!sym(feature))) +
        facet_wrap(as.formula(paste("~", facet_var))) +
        geom_histogram(aes(y = after_stat(density)), width = 0.7, position = 'dodge') +
        scale_x_continuous(name = comment_var, breaks = seq(1, 10)) +
        scale_fill_discrete(labels = legend_labels) + 
        labs(y = 'Density')
    p
}

################ Defining variables of interest ################

colnames(merged)[67] <- 'Sentiment.stanza'
comment_vars <- c('Ability', 'Achievement', 'Agentic', 'Research', 'StandoutAdj',
                  'Masculine', 'Feminine', 'Sentiment.stanza')
project_vars <- c('GrantCategory', 'Outline', 'Driver', 'AnonTHM', 'GrantOutcome')
investigator_vars <- c('Gender.inv', 'AgeRange.inv', 'DisabilityInformationGrouped.inv',
                       'EthnicOriginGrouped.inv', 'EthnicityBinary.inv',
                       'NationalityBinary.inv', 'Country.inv')
reviewer_vars <- c('Gender.rev', 'AgeRange.rev', 'DisabilityInformationGrouped.rev',
                       'EthnicOriginGrouped.rev', 'EthnicityBinary.rev',
                       'NationalityBinary.rev', 'ReviewerSource', 'MultiDisc', 'Country')

################ Generating plots ################

for(f in comment_vars) {
    f_lower = tolower(f)
    if(!dir.exists(paste0('plots/', f_lower, '/comment_by_applicant_char'))) {
        dir.create(paste0('plots/', f_lower, '/comment_by_applicant_char'), recursive = TRUE)
    }
    for(v in investigator_vars) {
        pdf(paste0('plots/', f_lower, '/comment_by_applicant_char/', strsplit(v, "\\.")[[1]][1], ".pdf"),
            width = 12, height = 8)
        for(p in project_vars) {
            print(comments_by_feature(merged, f, v, p))
        }
        dev.off()
    }
}

for(f in comment_vars) {
    f_lower = tolower(f)
    if(!dir.exists(paste0('plots/', f_lower, '/comment_by_reviewer_char'))) {
        dir.create(paste0('plots/', f_lower, '/comment_by_reviewer_char'), recursive = TRUE)
    }
    for(v in reviewer_vars) {
        pdf(paste0('plots/', f_lower, '/comment_by_reviewer_char/', strsplit(v, "\\.")[[1]][1], ".pdf"),
            width = 12, height = 8)
        for(p in project_vars) {
            print(comments_by_feature(merged, f, v, p))
        }
        dev.off()
    }
}


