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

merged <- merge(grant, fellow_PI, by = 'AnonPROJ', suffixes = c('.proj', '.inv'))
merged <- merged %>% group_by(AnonPROJ) %>% mutate(PI_num = n()) %>% filter(PI_num == 1)
merged <- merge(merged, reviewer, by = 'AnonPROJ', suffixes = c('.inv', '.rev'))
merged <- merged %>% select(!c(PI_num, ConclusionsOnProposalDescription)) %>%
    filter(!is.na(OverallAssessmentScore))

## Function that plots the distribution of the scores by any variable

scores_by_feature <- function(df, feature, facet_var) {
    plot_df <- df %>%
        select(OverallAssessmentScore, !!sym(facet_var), !!sym(feature))
    uniq_features <- sort(unique(df[, feature]))
    legend_labels <- c()
    for(i in uniq_features) {
        legend_labels <- c(legend_labels, paste0(i, " (n=", sum(df[, feature] == i), ")"))
    }
    p <- ggplot(plot_df, aes(x = OverallAssessmentScore, fill = !!sym(feature))) +
        facet_wrap(as.formula(paste("~", facet_var))) +
        geom_bar(aes(y = after_stat(prop)), width = 0.7, position = 'dodge') +
        scale_x_continuous(name = "Reviewer scores", breaks = seq(1, 10)) +
        scale_fill_discrete(labels = legend_labels) + 
        labs(y = 'Proportion')
    return(p)
}

################ Defining variables of interest ################

project_vars <- c('GrantCategory', 'Outline', 'Driver', 'AnonTHM', 'GrantOutcome')
investigator_vars <- c('Gender.inv', 'AgeRange.inv', 'DisabilityInformationGrouped.inv',
                       'EthnicOriginGrouped.inv', 'EthnicityBinary.inv',
                       'NationalityBinary.inv', 'Country.inv')
reviewer_vars <- c('Gender.rev', 'AgeRange.rev', 'DisabilityInformationGrouped.rev',
                       'EthnicOriginGrouped.rev', 'EthnicityBinary.rev',
                       'NationalityBinary.rev', 'ReviewerSource', 'MultiDisc', 'Country')

################ Generating plots ################

for(v in investigator_vars) {
    pdf(paste0("plots/scores_by_applicant_char/", strsplit(v, "\\.")[[1]][1], ".pdf"),
        width = 12, height = 8)
    for(p in project_vars) {
        print(scores_by_feature(merged, v, p))
    }
    dev.off()
}

for(v in reviewer_vars) {
    pdf(paste0("plots/scores_by_reviewer_char/", strsplit(v, "\\.")[[1]][1], ".pdf"),
        width = 12, height = 8)
    for(p in project_vars) {
        print(scores_by_feature(merged, v, p))
    }
    dev.off()
}
