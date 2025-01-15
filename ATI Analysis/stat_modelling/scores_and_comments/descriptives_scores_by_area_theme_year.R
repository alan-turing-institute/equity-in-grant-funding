library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(cowplot)

################ Loading datasheets #################

grant <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Grants")
reviewers <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "Reviewers")
reviewer_scores <- reviewers %>%
    select(AnonPROJ, AnonREV, OverallAssessmentScore)

research_areas <- read_excel('/data/PROJ2023.002-DATA001.xlsx', sheet = "ResearchAreas")

## Note that some projects belong to multiple Research Area Groups,
## resulting in multiple rows.
## The RAFractions in some projects do not add up to 1. Excluding.

research_areas <- research_areas %>%
    select(AnonPROJ, AnonRAG, RAFraction) %>%
    filter(!is.na(AnonRAG)) %>%
    group_by(AnonPROJ) %>%
    filter(sum(RAFraction) == 1) %>%
    group_by(AnonPROJ, AnonRAG) %>%
    mutate(RAGFraction = sum(RAFraction)) %>%
    distinct(AnonPROJ, AnonRAG, RAGFraction)

grant <- merge(grant, research_areas, by = 'AnonPROJ')
grant <- merge(grant, reviewer_scores, by = 'AnonPROJ')
fellowship <- grant %>% filter(GrantCategory == 'Fellowship')
researchgrant <- grant %>% filter(GrantCategory == 'Research Grant')

################## Reviewer scores ##################

## The distribution of reviewer scores by theme

scores_by_theme <- function(df, plot_title = 'Distribution of reviewer scores') {
    group_var <- 'AnonTHM'
    plot_df <- df %>%
        select(OverallAssessmentScore, GrantOutcome, !!sym(group_var))
    facet_labels <- c()
    unique_groups <- unique(plot_df[, group_var])
    for(g in unique_groups) {
        facet_labels <- c(facet_labels,
                          paste0(g, " (n=", as.character(sum(plot_df[, group_var] == g)), ")"))
    }
    names(facet_labels) <- unique_groups
    ggplot(plot_df, aes(x = OverallAssessmentScore, fill = GrantOutcome)) +
        facet_wrap(as.formula(paste("~", group_var)), labeller = labeller(AnonTHM = facet_labels)) +
        geom_bar(aes(y = after_stat(prop)), width = 0.7) +
        scale_x_continuous(name = "Reviewer scores", breaks = seq(1, 10)) +
        labs(title = plot_title, y = 'Proportion')
        
}

scores_by_theme(grant, 'Distribution of reviewer scores by theme - all reviews')
ggsave('plots/scores_by_theme.all.pdf')
scores_by_theme(fellowship, 'Distribution of reviewer scores by theme - fellowship applications')
ggsave('plots/scores_by_theme.fellowship.pdf')
scores_by_theme(researchgrant, 'Distribution of reviewer scores by theme - research grant applications')
ggsave('plots/scores_by_theme.research_grants.pdf')

## The distribution of reviewer scores by research area groups

scores_by_RAG <- function(df, plot_title = 'Distribution of reviewer scores') {
    group_var <- 'AnonRAG'
    plot_df <- df %>%
        select(OverallAssessmentScore, GrantOutcome, !!sym(group_var), RAGFraction) %>%
        filter(!is.na(AnonRAG) & !is.na(OverallAssessmentScore))
        
    facet_labels <- c()
    unique_groups <- unique(plot_df[, group_var])
    for(g in unique_groups) {
        facet_labels <- c(facet_labels,
                          paste0(g, " (n=", as.character(sum(plot_df[plot_df[, group_var] == g, 'RAGFraction'])), ")"))
    }
    names(facet_labels) <- unique_groups

    count_df <- plot_df %>%
        group_by(AnonRAG, OverallAssessmentScore, GrantOutcome) %>%
        count(wt = RAGFraction) %>%
        group_by(AnonRAG) %>%
        mutate(RAGTotal = sum(n)) %>%
        ungroup() %>%
        mutate(Prop = n/RAGTotal)

    ggplot(count_df, aes(x = OverallAssessmentScore, y = Prop, fill = GrantOutcome)) +
        facet_wrap(as.formula(paste("~", group_var)), labeller = labeller(AnonRAG = facet_labels)) +
        geom_bar(stat = 'identity', width = 0.7) +
        scale_x_continuous(name = "Reviewer scores", breaks = seq(1, 10)) +
        labs(title = plot_title, y = 'Proportion')
        
}

scores_by_RAG(grant, 'Distribution of reviewer scores by Research Area Group - all reviews')
ggsave('plots/scores_by_RAG.all.pdf')
scores_by_RAG(fellowship, 'Distribution of reviewer scores by Research Area Group - fellowship applications')
ggsave('plots/scores_by_RAG.fellowship.pdf')
scores_by_RAG(researchgrant, 'Distribution of reviewer scores by Research Area Group - research grant applications')
ggsave('plots/scores_by_RAG.research_grants.pdf')

## The distribution of reviewer scores by year

scores_by_year <- function(df, plot_title = 'Distribution of reviewer scores') {
    group_var <- 'FYReceivedDate'
    plot_df <- df %>%
        select(OverallAssessmentScore, GrantOutcome, !!sym(group_var))
    facet_labels <- c()
    unique_groups <- unique(plot_df[, group_var])
    for(g in unique_groups) {
        facet_labels <- c(facet_labels,
                          paste0(g, " (n=", as.character(sum(plot_df[, group_var] == g)), ")"))
    }
    names(facet_labels) <- unique_groups
    ggplot(plot_df, aes(x = OverallAssessmentScore, fill = GrantOutcome)) +
        facet_wrap(as.formula(paste("~", group_var)), labeller = labeller(FYReceivedDate = facet_labels)) +
        geom_bar(aes(y = after_stat(prop)), width = 0.7) +
        scale_x_continuous(name = "Reviewer scores", breaks = seq(1, 10)) +
        labs(title = plot_title, y = 'Proportion')
        
}

scores_by_year(grant, 'Distribution of reviewer scores by year - all reviews')
ggsave('plots/scores_by_year.all.pdf')
scores_by_year(fellowship, 'Distribution of reviewer scores by year - fellowship applications')
ggsave('plots/scores_by_year.fellowship.pdf')
scores_by_year(researchgrant, 'Distribution of reviewer scores by year - research grant applications')
ggsave('plots/scores_by_year.research_grants.pdf')


