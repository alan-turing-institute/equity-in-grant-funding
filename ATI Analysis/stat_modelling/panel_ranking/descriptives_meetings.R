library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)
library(cowplot)

ranking <- read.table('data/merged_ranking_table.tsv', sep = "\t", header = TRUE)

## Plot the features about panel meetings by year and research theme (according to the meeting tab)
## Infer the year of the meeting by FYDecisionDate of the projects - however often there are two years. Taking the earlier one (as there might be further meetings).

mode <- function(x) {
    names(which.max(table(x)))
}

feature_barplot <- function(df, feature, plot_title = '') {
    plot_df <- df %>% select(AnonMTG, !!sym(feature)) %>%
        distinct()
    ggplot(plot_df, aes(x = !!sym(feature), fill = !!sym(feature),
                        label = scales::percent(prop.table(stat(count))))) +
        geom_bar() +
        geom_text(stat = 'count', vjust = -0.5) + 
        labs(y = 'counts', x = feature) +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
}


feature_histogram <- function(df, feature, plot_title = '') {
    plot_df <- df %>% select(AnonMTG, !!sym(feature)) %>%
        distinct()
    ggplot(plot_df, aes(x = !!sym(feature))) +
        geom_histogram() +
        labs(y = 'counts', x = feature) +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
}
    
feature_lineplot_by_year <- function(df, feature, plot_title = '') {
    plot_df <- df %>% select(AnonMTG, !!sym(feature), FYDecisionDate) %>%
        group_by(AnonMTG) %>%
        mutate(MeetingDate = min(FYDecisionDate)) %>%
        select(-FYDecisionDate) %>% 
        distinct() %>%
        ungroup() %>%
        add_count(MeetingDate, name = 'MeetingPerYear') %>%
        add_count(MeetingDate, !!sym(feature), name = 'FeatureCount') %>%
        mutate(FeatureProp = FeatureCount / MeetingPerYear) %>%
        select(-AnonMTG) %>%
        distinct() ## %>%
        ## filter(!is.na(!!sym(feature)))

    ggplot(plot_df, aes(x = MeetingDate, y = FeatureProp, group = !!sym(feature), fill = !!sym(feature))) +
        geom_line(aes(color = !!sym(feature))) +
        geom_point(aes(color = !!sym(feature))) +
        labs(y = 'proportion', x = 'year') +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
}

feature_boxplot_by_year <- function(df, feature, plot_title = '') {
    plot_df <- df %>% select(AnonMTG, !!sym(feature), FYDecisionDate) %>%
        group_by(AnonMTG) %>%
        mutate(MeetingDate = min(FYDecisionDate)) %>%
        select(-FYDecisionDate) %>% 
        distinct()

    ggplot(plot_df, aes(x = MeetingDate, y = !!sym(feature))) +
        geom_boxplot() +
        labs(y = 'proportion', x = 'year') +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
            
}

feature_count_barplot_by_theme <- function(df, feature, plot_title = '') {
    plot_df <- df %>% select(AnonMTG, !!sym(feature), AnonTHM.meeting) %>%
        distinct() %>%
        group_by(AnonTHM.meeting) %>%
        add_count() %>%
        mutate(AnonTHM_labels = paste0(AnonTHM.meeting, " (n=", as.character(n), ")"))
    
    ggplot(plot_df, aes(x = !!sym(feature), fill = !!sym(feature), )) +
        geom_bar() +
        facet_wrap(~AnonTHM_labels) +
        labs(y = 'counts', x = feature) +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
}

feature_proportion_barplot_by_theme <- function(df, feature, plot_title = '') {
    plot_df <- df %>% select(AnonMTG, !!sym(feature), AnonTHM.meeting) %>%
        distinct() %>%
        group_by(AnonTHM.meeting) %>%
        add_count() %>%
        mutate(AnonTHM_labels = paste0(AnonTHM.meeting, " (n=", as.character(n), ")"),
               Weight = 1/n)
    
    ggplot(plot_df, aes(x = !!sym(feature), fill = !!sym(feature))) +
        facet_wrap(~AnonTHM_labels) +
        geom_bar(aes(weight = Weight)) +
        labs(y = 'proportion', x = feature) +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
}

feature_histogram_by_theme <- function(df, feature, plot_title = '') {
    plot_df <- df %>% select(AnonMTG, !!sym(feature), AnonTHM.meeting) %>%
        distinct() %>%
        group_by(AnonTHM.meeting) %>%
        add_count() %>%
        mutate(AnonTHM_labels = paste0(AnonTHM.meeting, " (n=", as.character(n), ")"))
    
    ggplot(plot_df, aes(x = !!sym(feature))) +
        geom_histogram(aes(y = after_stat(density))) +
        facet_wrap(~AnonTHM_labels) +
        labs(y = 'density', x = feature) +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
}


ranking <- ranking %>% group_by(AnonMTG) %>%
    add_count(name = 'NumProjDiscussed') %>%
    mutate(FundProportion = sum(Decision == 'Fund') / NumProjDiscussed,
           FundableProportion = sum(Decision %in% c('Fund', 'Fundable', 'Advance to next stage') /
                                    NumProjDiscussed),
           NumList = length(unique(AnonLST)))

meeting_vars <- c('ChairGender', 'ChairEthnicityBinary', 'ChairEthnicityDetailed', 'ChairAgeRange',
                  'ChairDisability', 'ChairNationality', 'FemaleProportion', 'NonWhiteProportion',
                  'AgeRangeMode', 'MaxAgeRange', 'NonUKProportion', 'DisabilityProportion',
                  'FundProportion', 'FundableProportion', 'NumProjDiscussed', 'PanelSize',
                  'NumList')

pdf('plots/meeting_features.all.pdf', width = 8, height = 6)
for(v in meeting_vars) {
    if(is.character(ranking[[v]])) {
        print(feature_barplot(ranking, v))
    } else {
        print(feature_histogram(ranking, v))
    }
}
dev.off()    

pdf('plots/meeting_features.by_year.pdf', width = 8, height = 5)
for(v in meeting_vars) {
    if(is.character(ranking[[v]])) {
        print(feature_lineplot_by_year(ranking, v, paste0("Changes in ", v, " through time")))
    } else {
        print(feature_boxplot_by_year(ranking, v, paste0("Changes in ", v, " through time")))
    }
}
dev.off()    

pdf('plots/meeting_features.by_theme.pdf', width = 12, height = 10)
for(v in meeting_vars) {
    if(is.character(ranking[[v]])) {
        print(feature_proportion_barplot_by_theme(ranking, v, paste0("Variations in ", v, " between research themes")))
    } else {
        print(feature_histogram_by_theme(ranking, v, paste0("Variations in ", v, " between research themes")))
    }
}
dev.off()    
