library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)
library(cowplot)

ranking <- read.table('data/merged_ranking_table.tsv', sep = "\t", header = TRUE)

## Try out different ways to filter

ranking_hist <- function(df) {
    ggplot(df, aes(x = RankQuantile)) +
        geom_histogram(binwidth = 0.05)
}

pdf('plots/rankquantiles.filter_lists.pdf', width = 8, height = 4.8)

## Without any filters
print(ranking_hist(ranking) + labs(title = 'Distribution of rank quantiles with no filtering'))

## After excluding binary lists, 14260 records have proper ranks (distinct positive rank for each project); 877 records come from lists with a mixture of proper ranks and 0 (recorded to 999 previously to get correct quantiles)

list <- ranking %>% filter(!is.na(RankQuantile)) %>%
    group_by(AnonLST) %>%
    add_count(name = 'ListSize')

## Histogram showing the distribution of the size of ranked lists
h <- ggplot(list, aes(x = ListSize)) +
    geom_bar() +
    labs(title = 'Distribution of the size of ranked lists')
print(h)

filter_list_size <- function(rank_df, min_list_size) {
    valid_list <- list %>% 
        filter(ListSize >= min_list_size)
    filtered_ranking <- rank_df %>% filter(AnonLST %in% valid_list$AnonLST)
    return(filtered_ranking)
}

## Filtering for larger lists reduces the spikes of rank quantiles at 0 and 1
for(min_list_size in 2:10) {
    filtered_ranking <- filter_list_size(ranking, min_list_size)
    print(ranking_hist(filtered_ranking) +
          labs(title = paste0(title = 'Distribution of rank quantiles among ranked lists containing at least ', min_list_size, ' projects')))
}

dev.off()

## A quick look at the overall correlation between ScoreQuantile and RankQuantile
filtered_ranking <- filter_list_size(ranking, 2)

ggplot(data = filtered_ranking, aes(x = ScoreQuantile, y = RankQuantile, color = Gender)) +
    geom_point(alpha = 0.5, stroke = 0, size = 2) +
    labs(title = paste0('Rank quantile vs. quantile of mean reviewer scores in lists containing at least ',
         min_list_size, ' projects'))
ggsave(file = 'plots/rankquantile_vs_scorequantile.pdf', height = 8, width = 10)

## Looking at the correlation coefficients
score_rank_cor <- function(df) {
    cor(df$"ScoreMean", df$"RankQuantile", use = 'pairwise.complete.obs')
}

score_rank_cor(ranking)
score_rank_cor(ranking %>% filter(Gender == 'Female'))
score_rank_cor(ranking %>% filter(Gender == 'Male'))

score_rank_cor(ranking %>% filter(EthnicityBinary == 'White'))
score_rank_cor(ranking %>% filter(EthnicityBinary != 'White'))

for(min_list_size in 2:10) {
    print(score_rank_cor(filter_list_size(ranking, min_list_size)))
}

## Plot the distribution of ranking quantiles by applicant characteristics

ranking_by_feature <- function(df, feature, density = TRUE) {
    plot_df <- df %>%
        select(RankQuantile, !!sym(feature))
    plot_df[plot_df[, feature] %in% c(NA, 'Unknown', 'unknown'), feature] <- 'Unknown/NA'
    uniq_features <- sort(unique(plot_df[[feature]]))
    legend_labels <- c()
    for(i in uniq_features) {
        legend_labels <- c(legend_labels, paste0(i, " (n=", sum(plot_df[, feature] == i), ")"))
    }

    p <- ggplot(plot_df, aes(x = RankQuantile, fill = !!sym(feature), group = !!sym(feature)))
    if(density) {
        p <- p + geom_histogram(aes(y = after_stat(density)), position = 'dodge', binwidth = 0.05) +
            scale_fill_discrete(labels = legend_labels) + 
            labs(y = 'Density', x = "Panel ranking quantile")
    } else {
        p <- p + geom_histogram(position = 'dodge', binwidth = 0.05) +
            scale_fill_discrete(labels = legend_labels) + 
            labs(y = 'Counts', x = "Panel ranking quantile")
    }
    return(p)
}

ranking_by_feature_facet <- function(df, feature, facet_var, density = TRUE) {
    plot_df <- df %>%
        select(RankQuantile, !!sym(facet_var), !!sym(feature))
    plot_df[plot_df[, feature] %in% c(NA, 'Unknown', 'unknown'), feature] <- 'Unknown/NA'
    uniq_features <- sort(unique(plot_df[[feature]]))
    legend_labels <- c()
    for(i in uniq_features) {
        legend_labels <- c(legend_labels, paste0(i, " (n=", sum(plot_df[, feature] == i), ")"))
    }
    p <- ggplot(plot_df, aes(x = RankQuantile, fill = !!sym(feature), group = !!sym(feature))) +
        facet_wrap(as.formula(paste("~", facet_var)))
    if(density) {
        p <- p + geom_histogram(aes(y = after_stat(density)), position = 'dodge', binwidth = 0.05) +
            labs(y = 'Density', x = "Panel ranking quantile") +
            scale_fill_discrete(labels = legend_labels)
    } else {
        p <- p + geom_histogram(position = 'dodge', binwidth = 0.05) +
            labs(y = 'Counts', x = "Panel ranking quantile") +
            scale_fill_discrete(labels = legend_labels)
    }
    return(p)
}

ranking_facet <- function(df, facet_var) {
    plot_df <- df %>%
        select(RankQuantile, !!sym(facet_var))

    p <- ggplot(plot_df, aes(x = RankQuantile)) +
        facet_wrap(as.formula(paste("~", facet_var))) +
        geom_histogram(binwidth = 0.05) +
        labs(y = 'Counts', x = "Panel ranking quantile")
    return(p)
}

investigator_vars <- c('Gender', 'AgeRange', 'DisabilityInformationGrouped',
                       'EthnicOriginGrouped', 'EthnicityBinary',
                       'NationalityBinary', 'Country')
project_vars <- c('GrantCategory', 'Outline', 'Driver', 'AnonTHM.grant', 'GrantOutcome')

for(min_list_size in c(2, 6, 10)) {
    filtered_ranking <- filter_list_size(ranking, min_list_size)
    for(v in investigator_vars) {
        pdf(paste0("plots/RankQuantiles_by_applicant_characteristics/min_listsize", min_list_size, "/rankquantiles_by_applicant_", v, ".pdf"),
            width = 12, height = 6)
        print(ranking_by_feature(filtered_ranking, v))
        print(ranking_by_feature(filtered_ranking, v, FALSE))
        for(p in project_vars) {
            print(ranking_by_feature_facet(filtered_ranking, v, p))
            print(ranking_by_feature_facet(filtered_ranking, v, p, FALSE))
        }
        dev.off()
    }

    pdf(paste0('plots/RankQuantiles_by_applicant_characteristics/min_listsize', min_list_size, '/rankquantiles.all.pdf'),
        width = 12, height = 6)
    print(ggplot(ranking, aes(x = RankQuantile)) +
          geom_histogram(binwidth = 0.05))
    for(p in project_vars) {
        print(ranking_facet(ranking, p))
    }
    dev.off()
}
