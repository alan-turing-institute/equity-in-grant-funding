library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(cowplot)

input_table <- '/data/PROJ2023.002-DATA001.xlsx'
grant <- read_excel(input_table, sheet = "Grants")
investigator <- read_excel(input_table, sheet = "Investigators") %>%
    rename('Sex' = 'Gender') %>%
    mutate(EthnicityBinary = ifelse(EthnicityBinary == "Ethnic minority (excluding white minority)",
                                    "Ethnic minority", EthnicityBinary))
reviewer <- read_excel(input_table, sheet = "Reviewers") %>%
    rename('Sex' = 'Gender') %>%
    mutate(EthnicityBinary = ifelse(EthnicityBinary == "Ethnic minority (excluding white minority)",
                                    "Ethnic minority", EthnicityBinary))
meeting <- read_excel(input_table, sheet = "GrantMeeting")
panelist <- read_excel(input_table, sheet = "MeetingParticipants") %>%
    rename('Sex' = 'Gender') %>%
    mutate(EthnicityBinary = ifelse(EthnicityBinary == "Ethnic minority (excluding white minority)",
                                    "Ethnic minority", EthnicityBinary))
researcharea <- read_excel(input_table, sheet = "ResearchAreas") 

grant_PI <- merge(grant, investigator %>% filter(Role %in% c('Fellow', 'Principal Investigator')),
                  by = 'AnonPROJ', all.x = TRUE, all.y = FALSE)
grant_reviewer <- merge(grant, reviewer, by = 'AnonPROJ', all.x = TRUE, all.y = FALSE)
grant_meeting <- merge(grant, meeting %>% filter(MeetingType %in% c('Proposal', 'Interview')),
                                               by = 'AnonPROJ', all.x = TRUE, all.y = FALSE)

feature_barplot <- function(df, group_var, feature, plot_title = '') {
    plot_df <- df %>% select(!!sym(group_var), !!sym(feature)) %>%
        distinct()
    ggplot(plot_df, aes(x = !!sym(feature), fill = !!sym(feature),
                        label = scales::percent(prop.table(stat(count)), accuracy = 0.1))) +
        geom_bar() +
        geom_text(stat = 'count', vjust = -0.5, size = 4) + 
        labs(y = 'Counts', x = '') +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9),
              plot.margin = margin(t = 5, r = 5, b = 8, l = 8)) +
        ggtitle(plot_title)
}

plot_demographics <- function(df, ID_col, save_output = FALSE, savename = 'demographics') {
    
    p1 <- feature_barplot(df, ID_col, 'Sex', "Sex") + theme(legend.position = "none")
    p2 <- feature_barplot(df, ID_col, 'EthnicityBinary', 'Ethnicity (binary)') + theme(legend.position = "none")
    p3 <- feature_barplot(df, ID_col, 'EthnicOriginGrouped', 'Ethnicity (detailed)') + theme(legend.position = "none")
    p4 <- feature_barplot(df, ID_col, 'AgeRange', 'Age range') + theme(legend.position = "none")
    p5 <- feature_barplot(df, ID_col, 'NationalityBinary', 'Nationality (binary)') + theme(legend.position = "none")
    p6 <- feature_barplot(df, ID_col, 'DisabilityInformationGrouped', 'Disability information') + theme(legend.position = "none")

    plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3)
    
    if(save_output) {
        ggsave(paste0("plots/", savename, '.pdf'), width = 15, height = 9)
        save(p1, p2, p3, p4, p5, p6, file = paste0('plots_data/', savename, '.RData'))
    }
}

plot_demographics(investigator, 'AnonINV', TRUE, 'applicant_demographics.all')
plot_demographics(investigator %>% filter(Role %in% c('Fellow', 'Principal Investigator')), 'AnonINV', TRUE, 'applicant_demographics.PI_fellow')
plot_demographics(investigator %>% filter(Role == 'Fellow'), 'AnonINV', TRUE, 'applicant_demographics.fellow')
plot_demographics(investigator %>% filter(Role == 'Principal Investigator'), 'AnonINV', TRUE, 'applicant_demographics.PI')

grant_reviewer %>% filter(!is.na(AnonREV)) %>% select(AnonPROJ, GrantCategory) %>% distinct() %>% count(GrantCategory)

plot_demographics(reviewer, 'AnonREV', TRUE, 'reviewer_demographics.all')

grant_meeting %>% filter(!is.na(AnonMTG)) %>% select(AnonPROJ, GrantCategory) %>% distinct() %>% count(GrantCategory)

plot_demographics(panelist %>% filter(AttendedInd), 'AnonPNL', TRUE, 'panelist_demographics.attended')
plot_demographics(panelist %>% filter(MeetingRole == 'Chair'), 'AnonPNL', TRUE, 'panelist_demographics.chair')

freq_by_year_outcome <- function(df, feature) {
    all_df <- df %>%
        select(FYReceivedDate, AnonINV, !!feature) %>%
        filter(!!sym(feature) != 'Unknown') %>%
        group_by(FYReceivedDate, AnonINV) %>%
        slice(1) %>%
        group_by(FYReceivedDate) %>%
        mutate(Weight = 1/n()) %>%
        group_by(!!!syms(c("FYReceivedDate", feature))) %>%
        count(wt = Weight) %>%
        mutate(Outcome = 'All applications')

    funded_df <- df %>%
        filter(GrantOutcome == 'Funded') %>%
        select(FYReceivedDate, AnonINV, !!feature) %>%
        filter(!!sym(feature) != 'Unknown') %>%
        group_by(FYReceivedDate, AnonINV) %>%
        slice(1) %>%
        group_by(FYReceivedDate) %>%
        mutate(Weight = 1/n()) %>%
        group_by(!!!syms(c("FYReceivedDate", feature))) %>%
        count(wt = Weight) %>%
        mutate(Outcome = 'Funded applications')

    ggplot(rbind(all_df, funded_df),
           aes(x = FYReceivedDate, y = n, group = interaction(!!sym(feature), Outcome))) +
        geom_line(aes(color = !!sym(feature), linetype = Outcome)) +
        geom_point(aes(color = !!sym(feature))) +
        labs(y = 'Proportion', x = 'Application year (received)') +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9),
              axis.title.x = element_text(margin = margin(t = 20)),
              plot.margin = margin(t = 25, r = 10, b = 0, l = 10))
}

pdf("plots/applicant_demographics_by_year.PI_fellow.pdf", width = 15, height = 5)
for(f in c('Sex', 'EthnicityBinary', 'EthnicOriginGrouped', 'AgeRange', 'NationalityBinary',
           'DisabilityInformationGrouped')) {
    p1 <- freq_by_year_outcome(grant_PI %>% filter(GrantCategory == 'Research Grant'), f)
    p2 <- freq_by_year_outcome(grant_PI %>% filter(GrantCategory == 'Fellowship'), f)

    print(plot_grid(p1, p2, ncol = 2, labels = c('Research Grant applications', 'Fellowship applications')))
    save(p1, p2, file = paste0('plots_data/applicant_by_year/', f, '.RData'))
}
dev.off()

feature_by_year <- function(df, ID_col, feature, plot_title = '') {
    plot_df <- df %>%
        select(Year, !!sym(ID_col), !!sym(feature)) %>%
        filter(!!sym(feature) != 'Unknown') %>%
        distinct() %>%
        group_by(Year) %>%
        mutate(Weight = 1/n()) %>%
        group_by(Year, !!sym(feature)) %>%
        count(wt = Weight, name = 'FeatureProp')

    ggplot(plot_df, aes(x = Year, y = FeatureProp, group = !!sym(feature), fill = !!sym(feature))) +
        geom_line(aes(color = !!sym(feature))) +
        geom_point(aes(color = !!sym(feature))) +
        labs(y = 'Proportion', x = 'Year') +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
}

reviewer_demographics_by_year <- function(df, save_output = FALSE, savename = 'demographics') {

    p1 <- feature_by_year(df, 'AnonREV', 'Sex', "Sex")
    p2 <- feature_by_year(df, 'AnonREV', 'EthnicityBinary', 'Ethnicity (binary)') 
    p3 <- feature_by_year(df, 'AnonREV', 'EthnicOriginGrouped', 'Ethnicity (detailed)')
    p4 <- feature_by_year(df, 'AnonREV', 'AgeRange', 'Age range')
    p5 <- feature_by_year(df, 'AnonREV', 'NationalityBinary', 'Nationality (binary)') 
    p6 <- feature_by_year(df, 'AnonREV', 'DisabilityInformationGrouped', 'Disability information')

    plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3)
    
    if(save_output) {
        ggsave(paste0("plots/", savename, '.pdf'), width = 15, height = 9)
        save(p1, p2, p3, p4, p5, p6, file = paste0('plots_data/', savename, '.RData'))
    }
}

reviewer_demographics_by_year(grant_reviewer %>% mutate(Year = FYReceivedDate), TRUE, 'reviewer_demographics_by_year')

panelist_demographics_by_year <- function(df, save_output = FALSE, savename = 'demographics') {

    p1 <- feature_by_year(df, 'AnonPNL', 'Sex', "Sex")
    p2 <- feature_by_year(df, 'AnonPNL', 'EthnicityBinary', 'Ethnicity (binary)') 
    p3 <- feature_by_year(df, 'AnonPNL', 'EthnicOriginGrouped', 'Ethnicity (detailed)')
    p4 <- feature_by_year(df, 'AnonPNL', 'AgeRange', 'Age range')
    p5 <- feature_by_year(df, 'AnonPNL', 'NationalityBinary', 'Nationality (binary)') 
    p6 <- feature_by_year(df, 'AnonPNL', 'DisabilityInformationGrouped', 'Disability information')

    plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3)
    
    if(save_output) {
        ggsave(paste0("plots/", savename, '.pdf'), width = 15, height = 9)
        save(p1, p2, p3, p4, p5, p6, file = paste0('plots_data/', savename, '.RData'))
    }
}

grant_meeting <- grant_meeting %>%
    group_by(AnonMTG) %>%
    mutate(Year = min(FYDecisionDate))

grant_meeting_panelist <- merge(grant_meeting, panelist, by = 'AnonMTG', all.x = TRUE)

panelist_demographics_by_year(grant_meeting_panelist, TRUE, savename = 'panelist_demographics_by_year.all')
panelist_demographics_by_year(grant_meeting_panelist %>% filter(AttendedInd), TRUE, savename = 'panelist_demographics_by_year.attended')
panelist_demographics_by_year(grant_meeting_panelist %>% filter(MeetingRole == 'Chair'), TRUE, savename = 'panelist_demographics_by_year.chair') 

feature_proportion_barplot_by_theme <- function(df, ID_col, feature, plot_title = '') {
    plot_df <- df %>% select(!!sym(ID_col), !!sym(feature), AnonTHM) %>%
        filter(!is.na(!!sym(feature)) & !!sym(feature) != 'Unknown') %>%
        distinct() %>%
        group_by(AnonTHM) %>%
        add_count() %>%
        mutate(AnonTHM_labels = paste0(AnonTHM, " (n=", as.character(n), ")"),
               Weight = 1/n)
    
    ggplot(plot_df, aes(x = !!sym(feature), fill = !!sym(feature))) +
        geom_bar(aes(weight = Weight)) +
        facet_wrap(~AnonTHM_labels) +
        labs(y = 'Proportion', x = feature) +
        theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust = 0.9)) +
        ggtitle(plot_title)
}

pdf("plots/applicant_demographics_by_theme.PI_fellow.pdf", width = 12.5, height = 7)

for(f in c('Sex', 'EthnicityBinary', 'EthnicOriginGrouped', 'AgeRange', 'NationalityBinary',
           'DisabilityInformationGrouped')) {
    p <- feature_proportion_barplot_by_theme(grant_PI, 'AnonINV', f, plot_title = '')
    print(p)
    save(p, file = paste0('plots_data/applicant_by_theme/', f, '.RData'))
}
dev.off()


