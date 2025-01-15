import pandas as pd
from nltk.sentiment.vader import SentimentIntensityAnalyzer

comments = pd.read_csv("/home/ruoyun.hui/ingressed_copy/DATA002_ReviewerCommentsMerged.tsv", sep = "\t", dtype = {'NOTE': str}, keep_default_na = False)

nlp = SentimentIntensityAnalyzer()

res = comments['NOTE'].apply(lambda x: nlp.polarity_scores(x))
res = pd.DataFrame(res.tolist())

output = pd.concat([comments.iloc[:, :5], res], axis = 1)
output.to_csv('ReviewerComments_perSection.nltk_vader.tsv', sep = "\t")

def combine_sections(input_df):
    combined_comments = input_df.groupby(by = ['AnonPROJ', 'AnonREV']).apply(lambda x: ' '.join(x.NOTE)).reset_index(name = 'NOTE')
    combined_res = combined_comments['NOTE'].apply(lambda x: nlp.polarity_scores(x))
    combined_res = pd.DataFrame(combined_res.tolist())
    combined_output = pd.concat([combined_comments.iloc[:, :2], combined_res], axis = 1)
    return(combined_output)
    
# Combine all sections of each review

combine_sections(comments).to_csv('ReviewerComments_perReview.nltk_vader.tsv', sep = "\t")

# Conclusions of each review

conclusions = comments[comments.NOTE_TYPE == 'Conclusions On Proposal']
combine_sections(conclusions).to_csv('Conclusions_perReview.nltk_vader.tsv', sep = "\t")

# Section on applicant of each review

applicant = comments[comments.NOTE_TYPE == 'Applicant']
combine_sections(applicant).to_csv('Applicant_perReview.nltk_vader.tsv', sep = "\t")

# Section on aspects not related to applicants of each review

nonapplicant = comments.loc[(comments.NOTE_TYPE != 'Applicant') & (comments.NOTE_TYPE != 'Conclusions On Proposal')]
combine_sections(nonapplicant).to_csv('NonApplicant_perReview.nltk_vader.tsv', sep = "\t")
