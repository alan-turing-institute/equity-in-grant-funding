import pandas as pd
import numpy as np
import stanza

nlp = stanza.Pipeline(lang = 'en', processors = "tokenize, sentiment", download_method = None, tokenize_no_ssplit = True)
nlp_split = stanza.Pipeline(lang = 'en', processors = "tokenize, sentiment", download_method = None)

comments = pd.read_csv('/home/ruoyun.hui/ingressed_copy/DATA002_ReviewerCommentsMerged.csv', dtype = {'NOTE': str}, keep_default_na = False)

# This function returns a) result on the entire text (0, 1, 2); b) result on each sentence separately; c) average score of all sentences

def sentences_sentiment(text):
    if len(text) > 0:
        res = nlp(text).sentences[0].sentiment
        res_split = [s.sentiment for s in nlp_split(text).sentences]
        return([res, len(res_split), np.mean(res_split)])
    else:
        return([np.nan, 0, np.nan])
    
res = comments['NOTE'].apply(sentences_sentiment)
res = pd.DataFrame(res.tolist(), columns = ['combined_text', 'num_sentences', 'sentence_avg'])

output = pd.concat([comments.iloc[:, :5], res], axis = 1)
output.to_csv('ReviewerComments_perSection.stanza.tsv', sep = "\t", index = False)

