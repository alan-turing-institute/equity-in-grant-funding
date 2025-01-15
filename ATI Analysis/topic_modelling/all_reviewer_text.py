import pandas as pd
import re
import string
import nltk
from nltk.corpus import stopwords, wordnet
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.tokenize import sent_tokenize
import gensim
from gensim import corpora

comments = pd.read_csv('~/ingressed_copy/DATA002_ReviewerCommentsMerged.tsv', sep = '\t')
comments = comments.loc[~comments.NOTE.isna(), ]
comments_only = comments.NOTE

stop = set(stopwords.words('english'))
punct = set(string.punctuation)
exclude_words = stop.union(punct)
exclude_words = exclude_words.union(set(['', '``', "''"]))
lemma = WordNetLemmatizer()

def wordnet_pos(treebank_tag):
    if treebank_tag.startswith('J'):
        return wordnet.ADJ
    if treebank_tag.startswith('V'):
        return wordnet.VERB
    if treebank_tag.startswith('R'):
        return wordnet.ADV
    else:
        return wordnet.NOUN

def clean_pos(doc):
    pos_res = nltk.pos_tag(nltk.word_tokenize(doc))
    stop_free = [i for i in pos_res if i[0].lower() not in exclude_words]
    lemmatised = ' '.join([lemma.lemmatize(i[0], pos = wordnet_pos(i[1])) for i in stop_free])
    return(lemmatised)

def clean(doc):
    stop_free = ' '.join([i for i in doc.lower().split() if i not in stop])
    punc_free = ''.join([i for i in stop_free if i not in punct])
    lemmatised = ' '.join([lemma.lemmatize(word) for word in punc_free.split()])
    return(lemmatised)
    
doc_clean = [clean_pos(s.lower()).split() for doc in comments_only for s in sent_tokenize(doc)]

import pickle

with open('comments_lemmatised.pkl', 'wb') as f:
    pickle.dump(doc_clean, f)

from gensim.test.utils import datapath

dictionary = corpora.Dictionary(doc_clean)
doc_term_matrix = [dictionary.doc2bow(doc) for doc in doc_clean]

for n_topics in range(5, 16):
    lda_res = gensim.models.LdaMulticore(corpus = doc_term_matrix,
                                     id2word = dictionary,
                                     num_topics = n_topics,
                                     passes = 5,
                                     workers = 7)
    lda_res.save("lda_res/lda_res.{}topics".format(n_topics))

# Plot word cloud

from matplotlib import pyplot as plt
from wordcloud import WordCloud, STOPWORDS
import matplotlib.colors as mcolors
from matplotlib.backends.backend_pdf import PdfPages

colors = [color for name, color in mcolors.TABLEAU_COLORS.items()]

cloud = WordCloud(stopwords = stop,
                  background_color = 'white',
                  width = 500,
                  height = 360,
                  max_words = 15,
                  colormap = 'tab10',
                  color_func = lambda *args, **kwargs: colors[i],
                  prefer_horizontal = 1.0)

for n_topics in range(5, 16):
    res = gensim.models.LdaModel.load("lda_res/lda_res.{}topics".format(n_topics))
    topics = res.show_topics(num_topics = n_topics, formatted = False)

    with PdfPages('lda_res/wordcloud.{}topics.pdf'.format(n_topics)) as pdf:
        n_page = n_topics // 8 + 1
        for i_page in range(n_page):
            f, ax = plt.subplots(4, 2, figsize = (10, 20), sharex = True, sharey = True)
            for i, a in enumerate(ax.flatten()):
                i_topic = i + 8 * i_page
                if  i_topic < n_topics:
                    f.add_subplot(a)
                    topic_words = dict(topics[i_topic][1])
                    cloud.generate_from_frequencies(topic_words, max_font_size = 300)
                    plt.gca().imshow(cloud)
                    plt.gca().set_title('Topic {} of {}'.format(i_topic + 1, n_topics),
                                        fontdict = {'fontsize': 20}, pad = 20)
                else:
                    f.add_subplot(a)
                    plt.gca().set_visible(False)
                plt.gca().axis('off')

            plt.subplots_adjust(wspace = 0, hspace = 0)
            plt.axis('off')
            plt.margins(x = 0, y = 0)
            plt.tight_layout()
            pdf.savefig()
            plt.close()
