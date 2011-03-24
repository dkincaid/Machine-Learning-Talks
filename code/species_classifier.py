#!/usr/bin/env python

import csv
import numpy
from scikits.learn import svm
from scikits.learn.naive_bayes import GNB
from nltk.classify.naivebayes import NaiveBayesClassifier


def load_data():
    reader = csv.DictReader(open("../data/species_test.csv"), delimiter=",", quotechar="'")

    rows = [row for row in reader]
    
#    features = numpy.array([[row['name'], row['sex'], int(row['age']), float(row['weight'])] for row in rows if row['age']!=''])
#    targets = numpy.array([row['species'] for row in rows if row['age']!=''])

#    features = [ dict(name=row['name'], sex=row['sex'], age=int(row['age']), weight=float(row['weight'])) for row in rows if row['age']!='']
#    targets = [ row['species'] for row in rows if row['age']!='']

    features = [ dict(age=int(row['age']), weight=float(row['weight'])) for row in rows if row['age']!='']
    targets = [ row['species'] for row in rows if row['age']!='']

    return features,targets

def train_naive_bayes(features, targets):
    classifier = NaiveBayesClassifier.train((features,targets))
#    model = GNB()
#    model.fit(features, targets)

def train_svc(features, targets):
    model = svm.SVC(kernel='linear').fit(features,targets)



if __name__ == "__main__":
    features, targets = load_data()
    
    print (features,targets)
    print targets

    train_naive_bayes(features, targets)
