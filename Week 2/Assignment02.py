#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 30 12:28:14 2025

@author: lipikabania
"""

print("Lipika Baniya", "CMIS 527 – 73D", "assignment-number(01)")
print("I Lipika Baniya certify that I did this work on assignment 01 by myself")

# import OS 
import os

# change working directory to the specific folder
os.chdir(os.path.expanduser("~") + "/Documents/ISA/Week 02/") #setting directory 

# import required libraries
import urllib
import urllib.request
import os.path
import sys
from IPython.display import HTML

# link to the dataset
avURL = "http://datadrivensecurity.info/book/ch03/data/reputation.data"

# download the file locally
avRep = os.path.expanduser("~/Documents/ISA/Week 02/reputation.data")

# if does not exist locally download thropugh the url using urllib library
if not os.path.isfile(avRep) :
    urllib.urlretrieve(avURL, filename=avRep)
    
# import pandas for dataframe
import pandas as pd

# read data into Pandas DataFrame(av) using # as the delimeter
av = pd.read_csv(avRep, sep="#")

# rename columns for better readability & display data
av.columns = ["IP", "Reliability", "Risk", "Type", "Country", "Locale", "Coords", "x"]
print(av)

# display first 10 line of dataframe as formatted HTML
av.head().to_csv(sys.stdout)
HTML(av.head(10).to_html())

# display descriptive statitics for "Reliability" and "Risk"
print(av['Reliability'].describe())
print(av['Risk'].describe())

# frequency count for Categorical Columns
def factor_col(col):
    factor = pd.Categorical(col) #convert column into a Pandas Categorical type
    return pd.Series(factor).value_counts(sort=True).reindex(factor.categories) #Convert categorical value into Pandas Series, .value_counts(sort=True) to count occurances of each category in the column; 
    #reindex ensures if some categories have a count of zero (i.e., they are present in the dataset definition but not in the actual data), they still appear in the output. 

# create a Pandas Series from the column of av; .value_counts(sort=True) to count occurences; sort=True to sort in descending order
rel_ct = pd.Series(av['Reliability']).value_counts(sort=True) 
risk_ct = pd.Series(av['Risk']).value_counts(sort=True)
type_ct = pd.Series(av['Type']).value_counts(sort=True)
country_ct = pd.Series(av['Country']).value_counts(sort=True)

# print factor_col() to count unique values for Reliability, Risk, Type, and Country
print(factor_col(av['Reliability']))

print("Risk Counts:")
print(factor_col(av['Risk']))

print("Type Counts:")
print(factor_col(av['Type']).head(n=10)) #only first 10 rows

print("Country Counts:")
print(factor_col(av['Country']).head(n=10)) #only first 10 rows

# import plt from matplotlin.pyplot
import matplotlib.pyplot as plt

# sort by country
country_ct = pd.value_counts(av['Country'])

# create first figure for country summary
plt.figure(figsize=(8, 5)) #create a new figure and set size of the figure
plt.axes(frameon=0)  # Reduce chart junk - remove extra borders
country_ct[:20].plot(kind='bar', rot=0, title="Summary By Country") #top 20 countries in the dataset
plt.grid(False) #Remove gridline to simplify visualization
plt.show()  # Show the first plot before creating the next one

# create second figure for reliability summary
plt.figure(figsize=(8, 5)) #Create figure with size 
plt.axes(frameon=0)  # Reduce chart junk - remove extra borders
factor_col(av['Reliability']).plot(kind='bar', rot=0, title="Summary By Reliability") #Create a bar plot
plt.grid(False)
plt.show()  # Show the second plot

# create third figure for risk summary
plt.figure(figsize=(8, 5)) #Create figure with size 
plt.axes(frameon=0)  # Reduce chart junk - remove extra borders
factor_col(av['Risk']).plot(kind='bar', rot=0, title="Summary By Risk") #Create a bar plot
plt.grid(False)
plt.show()  # Show the second plot


# extract the top 10 most prevalent countries
top10 = pd.Series(av['Country']).value_counts(sort=True)[0:9]

# Calculate the percentage for each of the top 10
top10_percentage = top10 / len(av)  
print("Top 10 percentage countries", top10_percentage)

# Bar chart for top 10 countries (by percentage)
plt.figure(figsize=(10, 6))
top10_percentage.plot(kind='bar', color='skyblue')

plt.title("Top 10 Most Prevalent Countries (Percentage)")
plt.xlabel("Country")
plt.ylabel("Percentage")
plt.xticks(rotation=45)
plt.grid(axis='y', linestyle='--', alpha=0.7)
plt.tight_layout()
plt.show()

from matplotlib import cm # Import cm for basic colors for matplotlib
from numpy import arange # Import arange to modify axes display

# graphical view of contingency table (swapping risk/reliability)
xtab = pd.crosstab(av['Risk'], av['Reliability'])
print(xtab)

# import sns for visualization
import seaborn as sns
sns.heatmap(xtab, cmap="Greens", annot=True, fmt="d")
plt.show()

# create new column as a copy of Type column
av['newtype'] = av['Type']

# Replace rows where 'newtype' contains ";" (indicating multiple categories) with "Multiples"
# This groups multi-type entries under one label for easier analysis.
av[av['newtype'].str.contains(";", na=False)] = "Multiples"

# setup new crosstab structures
typ = av['newtype'] # 'newtype' column (after modification)
rel = av['Reliability'] # 'Reliability' column (risk trust level)
rsk = av['Risk'] # 'Risk' column (risk severity level)

# compute crosstab making it split on the
# new type column
xtab = pd.crosstab(typ, [ rel, rsk ],
rownames=['typ'], colnames=['rel', 'rsk'])

# the  print statement shows a huge text
# representation of the contingency table. 
print(xtab.to_string()) #output not shown

# Plot three-way risk/reliability/type contigency table bar chart
xtab.plot(kind='bar',legend=False,
title="Risk ~ Reliabilty | Type").grid(False)


# filter out all "Scanning Hosts"
rrt_df = av[av['newtype'] != "Scanning Host"]
typ = rrt_df['newtype']
rel = rrt_df['Reliability']
rsk = rrt_df['Risk']
xtab = pd.crosstab(typ, [ rel, rsk ],
rownames=['typ'], colnames=['rel', 'rsk'])
xtab.plot(kind='bar',legend=False,
title="Risk ~ Reliabilty | Type").grid(False)

#filter out malware distribution and malware domain
rrt_df = rrt_df[rrt_df['newtype'] != "Malware distribution" ]
rrt_df = rrt_df[rrt_df['newtype'] != "Malware Domain" ]
typ = rrt_df['newtype']
rel = rrt_df['Reliability']
rsk = rrt_df['Risk']
xtab = pd.crosstab(typ, [ rel, rsk ],
rownames=['typ'], colnames=['rel', 'rsk'])
print("Count: %d; Percent: %2.1f%%" % (len(rrt_df), (float(len(rrt_df)))
/ len(av)) * 100)
## Count: 15171; Percent: 5.9%
xtab.plot(kind='bar',legend=False)