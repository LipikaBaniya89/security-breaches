# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""


print("Lipika Baniya", "CMIS 527 – 73D", "assignment-number(01)")
print("I Lipika Baniya certify that I did this work on assignment 01 by myself")

#Import necessary libraries
import pandas as pd 
import numpy as np

#set random seed for reproducibility (ensures the same random numbers are generated every time)
np.random.seed(1492)

# Create a DataFrame with a column named 'varl' containing 5000 random numbers from a standard normal distribution
test_df = pd.DataFrame({"varl" : np.random.randn(5000)}) #dataframe and 5000 random number

#generate histogram for the column name ‘varl’
test_df.hist(column="varl", bins=30, color="skyblue", edgecolor="black", alpha=0.7) 

#display descriptive statistics of the dataset
print(test_df.describe())


#Listing 2-1

#Import necessary libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#create a dataframe with columns "name", "os" and "highvulns"
assets_df = pd.DataFrame({
    "name" : ["danube", "gander", "ganges", "mekong", "orinoco"],
    "os" : ["W2K8", "RHEL5", "W2K8" ,"RHEL5","RHEL5"],
    "highvulns" : [1, 0,2,0,0]
    })

#Display the datatypes of each column in the DataFrame
print(assets_df.dtypes)

#Display first 5 rows of DataFrame to inspect the data
assets_df.head()

#Display only the first 5 rows of "os" column from the DataFrame
assets_df.os.head()

#Add a new column called 'ip' with IP addresses for each asset
assets_df['ip'] = ["192.168.1.5", "10.2.7.5", "192.168.1.7", "10.2.7.6", "10.2.7.7"]

#This filters out and shows only rows with more than one high vulnerability 
assets_df[assets_df.highvulns > 1].head()

#Create a new column called 'zones' based on the IP address prefix
assets_df['zones'] = np.where(
     assets_df.ip.str.startswith("192"),"Zone1","Zone2"
    )

#Display the final version of DataFrame adding the 'zones' column
assets_df.head()

# Pie chart showing the distribution of assets across zones
zone_counts = assets_df['zones'].value_counts()
zone_counts.plot(kind='pie', autopct='%1.1f%%', colors=['lightgreen', 'lightcoral'])
plt.title('Distribution of Assets Across Zones')
plt.ylabel('')
plt.show()