#this code is used to merge all our datasets together

import pandas as pd

food = pd.read_csv('FoodAccess19IL.csv')
obesity = pd.read_csv('Ob19.csv')
poverty = pd.read_csv('PovertyIL19.csv')
rate = pd.read_csv('URateIL19.csv')



food = food.groupby('county_name')['value'].mean().reset_index()
obesity = obesity.groupby('county_name')['value'].mean().reset_index()
poverty = poverty.groupby('county_name')['value'].mean().reset_index()
rate = rate.groupby('county_name')['value'].mean().reset_index()

food.columns = ['county_name','Food Access' ]
obesity.columns = ['county_name','Obesity Prevalence']
poverty.columns = ['county_name','Poverty']
rate.columns = ['county_name','Unemployment Rate']

food.head()
obesity.head()


merge1 = food.merge(obesity, on='county_name', how='left')
merge2 = poverty.merge(rate, on='county_name', how='left')
merge3 = merge1.merge(merge2, on='county_name', how='left')

merge3.to_csv('final_merge.csv',index=False)

hispanic = pd.read_csv('HispanicIL.csv')
asian = pd.read_csv('AsianIL.csv')
black = pd.read_csv('BlackIL.csv')
white = pd.read_csv('WhiteIL.csv')

hispanic = hispanic.groupby('county_name')['Hispanic'].mean().reset_index()
asian = asian.groupby('county_name')['value'].mean().reset_index()
black = black.groupby('county_name')['value'].mean().reset_index()
white = white.groupby('county_name')['value'].mean().reset_index()

hispanic.columns = ['county_name','Hispanic']
asian.columns = ['county_name','Asian']
black.columns = ['county_name','Black']
white.columns = ['county_name','White']

hispanic.head()


merge4 = hispanic.merge(asian, on='county_name', how='left')
merge5 = black.merge(white, on='county_name', how='left')
merge6 = merge4.merge(merge5, on='county_name', how='left')

final_finalmerge = merge3.merge(merge6, on='county_name', how='left')

final_finalmerge.to_csv('thelast_merge.csv',index=False)

final_finalmerge.head()
