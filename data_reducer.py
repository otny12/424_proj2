import pandas as pd

df = pd.read_csv('CTA_-_System_Information_-_List_of__L__Stops.tsv', sep='\t')
print(df.shape)
df = df[['MAP_ID', 'Location']]
print(df.head)
df['longitude'] = df['Location'].apply(lambda x: x.split(',')[-1][0:-1])
df['latitude'] = df['Location'].apply(lambda x: x.split(',')[0][1:])
df = df.drop('Location', axis=1)
df.rename(columns={'MAP_ID':'station_id'}, inplace=True)
print(df.shape)
df =df.drop_duplicates(subset=['station_id'])

df.loc[len(df.index)] = [40200, 41.88443, -87.62614] #ran/waba
df.loc[len(df.index)] = [40640, 41.88202, -87.62609] #mad/waba
df.loc[len(df.index)] = [40500, 41.8837, 87.6278] #wash/state
df.loc[len(df.index)] = [41580, 41.88491, 87.71132] #homan


print(df.tail)
df.to_csv('station_coor.tsv',index=False, sep='\t')


#read ridership file
df = pd.read_csv('CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv', sep='\t')
df = df.drop('daytype', axis=1)
df['y'] = df['date'].apply(lambda x: x.split('/')[-1])
print(df.tail())

for i in set(df.y):
    df.loc[df.y==i].to_csv(str(i)+'.tsv', sep='\t')
