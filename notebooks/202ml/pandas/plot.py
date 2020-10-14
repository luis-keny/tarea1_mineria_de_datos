import json
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors
import numpy as np

# The pandas read_csv function creates a DataFrame from a CSV file
all_gdp = pd.read_csv('GDP.csv')
# Select just the columns we will need
gdp = all_gdp[['Country Code','1995','2005']]
gdp = gdp.set_index('Country Code')

# Read the raw JSON data
with open('population_data.json') as f:
    pop_data = json.load(f)

# Prepare a DataFrame to receive the population data
pop = pd.DataFrame(np.nan,index=gdp.index,columns=['1995','2005'])
# Process the population data, selecting only those entries for
# the years 1995 and 2005
for p in pop_data:
    if p['Year'] == '1995' or p['Year'] == '2005':
        pop.at[p['Country Code'],p['Year']] = float(p['Value'])

# Now do the calculations
gdp_pc_1995 = gdp['1995']/pop['1995']
gdp_pc_2005 = gdp['2005']/pop['2005']
gdp_growth = (gdp_pc_2005-gdp_pc_1995)/gdp_pc_1995
pop_growth = (pop['2005']-pop['1995'])/pop['1995']

# The GDP growth data contains outliers: filter them out.
gdp_growth = gdp_growth[gdp_growth < 3]

# Make a combined DataFrame from the two growth series...
combined = pd.DataFrame({'pg':pop_growth,'gg':gdp_growth,'w':gdp_pc_2005,'p':pop['2005']})
# ...and filter out bogus rows.
final = combined.dropna()

# Now make the plot.
cmap = plt.cm.rainbow
norm = matplotlib.colors.Normalize(vmin=np.log2(100), vmax=np.log2(5.5e4))
plt.scatter(final['pg'],final['gg'],s=(np.log2(final['p'])**2)/16,color=cmap(norm(np.log2(final['w']))))
plt.show()