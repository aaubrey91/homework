import os
import re

### Take out all instances of null strings values and shift them to the left after we transformed the data in R ###
outfile = open("maincharlesbook_cleaned.csv","w")

for line in open("maincharlesbook_clean.csv"):
	#Replace all instances that exist in the line
	for item in range(0, line.count(',""')):
		line = line.replace(',""', ',')

	outfile.write(line)

outfile.close()

### Take out all the  null values and shift them to the left after we took out the null strings ###
outfile = open("maincharlesbook_clean_final.csv", 'w')

for line in open("maincharlesbook_cleaned.csv"):
	#Replace all instances that exist in the line	
	for item in range(0, line.count(',,')):
		line = line.replace(',,', ',')
	
	#Replace comma at the end of the line
	line = re.sub('\,$', '',line)
	outfile.write(line)

outfile.close()

#Don't need the tmp file anymore..
os.system("rm maincharlesbook_cleaned.csv")
