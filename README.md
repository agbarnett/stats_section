# Stats section
Clustering statistical methods sections from PLOS ONE.

We need to pre-process the data to create standard text, for example:
* lower case throughout
* 'p-value' to 'p value'
* change from US to UK spelling

We need to think about equations and Greek letters. Ideally we will recode these to words, e.g., 
* "\alpha" to "alpha"
* "f(" to "function"

Export the data to a text file with:
* Column 1: DOI
* Column 2: Stats methods section in one line with no paragraph returns
* Columns 3+: Meta-data, e.g., field of research
