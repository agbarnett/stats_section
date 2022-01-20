# Stats section
Accompanying code for the study "An observational analysis of the trope 'A p-value of <0.05 was considered statistically significant' and other cut-and-paste statistical methods"

Preprint available for download at: https://osf.io/preprints/r8wk9/.

Datasets used in topic modelling are in /data.

R scripts for data processing and analysis are provided for PLOS ONE and ANZCTR datasets in /code.

Full details of data processing and analysis are provided in the Material and Methods section of the manuscript.

## code/plosone
* 0_rplos_search.R: Conducts searches of PLOS ONE API using targeted search terms.
* 1_split_meta_dat.R: Convenience function to split the full set of search records into smaller batches, prior to XML data extraction
* 2_extract_fulltext_xml.R: Extracts article full text by DOI to identify statistical methods sections
* 3_combine_batch_info.R: Convenience function to recombine batches into a single data.frame. Applies second stage filter based on section headings.
* 4_extract_unicodes.R: Identifies full list of unicode characters to assist with data cleaning
* 5_process_stats_section.R: Data cleaning script to standardise formatting, including references to common statistical methods
* 6_analysis_plos_jaccard.R: Boilerplate text analysis of topic modelling results


## code/anzctr
* 0_read_data_anzctr_stats_section.R: Reads XML data downloaded from the ANZCTR.
* 1_analysis_anzctr_stats_section.R: Logistic regression analysis on missing statistical methods sections.
* 2_process_anzctr_stats_section.R: Data cleaning script to standardise formatting, including references to common statistical methods
* 3_analyse_anzctr_jaccard.R: Boilerplate text analysis of topic modelling results

## code/topicmodelling
* Statistical data clustering Demo.ipynb : Main analysis script (Python). Input is cleaned dataset as .txt file
* stats_section_cleaned_anzctr_small.txt: Example dataset for running topic modelling.
