# The IAT as an analogical learning task II

## Description & purpose

Explores whether the IAT functions as an analogical learning task when known stimuli (images of black and white individuals) are employed. That is, whether the IAT trains what it seeks to test.

Measures, processing scripts, and analysis scripts for an experiment on the IAT as an analogical learning task. Project on Open Science Framework with the design, data, and analysis can be found here http://osf.io/7pbjq

- Measures written in Inquisit (v4.0).
- Processing scripts and analyses scripts written in R (v3.3) with additional packages.

## Licenses

### Code

Processing, analysis and some measures code is copyright (c) Ian Hussey 2016 (ian.hussey@ugent.be) and released under the GPLv3+ open source license. 

IAT and SC-IAT code is copyright (c) Millisecond Software (2015) with modifications by Ian Hussey

### Stimuli

Stimuli from Xu, Nosek & Greenwald (2014)

## Code usage

Run in order:

1. `1 screening.r`
   1. Compares the number of trials on each task to the modal number for the sample and creates two lists of participants, those with complete vs incomplete data. Participants can be paid/rejected on prolific.ac appropriately.
2. `2 processing`
   1. Processes data into analysable format.
3. `Analysis.rmd`
   1. Applies exclusion criteria. If sample doesn't meet stopping rule, increase N on prolific.ac and iteratively rerun these scripts on the new, complete data. When sample has been reached, interpret analyses.
4. `3 Remove prolific codes.r`
   1. Once final sample has been completed, remove participants' prolific codes from data using this script. Ensure that the original demographics.csv file is either overwritten or deleted. This anonymised data can then be put online.