# The IAT as an analogical learning task II - known stimuli 

Previous research demonstrated that the IAT functions as an analogical learning task as well as its traditional employment as a testing task (Hussey & De Houwer, under review). These experiments demonstrated the acquisition of attitudes towards previously unknown stimuli (Chinese characters). The current experiment examines whether the IAT can also modify existing (racial) attitudes.

We assess the influence of a Race IAT vs. (control) Flowers-Insects IAT on evaluations of black people (both self reported ratings and automatic evaluations on a SC-IAT), after controlling for baseline racism. We recruit white participants only so as to have a homogenous in/out-group.

## Design

Between groups design

### Additional inclusion criteria

- Race = caucasian/white (for homogenous in/out-group)

### IV

- Training IAT 
  - Condition 1: Race IAT 
  - Condition 2: Flowers/insects IAT (control) 

### DVs

- Black faces self-report ratings
- Black faces SC-IAT

### Counterbalancing

Not included in primary analyses.

- Task order (ratings & SC-IATs)
- Block order (identical for IAT & SCIAT)

### Covariates

- Sum scores on the (modified) Modern Racism Scale
  - References to the US changed to "my country."
  - Completed first so that it's not influenced by condition differences.
  - Used to control for baseline level of racism between groups.

## Measures

1. Demographics
2. Modern racism scale
3. IAT
4. (counterbalenced)
   - Ratings
   - SC-IAT

## Hypotheses

1. The conditions will differ in their automatic evaluations on the black faces SC-IAT.
2. The conditions will differ in their self-reported evaluations of the black faces.

## Sample

- Recruit online via prolific


- White participants only, as we need a uniform out group.

### Power analysis

No power analysis as I'm still working out how to do this for interaction effects in LMMs. 

### Planned sample size

150

### Payment

£1.20 per participant (12 mins)

## Analytic strategy

### Exclusion criteria

Participants who have >10% of RTs < 300ms on the IAT's test blocks (3, 4, 6, 7) or on the SCIAT test blocks (2 and 3) will be excluded from the analysis. This provides a way to screen for low quality responding given the internet based sample.

- This exclusion strategy was preregistered for the IAT only, but added for the SC-IAT too before analyses were conducted.

Individual SC-IAT RTs that deviate from the mean by more than 2.5 SD will be defined as outliers and excluded from the analysis.

### Analyses 

Linear mixed effects modelling
1. SCIAT_rt ~ SCIAT_block * condition + modern_racism + (1 | participant)
2. black_faces_rating ~ condition + modern_racism + (1 | participant)

Modifications since preregistration

- Report only frequentist statistics. Having read more about BF I'm not convinced that I understand them enough to employ them correctly, have concerns over sensitivity to priors, and would ultimately prefer a full Bayesian analysis that updates posterior beliefs. 
- Report LMM analysis only and not traditional D score analysis. Preregistration stated only the LMMs would be used to test the hypotheses, so reporting alternatives only confuses things.