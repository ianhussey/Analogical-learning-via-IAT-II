# The IAT as an analogical learning task II - known stimuli - Experiment 4 

Direct replication of Experiment 2. Increased sample size by roughly X1.5 relative to Experiment 2. 

## Design

Between groups design

### Additional inclusion criteria

- Race = Caucasian/white (for homogeneous in-group)

### IV

- Training IAT 
  - Condition 1: Race IAT 
  - Condition 2: Flowers/insects IAT (control) 

### DVs

- Black faces SC-IAT
- Black faces self-report ratings

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
4. (counterbalanced)
   - Ratings
   - AMP + AMP recognition test
     - The AMP recognition test asks participants whether they recognized the faces presented in the AMP as only black, only white, black and white, or not sure. It may be used within exploratory tests on the AMP effect.

## Hypotheses

1. The conditions will differ in their automatic evaluations on the black faces AMP.
2. The conditions will differ in their self-reported evaluations of the black faces.

## Sample

- Recruit online via prolific


- White participants only, as we need uniform racial in- and out-groups between participants and stimuli.

### Planned sample size

Recruit 350 (i.e., sample size before exclusions) based on roughly 1.5 the N of Experiment 2.

### Payment

£0.90 per participant (9 mins).

## Analytic strategy

### Exclusion criteria

Participants who have >10% of RTs < 300ms on the IAT's test blocks (3, 4, 6, 7) will be excluded from the analysis. This provides a way to screen for low quality responding given the internet based sample.

NB removed the exclusion based on RTs in the AMP that was employed pre-registered for Experiment 2 but removed post hoc due to unacceptable attrition rates.

### Analyses 

1. AMP_rating ~ prime_type * IAT_condition + modern_racism + (1 | participant)
2. self_report_rating ~ IAT_condition + modern_racism + (1 | participant)


### Meta analyses

#### Meta analysis of AMP effects

AMP data from Experiment 2 and 4 will be subjected to a random effects meta analysis:

- AMP_rating ~ prime_type * IAT_condition + modern_racism + (1 | participant) + (1 | experiment)

#### Meta analysis of self-report effects

Self-report ratings of the images of Black faces Experiments 1 to 4 will be subjected to a random effects meta analysis. Previous pre-registrations specified a combination analysis (fixed effects meta analysis), but on reflection a random effects meta analysis is more appropriate give that the studies were not identical. An identical but un-registered meta-analysis was conducted on data from Experiment 1 to 3 prior to data collection for Experiment 4. 

- self_report_rating ~ IAT_condition + modern_racism + (1 | participant) + (1 | experiment)