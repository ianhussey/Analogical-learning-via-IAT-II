# The IAT as an analogical learning task II - known stimuli - Experiment 2 

The results of Experiment 1 suggested that completing a Race IAT changes participants' implicit (but not explicit) racial attiudes. Given the potential implications of this for the use of the IAT as a measure of implicit racial bias, we then sought to replicate this result. Experiment 2 represents a conceptual replication of Experiment 1. We employ an AMP rather than a SC-IAT as DV, under the rationale that the procedural similarities between the IAT and SC-IAT may have driven the effect. If the effect is robust under a procedurally dissimilar task (the AMP) this would provide more credible evidence that the IAT can modify established attitudes toward known (racial) stimuli via its structural analogy. 

We retained both hypotheses, i.e., that the IAT will change both explicit and implicit racial attitudes, although Experiment 1 did not support the former hypothesis. We also include a combination analysis of self report data from both Experiments 1 and 2 in order to increase power, given that the effect may be very small for explicit attitudes. 

## Design

Between groups design

### Additional inclusion criteria

- Race = caucasian/white (for homogenous in-group)

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
     - The AMP recognition test asks participants whether they recognised the faces presented in the AMP as only black, only white, black and white, or not sure. It may be used within exploratory tests on the AMP effect.

## Hypotheses

1. The conditions will differ in their automatic evaluations on the black faces AMP.
2. The conditions will differ in their self-reported evaluations of the black faces.

## Sample

- Recruit online via prolific


- White participants only, as we need a uniform out group.

### Power analysis

No power analysis as I'm still working out how to do this for interaction effects in LMMs. 

### Planned sample size

200.

This is slightly larger than the previous experiment, on the basis that effects on the AMP might be smaller than the SC-IAT given that the AMP shares less procedural similarities with the IAT. This sample size therefore balances what we know from Exp 1 with pragmatic limitations (namely cost).

### Payment

£0.90 per participant (9 mins). Should be slighly shorter than Exp 1 given that the AMP is shorter.

## Analytic strategy

### Exclusion criteria

Participants who have >10% of RTs < 300ms on the IAT's test blocks (3, 4, 6, 7) or on the AMPs test block will be excluded from the analysis. This provides a way to screen for low quality responding given the internet based sample.

### Analyses 

1. AMP_rating ~ prime_type * IAT_condition + modern_racism + (1 | participant)
2. self_report_rating ~ IAT_condition + modern_racism + (1 | participant)


3. Combination analysis of ratings data from Experiments 1 and 2.  
   - self_report_rating ~ IAT_condition + modern_racism + (1 | participant)

## To do

Find more approparite way to plot binomial results