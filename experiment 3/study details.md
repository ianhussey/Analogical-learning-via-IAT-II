# The IAT as an analogical learning task II - known stimuli 3 

The results of Experiment 1 and 2 suggested that completing a Race IAT changes participants' implicit (but not explicit) racial attiudes. Given the potential implications of this for the use of the IAT as a measure of implicit racial bias, we then sought to assess whether this effect generalises to forms of implicit bias other than implicit evaluations. Experiment 3 represents an extension of Experiment 1 and 2. We employ a Shooter Bias Task rather than a SC-IAT or AMP as DV. This task requires participants to "shoot" when pictures of men with guns are presented and "not shoot" when pictures of unarmed men are presented. Only pictures of black men were included, so as to provide a non-relative task, as in Experiments 1 and 2. If the effect of the IAT acting as a training task impacts not only implicit evaluations but also implicit shooter bias, this would indicate the IAT training effect has generalisability. 

Building on the previous experiments, we assess the influence of a Race IAT vs. (control) Flowers-Insects IAT on evaluations of black people (both self reported ratings and automatic biases on a Shooter Bias Task), after controlling for baseline racism. We recruit white participants only so as to have a homogenous in/out-group.

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
- Black men shooter bias task (two trial types, gun vs non-gun object trials)

### Counterbalancing

Not included in primary analyses.

- Task order (ratings & shooter task)
- IAT block order & shooter task key locations

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
   - Shooter bias task

## Hypotheses

H1-H3 The conditions will differ in their automatic biases evaluations on the shooter bias task. The shooter bias task is relatively liberal in that it provides multiple scoring metrics. We will employ three of the most common ones. The first is consistent with the previous analyses of RT biases (e.g., on the SC-IAT), the second and third represent two forms of signal detection analysis that are frequently employed in the shooter bias task literature.

- H1: RT biases 
  - condition*trial type interaction effect or main effect for condition
- H2: sensitivity differences (d')
  - main effect for condition
- H3: response bias differences (c)
  - main effect for condition

H4 The conditions will differ in their self-reported evaluations of the black faces.

- H4: ratings differences (c)
  - main effect for condition

## Sample

- Recruit online via prolific


- White participants only, as we need a uniform out group.

### Power analysis

No power analysis as I'm still working out how to do this for interaction effects in LMMs. Sample size increased relative to previous experiments given larger number of analyses on the shooter task.

### Planned sample size

300

### Payment

£2 per participant (20 mins)

## Analytic strategy

### Exclusion criteria

Participants who have >10% of RTs < 300ms on the IAT's test blocks (3, 4, 6, 7) will be excluded from the analysis. This provides a way to screen for low quality responding given the internet based sample.

Individual shooter task RTs that deviate from the mean by more than 2.5 SD will be defined as outliers and excluded from the analysis of RT level data.

### Analyses 

*Analyses of the shooter bias task:*

#### H1 shooter task RT analysis

Linear mixed effects model

shooter_rt ~ condition * trial_type + modern_racism + (1 | participant)

#### H2 shooter task sensitivity analysis

shooter_d' ~ condition + modern_racism + (1 | participant)

#### H3 shooter task response bias analysis

shooter_c ~ condition + modern_racism + (1 | participant)



*Analyses of ratings data:*

#### H4 ratings analysis

Linear mixed effects model

black_faces_rating ~ condition + modern_racism + (1 | participant)