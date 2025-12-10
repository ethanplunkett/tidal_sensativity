# Tidal Protocol Sensativity Analysis

## Goals

What happens when we loose variables? 
Assume limiting variables are essential.
With data that are part of the weighted average run score with those
weights omitted.

Report results overall and stratified by channel width, tide range, and 
water body type (salt creek, salt and brackish, freshwater tidal).

## Data

The data was downloaded on 2005-11-14 from NAACC.org using the search function to search
for all Tidal Connectivity Assessments.  The files in the zip where extracted
to the /data subdirectory without renaming.

## Scoring 

The scoring is described in the 
[NAACC documentation](https://streamcontinuity.org/resources/aquatic-passability-scoring-systems-tidal-stream-crossings)

This sensativity analysis is based on the **Numeric Scoring System** which 
scores from 0 (no passage) to 1 (full passage).

# Current Status
* Scoring function complete but for 16 crossings the scores differ from the exported score from NAACC

* Sensitivity analysis
  * Made plots of how much holding out 1 to 4 variables affects the scores.

* Next step:  
  * figure out why scores differ
  * make sensitivity plots by variable indicating how much holding each variable
     out affects the scores - both alone and in combination with other variables.





