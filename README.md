
# NBDPS Replication Code

Adrien et al. (2020) Ultraviolet light, dietary vitamin D, and risk of
select birth defects in the National Birth Defects Prevention Study.

<br> <br> <br>

## Instructions

This quick README will guide you through how to use this repository
alongside code sent by Kathy at the NBDPS center (hopefully, a dataset
called `BD3902_Analytic_VITD.csv`). You may not be running the code
contained in this repository, but I wanted to set this up in case you
needed to.

### Downloading this code

1.  Use the green Code button above; click “Download ZIP.” Save the
    folder somewhere on your computer you can easily access it.
2.  Unzip the folder. It should result in a folder called
    `vitd_replication-main` or similar.
3.  IMPORTANT: Add a folder called `data` to the unzipped folder. **Copy
    the `BD3902_Analytic_VITD.csv` file into it.** (If you do not name
    the folder `data` - all lowercase\! - or don’t put it in the main
    folder, this code will not work.)

### A note on this repository’s file structure

Both folders and files are often prepended with numbers (e.g. the
`1-create-analytic-sample` subfolder in the `code` folder). When this is
the case, run codefiles in order of their prefix.

For example, within the `1-create-analytic-sample` folder, you would run
`1-import-merge.R` before `2-vitd-tertiles.R`, and so on.

<br>

-----

### Appendix: NCCBDRP/CDC Replication Requirements

<br>

#### Create primary analytic sample

The replicator must begin with a clean NBDPS dataset and apply
analyst-designated filters to arrive at the final analytic dataset used
by the analyst.

<br>

#### Replicate primary analyses

**1. Confirmation of case and control counts for analyses**

**2. Confirmation of all main exposure and covariate distributions by
case-control status**

In this case, covariates that are included in Table 1.

**3. Confirm main effect analyses intended to be reported in the
published paper**

If both crude and adjusted estimates are to be included in the main
paper, both should be replicated.

Estimates presented as supplementary material may be replicated but are
not required to be replicated. If the presence of interactions are to be
reported, confirmation of interaction effects must be performed.

<br>
