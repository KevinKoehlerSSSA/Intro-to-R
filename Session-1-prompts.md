# Session 1: Prompts and commands

Use the copy button in the top-right corner of each box. Type the prompts to OpenCode one at a time, and read what it wants to do before you approve it.

## Step 1: a project

In the RStudio **Console**, check where R is looking:

```r
getwd()
```

## Step 2: start OpenCode

If you are not using the OpenCode app, open the **Terminal** tab in RStudio and type:

```
opencode
```

Then connect MiniMax with the course key and choose the course model, **MiniMax M2**. The [setup guide](OpenCode_MiniMax_setup.pdf) shows each step in the app. In the terminal, connect with:

```
/connect
```

and choose the model with:

```
/models
```

No key yet, or it does not work? Use a free OpenCode Zen model for today.

## Step 3: test it

```
List the files in this project and say what each folder is for.
```

```
Run Rscript -e "sessionInfo()" and tell me which version of R you found.
```

OpenCode should ask your permission before running the second one. If it does not, it was probably started in the wrong folder.

## Demo

```
Read the file data/tunisia_survey.csv and the codebook. Write a script scripts/01_first_look.R that loads the data with the tidyverse, reports the number of respondents, and makes a bar chart of the variable july25. Run it.
```

## Packages and help

In the RStudio **Console**:

```r
install.packages("tidyverse")  # once
library(tidyverse)             # every session
?mean
```

## Exercise: build a project, then check it

```
Set up this project for a small analysis of the Tunisia survey: create sensible folders if any are missing, a README explaining the structure, and a script that loads and cleans the data. Explain each file you create.
```

Then check, without asking the assistant:

1. Open every file it created. Does the README describe what is actually there?
2. Pick one line of the cleaning script you do not understand. Look up the function with `?` in RStudio. Write down what it does in your own words.
3. Did the "cleaning" change the data? How many rows before and after? Check with `nrow()` yourself.
4. Add one line to `log.md`.
