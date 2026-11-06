# Session 3: Prompts and commands

Use the copy button in the top-right corner of each box. Type the prompts to OpenCode one at a time, and read what it wants to do before you approve it. Parts in **[square brackets]** are for you to fill in.

## Step 1: fix the frame

The Session 2 tables contained only country-years *with* a conflict, so there was nothing to compare them with. Start a fresh OpenCode session:

```
/new
```

Then rebuild the table:

```
Rebuild output/conflict_income.csv. The final table must contain every country in the World Bank GDP file in every year from 1990 to 2025, including the years without conflict. Remove the World Bank rows that are regions or income groups, not countries. Add a column conflict that is 1 if UCDP records an internal conflict (type_of_conflict 3 or 4) in that country and year, and 0 if not. Convert the UCDP gwno_loc codes to ISO-3 with the countrycode package and print the codes it could not convert. Add a column with GDP per capita in the previous year.
```

**Check it yourself** in the RStudio Console (not through the assistant):

```r
d <- read.csv("output/conflict_income.csv")
nrow(d)
mean(d$conflict)
table(d$conflict, useNA = "always")
```

1. Rows: about 170 countries × 36 years, i.e. roughly 6,200.
2. Mean of `conflict`: about 0.18.
3. No `NA` in `conflict`: a peaceful year is 0, not missing.
4. Are "World", "Arab World" and "Low income" gone?
5. Spot-check a country you know: Syria 2010 should be 0, 2012 should be 1.

**Stuck after 15 minutes?** Download [`lab2_full_table.csv`](Session-3-gallery/lab2_full_table.csv) into your `data/` folder and use it for the rest of the session.

## Step 2: Step 5 of the lab as a figure

First, **without** the assistant, answer the five design questions in `notes/lab2.md`: what does "concentrated" mean, what does "poorer" mean, which years, which year's income, and what result would make the claim false? Then:

```
Using output/conflict_income.csv, make a figure that tests whether internal conflict since 1990 is concentrated in poorer countries. Our test is: [your answers to the design questions]. Save it as output/step5.png.
```

Then at least **three follow-up prompts of your own**. For example:

```
Change the title so that it states what the figure shows.
```

```
Give both axes readable labels, with units.
```

```
Add a caption with the data sources: UCDP/PRIO Armed Conflict Dataset and World Bank World Development Indicators.
```

```
[Your own prompt: ordering, colour, facets, ...]
```

Save the final figure, the script that makes it, and the full sequence of prompts.

**Check:**

1. Did the Console say "Removed … rows"? How many, and which countries?
2. Does the figure show shares (conflict country-years *out of* all country-years) or counts? Does the title match?
3. Is there a comparison in the picture? Can you see the peaceful country-years?
4. Which year's income is it?

## Step 3: correct, but misleading

Open the [gallery](Session-3-gallery/) and follow its README for your two figures. Do steps 1 to 3 **without** the assistant. Write your fix prompt in `notes/session3.md`; it could start like this:

```
Change [figure_N.R] so that the figure [what an honest version would show]. Save it as output/figure_N_fixed.png.
```

Only then ask the assistant:

```
Read figure_N.R. Is anything misleading about the figure this script makes?
```

Did it find what you found? Did it find something you missed? Did it say there is no problem?

## Step 4: critique swap

Swap your Step 5 figure with a neighbouring pair. Write three sentences about theirs: what the figure claims, whether the data support it, and one thing to change. If you take the advice you get, tell the assistant:

```
Change output/step5.png as follows: [the change you agreed with]. Keep everything else the same.
```

## Before next session

Send me the final Step 5 figure, the script that makes it, and three lines in `notes/lab2.md`: is the claim true, how sure are you, and which decision changed the answer the most?

Add one line to `log.md`: something the assistant did not tell you today until you asked.
