# Session 4: Prompts and commands

Use the copy button in the top-right corner of each box. Type the prompts to OpenCode one at a time, and read what it wants to do before you approve it. Parts in **[square brackets]** are for you to fill in.

Background: the reader *Could It Be Chance?* ([Reader-Hypothesis-Testing.pdf](Reader-Hypothesis-Testing.pdf)), Sections 1–6. Data: `data/lab2_full_table.csv`.

---

## 1. Setup check (with Gena)

1. Download the current [AGENTS.md](AGENTS.md) from this repository. Put it in the **top folder** of your course project, replacing the old copy. Keep the name exactly `AGENTS.md`, with no `.txt` added.
2. Quit OpenCode and start it again **inside the project folder**.
3. Ask OpenCode:

```
Which instructions file are you following, and what does it say about how simple the code should be?
```

The answer should name `AGENTS.md` and the rule against writing your own functions.

4. In the R Console (not OpenCode), check that the data loads:

```r
nrow(read.csv("data/lab2_full_table.csv"))
```

If you get an error at any step, raise your hand.

---

## 2. The shuffle test, and the formula

Start a fresh OpenCode session:

```
/new
```

```
Using data/lab2_full_table.csv: add income in the previous year (gdppc lagged within each country), keep 1991–2025, drop rows without it and print how many. Within each year, split countries into four equal groups by that income. Compute the share of country-years in conflict in each group, and the gap between the poorest and the richest group. Then shuffle the group labels within each year 1,000 times, recompute the gap each time, draw a histogram of the 1,000 gaps with a red line at the real gap, and print the share of shuffled gaps at least as large as the real one. Write simple code: no functions of your own. Print every number; do not write any interpretation.
```

**Check, without asking the assistant:**

- Compare your gap and shares with the lecture.
- Run `sd()` on the shuffled gaps. How big is it compared with the real gap?
- Find the line with `sample()`. What does it shuffle, and within what?

**Then try the formula yourself, in the Console:**

```r
prop.test(c(485, 67), c(1460, 1428))
```

Does the interval match the lecture? Is the standard error close to the `sd()` of your shuffles? Then run `chisq.test()` for all four groups at once (Kennedy and Waggoner 7.3).

---

## 3. Shuffle whole countries

Stay in the same OpenCode session.

```
The same countries appear in every year and conflicts last for years, so country-years are not independent. Change the shuffle: keep each country's conflict history, but give it the whole income history of another country picked at random; then redo the four groups within each year and the gap. Repeat 1,000 times. Print the standard deviation of the shuffled gaps, the share at least as large as the real gap, and the real gap plus and minus 1.96 times that standard deviation.
```

**Check, without asking the assistant:**

- Compare the standard deviation with the one from Section 2. How many times bigger is it?
- Compare the interval with the one from `prop.test()`.
- Find the join that gives each country another country's income. Which column is the key?

---

## 4. Test a claim from your own figure: "the gap is widening"

**First, without the assistant (4 minutes)**, write down in `notes/lab2.md`:

1. the null hypothesis, in words;
2. the number you will compute;
3. what result would support "widening".

Then complete the first sentence with your test and send the prompt:

```
Our test: [...]. Using the country shuffle from before, compute for each shuffle the gap in 2011–2025 minus the gap in 1991–2005. Print the real change, a histogram of the shuffled changes with a red line at the real one, and the share of shuffled changes at least as large in absolute value.
```

**Then pick one other version.** Write it down *before* you run it:

- today's World Bank income groups (low vs high income) instead of the four groups by income;
- "the poorest group is on top in every year": a test for each year;
- wars only (1,000+ battle deaths).

Be ready to report one line: the version, the estimate, the interval, the *p*-value.

---

## Due before next session

- In `notes/lab2.md`: a five-line report of the main test, using the checklist in reader Section 10: the null hypothesis; the estimate; the interval and how its standard error was computed; the *p*-value; what the test does not show. Add the script that produces it.
- One line in `log.md`: a number the assistant printed without telling you what it assumed.
