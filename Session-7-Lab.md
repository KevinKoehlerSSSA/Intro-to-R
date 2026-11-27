# Session 7 Lab: Reproduce Goldsmith and Horiuchi (2012), Models 5 and 6

**Your task:** re-estimate Models 5 and 6 of Goldsmith and Horiuchi (2012) and hand in a short document with:

1. a **regression table** with the two models side by side, and
2. **one paragraph** interpreting the results.

Work through the steps in order. Prompts and commands are in grey boxes: use the copy button in the top-right corner of each box.

---

## Step 1: Put the data and the article in your course folder

- **Data:** download [data_2011-07-26.dta](data_2011-07-26.dta) from this repository (if you do not have it from Session 6). Put it in the `data/` folder of your course project.
- **Article:** download Goldsmith, Benjamin E., and Yusaku Horiuchi. 2012. "In Search of Soft Power: Does Foreign Public Opinion Matter for US Foreign Policy?" *World Politics* 64 (3): 555–585, [doi:10.1017/S0043887112000123](https://doi.org/10.1017/S0043887112000123) (free from the Sant'Anna network). Save the PDF in the top folder of your course project (the folder that contains `data/`), not inside `data/`.

Your course folder should now contain:

```
Data-Analysis-in-R/
├── data/
│   └── data_2011-07-26.dta
├── scripts/
├── output/
├── Data-Analysis-in-R.Rproj
└── (the article, as a PDF)
```

---

## Step 2: Make sure R is working in the course folder

R looks for `data/data_2011-07-26.dta` **starting from its working directory**. The working directory must be the folder that contains `data/`.

Check it in the RStudio Console:

```r
getwd()
```

The answer should end in your course folder (for example `.../Data-Analysis-in-R`). If it does not, use **one** of these two fixes.

**Option A (recommended): open R through the project.** Close RStudio and double-click `Data-Analysis-in-R.Rproj` in your course folder. RStudio opens with the right working directory, and the Console says `Course project OK`.

**Option B: set it by hand with `setwd()`.** Type the full path to your course folder:

```r
# Mac
setwd("/Users/yourname/Documents/Data-Analysis-in-R")
```

```r
# Windows: write every backslash twice ...
setwd("C:\\Users\\yourname\\Documents\\Data-Analysis-in-R")
# ... or use forward slashes instead
setwd("C:/Users/yourname/Documents/Data-Analysis-in-R")
```

A single backslash (`C:\Users\...`) gives an error: in R, `\` starts a special character, so it has to be "escaped" as `\\`.

Then check that R finds the data:

```r
file.exists("data/data_2011-07-26.dta")
```

This must print `TRUE`. If it prints `FALSE`, raise your hand.

---

## Step 3: Ask the assistant to write the code

Start OpenCode in your course folder (`start-opencode.bat` on Windows, `start-opencode.command` on Mac) and begin a fresh session:

```
/new
```

Then copy this prompt:

```
Write an R script, scripts/session7_lab.R, that re-estimates Models 5 and 6 of Table 3 in Goldsmith and Horiuchi (2012). The article is the PDF in the top folder of the project; the data are data/data_2011-07-26.dta (read it with haven::read_dta).
1. Keep only the 58 countries used in the paper (keep == 1). Print the number of rows.
2. Opinion of US foreign policy: for each of the three Gallup surveys, (pos - neg) / 100 (pos1/neg1, pos2/neg2, pos3/neg3). Then take each country's average of the surveys it has. Print how many countries miss one or two surveys.
3. Build two controls as in the paper: democ = pr + cl (Freedom House) and afgh = 1 if troops_afgh > 0, otherwise 0.
4. Model 5: lm() of unvoting on opinion only. Model 6: lm() of unvoting on opinion plus afgh, icc, s_lead, nato, aid_m, aid_e, lntrade, lngdppc, democ, muslimpct and europe.
5. Print summary() of each model, the number of observations, and confint() for each model.
Write simple code: no functions of your own. Print every number; do not write any interpretation.
```

Read what OpenCode wants to do before you approve it.

---

## Step 4: Run the script yourself and read the output

Open `scripts/session7_lab.R` in RStudio. Run it **line by line** with Ctrl+Enter (Mac: Cmd+Enter) and look at what each line prints in the Console.

**Check, without asking the assistant:**

- Are there 58 countries in both models?
- Opinion slope in Model 5: **2.07**? In Model 6: **15.78**?
- Open Table 3 in the article (Models 5 and 6, "UN Voting with US in 2003"). Do your **coefficients** match the article's?
- Now compare the **standard errors** (in square brackets in the article) with yours. Do they match?

If your coefficients differ, find the line where your script differs from the instructions in Step 3 before you ask OpenCode to fix it.

### Step 4b: Why the standard errors differ

Your coefficients should match the article exactly, but your standard errors will not: R gives **9.55** and **4.97** for the opinion slope, the article **9.06** and **3.62**.

The reason is condition 2 on today's slides (equal spread of the prediction errors). R's default formula assumes it. Goldsmith and Horiuchi do not: they report **robust standard errors**, which allow the spread of the prediction errors to differ across countries. There are several versions; theirs is the one called **HC1** (the same as Stata's `robust` option).

Ask OpenCode:

```
Add robust standard errors (type HC1) to scripts/session7_lab.R for both models, using the sandwich and lmtest packages: print coeftest() with vcovHC(type = "HC1") for each model, and the 95% intervals with coefci() using the same robust standard errors. Keep the default results above. Write simple code: no functions of your own.
```

Run the new lines yourself. **Check:** do you now get 9.06 (Model 5) and 3.62 (Model 6) for the opinion slope, and the article's standard errors for the controls?

For the opinion slope in each model, write down the **robust standard error**, the **95% interval** and the **p-value**. Does the interval contain 0?

---

## Step 5: Ask the assistant for a table you can paste into your document

In the same OpenCode session:

```
Add a final step to scripts/session7_lab.R: a regression table with Model 5 and Model 6 side by side, saved as output/session7_table.html. Use the modelsummary package (tell me if I need to install it). Use the robust standard errors (HC1), as in the article, and show them in parentheses under each coefficient, stars for p < 0.05 and p < 0.01, the number of observations and the R-squared. Give the variables readable names (for example "Opinion of US foreign policy" instead of opinion). Keep all controls in the table. Write simple code: no functions of your own.
```

If OpenCode says a package is missing, install it in the RStudio Console, for example:

```r
install.packages("modelsummary")
install.packages("sandwich")
install.packages("lmtest")
```

Run the new lines yourself. Then open `output/session7_table.html` in your web browser, select the whole table, copy it and paste it into your word processor (Word, Google Docs, Pages or LibreOffice). Check that the numbers in the pasted table match your R output and Table 3 of the article.

---

## Step 6: Write your document

Create a document with:

1. **Title** and your name.
2. **The regression table** from Step 5, with a short caption, e.g. *Table 1: Opinion of US foreign policy and UN voting with the US, 58 countries, 2002–03 (OLS, robust standard errors in parentheses).*
3. **One paragraph (about 150–200 words) interpreting the results.** Answer these questions in your own words:
   - How large is the opinion slope in each model? What does it mean in terms of UN voting (percentage points) for one unit of opinion?
   - Could each slope be due to chance? Use the (robust) standard error, the 95% interval or the p-value.
   - Why does the slope change so much from Model 5 to Model 6? Which control matters most (remember Lab 2 in Session 6)?
   - Would you call the Model 6 slope the **effect** of public opinion on UN voting? Why, or why not?

Save the document as a PDF and hand it in as instructed in class.
