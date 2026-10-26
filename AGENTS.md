# Instructions for Coding Agent: Data Analysis in R

This is a course project for a class on data analysis in R. The student is learning to direct, read and check your code, and most have no prior programming experience. Help them understand, not just finish.

## How to work
- Use R with the tidyverse.
- Put scripts in `scripts/`, outputs (figures, tables) in `output/`. Never modify files in `data/`.
- The data are `data/tunisia_survey.csv`. The codebook is `data/codebook.md` (the same content as `data/Codebook-TUN-Survey.pdf`).
- Comment every step in plain language: what it does and why.
- Before running a script, say in one or two sentences what it will do.

## Keep the code simple
The students are first-time R users. They must be able to read every script top to bottom and explain each line. Clarity and simplicity always come before elegance, brevity or generality.
- Do not write your own functions (`function()`) unless the student explicitly asks for one. If the same steps are needed twice (e.g. for two variables), write the block out again and change only what differs.
- Prefer longer, simpler code to shorter, clever code. One operation per line; save intermediate results under clear names instead of nesting function calls inside each other. Keep pipes (`%>%`) to a few steps.
- Avoid advanced constructs: `lapply()`/`sapply()`/`purrr::map()`, `do.call()`, `Reduce()`, `repeat`/`while`, nested loops, lists of lists, list-columns, `tryCatch()`, `sink()`, tidy evaluation.
- Assume the script is run from the project folder and use simple relative paths (e.g. `"data/tunisia_survey.csv"`). Do not write code to locate folders, check that files exist or handle errors unless asked.
- Do only what the prompt asks. No extra logs, options, formatting or analyses; suggest extras in the chat instead.
- Keep output and plots plain: `print()` or `round()` rather than elaborate `sprintf()` formatting; one plot type with a title and axis labels, nothing more unless asked.
- Keep comments short: one or two plain-language lines per step, not long blocks of text.
- Handle one variable at a time. Do not reshape data (`pivot_longer()`, `pivot_wider()`) just to process several variables in one go; repeating a short block for each variable is fine.
- Use one pipe (`%>%`) and one way of doing each thing throughout a script, e.g. one way of attaching value labels.
- Explanations and teaching points belong in the chat, not in the script.

## Report what happens to the data
- After every step that can change the number of rows (reading, filtering, joining, dropping missing values), print the row count.
- Never use `na.rm = TRUE`, `na.omit()`, `drop_na()` or similar without saying how many cases are dropped and from which variables.
- If a variable has codes that look like missing values (e.g. 98, 99), flag them; do not decide on your own how to treat them.
- Once the student has decided how to treat missing-value codes, recode them to `NA` once, near the top of the script, one visible line per variable, and print how many were recoded. Use the cleaned variables from then on.
- Never type a number taken from the data into a comment or printed message; compute it and print it.
- Do not say why values are missing (not asked, skipped, refused) unless the codebook says so. If you assume a skip pattern, show it with a cross-tabulation, e.g. `table(pres2019_1_voted, is.na(pres2019_1), useNA = "ifany")`.

## Statistics
- Report means and standard deviations only where an average is meaningful. For categories, report counts and percentages; for yes/no variables, report the share.
- For every percentage, state the denominator and whether missing values are included in it.

## When unsure
- Ask rather than guess about analytical choices. Offer the options and their consequences; let the student decide.
- If you are not sure what a function does, say so and suggest checking its help page (`?function_name`).
