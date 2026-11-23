# =====================================================================
# Data Analysis in R -- Session 6, Monday 23 November 2026
# Linear regression on ten students: slides S3.1 to S6.5
#
#   Sections 3 to 6 of the session again, on a small example:
#   x = questions correct out of 20, y = attitude to taking tests (%).
#   Data: Salkind (2014), Statistics for People Who (Think They) Hate
#   Statistics. The names are added. No data file needed: the ten
#   students are typed in below.
#
# Run it line by line (Ctrl/Cmd + Enter), not all at once.
# Every block starts with a comment that says what the block is for.
# A line that is only a name (for example  mean_x ) prints that value.
# =====================================================================

library(dplyr)     # for %>%, filter() and select()
library(ggplot2)   # for the scatterplot


# ---- S3.1: set-up ----

# We type the data in ourselves. A data frame is a table.
# Each row is one student. There are three columns:
#   name = who the student is
#   x    = how many of the 20 questions the student got right
#   y    = the student's attitude to taking tests, from 0 to 100
# The first name goes with the first x and the first y, and so on:
# Anna got 17 right and has an attitude of 94.
students <- data.frame(
  name = c("Anna", "Bruno", "Chiara", "Dario", "Elena",
           "Fabio", "Giulia", "Luca", "Irene", "Paolo"),
  x = c(17, 13, 12, 15, 16, 14, 16, 16, 18, 19),
  y = c(94, 73, 59, 80, 93, 85, 66, 79, 77, 91)
)

# How many rows? It must say 10.
nrow(students)

# Look at the whole table.
students


# ---- S3.2: test score and attitude ----

# A picture of the table. One dot is one student.
# Further to the right = more questions right.
# Higher up = a more positive attitude.
# geom_point() draws the dots; shape = 18 makes them diamonds.
# geom_smooth(method = "lm") draws the straight line that fits the dots
# best: the regression line. The rest of this script builds that same
# line by hand, one step at a time.
# xlim() and ylim() make both axes start at 0, so we see the whole
# scale: 0 to 20 questions, 0 to 100 attitude points.
# theme_classic() gives a plain white background.
ggplot(students, aes(x = x, y = y)) +
  geom_point(shape = 18, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 20) + ylim(0, 100) +
  labs(x = "Questions correct (out of 20)", y = "Attitude to tests (%)") +
  theme_classic()

# Reading the line like a ruler: what attitude goes with 15 questions
# right? The same plot with two red dashed guide lines added:
#   the first goes straight up from 15 on the bottom axis to the line,
#   the second goes from there straight left to the attitude axis.
# Where the second one arrives (a little under 80) is what the line
# predicts for a student with 15 right. Slide S4.7 gets the exact
# number by calculation.
ggplot(students, aes(x = x, y = y)) +
  geom_point(shape = 18, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_segment(x = 15, xend = 15, y = 0, yend = 77.8,
               colour = "red", linetype = "dashed") +
  geom_segment(x = 15, xend = 0, y = 77.8, yend = 77.8,
               colour = "red", linetype = "dashed") +
  xlim(0, 20) + ylim(0, 100) +
  labs(x = "Questions correct (out of 20)", y = "Attitude to tests (%)") +
  theme_classic()


# ---- S4.1: 4a, the mid-point ----

# Step 1: find the "average student".
# mean() adds up the ten numbers and divides by ten.
# mean_x is the average number of questions right.
# mean_y is the average attitude.
# The regression line always passes through this point. Think of a pin
# stuck into the plot there: the line can tilt around the pin, but it
# cannot leave it.
mean_x <- mean(students$x)
mean_y <- mean(students$y)
mean_x
mean_y


# ---- S4.2: 4a, each student's distance from the mid-point ----

# Step 2: for every student, ask two questions.
#   dx: how many questions more (or fewer) than the average student?
#   dy: how many attitude points more (or fewer) than the average?
# We get the answer by subtracting the average from the student's value.
# A positive number means "above average", a negative one "below".
# R does the subtraction for all ten students at once and stores the
# answers as two new columns.
students$dx <- students$x - mean_x
students$dy <- students$y - mean_y

# To keep the screen readable, look at four students only.
# filter() keeps the rows whose name is in the list.
# select() keeps the columns we want to see.
four_students <- students %>%
  filter(name %in% c("Chiara", "Fabio", "Irene", "Paolo"))
four_students %>% select(name, x, y, dx, dy)


# ---- S4.3: 4b, multiply the two distances ----

# Step 3: multiply each student's two distances.
# Why multiply? The product is a vote on the rule
# "the higher the score, the more positive the attitude".
#   Above average on both: plus times plus = plus.  A vote FOR the rule.
#   Below average on both: minus times minus = plus. Also a vote FOR.
#   Above on one, below on the other: plus times minus = minus.
#                                                    A vote AGAINST.
# The size of the product says how loud the vote is: a student far from
# the average on both counts a lot, a student close to it hardly at all.
students$product <- students$dx * students$dy

# The same four students again, now with their product.
# (We build four_students again because the product column is new.)
four_students <- students %>%
  filter(name %in% c("Chiara", "Fabio", "Irene", "Paolo"))
four_students %>% select(name, dx, dy, product)


# ---- S4.5: 4b, all ten students, row by row ----

# Now the full table for all ten students, as on slide S4.5.
# The table has two more columns. Both are a distance
# multiplied by itself ("squared"; in R:  ^2 ):
#   dx_squared = the score distance times itself
#   dy_squared = the attitude distance times itself
# Why square? If we just added up the distances, the pluses and the
# minuses would cancel and the sum would always be 0. A squared number
# is never negative, so the sum of the squares tells us how spread out
# the students are: a large sum = students differ a lot from each other.
students$dx_squared <- students$dx^2
students$dy_squared <- students$dy^2

# The whole table: compare it with slide S4.5, row by row.
students

# The three sums in the bottom row of the table.
# sum() adds up all ten numbers of a column.
#   1. sum of the products:   the result of the vote
#   2. sum of dx_squared:     how spread out the scores are
#   3. sum of dy_squared:     how spread out the attitudes are
# Everything that follows (slope, correlation) is made from these three.
sum(students$product)
sum(students$dx_squared)
sum(students$dy_squared)


# ---- S4.6: 4c, add up all ten products ----

# Step 4: count the votes.
# The first line is the total: all ten products added up.
sum(students$product)

# The square brackets pick out some of the products.
# [students$product > 0] keeps only the positive ones: the students who
# vote FOR the rule and push the line up.
sum(students$product[students$product > 0])   # pushing the slope up

# [students$product < 0] keeps only the negative ones: the students who
# vote AGAINST and pull the line down.
sum(students$product[students$product < 0])   # pulling it down

# The total is the first of these minus the size of the second. Like a
# tug of war: the stronger side wins, by the difference.


# ---- S4.7: 4d, divide by the squared score distances ----

# Step 5: turn the vote into a slope.
# The sum of the products says how strongly the line is pulled upwards.
# It is not yet "attitude points per question". To get that, we divide
# by how spread out the scores are (the sum of dx_squared).
# The result is the slope: how many attitude points the line goes up
# when the score goes up by one question.
students$dx_squared <- students$dx^2
student_slope <- sum(students$product) / sum(students$dx_squared)
student_slope

# The intercept is where the line starts: the attitude the line gives to
# a student with 0 questions right.
# We find it by starting at the mid-point and walking back to a score of
# 0: that is mean_x steps to the left, and at each step the line drops
# by the slope.
student_intercept <- mean_y - student_slope * mean_x
student_intercept

# R can do all five steps in one go. lm() means "linear model".
# y ~ x reads "y explained by x": attitude explained by score.
# coef() prints the intercept and the slope. They must be the same two
# numbers we just built by hand.
m_students <- lm(y ~ x, data = students)
coef(m_students)

# Using the line: what attitude does it predict for a student with 15
# questions right? Start at the intercept, add the slope 15 times.
student_intercept + student_slope * 15


# ---- S4.8: the correlation, by hand ----

# The slope says how STEEP the line is. The correlation (r) says how
# TIDY the cloud of dots is: do the dots hug the line (r near 1 or -1)
# or are they all over the place (r near 0)?
# It comes from the same three sums.

# The third sum: how spread out the attitudes are.
sum(students$dy_squared)

# Multiply the two spreads and take the square root (sqrt).
sqrt(sum(students$dx_squared) * sum(students$dy_squared))

# r = the result of the vote, divided by that number.
# The slope divided the vote by the spread of the scores only.
# r divides it by the spread of both, so the units drop out and r always
# lies between -1 and +1.
student_r <- sum(students$product) /
  sqrt(sum(students$dx_squared) * sum(students$dy_squared))
student_r

# R's own function for the correlation. It must give the same number.
cor(students$x, students$y)


# ---- S4.9: a second route to the same slope ----

# The slope can be computed in another way:
#   slope = r  x  (standard deviation of y) / (standard deviation of x)
# A standard deviation is a typical distance from the average.
# By hand: take the sum of the squared distances, divide by the number
# of students minus one (10 - 1), and take the square root.
sqrt(sum(students$dy_squared) / (10 - 1))   # typical attitude distance
sqrt(sum(students$dx_squared) / (10 - 1))   # typical score distance

# R's own function sd() does the same. Compare with the two lines above.
sd(students$y)
sd(students$x)

# The second formula. In words: if score and attitude moved perfectly
# together, one typical step in score would go with one typical step in
# attitude. They do not move perfectly together, so we shrink that by r.
# The result must be the slope we already have.
student_r * sd(students$y) / sd(students$x)


# ---- S4.10 and S4.11: in class, Kevin (attitude 93.7) ----

# Kevin's attitude is 93.7. What was his score?
# Our line works like a machine: a score goes in, an attitude comes out:
#   attitude = intercept + slope x score
# Here we know what came out (93.7) and want to know what went in.
# So we run the machine backwards: first take away the intercept, then
# divide by the slope.
(93.7 - student_intercept) / student_slope

# A caution. Our line was built to guess the attitude from the score.
# To guess the score from the attitude, statisticians build a second
# line with the two roles swapped: x ~ y instead of y ~ x.
m_score <- lm(x ~ y, data = students)
coef(m_score)

# What that second line predicts for an attitude of 93.7.
# It is not the same answer as above: two questions, two lines.
predict(m_score, data.frame(y = 93.7))


# ---- S5.1: an invented world of 20,000 students ----

# So far: ten students, one slope. But ten other students would have
# given another slope. How much can a slope change from one group of ten
# to the next? With real data we cannot know, because we only have these
# ten. So we invent a world in which we know everything.

# set.seed() fixes the random numbers, so that everybody in the room
# gets the same invented world and the same results.
set.seed(2084)   # same random numbers for everyone

# Invent test scores for 20,000 students. rnorm() draws random numbers
# that are centred on a mean and spread around it. We use the mean and
# the spread of our ten students, so the invented scores look like
# theirs.
all_students <- data.frame(x = rnorm(20000, mean = mean(students$x),
                                     sd = sd(students$x)))

# Invent their attitudes with a rule that we choose ourselves:
#   attitude = 30 + 3 x score + luck
# So in this world every extra question is worth 3 attitude points.
# The second rnorm() is the luck: everything else that makes one
# student's attitude higher or lower.
all_students$y <- 30 + 3 * all_students$x +
  rnorm(20000, mean = 0, sd = 10)

# The slope of the line through ALL 20,000 students. This is "the
# truth" of the invented world. It is close to 3, because we put 3 into
# the rule. In real research nobody ever sees this number.
true_student_slope <- coef(lm(y ~ x, data = all_students))["x"]
true_student_slope


# ---- S5.2: one sample, one slope ----

# Now pretend we are researchers again and can only afford ten students.
# sample(20000, 10) picks 10 of the 20,000 row numbers at random, like
# pulling ten names out of a hat. The square brackets keep those rows.
ten_students <- all_students[sample(20000, 10), ]

# The slope for these ten. How close is it to the true slope?
coef(lm(y ~ x, data = ten_students))["x"]

# The picture on slide S5.2. Read it layer by layer:
#   grey dots     all 20,000 invented students
#   dashed line   the regression line through all 20,000: the truth
#   blue dots     the ten students we happened to pull out of the hat
#   blue line     the regression line through those ten only
# The two lines are not the same: the blue one is our guess, made from
# ten students, of where the dashed one lies.
# fullrange = TRUE draws the blue line across the whole plot, not only
# between the ten blue dots.
ggplot(all_students, aes(x = x, y = y)) +
  geom_point(colour = "grey55", size = 0.3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black",
              linetype = "dashed") +
  geom_point(data = ten_students, colour = "blue", size = 2) +
  geom_smooth(data = ten_students, method = "lm", se = FALSE,
              colour = "blue", fullrange = TRUE) +
  labs(x = "Questions correct (invented)",
       y = "Attitude to tests (invented)")


# ---- S5.3: every sample, its own slope ----

# Do the same thing 1,000 times: pull ten names out of the hat, compute
# the slope, write it down. replicate(1000, ...) repeats the line inside
# it 1,000 times and keeps the 1,000 results.
# This takes a few seconds.
student_slopes <- replicate(1000,
  coef(lm(y ~ x, data = all_students[sample(20000, 10), ]))["x"])

# The first three of the 1,000 slopes. Same world, same recipe, and
# still three different answers. One of them may even be negative.
student_slopes[1:3]

# See it happen. The next two commands pull ten NEW names out of the hat
# and draw them with their own regression line (blue) over the true line
# (dashed). Run the two commands again and again: every time other
# students, and every time another blue line. Sometimes it is steeper
# than the dashed line, sometimes flatter, now and then it even falls.
# (The numbers further down do not change when you do this.)
another_ten <- all_students[sample(20000, 10), ]
ggplot(all_students, aes(x = x, y = y)) +
  geom_point(colour = "grey55", size = 0.3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black",
              linetype = "dashed") +
  geom_point(data = another_ten, colour = "blue", size = 2) +
  geom_smooth(data = another_ten, method = "lm", se = FALSE,
              colour = "blue", fullrange = TRUE) +
  labs(x = "Questions correct (invented)",
       y = "Attitude to tests (invented)")

# The slope of the blue line you are looking at.
coef(lm(y ~ x, data = another_ten))["x"]


# ---- S5.4: the law of large numbers ----

# One slope can be far off. What about the average of many?
# cumsum() is a running total: the first slope, the first two added up,
# the first three added up, and so on. Dividing by 1, 2, 3, ... turns
# each running total into the average of the slopes so far.
student_running_average <- cumsum(student_slopes) / (1:1000)

# Plot that running average. At the start it jumps around. The more
# samples we add, the calmer it gets.
plot(student_running_average, type = "l",
     xlab = "Number of samples", ylab = "Average slope so far")

# The dashed line is the true slope. The running average settles on it.
abline(h = true_student_slope, lty = 2)

# The average of all 1,000 slopes. Compare it with the true slope:
# the recipe is right on average. That is what "unbiased" means.
mean(student_slopes)


# ---- S5.5: the central limit theorem ----

# A histogram sorts the 1,000 slopes into bins and counts how many fall
# into each bin. Most slopes land near the true slope, few far away.
# The pile has the shape of a bell.
hist(student_slopes, breaks = 40,
     main = "1,000 slopes from samples of 10", xlab = "Slope")

# The dashed line marks the true slope: the middle of the bell.
abline(v = true_student_slope, lty = 2)

# The smallest and the largest of the 1,000 slopes.
range(student_slopes)


# ---- S6.1: the standard error ----

# How wide is the bell? sd() gives the typical distance of a slope from
# the middle. This width has its own name: the standard error. It tells
# us how far off a slope from one sample of ten typically is.
sd(student_slopes)

# What share of the 1,000 slopes lies within two standard errors of the
# true slope? Read from the inside out:
#   student_slopes - true_student_slope   how far each slope is off
#   abs(...)                              ignore the sign
#   ... < 2 * sd(student_slopes)          TRUE if within two widths
#   mean(...)                             the share of TRUEs
# It should be close to 0.95: about 95 in 100.
mean(abs(student_slopes - true_student_slope) < 2 * sd(student_slopes))


# ---- S6.2: one sample is all we ever have ----

# In real life we cannot draw 1,000 samples. We have one. R can still
# estimate the width of the bell from that one sample: from how far the
# dots lie from the line, and from how many dots there are.
# summary() prints it in the column "Std. Error", in the row "x".
# Compare it with sd(student_slopes) above: it should be close.
summary(lm(y ~ x, data = ten_students))


# ---- S6.3 to S6.5: the real ten students ----

# Back to the ten real students (Anna to Paolo). In the row "x":
#   Estimate    the slope we built by hand
#   Std. Error  how far off such a slope typically is
#   t value     Estimate divided by Std. Error: how many standard
#               errors the slope is away from 0
#   Pr(>|t|)    the p-value: if score and attitude had nothing to do
#               with each other, how often would ten students show a
#               slope at least this far from 0, by luck alone?
summary(m_students)

# The 95% confidence interval, in the row "x": the range of slopes that
# fit these ten students. If 0 lies inside the range, "no relationship
# at all" is still compatible with the data.
confint(m_students)
