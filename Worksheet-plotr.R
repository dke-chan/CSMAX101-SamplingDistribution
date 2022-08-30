library(ggplot2); library(tidyr)

x11()

first <- numeric(100) -> second

for (i in 1:100) {
  first[i] = mean(president.df[sample(1:46, 5), 3])
  second[i] = mean(president.df[sample(1:46, 15), 3])
}

form.data = data.frame(ids = 1:100, n.05 = first, n.15 = second)
# form.data = read.csv(file.choose())

colnames(form.data)[2:3] = c("Sample Size = 5", " Sample Size = 15")

form.data.flip = form.data |>
  pivot_longer(cols = colnames(form.data)[2:3])

form.data.flip$name = factor(form.data.flip$name, levels = c("Sample Size = 5", " Sample Size = 15"))

ggplot(data = form.data.flip, aes(x = value)) +
  geom_histogram(aes(y=..ndensity..), bins = 15, fill = "lightblue", col = "black") + 
  facet_wrap(~ name) +
  theme_bw() +
  geom_vline(xintercept = 70.83, col = "red", size = 2) +
  xlab("Sample Mean Height (in)") +
  ylab("Frequency")
