kstarter <- df_nc

table(kstarter$category_parent_name)

ggplot(data = kstarter, aes(y = category_parent_name)) +
  geom_bar()

ggplot(data = kstarter, aes(y = fct_rev(fct_infreq(category_parent_name)))) +
  geom_bar() +
  theme_bw() +
  labs(
    y = "Kickstarter Categories",
    x = "Number of Projects"
  )

kstarter |> 
  filter(category_parent_name == "Music") |> 
  ggplot(aes(x = backers_count)) +
    geom_bar() +
    theme_bw()

kstarter |> 
  filter(category_parent_name == "Music") |> 
  ggplot(aes(x = backers_count)) +
    geom_histogram(bins = 100) +
    theme_bw()

library(gridExtra)
kstarter_m <- kstarter |> filter(category_parent_name == "Music")
p1 <- ggplot(data=kstarter_m,aes(x=backers_count))+geom_histogram(bins=10)+theme_bw()+ggtitle("10 bins")
p2 <- ggplot(data=kstarter_m,aes(x=backers_count))+geom_histogram(bins=50)+theme_bw()+ggtitle("50 bins")
p3 <- ggplot(data=kstarter_m,aes(x=backers_count))+geom_histogram(bins=100)+theme_bw()+ggtitle("100 bins")
grid.arrange(p1,p2,p3,nrow=1)

kstarter |> 
  filter(category_parent_name == "Music") |> 
  ggplot(aes(x = log10(backers_count))) +
    geom_histogram(bins = 100) +
    theme_bw()

kstarter |> 
  filter(category_parent_name == "Music") |> 
  ggplot(aes(x = backers_count, y = category_name)) +
    geom_boxplot() +
    theme_bw()

kstarter |> 
  ggplot(aes(x = backers_count, y = pledged)) +
    geom_point() +
    theme_bw()

kstarter |> 
  ggplot(aes(x = backers_count, y = pledged)) +
    geom_point(alpha = 0.08) +
    theme_bw()

p1 <- kstarter |> 
  filter(backers_count < 5000, pledged < 500000) |> 
  ggplot(aes(x = backers_count, y = pledged)) +
    geom_point() +
    theme_bw()

p2 <- kstarter |> 
  filter(backers_count < 5000, pledged < 500000) |> 
  ggplot(aes(x = backers_count, y = pledged)) +
    geom_point(alpha = 0.08) +
    theme_bw()

grid.arrange(p1, p2)

table(kstarter$category_parent_name, kstarter$goal_exceeded)

kstarter |>
  ggplot(aes(x = goal_exceeded, y = category_parent_name)) +
  geom_count() +
  scale_size_area(max_size = 8) +
  theme_bw()

kstarter |> 
  filter(category_parent_name == "Publishing") |> 
  group_by(category_name) |>
  summarize(mean_goal = mean(goal), median_goal = median(goal))

kstarter |> 
  filter(category_parent_name == "Publishing") |> 
  ggplot(aes(x = goal, y = category_name)) + 
    geom_boxplot() +
    theme_bw()

kstarter |> 
  filter(category_parent_name == "Publishing") |> 
  ggplot(aes(x = goal, y = reorder(category_name, goal, mean))) + 
  geom_boxplot() +
  theme_bw()

kstarter |> 
  filter(category_parent_name == "Publishing") |> 
  ggplot(aes(x = goal)) + 
  geom_histogram(bins = 50) +
  facet_wrap(vars(category_name)) +
  theme_bw()

kstarter |> 
  filter(category_parent_name == "Publishing") |> 
  ggplot(aes(x = goal, color = category_name)) +
    geom_freqpoly(size = 1)+
    theme_bw()

# geom tile
# correlation matrix
# gtsummary
# hmisc
# summarytools
