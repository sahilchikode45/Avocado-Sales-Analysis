avocado <- read.csv("................../avocado.csv")
head(avocado)
as.tibble(avocado)

# X(4046(s), 4225(L), 4770(XL)) are individual avocados sales by size in a week.
# Bag columns(small, large, Xlarge) are avocados sold in packaged bags in a week.
# Total volume is sum of all individual avocados + all bagged avocados sold in a week.

# According to my calculation, after adding all individual and bags, I conclude that 
# small_bag, large_bag, xl_bag columns represent no. avocado sold that were packaged in those bags, not no. of bags.

n_distinct(avocado)
nrow(avocado)

unique(avocado$region)
unique(avocado$type)

is.na(avocado)
any(is.na(avocado))                # There is no NA values in data set

skim_without_charts(avocado)

conventional_avocado <- filter(avocado, type == "conventional")
mean(conventional_avocado$Total_Bags)
sum(conventional_avocado$Total_Volume)

organic_avocado <- filter(avocado, type == "organic")
mean(organic_avocado$Total_Bags)
sum(organic_avocado$Total_Volume)

filterd_year <- avocado %>% group_by(year) %>% 
  summarise(avg_toalPrice = mean(AveragePrice),
            avg_totalVolume = mean(Total_Volume),
            avg_totalBags = mean(Total_Bags),
            sum_totalVolume = sum(Total_Volume),
            sum_totalBags = sum(Total_Bags))

ggplot(data = avocado, aes(x=year, y=Total_Volume)) + geom_bar(stat = "identity", fill = 'skyblue')
# The hypothesis was correct till 2017, but at 2018 the sales appears lower in chart, that's because
# only 3 months of 2018 included in the data set. 
# If we adjust for missing months, sales of 2018 would actually surpass 2017, continuing the growth trend.

ggplot(data = avocado, aes(x=Date, y=Total_Volume)) + geom_bar(stat = "identity", fill = 'skyblue')

ggplot(data = filterd_year, aes(x=year, y=sum_totalBags)) + geom_bar(stat = "identity", fill = 'skyblue')

ggplot(data = avocado) + geom_point(mapping = aes(x=AveragePrice, y=Total_Volume, colour = type))
# The overall relationship for both is negative, the general trend is as avg_price increases, total_volume sales decreases.

ggplot(data = avocado) + geom_bar(mapping = aes(x=Total_Volume, colour = 'orange')) +
  facet_wrap(~type)

total_loose <- avocado %>% mutate(total_loose = X4046 + X4225 + X4770)

avocado_long <- total_loose %>% select(Date, region, type, Total_Bags, total_loose) %>% 
  pivot_longer(cols = c(total_loose, Total_Bags),
               names_to = "Category", values_to = "Volume")
# in wide format(total_loose, total_bags as separate columns), ggplot could need multiple geom_line() or geom_bar calls.
# in long format each row is one observation and each column is variable, this makes plot more flexible.

ggplot(data = avocado_long, aes(x=Date, y=Volume, colour = Category)) +
  geom_bar(stat = 'identity') + labs(title = "Loose Vs. Bagged Avocado sales", y='Avocado Sold') +
  theme_minimal()
# Date on X-axis because stakeholder asks about customer shifting form loose to bagged (shift means change over time)

ggplot(data = avocado_long, aes(x=Category, y=Volume, colour = Category)) +
  geom_bar(stat = 'identity') + labs(title = "Loose Vs. Bagged Avocado sales", y='Avocado Sold') +
  theme_minimal()
# Category on X-axis is for overall comparison between loose and bagged.

region_analysis <- avocado %>% group_by(region) %>% 
  summarise(sum_totalVolume = sum(Total_Volume),
            sum_totalBags = sum(Total_Bags)) %>% 
  arrange(-sum_totalVolume)
print(region_analysis, n=54)

ggplot(data = region_analysis, aes(x=region, y=sum_totalVolume)) + geom_bar(stat = 'identity', fill = 'olivedrab') +
  coord_flip() + theme_minimal()

type_analysis <- avocado %>% group_by(type) %>% select(AveragePrice, year, Total_Volume, Total_Bags)

ggplot(data = type_analysis, mapping = aes(x=type, y=AveragePrice, colour = type)) +
  geom_point() +
  labs(title = "Conventional vs. Organic Avocados")

ggplot(data = type_analysis, mapping = aes(x=type, y=Total_Volume, colour = type)) +
  geom_point() +
  labs(title = "Conventional vs. Organic Avocados")

size_long <- avocado %>% select(Date, AveragePrice, X4046, X4225, X4770, type, region) %>% 
  pivot_longer(cols = c(X4046, X4225, X4770),
               names_to = 'Category', values_to = 'Volume')

ggplot(data = size_long, aes(x=Category, y=Volume, colour = Category)) + 
  geom_point() + labs(title = "Avocado Size Comparison") + theme_minimal()

ggplot(data = size_long, aes(x=Category, y=Volume, colour = Category)) + 
  geom_bar(stat = 'identity') + labs(title = "Avocado Size Comparison") + theme_minimal()




