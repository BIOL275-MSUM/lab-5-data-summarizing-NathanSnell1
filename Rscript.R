
# Load Packages -----------------------------------------------------------

library(tidyverse)

# Read Data --------------------------------------------------------------

iris
iris <- as_tibble(iris)
iris

# Question 1 --------------------------------------------------------------

iris_new <-
  rename( iris, 
          sepal_length = Sepal.Length, 
          sepal_width = Sepal.Width, 
          petal_length = Petal.Length,
          petal_width = Petal.Width,
          species = Species)
iris_new



# Question 2 --------------------------------------------------------------


iris_mm <-
mutate(iris_new,
       sepal_length =sepal_length *10,
       sepal_width =sepal_width *10,
       petal_length =petal_length *10,
       petal_width =petal_width *10)
iris_mm       

# Question 3 --------------------------------------------------------------

iris_area <- select(iris_mm, petal_length, petal_width)
iris_area

iris_area2 <-
mutate(iris_area, area = petal_length * petal_width)

iris_area2

# Question 4 --------------------------------------------------------------

summary_sepallength <-
   summarise(iris_mm,
             sample_size =n(),
             max =max(sepal_length),
             min =min(sepal_length),
             range = max-min,
             median = median(sepal_length),
             first_quartile =quantile(sepal_length, .25),
             third_quartile =quantile(sepal_length, .75),
             IQR = IQR(sepal_length))
summary_sepallength

# Question 5 --------------------------------------------------------------
(species_grouped <- group_by(iris_mm, species))
species_grouped

summary_petalwidth <-
  summarise(species_grouped,
            sample_size =n(),
            mean =mean(petal_width),
            sd =sd(petal_width),
            var =var(petal_width),
            standard_error =mean/sqrt(sample_size),
            conf_upper =mean + 2 * standard_error,
            conf_lower =mean - 2 * standard_error)
summary_petalwidth

# Question 6 and 7--------------------------------------------------------------

ggplot(data =iris_mm)+
  geom_jitter(mapping = aes(y =petal_width, x =species))+
  geom_crossbar(data =summary_petalwidth,
  mapping = aes(x =species, y =mean, ymax =conf_upper, ymin =conf_lower),
  color = "blue")

# Question 8 --------------------------------------------------------------

summary_final <- select(iris_mm, petal_length, petal_width, species)                
ggplot(data=summary_final)+
  geom_point(mapping = aes(x =petal_length, y =petal_width, color =species))





