
''' Identifying the threshold cost value to generate Expensive attribute

Here we will fixate on a cost value to create the expensive attribute column. Based on the statistical summary of the 'cost' attribute, we are picking the 3rd Quartile value for the cost column as the threshold value.'''

lm_hmo1 <- lm(cost ~ age + smoker + exercise + bmi + hypertension + yearly_physical, data = HMO_data_new)
summary(lm_hmo1)
#Using linear regression model, we can predict the cost variable with almost 57% accuracy using the predictors such as age, smoker, exercise, bmi and hypertension.

#Next, we set the threshold for a person to be termed as 'expensive' as the cost value greater
#than the 75th percentile cost value (4748.00). All other records will be viewed as 'non-expensive'
HMO_t_quar <- quantile(HMO_data_new$cost, 0.75)
HMO_data_new$expensive <- ifelse(HMO_data_new$cost >= HMO_t_quar, 1, 0)
head(HMO_data_new)

#Converting the expensive variable into two level factor variable to run regression on it
HMO_data_new$expensive <- as.factor(HMO_data_new$expensive)

#Here, we use maps to display the states as expensive/non-expensive based on overall state average.
HMO_data_new_with_states <- merge(HMO_data_new, states, all.y=TRUE, by.x="location", by.y="region")

HMO_data_new_with_states <- HMO_data_new_with_states %>% arrange(order)

ggplot(HMO_data_new_with_states) + geom_polygon(color="black",
                 aes(x=long,y=lat,group=group,fill=expensive)) +
        ggtitle('Expensive vs non-expensive states') +
coord_map()

