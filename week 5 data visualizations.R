#load data file
insurance_data <- read.csv("C:/temp/Insurance data.csv", header = TRUE, sep = ",")
insurance_data <- insurance_data %>%
  mutate(ed_cat = factor(ed_cat, 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("No HS Degree", 
                                    "HS Degree", 
                                    "Some College", 
                                    
                                    
                                    "UG Degree", 
                                    "Post-UG Degree")))
#Plot 1
insurance_data <- insurance_data %>%
  group_by(retire, ed_cat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  group_by(retire) %>% 
  mutate(percentage = (count / sum(count)) * 100) 
palette <- brewer.pal(3, "Set2")
p <- ggplot(data = insurance_data, 
            mapping = aes(x = ed_cat, y = percentage, fill = factor(retire, labels = c("Not Retired", 
                                                                                       "Retired")))) +
  geom_bar(
    position = position_dodge(width = 0.8),
    stat = "identity", 
    alpha = 0.8
  ) + 
  scale_fill_manual(
    values = palette[1:2],
    name = "Retired Status"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  labs(
    x = "Education Level",
    y = "Percentages",
    title = "Education by Retired Status",
    subtitle = "Distribution of education levels for retired status ",
    caption = "Source: Insurance Data"
    
    
  ) +
  theme(legend.position = "top", panel.background = element_blank(), 
        plot.background = element_blank()) 

#Plot 2
palette <- brewer.pal(5, "Set2")
p <- ggplot(data = insurance_data, 
            aes(x = reorder(ed_cat, -claim_amount), y = claim_amount, fill = factor(ed_cat))) +
  geom_col(alpha = 0.9) + 
  scale_fill_manual(
    values = palette,
    name = "Education Category"
  ) +
  labs(
    x = "Education Category",
    y = "Cost of Claim (in thousands)",
    fill = "Education Level",
    title = "Claim Amounts by Education Category",
    subtitle = "Distribution of Claims across Education Levels",
    caption = "Source: Insurance Data"
  ) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) + 
  theme(legend.position = "top", panel.background = element_blank(), 
        plot.background = element_blank()) +
  guides(fill = "none")

#Plot 3
insurance_data <- insurance_data %>%
  group_by(marital, ed_cat) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  
  
  
  group_by(marital) %>% 
  mutate(pct = count / sum(count) * 100) 
insurance_data$marital <- factor(insurance_data$marital,
                                 levels = c(0, 1),
                                 labels = c("Not Married", "Married"))
p_xlab <- "Education Category"
p_title <- "Distribution of Education Levels by Marital Status"
p_subtitle <- "Percentage of each education level within marital groups"
p_caption <- "Source: Insurance Data"
palette <- brewer.pal(3, "Set2") 
p <- ggplot(data = insurance_data,
            mapping = aes(x = ed_cat, y = pct, fill = marital)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = palette, name = "Marital Status") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  labs(
    x = p_xlab,
    y = "Percentage",
    title = p_title,
    subtitle = p_subtitle,
    caption = p_caption
  ) +
  facet_wrap(~ marital, ncol = 1) + 
  coord_flip() + 
  theme_minimal() + 
  theme(strip.text.x = element_text(face = "bold"))