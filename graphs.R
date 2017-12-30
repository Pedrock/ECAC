require(ggplot2)
require("RPostgreSQL")
library(scales)
library(dplyr)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "ecac",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "postgres")

dbSendQuery(con, "set search_path = 'import';")

crimes_data <- dbGetQuery(con, 
"SELECT 
	no__of_commited_crimes_96::int,
	no__of_commited_crimes_95::int,
  no__of_commited_crimes_96::int > no__of_commited_crimes_95::int AS has_increased
FROM district
WHERE no__of_commited_crimes_95 <> '?'")

ggplot(crimes_data, aes(x=no__of_commited_crimes_95,
                        y=no__of_commited_crimes_96,
                        color=has_increased)) +
  geom_point() +
  scale_x_log10(labels = comma, limits=c(800, 100000)) +
  scale_y_log10(labels = comma, limits=c(800, 100000)) +
  theme(legend.position = "none") +
  geom_abline(slope=1) +
  labs(title="Crimes", x="Crimes 95", y="Crimes 96")

#####################

unemploymant_data <- dbGetQuery(con, 
"SELECT 
  unemploymant_rate_96::real,
  unemploymant_rate_95::real,
  unemploymant_rate_96::real > unemploymant_rate_95::real AS has_increased
FROM district
WHERE unemploymant_rate_95 <> '?'")

ggplot(unemploymant_data, aes(x=unemploymant_rate_95,
                        y=unemploymant_rate_96,
                        color=has_increased)) +
  geom_point() +
  theme(legend.position = "none") +
  geom_abline(slope=1) +
  labs(title="Unemployment", x="Unemployment 95", y="Unemployment 96")

income_distribution <- dbGetQuery(con, 
"WITH month_transactions AS
(
  SELECT
    account_id,
    substring(date FROM 1 FOR 4) AS month,
    COALESCE(SUM(amount::REAL) FILTER (WHERE type = 'credit' AND k_symbol <> 'interest credited'), 0) AS income
  FROM (
    SELECT * FROM trans_train
    UNION ALL SELECT * FROM trans_test
  ) trans
  GROUP BY (account_id, substring(date FROM 1 FOR 4))
)
SELECT
  account_id,
  CASE WHEN substring(co.birth_number FROM 3 FOR 2)::int >= 50 THEN 'female' ELSE 'male' END AS owner_sex,
  AVG(income) avg_income
FROM month_transactions
INNER JOIN disp USING (account_id)
INNER JOIN client AS co ON (co.client_id = disp.client_id AND disp.type = 'OWNER')
GROUP BY (account_id, co.birth_number);")

ggplot(income_distribution, aes(avg_income, color=owner_sex, fill=owner_sex)) +
  geom_density(alpha=0.5) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = scales::percent) +
  guides(color = FALSE, fill=guide_legend(title="Client Gender")) +
  labs(title="Income Distribution by Gender", x="Income", y="Density")


card_type_distribution <- dbGetQuery(con, 
"SELECT card.type, count(card.type) AS count
FROM ( SELECT * FROM card_train UNION ALL SELECT * FROM card_test) card
LEFT JOIN disp USING (disp_id)
LEFT JOIN client USING (client_id)
GROUP BY card.type;")

card_type_distribution <- card_type_distribution %>% 
  mutate(per=`count`/sum(`count`), midpoint = cumsum(per) - per / 2) %>% 
  arrange(midpoint)
card_type_distribution$label <- scales::percent(card_type_distribution$per)

ggplot(card_type_distribution, aes(x = "", y = per, fill = type)) + 
  geom_bar(width = 1, stat="identity") + 
  geom_text(aes(label = label, x=1.1), size=3, position = position_stack(vjust = 0.5))  + 
  coord_polar(theta = "y") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    axis.text.x=element_blank()
  ) +
  guides(fill=guide_legend(title="Card Type")) +
  labs(title="Card Types")



