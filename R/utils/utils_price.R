library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)


sa1_water = fread("../data/SA1_2016_to_water_corporation_centroids.csv", key="SA1_MAIN16", colClasses = c(SA1_MAIN16="character"))
persons = fread("../data/sa2_persons_water.csv", key="sa2", colClasses = c(sa2="character"))
print(head(sa1_water))

get_company = function(sa1_code) {
    sa1_water[sa1_code, water_corporation][1]
}

get_usually_resident = function(sa2) {
    #sa2 = substr(sa1_code, 1, 7)
    persons[sa2, persons][1]
}


base_df <- tibble::tribble(
                       ~retailer,   ~base,
                 "Western Water",  207.48,
               "City West Water",  207.56,
            "Yarra Valley Water",   78.19,
              "South East Water",   93.12,
                  "Barwon Water",  137.32,
       "Central Highlands Water",  208.17,
                 "Coliban Water",  224.69,
          "East Gippsland Water",  214.61,
               "Gippsland Water",  172.71,
         "Goulburn Valley Water",  162.12,
  "Grampian Wimmera Malle Water",  428.28,
            "Lower Murray Water",  207.36,
              "North East Water",  227.83,
         "South Gippsland Water",  334.66,
                  "Wannon Water", 236.815,
             "Westernport Water",   391.6
)

  
rate_df <- tibble::tribble(
                         ~retailer, ~tier, ~upper_limit, ~charge_per_kl,
                   "Western Water",    1L,          440,         1.9954,
                   "Western Water",    2L,          880,         2.6475,
                   "Western Water",    3L,          Inf,         3.7493,
                 "City West Water",    1L,          440,         2.7562,
                 "City West Water",    2L,          Inf,         3.2112,
              "Yarra Valley Water",    1L,          440,         2.4749,
              "Yarra Valley Water",    2L,          880,         3.1383,
              "Yarra Valley Water",    3L,          Inf,         4.6238,
                "South East Water",    1L,          Inf,         3.4307,
                    "Barwon Water",    1L,          Inf,         2.0897,
         "Central Highlands Water",    1L,          175,           1.98,
         "Central Highlands Water",    2L,          Inf,           2.38,
                   "Coliban Water",    1L,          Inf,         2.2178,
            "East Gippsland Water",    1L,          Inf,         2.2511,
                 "Gippsland Water",    1L,          Inf,         2.1434,
           "Goulburn Valley Water",    1L,          Inf,         1.1158,
    "Grampian Wimmera Malle Water",    1L,          Inf,         1.7115,
              "Lower Murray Water",    1L,           50,         0.4503,
              "Lower Murray Water",    2L,          100,         0.8195,
              "Lower Murray Water",    3L,          Inf,         1.0534,
                "North East Water",    1L,          Inf,         2.3558,
           "South Gippsland Water",    1L,          Inf,         2.0227,
                    "Wannon Water",    1L,          438,        1.26535,
                    "Wannon Water",    2L,          822,        1.93015,
                    "Wannon Water",    3L,          Inf,         2.8954,
               "Westernport Water",    1L,          Inf,         2.0262
  )

new_rate_df <- rate_df %>% 
  group_by(retailer) %>%  
  filter(tier == min(tier)) %>%
  select(retailer, charge_per_kl) %>%
  mutate(charge_per_kl = if_else(retailer == "Lower Murray Water", 0.8, charge_per_kl),
         charge_per_kl.60 = 0.8,
         charge_per_kl.80 = charge_per_kl.60 * 1.25,
         charge_per_kl.Inf = charge_per_kl.80 * 2) %>%
  pivot_longer(cols = starts_with("charge"), names_prefix = "charge_per_kl.", names_to = "upper_limit") %>%
  mutate(upper_limit = as.numeric(upper_limit))


avg_persons_df <- tibble::tribble(
                     ~retailer, ~persons,
                "Barwon Water",       2L,
     "Central Highlands Water",       2L,
             "City West Water",       3L,
               "Coliban Water",       2L,
        "East Gippsland Water",       2L,
             "Gippsland Water",       2L,
       "Goulburn Valley Water",       2L,
"Grampian Wimmera Malle Water",       2L,
          "Lower Murray Water",       2L,
            "North East Water",       2L,
            "South East Water",       3L,
       "South Gippsland Water",       2L,
                "Wannon Water",       2L,
               "Western Water",       3L,
           "Westernport Water",       2L,
          "Yarra Valley Water",       3L
)

old_price <- function(my_retailer, my_consumption, rate = rate_df, base = base_df){
  
  base_amt <- base[base$retailer == my_retailer, ]$base
  
  retailer_rates <- rate_df %>%
    filter(retailer == my_retailer)
  
  base_amt + sum(diff(c(0, pmin(my_consumption, retailer_rates$upper_limit))) * retailer_rates$charge_per_kl)
}

# In prod: connect cluster to clustering outputs from python
new_price = function(
    my_retailer, my_consumption, avg_persons,
    cluster = 1, has_rainwater = F, has_pool = F
) {
  # price = tier*tier_prices, tier_price = default_person*equity_cluster*survey_q
  upper_limit_per_person  = c(60, 80, Inf)
  upper_limit = upper_limit_per_person * avg_persons

  rainwater_benefit = 0.95
  pool_deficit = 1.1

  base_amt <- base_df[base_df$retailer == my_retailer, ]$base
  
  retailer_rates <- new_rate_df %>%
    filter(retailer == my_retailer)
  
  if (!is.integer(my_consumption))
    my_consumption = 100
  rates = sum(diff(c(0, pmin(my_consumption, retailer_rates$upper_limit))) * retailer_rates$charge_per_kl)
  
  
  if(has_rainwater == "Yes") {
    rates = rates * rainwater_benefit
  }

  if(has_pool == "Yes") {
    rates = rates * pool_deficit
  }
  
  # final rate
  (base_amt + rates * cluster)
  
### need to add SA1 variable input
  
}

use_1819 <- tibble::tribble(
                      ~retailer, ~residential_customers, ~megalitre, ~kilolitre_per_residence,         ~Category,
                 "Western Water",                 61811L,     11382L,                   184L,           "Regional",
               "City West Water",                418189L,     49246L,                   118L,              "Urban",
            "Yarra Valley Water",                746925L,    112487L,                   151L,              "Urban",
              "South East Water",                702375L,    105560L,                   150L,              "Urban",
                  "Barwon Water",                148109L,     23738L,                   160L,           "Regional",
       "Central Highlands Water",                 63941L,      9996L,                   156L,           "Regional",
                 "Coliban Water",                 68343L,     13696L,                   200L,           "Regional",
          "East Gippsland Water",                 23194L,      3278L,                   141L,           "Regional",
               "Gippsland Water",                 65951L,     10642L,                   161L,           "Regional",
         "Goulburn Valley Water",                 52048L,     13770L,                   265L,           "Regional",
  "Grampian Wimmera Malle Water",                 27877L,      5784L,                   207L, "Regional and Rural",
            "Lower Murray Water",                 29862L,     14700L,                   492L, "Regional and Rural",
              "North East Water",                 46499L,      9619L,                   207L,           "Regional",
         "South Gippsland Water",                 17178L,      2034L,                   118L,           "Regional",
                  "Wannon Water",                 36649L,      5279L,                   144L,           "Regional",
             "Westernport Water",                 15485L,      1257L,                    81L,           "Regional"
)

use_1819 <- use_1819 %>%
  left_join(avg_persons_df, by = "retailer") %>%
  mutate(annual_fee_per_residence = mapply(old_price, retailer, kilolitre_per_residence),
         average_use_per_person = kilolitre_per_residence/persons) 

#ggplot(use_1819, aes(x = average_use_per_person, y = annual_fee_per_residence, colour = retailer)) + geom_point()
#ggplot(use_1819, aes(x = kilolitre_per_residence, y = annual_fee_per_residence, colour = retailer)) + geom_point()
