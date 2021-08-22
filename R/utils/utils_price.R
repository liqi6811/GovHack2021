library(data.table)

sa1_water = fread("../data/sa1_to_water_corporation.csv", key="SA1_CODE21", colClasses = c(SA1_CODE21="character"))
persons = fread("../data/sa2_persons_water.csv", key="sa2", colClasses = c(sa2="character"))

get_company = function(sa1_code) {
    sa1_water[sa1_code, water_corporation][1]
}

get_usually_resident = function(sa2) {
    #sa2 = substr(sa1_code, 1, 7)
    persons[sa2, persons]
}

price = function(sa1, company, consumption, cluster) {
    # price = tier*tier_prices, tier_price = default_person*equity_cluster*survey_q

    tier_prices = c(1.0, 2.0)
    cutoffs = c(200)
    remaining_consumption = consumption
    cost = 0.
    for (i in seq_along(tier_prices)) {
        bucket = ifelse(i > length(cutoffs), remaining_consumption, min(remaining_consumption, cutoffs[i]))
        rate = tier_prices[i]
        # now apply adjustments according to questions
        # TODO

        cost = cost + (bucket*tier_prices[i])
        remaining_consumption = remaining_consumption-bucket
        if (remaining_consumption <= 0)
            break
    }

    cost
}
