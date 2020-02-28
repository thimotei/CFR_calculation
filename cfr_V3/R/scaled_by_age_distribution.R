# age distributed CFR estimates

# Wuhan data

age_scaled_data_wuhan_new_cases <- scale_by_age_distribution_cases(allTogetherInferred, wuhan_SFs)
age_scaled_data_wuhan_new_deaths <- scale_by_age_distribution_deaths(allTogetherInferred, wuhan_SFs)

age_scaled_wuhan_data_range_1 <- data.frame(date = allTogetherInferred$date, 
                                    new_cases = round(age_scaled_data_wuhan_new_cases[,1]),
                                    new_deaths = round(age_scaled_data_wuhan_new_deaths[,1]))

age_scaled_wuhan_data_range_2 <- data.frame(date = allTogetherInferred$date, 
                                            new_cases = round(age_scaled_data_wuhan_new_cases[,2]),
                                            new_deaths = round(age_scaled_data_wuhan_new_deaths[,2]))

age_scaled_wuhan_data_range_3 <- data.frame(date = allTogetherInferred$date, 
                                            new_cases = round(age_scaled_data_wuhan_new_cases[,3]),
                                            new_deaths = round(age_scaled_data_wuhan_new_deaths[,3]))

age_scaled_wuhan_data_range_4 <- data.frame(date = allTogetherInferred$date, 
                                            new_cases = round(age_scaled_data_wuhan_new_cases[,4]),
                                            new_deaths = round(age_scaled_data_wuhan_new_deaths[,4]))

age_scaled_wuhan_data_range_5 <- data.frame(date = allTogetherInferred$date, 
                                            new_cases = round(age_scaled_data_wuhan_new_cases[,5]),
                                            new_deaths = round(age_scaled_data_wuhan_new_deaths[,5]))

age_scaled_wuhan_data_range_6 <- data.frame(date = allTogetherInferred$date, 
                                            new_cases = round(age_scaled_data_wuhan_new_cases[,6]),
                                            new_deaths = round(age_scaled_data_wuhan_new_deaths[,6]))

# cruise ship data


age_scaled_data_cruise_new_cases <- scale_by_age_distribution_cases(cruise_ship_by_confirmation, cruise_ship_SFs)
age_scaled_data_cruise_new_deaths <- scale_by_age_distribution_deaths(cruise_ship_by_confirmation, cruise_ship_SFs)

age_scaled_cruise_data_range_1 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,1]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,1]))

age_scaled_cruise_data_range_2 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,2]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,2]))

age_scaled_cruise_data_range_3 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,3]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,3]))

age_scaled_cruise_data_range_4 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,4]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,4]))

age_scaled_cruise_data_range_5 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,5]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,5]))

age_scaled_cruise_data_range_6 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,6]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,6]))

age_scaled_cruise_data_range_7 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,7]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,7]))

age_scaled_cruise_data_range_8 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = age_scaled_data_cruise_new_cases[,8],
                                            new_deaths = age_scaled_data_cruise_new_deaths[,8])

age_scaled_cruise_data_range_9 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,9]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,9]))

age_scaled_cruise_data_range_10 <- data.frame(date = cruise_ship_by_confirmation$date, 
                                            new_cases = round(age_scaled_data_cruise_new_cases[,10]),
                                            new_deaths = round(age_scaled_data_cruise_new_deaths[,10]))
