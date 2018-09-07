library(dplyr)

#######################
## SIMPLE PROCESSING ##
#######################

factorToNum <- function(x){
    # Convert a factor to number
    # For use in a vectorised operation
    #
    # Args:
    #  x: single factor value to convert
    #
    # Returns:
    #  y: Converted factor
    
    y <- as.numeric(as.character(x))
    return(y)
}


##################
## READING DATA ##
##################

readCommuterFlows <- function(file_path){
    # Read Stats NZ commuter flow data
    # Extracted and manually edited
    # With 2006 and 2013 data
    #
    # Args:
    #  file_path: relative path to ipynb file
    #
    # Returns:
    #  commuter flow dataset with sensible column names
    
    df <- read.csv(file_path)
    colnames(df) <- c('to_code', 'to_name', 'flow_type', 'year_2006',
                      'year_2013', 'from_code', 'from_name')

    df$year_2006 <- as.numeric(as.character(df$year_2006));
    df$year_2013 <- as.numeric(as.character(df$year_2013));

    return(df)
}


#####################
## PROCESSING DATA ##
#####################

cleanIncomeData <- function(income_file_path){
    # Specific function to clean XLS --> csv converted Stats NZ  census data
    #
    # Args:
    #  income_file_path:
    #
    # Returns:
    #  income_df: cleaned dataframe
    
    income_vals <- income_df[,5:19] # for 2013
    salary_levels <- colnames(income_vals)
    income_vals <- as.data.frame(sapply( income_vals, factorToNum ));
    income_vals$statistical_area <- income_df$rowname

    return(income_vals)

}


calculateMeanIncome <- function(income_vals){
    # Calculate mean personal income for brackets in 2013 census data
    # This is currently hardcoded. The ideal wuld be to get the values
    # from the column names
    #
    # Args:
    #  income_vals:
    #
    # Returns:
    #  mean_income_df

    # Generate dataframe of numeric income values
    income_numeric <- data.frame(income_mid = c(0, seq(2500, 40000, 5000),
                                                seq(45000, 70000, 10000), 90000, 125000, 200000))
    income_numeric$salary_levels <- salary_levels

    # Melt data by statistical area
    income_by_statistical_area <- melt(income_vals)
    income_by_statistical_area <- inner_join(income_by_statistical_area, 
                                             income_numeric,
                                             by = c("variable" = "salary_levels"))

    # Summarise number of commuters from each statistical area
    people <- income_by_statistical_area  %>%
        group_by(statistical_area) %>%
        summarise(total = sum(value))

    # Calculate mean income from bracket 
    mean_income_df <- income_by_statistical_area %>% 
        filter(statistical_area %in% df_clean$to_name) %>% 
        group_by(statistical_area) %>% 
        summarise(total_calc_income = sum(value * income_mid)) %>% 
        inner_join(people) %>% 
        mutate(mean_calc_income = total_calc_income / total) %>% 
        arrange(desc(mean_calc_income)) 

    mean_income_df$statistical_area <- as.factor(mean_income_df$statistical_area)

    return(mean_income_df)
}


labelledPairwiseDistance <- function(df_distance, geocode_map, names_map){
    # Add Geocode and Statistical Area names to pairwise distance matrix
    # Args:
    #  df_distance: pairwise distance matrix
    #  geocode_map: map from area unit name --> area unit code
    #  names_map_map: map from numeric sequence --> area unit name
    #
    # Returns:
    #  df_distance: labelled pairwise distance matrix 
    
    # Clean up pairwise distance dataframe
    # Join both To and From columns to names_map lookup DF
    df_distance <- inner_join(df_distance, names_map, by=c("Var1"= "num"))
    df_distance <- inner_join(df_distance, names_map, by=c("Var2"= "num"))

    # Clean up To and From column names_map
    colnames(df_distance) <- c('Var1', 'Var2', 'distance', 'from', 'to')

    # Join to geo_code df
    df_distance <- inner_join(df_distance, geocode_map, by=c("from"= "geo_code"))
    df_distance <- inner_join(df_distance, geocode_map, by=c("to"= "geo_code"))

    # Remove unnecessary numeric ID columns
    df_distance <- df_distance %>% dplyr::select(-c(Var1, Var2))
    colnames(df_distance) <- c( 'distance', 'from', 'to', 'Orig_Code', 'Dest_Code')

    # Final cleanup: Order DF such that geo_code columns are at the start
    # The geo_code ordering is important for the od2line function
    # Remove self-loop flows
    df_distance <- df_distance %>% 
        dplyr::select(Orig_Code, Dest_Code, distance, from, to) %>%
        filter(from != to)

    return(df_distance)
}


dataForFlowModel <- function(df_distance_clean, df_flow, orig_features, dest_features){
    # Add Geocode and Statistical Area names to pairwise distance matrix
    # Args:
    #  df_distance_clean: pairwise distance matrix with non-numeric IDs
    #  df_flow: Manual extraction from Commuter Viz. 
    #  orig_features: df of commuter flows
    #  dest_features: df of destination features 'statistical_area' key
    #
    # Returns:
    #  df_model: data ready for spatial flow modelling

    # Join to destination features dataset
    df_model <- inner_join(df_distance_clean,
                          dest_features %>% select(-total),
                          by=c("to" = "statistical_area"))

    # commuting population number from flow dataset 
    df_model <- inner_join(df_model,
                          orig_features,
                          by=c("from" = "from_name"))

    # Join to spatial flows dataset
    df_model <- inner_join(df_model, 
                          df_flow %>% select(-to_code, -from_code), 
                          by=c("from" = "from_name", "to" = "to_name"))

    # Clean dataset 
    df_model <- df_model %>% 
        select(-year_2006) %>%  
        filter(!is.na(year_2013)) %>% 
        filter(!is.na(mean_calc_income))

    return(df_model)
}

