#### Generate Artist Schedules for a given year #####

generate_schedules <- function(artists = artists, 
                               YEAR = YEAR, 
                               sp_data = NULL,
                               path = datapath_out,
                               show_main_period = TRUE,
                               add_url = FALSE,
                               schedule_name = "schedule") {
    
    # Keep only artist from this year
    artistsYear <- artists %>%
        filter(year == YEAR)
    
    # Create Url variable equal to either streams.creditUrl or a Youtube search
    artistsYear <- artistsYear %>%
        mutate(Url = paste0('https://soundcloud.com/search/sounds?q=', 
                            gsub(" ", "+", displayName)))
    
    # Delete empty variables, i.e. those that contains only NAs
    artistsYear <- artistsYear[,colSums(is.na(artistsYear))<nrow(artistsYear)]
    
    # Add Spotify url if Spotify ID present
    if (!is.null(sp_data)) {
        artistsYear <- merge(artistsYear,
                             sp_data,
                             by = "displayName",
                             all.x = TRUE) %>%
            mutate(Url = ifelse(!is.na(sp_id), paste0("spotify://artist/", sp_id), Url))
    }

    # Function for generating schedule list as hourly interval vs festival stage
    spread_schedule_data <- function(artistsYear,
                                     show_main_period = show_main_period,
                                     add_url = add_url) {
        
        if (add_url == FALSE) {
            schedule <- artistsYear %>%
                mutate(Name = paste(time_label, displayName))
        } else {
            schedule <- artistsYear %>%
                mutate(Name = paste0(time_label,
                                     ' <a href=', Url, 
                                     ' target=_blank >',
                                     displayName, 
                                     '</a>', 
                                     sep=""))
        }
        
        schedule <- schedule %>%
            filter(main_period == show_main_period) %>%
            select(day_label, 
                   gigs.stage.name, 
                   Name, 
                   stage_hour_slot) %>%
            spread(key = gigs.stage.name, 
                   value = Name, 
                   fill="") %>%
            arrange(stage_hour_slot) %>%
            select(-stage_hour_slot)
        
        return(schedule)
    }
    
    # Generate schedule
    schedule <- spread_schedule_data(artistsYear, 
                                     show_main_period = show_main_period, 
                                     add_url = add_url)
    
    # Return the schedule data
    return(schedule)
}