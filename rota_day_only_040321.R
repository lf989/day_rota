
#day_rota
#© Lachlan Fotheringham March 2021

#clear workspace
rm(list = ls());
cat("\14");

#load fav libraries
library(dplyr)
library(magrittr)
library(tidyverse)

#specially for this project
library(DataCombine) #to read rows
library(googlesheets4) #to interact with google sheets API
library(lubridate) #for dates


get_off_days <- function(ds){
  #gets the days off for each name
  with(ds,{
    nms = colnames(ds_off_days)[-1] #names taken from here
    
    temp_table <- function(ds_off_days,nm){
      #create generic smaller table to work with dplyr function below
      t = tibble(
        day_number = ds_off_days[['day_number']],
        in_or_not = ds_off_days[[nm]]
      )
    }
    
    when_are_they_off <- function(ds_off_days,nm){
      t <- temp_table(ds_off_days,nm) #from function above
      
      t %<>%
        select(day_number, in_or_not)%>%
        filter(in_or_not==1)%>%
        select(day_number)%>%
        as.list() 
      
      return(t)
    
  }
  
  #anonymous function to apply above functions to each name
  days_off <- sapply(nms, function(x) when_are_they_off(ds_off_days, x))
  
  #names the output
  names(days_off) <- nms
  
  #add to ds
  ds$days_off <- days_off
  
  return(ds)
  }) 
}




how_many_days <- function(ds){
  
  with(ds,{
    
    #looks up database for public hols then returns as a list
    public_holidays <- get_public_holidays(ds_off_days)
    
    tot_days = nrow(rota)-length(public_holidays)
    
    #what is the minimum number of days this person should be working
    #takes into account the proportion of the total sessions they work
    
    #days_in%<>%
    #  mutate(ideal_days_on = tot_days*sessions/sum(sessions))
    
    ds$days_in %<>% 
      mutate(ideal_days_on = tot_days*sessions/sum(sessions))
    
    return(ds)
  })
}

put_name_in_rota <- function(rota, day, nm){
  #function to put someone "On Call" for a specific day
  rota%<>%
    mutate(on_call = ifelse(day_number==day,nm,on_call))
  
  return(rota)
}

check_if_can_do_day <- function(day, nm, days_in, days_off, ds_off_days, rota){
  #for a given person and day, check if they can do that day
  #look at fixed availability (days_in, or usual working days), 
  #and variable availability (days_off, or leave requests)
  
  #what day is it today
  DOTW_today <- rota%>%
    filter(day_number==day)%>%
    select(DOTW)%>%
    as.character()
  
  #extract this person's weekly plan
  nm_weekly_plan <- days_in%>%
    filter(names==nm)
  
  #can they work that day usually - ie. fixed availability
  can_do_usually <- ifelse(nm_weekly_plan[[DOTW_today]]==1,TRUE,FALSE)
  
  #are they on leave - ie. variable availability
  nm_days_off <- days_off[[nm]]
  on_leave <- ifelse(day %in% nm_days_off,TRUE,FALSE)
  
  #is it a public holiday
  public_hol_today <- ds_off_days %>%
    filter(day_number==day)%>%
    select(public_holidays)%>%
    as.logical()
  
  #are they both in usually on that day, and not on leave
  can_do <- ifelse(can_do_usually == TRUE & on_leave == FALSE & public_hol_today == FALSE,TRUE,FALSE)
  
  return(can_do)
}


who_can_do_what_days <- function(days_in, days_off, ds_off_days, rota){
  #takes the function above, check_if_can_do_day, 
  #then applies it to each name, days_in$names, and each day, 1:nrow(rota)
  combined_availability <- lapply(days_in$names,
                                  function(nm){
                                    sapply(1:nrow(rota), 
                                           function(day) {
                                             check_if_can_do_day(day,nm, days_in, days_off, ds_off_days, rota)
                                           })
                                  })
  
  names(combined_availability) <- days_in$names #give the output appropriate names
  return(combined_availability)
}


adjust_running_total_and_sort <- function(running_totals, days_in){
  #takes the running totals, ie. how many days everyone has been allocated so far,
  #then adjusts according to how many they should be doing - ideal_days_on,
  #then sorts, so the person who is due to do one next is first on the list
  running_totals %<>% unlist()
  
  adjusted <- with(days_in, running_totals/ideal_days_on)%>%
    sort()
  
  return(adjusted)
}

get_running_totals <- function(rota, days_in){
  #works out how many days people have done so far
  nms = days_in$names
  
  get_tot_for_nm <- function(nm){
    #mini-fn to work out the total for a given name
    nm_tot <- rota%>%
      filter(on_call==nm)%>%
      nrow()
  }
  
  running_totals <- lapply(
    #now work it out for all names
    nms,
    get_tot_for_nm
  )
  
  #name the list appropriately before returning
  names(running_totals) <- nms
  
  return(running_totals)
}

get_adjusted_running_totals <- function(rota, days_in){
    #combines the running total, and adjusted running totals function
    
    running_totals <- get_running_totals(rota, days_in)
    
    running_totals %<>% adjust_running_total_and_sort(days_in) #adjust for sessions 
    
    return(running_totals)
}


allocate_days <- function(ds){
  
  with(ds, {
    # function that really does the business
    
    combined_availability <- who_can_do_what_days(days_in, days_off, ds_off_days, rota)%>%
      as_tibble()
    
    for (i in 1:nrow(rota)){
      
      #creates logical vector of who can work today
      who_can_do_today <- combined_availability[i,]%>%as.logical()
      
      #apply to names to give the names of the people who can work today
      who_can_do_today <- days_in$names[who_can_do_today]
      
      running_totals <- get_adjusted_running_totals(rota, days_in)
      
      
      for (nm in names(running_totals)){ #loop through in order of the running total (already adjusted and sorted)
        if (nm %in% who_can_do_today){ #if they can work that day
          rota <- put_name_in_rota(rota,i,nm) #write name to rota
          break #end the loop
        } # keeps going until someone is found. Will leave blank if no one is available
      }
      
    }
    
    ds$rota <- rota
    return(ds)
  })
  
}

#check
add_totals <- function(ds){
  
  with(ds, {
  
    #adds a total column so you can compare with the ideal totals
    running_totals <- get_running_totals(rota, days_in)
    
    running_tot_tab <- tibble(
      names = names(running_totals),
      total_days_on = running_totals %>% unlist()
    )
    
    days_in %<>%
      full_join(running_tot_tab, by="names")
    
    ds$days_in <- days_in
    return(ds)
  })
}

test_totals <- function(ds){
  
  with(ds, {
    #test the count, format identically
    A <- sapply(days_in$names, function(nm) rota%>%filter(on_call==nm)%>%nrow())
    
    B <- days_in%>%select(names, starts_with("total_days_on"))
    
    B <-pivot_wider(B, names_from = names, values_from = total_days_on)%>%unlist()
    
    test <- list(A=A,B=B)
    
    #if there is the same number of shifts in both counts for each name, it's a pass
    test <- with(test,
                 ifelse(A==B,TRUE,FALSE)
    )
    ifelse(all(test==TRUE)==TRUE, "pass","fail")%>%
      return()
  })
}

test_proportions <- function(proportional_load = ds$proportional_load, marg = 0.05){
  lower <- 1 - marg
  upper <- 1 + marg
  #are all the shifts within the bounds of acceptability - given by marg - passed to function
  ifelse(all(upper >= proportional_load & proportional_load >= lower)==TRUE, "pass", "fail")%>%
    return()
}

test_coverage <- function(ds){
  with(ds, {
    #is there an allocated person for each day
    ifelse(all(rota%>%select(on_call) != "" ) == TRUE, "pass","fail")%>%
      return()
  })
}



transform_to_timetable <- function(ds){
  #make the rota look like a weekly planner
  
  with(ds, {
  
    #add in blank days so week can start at monday
    rota <- rbind(c(NA, "Monday", "X"),
                  c(NA, "Tuesday", "X"),
                  rota,
                  c(NA, "Wednesday", "X"),
                  c(NA, "Thursday", "X"),
                  c(NA, "Friday", "X"))
    
    get_day <- function(rota, day){
      #gets all the people on call for a given day
      t <- tibble(
        day = rota%>%filter(DOTW==day)%>%select(on_call)
      )
      
      t<-t$day$on_call
      return(t)
    }
    
    #assembles in the format of a timetable
    tt <- tibble(
      Monday = rota%>%get_day("Monday"),
      Tuesday = rota%>%get_day("Tuesday"),
      Wednesday = rota%>%get_day("Wednesday"),
      Thursday = rota%>%get_day("Thursday"),
      Friday = rota%>%get_day("Friday")
    )%>%
      rowid_to_column("week_number")
    
    #put in a handy date column
    #takes the actual start and end dates of the rota, and adjusts the week_beginning date assuming the rota starts on a wed
    week_beginning <- week_beginning(start_date, end_date)
    
    tt %<>% mutate(week_beginning = format(week_beginning, "%d-%m-%Y"))%>%
      select(week_number, week_beginning, everything())
    
    ds$tt <- tt
    
    return(ds)
  })
}

output_rota_to_googlesheets <- function(ds,ss){
  with(ds, {
    sheet_write(data = rota, ss = ss, sheet = "rota_output")
    sheet_write(data = days_in, ss = ss, sheet = "days_in_output")
    sheet_write(data = tt, ss = ss, sheet = "timetable_output")
  })
}

generate_weekday_dates <- function(start_date, end_date){
  #generates all the dates which fall on a weekday between the designated start and end dates
  #program only works with wed as start and tue as end
  #would need tweaking otherwise
  
  #start_date = dmy("3/2/2021")
  #end_date = dmy("3/8/21")
  
  #use base R to generate all the dates between the start and end
  all_days <- seq(start_date, end_date, by="1 day")
  
  #pick out only the weekdays
  weekdays_only <- all_days[ ! weekdays(all_days) %in% c('Saturday','Sunday') ]
  
  return(weekdays_only)
}

week_beginning <- function(start_date, end_date){
  #start_date = dmy("3/2/2021")
  #end_date = dmy("3/8/21")
  start_date = start_date-2
  seq(start_date, end_date, by="7 day")
}


get_data_from_googlesheets <- function(ss = "<use own doc ID>"){
  
  ds_off_days <- read_sheet(ss, range = "off_days")%>%
    mutate(date = as.character(date))%>%
    mutate_all(~replace(., is.na(.), 0))
  
  days_in <- read_sheet(ss, range = "days_in")
  
  
  #start_date = dmy("3/2/2021")
  #end_date = dmy("3/8/21")
  
  #need list of weekday dates
  #read in start and end date from csv
  rota_dates <- ds_off_days$date%>%date()
  start_date <- rota_dates[1]
  end_date <- rota_dates[length(rota_dates)]
  weekdays <- generate_weekday_dates(start_date = start_date, end_date = end_date)
  
  
  #generate a blank rota
  rota <- tibble(
    day_number = 1:length(weekdays),
    DOTW = weekdays(weekdays),
    on_call = rep("",length(weekdays))
  ) %>% mutate(date = format(weekdays, "%d-%m-%Y")) # put in date column
  
  
  ds_out <- list(
    ds_off_days = ds_off_days,
    days_in = days_in,
    rota = rota,
    start_date = start_date,
    end_date = end_date,
    weekdays = weekdays
  )
  
  return(ds_out)
  
}

write_public_holidays <- function(ds){
  with(ds, {
    #write in public holidays
    public_hol_date_numbers <- get_public_holidays(ds_off_days)
    
    #writes to specified locations
    rota[public_hol_date_numbers,'on_call'] <- "public hol"
    
    ds$rota <- rota
    return(ds)
  })
}

get_public_holidays <- function(ds_off_days){
  public_holidays <- ds_off_days %>%
    filter(public_holidays == 1)%>%
    select(day_number)%>%
    unlist()
  return(public_holidays)
}



data_from_local <- function(){
  #use local version to avoid having to ask google over and over
  
  ds_off_days <- read_csv("R/rota/day rota - off_days.csv")%>%
    mutate(date = as.character(date))%>%
    mutate_all(~replace(., is.na(.), 0))
  
  days_in <- read_csv("R/rota/day rota - days_in.csv")
  
  #start_date = dmy("3/2/2021")
  #end_date = dmy("3/8/21")
  
  #need list of weekday dates
  #read in start and end date from csv
  rota_dates <- ds_off_days$date
  start_date <- dmy(rota_dates[1])
  end_date <- dmy(last(rota_dates))
  weekdays <- generate_weekday_dates(start_date = start_date, end_date = end_date)
  
  
  #generate a blank rota
  rota <- tibble(
    day_number = 1:length(weekdays),
    DOTW = weekdays(weekdays),
    on_call = rep("",length(weekdays))
  ) %>% mutate(date = format(weekdays, "%d-%m-%Y")) # put in date column
  
  
  ds_out <- list(
    ds_off_days = ds_off_days,
    days_in = days_in,
    rota = rota,
    start_date = start_date,
    end_date = end_date,
    weekdays = weekdays
  )
  
  return(ds_out)
}

main <- function(){
  
  #ID for google sheet to read from
  ss <- "1YiX5xLo8_qPvvsTu2VdUDVNLidUwUOf1sVruYkjGC7c"
  
  #read in data
  ds <- get_data_from_googlesheets()
  #ds <- data_from_local()
  
  #gather off days into a list
  ds %<>% get_off_days() %>%
    
    #work out how many days people should be on call ideally
    how_many_days() %>%
    
    allocate_days() %>%
    
    write_public_holidays()%>%
    
    add_totals() %>%
    
    transform_to_timetable()
  
  
  #using the results, work out how much they are contributing
  #proportional load = 1 is where their load is exactly right
  proportional_load = get_adjusted_running_totals(ds$rota, ds$days_in)
  
  #test counting method by doing it a different way to compare
  test <- tibble(
    test_shift_count = test_totals(ds),
    test_proportions = test_proportions(proportional_load, 0.95),
    test_coverage = test_coverage(ds)
  )
  
  output_rota_to_googlesheets(ds, ss)
  
  return(list(rota = ds$rota,
              days_in = ds$days_in,
              tt = ds$tt,
              proportional_load = proportional_load, 
              test = test))

  
  
}

ds <- main()
view(ds$tt)

