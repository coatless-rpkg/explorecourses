all_depts <- explorecourses::fetch_all_courses()

schedule_ay24_25 <- all_depts
usethis::use_data(schedule_ay24_25, overwrite = TRUE)
