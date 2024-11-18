ENDPOINT <- "https://explorecourses.stanford.edu/"
DEPARTMENTS_ENDPOINT <- paste0(ENDPOINT, "?view=xml-20140630")
COURSE_ENDPOINT <- paste0(
  ENDPOINT,
  "search?view=xml-20140630&academicYear={year}&q={name}&",
  "filter-departmentcode-{name}=on&filter-coursestatus-Active=on"
)
