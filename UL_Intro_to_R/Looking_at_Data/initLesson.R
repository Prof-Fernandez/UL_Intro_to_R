# Path to data
.datapath <- file.path(path.package('swirl'), 'Courses',
                      'UL_Intro_to_R', 'Looking_at_Data',
                      'heroes_information.csv')
# Read in data
heroes <- read.csv(.datapath, na.strings = c(-99,"-"))

# Remove annoying columns
heroes<-heroes[, !(names(heroes) %in% "X")]