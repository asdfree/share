if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_username <- Sys.getenv( "my_username" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
lodown( "share" , output_dir = file.path( getwd() ) , 
