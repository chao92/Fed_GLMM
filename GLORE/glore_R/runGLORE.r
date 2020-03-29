
# server


setwd("/Users/privacy/Dropbox/EDM_project_GLORE/glore_R_mac")
source("GLOREserver.r")

#client 1

setwd("/Users/privacy/Dropbox/EDM_project_GLORE/glore_R_mac")
source("GLOREclient_function.r")
GLOREclient("dbmiclient1","dbmiGLROEtest","",8999,"edin_part1.txt")


#client 2
setwd("/Users/privacy/Dropbox/EDM_project_GLORE/glore_R_mac")
source("GLOREclient_function.r")
GLOREclient("dbmiclient2","dbmiGLROEtest","",8999,"edin_part2.txt")

#clinet 3
setwd("/Users/privacy/Dropbox/EDM_project_GLORE/glore_R_mac")
source("GLOREclient_function.r")
GLOREclient("dbmiclient3","dbmiGLROEtest","",8999,"edin_part3.txt")
