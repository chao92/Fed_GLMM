require(tcltk)
require(svMisc)
require(svSocket)

#m<-3
u_username=""
usernames<-c("dbmiclient1","dbmiclient2","dbmiclient3")
n_user<-length(usernames)
taskid<-"dbmiGLROEtest"
passwords<-c("","","")
portNumber<-8999
featureNumber<-10
m<-featureNumber
## computing coefficient
epsilon<-10^(-6)
beta0<-matrix(-1,m,1)
beta1<-matrix(0,m,1)
d<-array(10^(-20),c(m,m,n_user))
e<-array(10^(-20),c(m,1,n_user))
k_server<-0
stop_connection<-rep(0,n_user)
covmatri<-matrix(10^(-20),m,m)
sd_true<-0
#control the ordre of clients communication
receive_ind<-0
startSocketServer(port = portNumber, server.name = "R_yw", procfun = processSocket, secure= FALSE, local= FALSE)
#startSocketServer(port = portNumber)

while(mean(abs(beta1-beta0))>epsilon)
{ 
  beta0<-beta1
 
  for(receive_ind in 1:n_user)
  {
    while(mean(abs(d[,,receive_ind]))==10^(-20)||mean(abs(e[,,receive_ind]))==10^(-20))
	{
	  Sys.sleep(0.1)
	}
  }
  beta1<-beta0+solve(rowSums(d,dims=2)+diag(0.0000001,m))%*%(rowSums(e,dims=2))
  receive_ind<-0
  k_server<-k_server+1
  d<-array(10^(-20),c(m,m,n_user))
  e<-array(10^(-20),c(m,1,n_user))
  cat("Iteration=",k_server,"\n")
  cat("beta=",beta1,"\n")
}

beta1


  for(receive_ind in 1:n_user)
  {
    while(mean(abs(d[,,receive_ind]))==10^(-20))
	{
	  Sys.sleep(0.1)
	}
  }
	covmatri<-solve(rowSums(d,dims=2)+diag(0.0000001,m))
	covmatri
	sd<-sqrt(diag(covmatri))
	sd
	covmatri<-matrix(covmatri,m*m,1)
	receive_ind<-0
	sd_true<-1
 for(receive_ind in 1:n_user)
  {
    while(stop_connection[receive_ind]!=1)
	{
	  Sys.sleep(0.1)
	}
  }
  
Sys.sleep(1)
stopSocketServer(port = portNumber)


