require(tcltk)
require(svMisc)
require(svSocket)
source("evalServer6.R")

GLOREclient<-function(username, taskid, password, portNumber, localFileName)
{
	ldata<-as.matrix(read.table(localFileName,sep="\t"))
	m<-dim(ldata)[2]
	n<-dim(ldata)[1]
	x<-matrix(1,n,m)
	x[,seq(2,m)]<-ldata[,seq(1,m-1)]
	y<-ldata[,m]
	y<-matrix(y,n,1)


	##computing coefficients
	epsilon<-10^(-6)
	beta0<-matrix(-1,m,1)
	beta1<-matrix(0,m,1)
	k_old<-0
	covmatri<-matrix(0,m*m,1)
	con = socketConnection(host = "127.0.0.1", port = portNumber, blocking = FALSE)
	#con = socketConnection( port = portNumber)
	#evalServer6(con,u_username,username)
	#clientID = evalServer6(con,"clientID=which(usernames==u_username)")	

	authenticationFlag = FALSE
		server_taskid<-evalServer6(con,"temp=taskid")
		cat(server_taskid)
		evalServer6(con,u_username,username)
		if (server_taskid==taskid)
		{
			clientID = evalServer6(con,"clientID=which(usernames==u_username)")	
			if( length(clientID))
				authenticationFlag = TRUE
		}

		if (authenticationFlag == FALSE)
		{
			close(con)
			stop("Please check your taskid and username and rerun your local code")
		}
	while(max(abs(beta1-beta0))>epsilon)
	{ 
	  
	  beta0<-beta1
	  p<-1/(1+exp(-x%*%beta0))
	  w<-diag(c(p*(1-p)))
	  d<-t(x)%*%w%*%x
	  e<-t(x)%*%(y-p)
	  while (evalServer6(con, "receive_ind=receive_ind")<clientID)
	  {
		Sys.sleep(0.1)
	  }
	  evalServer6(con, d[,,receive_ind], d)
	  evalServer6(con, e[,,receive_ind], e)
	  k_client<-evalServer6(con,"k_server=k_server")
	  while(k_client==k_old)
	  {
	   Sys.sleep(0.1)
	   k_client<-evalServer6(con,"k_server=k_server")
	  }
	  beta1<-evalServer6(con,"beta1=beta1")
	  k_old<-k_client
	  cat("Iteration=",k_client,"\n")
	  cat("beta=",beta1,"\n")
	}

	beta1
	##computing variance-covariance matrix
	hat_beta<-beta1
	p<-1/(1+exp(-x%*%hat_beta)) 
	w<-diag(c(p*(1-p)))  
	d<-t(x)%*%w%*%x
	while (evalServer6(con, "receive_ind=receive_ind")<clientID)
	{
		Sys.sleep(0.1)
	}
	evalServer6(con, d[,,receive_ind], d)

	sd_true<-evalServer6(con,"sd_true=sd_true")
	while(sd_true==0)
	{
	  Sys.sleep(0.1)
	  sd_true<-evalServer6(con,"sd_true=sd_true")
	}
	covmatri<-evalServer6(con,"covmatri=covmatri")
	covmatri<-matrix(covmatri,m,m)
	covmatri
	sd<-sqrt(diag(covmatri))
	sd
	while (evalServer6(con, "receive_ind=receive_ind")<clientID)
	{
		Sys.sleep(0.1)
	}
	
	evalServer6(con, stop_connection[receive_ind], 1)
	close(con)
	res<-list(hat_beta,covmatri)
	names(res)<-c("coefficients","variance_covariance_matrix")
	return(res)
}

#username <- "dbmiclient3"
#	taskid <- "dbmiGLROEtest"	
#	password <- ""
#	portNumber <- 8971
#	localFileName <- "C:/Users/yuan/Dropbox/distributed_lr_paper/edin_part3.txt"





