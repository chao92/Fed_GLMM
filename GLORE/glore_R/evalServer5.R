evalServer5<- function (con, expr, send = NULL) 
{
    x <- substitute(expr)
#    if (!missing(send) && (!length(x) == 1 || mode(x) != "name")) 
 #       stop("When send is supplied, expr must be a target variable name (unquoted) on the server to assign the result of the send expr to.")
    if (!is.character(x)) 
        x <- deparse(x)
		cat(x)
    cat(readLines(con))
	
    if (missing(send)) {
        cat("..Last.value <- try(eval(parse(text = \"", x, "\"))); .f <- file(); dump(\"..Last.value\", file = .f); flush(.f); seek(.f, 0); cat(\"\\n<<<startflag>>>\", readLines(.f), \"<<<endflag>>>\\n\", sep = \"\\n\"); close(.f); rm(.f, ..Last.value); flush.console()\n", 
            file = con, sep = "")
    }
    else {
        .f <- file()
        on.exit(close(.f))
        ..Last.value <- send
        dump("..Last.value", file <- .f)		
        flush(.f)
        seek(.f, 0)
		cat(readLines(.f), ";",file = con, sep = "")
		cat(x, " <- ..Last.value;",file = con, sep = "")
		cat("rm(..Last.value);",file = con, sep = "")
		cat("cat(\"\\n<<<endflag>>>\\n\"); flush.console()\n",file = con, sep = "")	
    }
		
    objdump <- ""
    endloc <- NULL
	endloc_ind<-0
    while (endloc_ind<2) {	  
        obj <- readLines(con, n = 1000, warn = FALSE)		
        if (!length(obj)) {
            Sys.sleep(0.01)
            next
        }	
		endloc <- grep("<<<endflag>>>", obj)	
        endloc_ind<-endloc_ind+length(endloc)		
        if (length(endloc)) 
		{			
	        if (endloc_ind==2)
				obj <- obj[0:(endloc[length(endloc)] - 1)]
		}
        objdump <- c(objdump, obj)
    }
	
	if (!missing(send)) {
        if (all(objdump == "")) 
            stop("You should send some things")
        return(TRUE)
    }
	
    startloc <- grep("<<<startflag>>>", objdump)	
	
    if (length(startloc) == 0) 
        stop("Unable to find <<<startflag>>>")
    objdump <- objdump[-(1:startloc[length(startloc)])]
    nospace <- grep("[^ ]$", objdump)
    nospace <- nospace[nospace < length(objdump)]
    for (i in rev(nospace)) {
        objdump[i] <- paste(objdump[i], objdump[i + 1], sep = "")
        objdump[i + 1] <- ""
    }
	
    objcon <- textConnection(objdump)
	
    on.exit(close(objcon))
    source(objcon, local = TRUE, echo = FALSE, verbose = FALSE)
    return(..Last.value)
}
