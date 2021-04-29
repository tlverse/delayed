
#' Get traceback for last delayed error
#' @param call_stack a call stack argument (defaults to last delayed error)
#' @param cat if TRUE, uses cat to print the traceback
#' @export
delayed_traceback <- function(call_stack=NULL, cat = TRUE){
	if(is.null(call_stack)){
		call_stack <- get0("traceback", envir=.delayed_env)
	}
	# TODO: maybe suppress the delayed boilerplate in here
	if(length(call_stack)==0){
		message("no traceback available")
	}
	
	stacktrace <- tryCatchLog::limitedLabelsCompact(call_stack, FALSE)
	stacktrace <- paste(" ", 
						seq_along(stacktrace), 
						stacktrace, 
						collapse = "\n")


	if(cat){
		cat(stacktrace)
	}
	
	invisible(stacktrace)
}

#' @import futile.logger
default_condition_handler <- function(c){
    condition_class <- class(c)[1]
    message <- sprintf("%s %s %s", condition_class, context, c$message)




    if (inherits(c,"error")){
    	log_fun <- flog.error

    	# TODO: add option to also do this for warnings
    	call_stack <- sys.calls()
    	assign("traceback", call_stack, envir=.delayed_env)

		if(getOption("delayed.dumpfile")){
			context <- fs::path_sanitize(context,"-")
			filename  <- sprintf("delayed_dump_%s_%s.rdata",
								 context, 
								 strftime(Sys.time(),"%Y%m%d%H%M%S"))
        	utils::dump.frames()
        	save.image(file = filename)

			message <- paste(message, 
				 sprintf("\tframes dumped to %s", filename),
				 sep = "\n")

		}

		if(getOption("delayed.stacktrace")){
			stacktrace <- delayed_traceback(call_stack, FALSE)
			message <- paste(message, stacktrace, sep = "\n")
		}



    } else if(inherits(c,"warning")){
		log_fun <- flog.warn
    } else {
    	log_fun <- flog.info

    }




	log_fun(message)


}

try_with_logs <- function(expr, condition_handler=default_condition_handler, context=NULL){
	# make a copy of the handler and add context
	ch_instance <- condition_handler
	environment(ch_instance) <- new.env(parent=environment(condition_handler))
	assign("context",context, envir=environment(ch_instance))

	tryCatch(
	    withCallingHandlers(expr,
	                        condition = ch_instance
	    )
    )
}
