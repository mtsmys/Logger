#===============================================================================
# Copyright (c) 2018, Akihisa Yasuda
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#===============================================================================

library(R6)


#' Logging library for R implemented in R6 class.
Logger <- R6::R6Class("Logger", 
	#===========================================================================
	# Public variables & methods
	#===========================================================================
	public = list(
		LEVEL_TRACE = 'TRACE',		# TRACE level indicator
		LEVEL_DEBUG = 'DEBUG',		# DEBUG level indicator
		LEVEL_INFO = 'INFO',		# INFO level indicator
		LEVEL_WARN = 'WARN',		# WARN level indicator
		LEVEL_ERROR = 'ERROR',		# ERROR level indicator
		LEVEL_FATAL = 'FATAL',		# FATAL level indicator


		#' Constructor.<br>
		initialize = function ()
			{
			#===== Set default log level =====
			self$setLogLevel(self$LEVEL_INFO)
			},


		#' Destructor.<br>
		finalize = function ()
			{
			#===== Close file descriptor =====
			private$closeFile()
			},


		#' This function creates new log message.<br>
		#' 
		#' @param log.level		Log level string
		#' @param log.message	Log message string
		#' @param log.function	Logging function string
		#' @param log.line		Log line number
		#' @return				Created new log message
		createNewLogMessage = function (log.level, log.message, log.function, log.line)
			{
			return(paste("[", private$getTime(), "][", tolower(log.level), "]["+Sys.getpid()+"][", log.function, ":", log.line, "][",log.message, "]", sep = ""))
			},


		#' Logs a message object with the DEBUG level.<br>
		#' 
		#' @param log.message	Log message string
		#' @param log.function	Function name string to output the log message
		#' @param log.line		Line number of source code
		debug = function (log.message, log.function = "non", log.line = 1)
			{
			#========== Variable ==========
			file.descriptor <- private$getFileDescriptor()
			log.level <- self$getLogLevel()
			message <- NULL

			#===== Check log level =====
			if (log.level<=2)
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_DEBUG, log.message, log.function, log.line)
				#===== Check existence of file pathname string =====
				if (is.null(file.descriptor)==FALSE)
					{
					private$writeMessage(message)
					}
				else
					{
					private$print(message)
					}
				}
			#===== In case of not appropriate =====
			else
				{
				}
			},


		#' Logs a message object with the ERROR level.<br>
		#' 
		#' @param log.message	Log message string
		#' @param log.function	Function name string to output the log message
		#' @param log.line		Line number of source code
		error = function (log.message, log.function = "non", log.line = 1)
			{
			#========== Variable ==========
			file.descriptor <- private$getFileDescriptor()
			log.level <- self$getLogLevel()
			message <- NULL

			#===== Check log level =====
			if (log.level<=5)
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_ERROR, log.message, log.function, log.line)
				#===== Check existence of file pathname string =====
				if (is.null(file.descriptor)==FALSE)
					{
					private$writeMessage(message)
					}
				else
					{
					private$print(message)
					}
				}
			#===== In case of not appropriate =====
			else
				{
				}
			},


		#' Logs a message object with the FATAL level.<br>
		#' 
		#' @param log.message	Log message string
		#' @param log.function	Function name string to output the log message
		#' @param log.line		Line number of source code
		fatal = function (log.message, log.function = "non", log.line = 1)
			{
			#========== Variable ==========
			file.descriptor <- private$getFileDescriptor()
			log.level <- self$getLogLevel()
			message <- NULL

			#===== Check log level =====
			if (log.level<=6)
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_FATAL, log.message, log.function, log.line)
				#===== Check existence of file pathname string =====
				if (is.null(file.descriptor)==FALSE)
					{
					private$writeMessage(message)
					}
				else
					{
					private$print(message)
					}
				}
			#===== In case of not appropriate =====
			else
				{
				}
			},


		#' Returns the logger name which is owned by logger object.
		#' 
		#' @return	The name of logging object
		getLoggerName = function ()
			{
			return(private$logger.name)
			},


		#' Returns the log level which is owned by logger object.<br>
		#' 
		#' @return	Log level number
		getLogLevel = function ()
			{
			return(private$log.level)
			},


		#' Logs a message object with the INFO level.<br>
		#' 
		#' @param log.message	Log message string
		#' @param log.function	Function name string to output the log message
		#' @param log.line		Line number of source code
		info = function (log.message, log.function = "non", log.line = 1)
			{
			#========== Variable ==========
			file.descriptor <- private$getFileDescriptor()
			log.level <- self$getLogLevel()
			message <- NULL

			#===== Check log level =====
			if (log.level<=3)
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_INFO, log.message, log.function, log.line)
				#===== Check existence of file pathname string =====
				if (is.null(file.descriptor)==FALSE)
					{
					private$writeMessage(message)
					}
				else
					{
					private$print(message)
					}
				}
			#===== In case of not appropriate =====
			else
				{
				}
			},


		#' Set indicated logger name into the member variable of logging object.
		#' 
		#' @param name	Logging object name to set
		setLoggerName = function (name)
			{
			if (is.null(name)==FALSE)
				{
				private$logger.name <- name
				}
			else
				{
				}
			},


		#' Set indicated log file path into the member variable of logging object.<br>
		#' 
		#' @param log.filepath		Log file pathname string
		setLogFilePath = function (log.filepath)
			{
			#===== Set log file pathname string into internal variable =====
			private$log.filepath <- log.filepath
			#===== Open file descriptor =====
			private$openFile(log.filepath)
			},


		#' Set indicated log level into the member variable of logging object.<br>
		#' 
		#' @param log.level	Predefined log level string
		setLogLevel = function (log.level)
			{
			#===== In the case of TRACE level =====
			if (self$LEVEL_TRACE==log.level)
				{
				private$log.level <- 1
				}
			#===== In the case of DEBUG level =====
			else if (self$LEVEL_DEBUG==log.level)
				{
				private$log.level <- 2
				}
			#===== In the case of INFO level =====
			else if (self$LEVEL_INFO==log.level)
				{
				private$log.level <- 3
				}
			#===== In the case of WARN level =====
			else if (self$LEVEL_WARN==log.level)
				{
				private$log.level <- 4
				}
			#===== In the case of ERROR level =====
			else if (self$LEVEL_ERROR==log.level)
				{
				private$log.level <- 5
				}
			#===== In the case of TRACE level =====
			else if (self$LEVEL_FATAL==log.level)
				{
				private$log.level <- 6
				}
			#===== Argument error =====
			else
				{
				}
			},


		#' Logs a message object with the TRACE level.<br>
		#' 
		#' @param log.message	Log message string
		#' @param log.function	Function name string to output the log message
		#' @param log.line		Line number of source code
		trace = function (log.message, log.function = "non", log.line = 1)
			{
			#========== Variable ==========
			file.descriptor <- private$getFileDescriptor()
			log.level <- self$getLogLevel()
			message <- NULL

			#===== Check log level =====
			if (log.level<=1)
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_TRACE, log.message, log.function, log.line)
				#===== Check existence of file pathname string =====
				if (is.null(file.descriptor)==FALSE)
					{
					private$writeMessage(message)
					}
				else
					{
					private$print(message)
					}
				}
			#===== In case of not appropriate =====
			else
				{
				}
			},


		#' Logs a message object with the WARN level.<br>
		#' 
		#' @param log.message	Log message string
		#' @param log.function	Function name string to output the log message
		#' @param log.line		Line number of source code
		warn = function (log.message, log.function = "non", log.line = 1)
			{
			#========== Variable ==========
			file.descriptor <- private$getFileDescriptor()
			log.level <- self$getLogLevel()
			message <- NULL

			#===== Check log level =====
			if (log.level<=4)
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_WARN, log.message, log.function, log.line)
				#===== Check existence of file pathname string =====
				if (is.null(file.descriptor)==FALSE)
					{
					private$writeMessage(message)
					}
				else
					{
					private$print(message)
					}
				}
			#===== In case of not appropriate =====
			else
				{
				}
			}
	),


	#===========================================================================
	# Private variables & methods
	#===========================================================================
	private = list(
		file.descriptor = NULL,		# File descriptor
		logger.name = NULL,			# The name of logger object
		log.filepath = NULL,		# Log file pathname string
		log.level = 0,				# Log level number


		#' This method closes the opened file descriptor.<br>
		closeFile = function ()
			{
			#========== Variable ==========
			METHOD <- "Logger.private.closeFile()"

			#===== Check existence of file descriptor =====
			if (is.null(private$file.descriptor)==FALSE)
				{
				tryCatch(
					{
					#===== Close file descriptor =====
					close(private$file.descriptor)
					},
				#===== Error handling =====
				error = function(e)
					{
					private$print(e$message)
					},
				#===== Warning handling =====
				warning = function(e)
					{
					private$print(e$message)
					},
				#===== finalization =====
				finally =
					{
					#===== Initialize file descriptor =====
					private$file.descriptor <- NULL
					},
				silent = TRUE)
				}
			#===== In the case of not existing file descriptor =====
			else
				{
				}
			},


		#' Returns a file descriptor owned by this class.<br>
		#' 
		#' @return	File descriptor
		getFileDescriptor = function ()
			{
			return(private$file.descriptor)
			},


		#' Returns file pathname string owned by this class.<br>
		#' 
		#' @return	Log file pathname string or NULL
		getLogFilepath = function ()
			{
			return(private$log.filepath)
			},


		#' Returns current time in milliseconds
		#' 
		#' @return	Current time in milliseconds
		getTime = function ()
			{
			#===== Change the maximum number of digits into milliseconds =====
			options(digits.secs=3)
			#===== Return current time in milliseconds =====
			return(Sys.time())
			},


		#' Opens file descriptor owned by this class.<br>
		#' 
		#' @param file.path	File pathname string
		#' @param append	A description of how to open the connection
		openFile = function (file.path, append = TRUE)
			{
			#========== Variable ==========
			METHOD <- "Logger.private.openFile()"

			#===== Check argument =====
			if (is.null(file.path)==FALSE)
				{
				tryCatch(
					{
					if (append==TRUE)
						{
						#===== Open file descriptor in "a+" mode =====
						private$file.descriptor <- file(file.path, open = "a+")
						}
					else
						{
						#===== Open file descriptor in "w+" mode =====
						private$file.descriptor <- file(file.path, open = "w+")
						}
					self$info("Now opened log file", METHOD)
					},
				#===== Error handling =====
				error = function(e)
					{
					private$print(e$message)
					#===== Initialize file descriptor =====
					private$file.descriptor <- NULL
					},
				#===== Warning handling =====
				warning = function(e)
					{
					private$print(e$message)
					#===== Initialize file descriptor =====
					private$file.descriptor <- NULL
					},
				#===== finalization =====
				finally =
					{
					},
				silent = TRUE)
				}
			#===== Argument error =====
			else
				{
				self$error("Argument error! Indicated \"file.path\" is NULL", METHOD)
				}
			},


		#' Show the message string in standard IO.<br>
		#' 
		#' @param log.message	Log message string
		print = function (log.message)
			{
			print(log.message)
			},


		#' Write the message string into log file.<br>
		#' 
		#' @param log.message	Log message string
		writeMessage = function (log.message)
			{
			#========== Variable ==========
			connection <- NULL
			METHOD <- "Logger.private.writeMessage()"

			tryCatch(
				{
				#===== Get file descriptor =====
				connection <- private$getFileDescriptor()
				#===== Output log message =====
				writeLines(log.message, con = connection, sep = "\r\n", useBytes = FALSE)
				#===== Flush out buffer data =====
				flush(connection)
				},
			#===== Error handling =====
			error = function(e)
				{
				private$print(e$message)
				},
			#===== Warning handling =====
			warning = function(e)
				{
				private$print(e$message)
				},
			#===== finalization =====
			finally =
				{
				},
			silent = TRUE)
			}
	)
)



# End Of File
