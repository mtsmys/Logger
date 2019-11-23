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
		LEVEL_TRACE = 'TRACE',		#' TRACE level indicator
		LEVEL_DEBUG = 'DEBUG',		#' DEBUG level indicator
		LEVEL_INFO = 'INFO',		#' INFO level indicator
		LEVEL_WARN = 'WARN',		#' WARN level indicator
		LEVEL_ERROR = 'ERROR',		#' ERROR level indicator
		LEVEL_FATAL = 'FATAL',		#' FATAL level indicator


		#' Constructor.<br>
		#' 
		#' @param config.file.path	Configuration file pathname string
		initialize = function (config.file.path = NULL)
			{
			#===== In case of set configuration file path =====
			if (is.null(config.file.path)==FALSE 
					&& is.character(config.file.path)==TRUE)
				{
				#===== Parse config file =====
				private$parseConfigFile(config.file.path)
				}
			#===== In case of not set configuration file path =====
			else
				{
				#===== Execute default initialization =====
				private$defaultInit()
				}
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
			return(paste("[", private$getTime(), "][", tolower(log.level), "][", Sys.getpid(), "][", log.function, ":", log.line, "][",log.message, "]", sep = ""))
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
					private$printMessage(message)
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
					private$printMessage(message)
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
					private$printMessage(message)
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
					private$printMessage(message)
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
		#' @param log.file.path		Log file pathname string
		setLogFilePath = function (log.file.path)
			{
			#========== Variable ==========
			message <- NULL
			METHOD <- 'Logger.public.setLogFilePath()'
			LOG_LINE <- 1

			#===== Check argument =====
			if (is.null(log.file.path)==FALSE 
					&& is.character(log.file.path)==TRUE)
				{
				tryCatch(
					{
					#===== Check the existence of parent directory =====
					private$getParentDirectoryPath(log.file.path)
					#===== Open file descriptor =====
					private$openFile(log.file.path)
					},
				#===== Error handling =====
				error = function(e)
					{
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_ERROR, e$message, METHOD, LOG_LINE)
					#===== Print error message =====
					private$printMessage(message)
					},
				#===== Warning handling =====
				warning = function(e)
					{
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_WARN, e$message, METHOD, LOG_LINE)
					#===== Print warn message =====
					private$printMessage(message)
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
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_ERROR, 'Argument error! Indicated "log.file.path" variable is invalid', METHOD, LOG_LINE)
				#===== Print error message =====
				private$printMessage(message)
				}
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


		#' Set max size of log file.<br>
		#' 
		#' @param log.file.size.max Max size of log file in bytes
		setMaxLogFileSize = function (log.file.size.max)
			{
			#========== Variable ==========
			METHOD <- 'Logger.public.setMaxLogFileSize()'

			#===== Check argument =====
			if (is.integer(log.file.size.max)==TRUE 
					&& log.file.size.max>0)
				{
				private$log.file.size.max <- log.file.size.max
				}
			#===== Argument error =====
			else
				{
				self$error('Argument error! Indicated "log.file.size.max" is invalid value', METHOD)
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
					private$printMessage(message)
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
					private$printMessage(message)
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
		file.descriptor = NULL,			#' File descriptor
		logger.name = NULL,				#' The name of logger object
		log.file.path = NULL,			#' Log file pathname string
 		log.file.size.max = 8388608,	#' Log file max size
		log.level = 0,					#' Log level number


		#' 
		#' 
		#' @return true: , false: 
		checkLogFileSize = function ()
			{
			if (private$getLofFileSize()<private$log.file.size.max)
				{
				return(TRUE)
				}
			else
				{
				return(FALSE)
				}
			},


		#' This method closes the opened file descriptor.<br>
		closeFile = function ()
			{
			#========== Variable ==========
			message <- NULL
			METHOD <- 'Logger.private.closeFile()'
			LOG_LINE <- 1

			#===== Check existence of file descriptor =====
			if (is.null(private$file.descriptor)==FALSE)
				{
				tryCatch(
					{
					#===== Close file descriptor =====
					close(private$file.descriptor)
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_INFO, 'Now closed log file', METHOD, LOG_LINE)
					#===== Print info message =====
					private$printMessage(message)
					},
				#===== Error handling =====
				error = function(e)
					{
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_ERROR, e$message, METHOD, LOG_LINE)
					#===== Print error message =====
					private$printMessage(message)
					},
				#===== Warning handling =====
				warning = function(e)
					{
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_WARN, e$message, METHOD, LOG_LINE)
					#===== Print warn message =====
					private$printMessage(message)
					},
				#===== finalization =====
				finally =
					{
					#===== Initialize private variables =====
					private$file.descriptor <- NULL
					private$logger.name <- NULL
					private$log.file.path <- NULL
					},
				silent = TRUE)
				}
			#===== In the case of not existing file descriptor =====
			else
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_WARN, 'There is not opened log file', METHOD, LOG_LINE)
				#===== Print warn message =====
				private$printMessage(message)
				}
			},


		#' Returns a file descriptor owned by this class.<br>
		#' 
		#' @return	File descriptor or NULL(= no use log file)
		getFileDescriptor = function ()
			{
			return(private$file.descriptor)
			},


		#' Returns file pathname string owned by this class.<br>
		#' 
		#' @return	Log file pathname string or NULL
		getLogFilepath = function ()
			{
			return(private$log.file.path)
			},


		#' Get log file size in bytes.<br>
		#' 
		#' @return File size in bytes
		getLofFileSize = function ()
			{
			#========== Variable ==========
			file <- NULL
			LOG_FILE_PATH <- private$getLogFilepath()

			#===== In the case of existing log file pathname string =====
			if (is.null(LOG_FILE_PATH)==FALSE)
				{
				#===== Get log file information =====
				file <- file.info(LOG_FILE_PATH)
				#===== Return log file size in bytes =====
				return(file$size)
				}
			#===== In the case of not existing =====
			else
				{
				return(0)
				}
			},


		#' Create old log file pathname string based on current file.<br>
		#' 
		#' @param log.file.path.current	Current log file pathname string
		#' @return						Old log file pathname string
		getOldLogFilePath = function (log.file.path.current)
			{
			parent.directory.path <- NULL
			log.file.name.base <- NULL
			log.file.regular.expression <- NULL
			log.file.list <- NULL
			log.file.list.number <- 0
			log.file.name.old <- NULL
			log.file.path.old <- NULL
			LOG_FILE_EXTENSION <- '.log'

			#=====  =====
			if (is.null(log.file.path.current)==FALSE 
					&& file.exists(log.file.path.current)==TRUE)
				{
				#=====  =====
				parent.directory.path <- private$getParentDirectoryPath(log.file.path.current)
				#=====  =====
				log.file.name.base <- substr(basename(log.file.path.current), 1, nchar(basename(log.file.path.current)) - nchar(LOG_FILE_EXTENSION))
				#=====  =====
				log.file.regular.expression <- paste(log.file.name.base, '[0-9].*', sep = '_')
				log.file.regular.expression <- paste(log.file.regular.expression, LOG_FILE_EXTENSION, sep = '\\')
				#=====  =====
				log.file.list <- list.files(parent.directory.path,
						pattern = log.file.regular.expression,
#						all.files = FALSE,
						recursive = FALSE,
						include.dirs = FALSE
				)
				#=====  =====
				if (is.null(log.file.list)==TRUE)
					{
					}
				#=====  =====
				else
					{
					log.file.list.number <- length(log.file.list)
					}
				#=====  =====
				log.file.name.old <- paste(log.file.name.base, log.file.list.number, sep = '_')
				log.file.name.old <- paste(log.file.name.old, LOG_FILE_EXTENSION, sep = '')
				log.file.path.old <- paste(parent.directory.path, log.file.name.old, sep = '/')
				#=====  =====
				return(log.file.path.old)
				}
			#=====  =====
			else
				{
				return(NULL)
				}
			},


		#' Get the parent direkutory pathname string.<br>
		#' 
		#' @param path	a child file or directory pathname string
		#' @return		parent directory pathname string or NULL(means error)
		getParentDirectoryPath = function (path)
			{
			#========== Variable ==========
			parent.directory.path <- NULL
			message <- NULL
			METHOD <- 'Logger.private.getParentDirectoryPath()'
			LOG_LINE <- 1

			#===== Check argument =====
			if (is.null(path)==FALSE)
				{
				#===== Get parent directory =====
				parent.directory.path <- dirname(path)
				#===== In the case of existing parent directory =====
				if (dir.exists(parent.directory.path)==TRUE)
					{
					return(parent.directory.path)
					}
				#===== In the case of not existing parent directory =====
				else
					{
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_INFO, 'Now create parent directory recursively', METHOD, LOG_LINE)
					#===== Print error message =====
					private$printMessage(message)
					#===== Create new parent directory =====					
					dir.create(parent.directory.path, recursive = TRUE)
					return(parent.directory.path)
					}
				}
			#===== Argument error =====
			else
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_ERROR, 'Argument error! Indicated \"path\" string is NULL', METHOD, LOG_LINE)
				#===== Print error message =====
				private$printMessage(message)
				}
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


		#' 
		defaultInit = function ()
			{
			#===== Set default log level =====
			self$setLogLevel(self$LEVEL_INFO)
			},


		#' Opens file descriptor owned by this class.<br>
		#' 
		#' @param file.path	File pathname string
		#' @param append	A description of how to open the connection
		openFile = function (file.path, append = TRUE)
			{
			#========== Variable ==========
			message <- NULL
			METHOD <- 'Logger.private.openFile()'
			LOG_LINE <- 1

			#===== Check argument =====
			if (is.null(file.path)==FALSE)
				{
				tryCatch(
					{
					if (append==TRUE)
						{
						#===== Open file descriptor in "a+" mode =====
						private$file.descriptor <- file(file.path, open = 'a+')
						}
					else
						{
						#===== Open file descriptor in "w+" mode =====
						private$file.descriptor <- file(file.path, open = 'w+')
						}
					self$info('Now opened log file', METHOD)
					#===== Set log file pathname string into internal variable =====
					private$log.file.path <- file.path
					},
				#===== Error handling =====
				error = function(e)
					{
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_ERROR, e$message, METHOD, LOG_LINE)
					#===== Print error message =====
					private$printMessage(message)
					#===== Initialize file descriptor =====
					private$file.descriptor <- NULL
					},
				#===== Warning handling =====
				warning = function(e)
					{
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_WARN, e$message, METHOD, LOG_LINE)
					#===== Print warn message =====
					private$printMessage(message)
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
				self$error('Argument error! Indicated "file.path" is NULL', METHOD)
				}
			},


		#' Parse indicated configuration file and store the parameters in class variable.<br>
		#' 
		#' Caller must set the configuration file format into "TSV" which<br>
		#' is similar to the "CSV" file and "key" and "value" data are<br>
		#' separated by TAB(="\t").<br>
		#' 
		#' @param config.file.path	Configuration file pathname string
		parseConfigFile = function (config.file.path)
			{
			#========== Variable ==========
			config <- NULL
			TSV_FILE_EXTENSION <- '.tsv'
			TAB <- '\t'

			#===== Check argument =====
			if (is.null(config.file.path)==FALSE 
					&& is.character(config.file.path)==TRUE 
					&& file.exists(config.file.path)==TRUE 
			)
				{
				#===== Get configuration data =====
				config <- read.delim(config.file.path, header = TRUE, sep = TAB, stringsAsFactor = FALSE)
				}
			#===== Argument error =====
			else
				{
				#===== Execute default initialization =====
				private$defaultInit()
				}
			},


		#' Show the message string in standard IO.<br>
		#' 
		#' @param log.message	Log message string
		printMessage = function (log.message)
			{
			print(log.message)
			},


		#' Rotate log file because of exceeding the file size.<br>
		rotateLogFile = function ()
			{
			#========== Variable ==========
			file.descriptor <- private$getFileDescriptor()
			LOG_FILE_PATH_CURRENT <- private$getLogFilepath()
			LOG_FILE_PATH_OLD <- private$getOldLogFilePath(LOG_FILE_PATH_CURRENT)

			#=====  =====
			if (is.null(LOG_FILE_PATH_CURRENT)==FALSE 
					&& is.null(LOG_FILE_PATH_OLD)==FALSE 
					&& file.exists(LOG_FILE_PATH_CURRENT)==TRUE 
					&& is.null(file.descriptor)==FALSE)
				{
				#===== Close file descriptor =====
				private$closeFile()
				#===== Rename the log file =====
				file.rename(LOG_FILE_PATH_CURRENT, LOG_FILE_PATH_OLD)
				#===== Open & Set log file =====
				self$setLogFilePath(LOG_FILE_PATH_CURRENT)
				}
			#=====  =====
			else
				{
				}
			},


		#' Write the message string into log file.<br>
		#' 
		#' @param log.message	Log message string
		writeMessage = function (log.message)
			{
			#========== Variable ==========
			connection <- NULL
			message <- NULL
			METHOD <- "Logger.private.writeMessage()"
			LOG_LINE <- 1

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
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_ERROR, e$message, METHOD, LOG_LINE)
				#===== Print error message =====
				private$printMessage(message)
				},
			#===== Warning handling =====
			warning = function(e)
				{
				#===== Create new log message =====
				message <- self$createNewLogMessage(self$LEVEL_WARN, e$message, METHOD, LOG_LINE)
				#===== Print warn message =====
				private$printMessage(message)
				},
			#===== finalization =====
			finally =
				{
				#===== In the case of begin within the specified file size =====
				if (private$checkLogFileSize()==TRUE)
					{
					# do nothing
					}
				#===== In the case of exceeding the file size =====
				else
					{
					#===== Create new log message =====
					message <- self$createNewLogMessage(self$LEVEL_WARN, 'The log file size exceeds max size', METHOD, LOG_LINE)
					#===== Print warn message =====
					private$printMessage(message)
					#===== Rotate log file =====
					private$rotateLogFile()
					}
				},
			silent = TRUE)
			}
	)
)


#===============================================================================
# for Debug
#===============================================================================
#Logger$debug("initialize")
#Logger$debug("finalize")
#Logger$debug("createNewLogMessage")
#Logger$debug("debug")
#Logger$debug("error")
#Logger$debug("fatal")
#Logger$debug("getLoggerName")
#Logger$debug("getLogLevel")
#Logger$debug("info")
#Logger$debug("setLoggerName")
#Logger$debug("setLogFilePath")
#Logger$debug("setLogLevel")
#Logger$debug("setMaxLogFileSize")
#Logger$debug("trace")
#Logger$debug("warn")

#Logger$debug("checkLogFileSize")
#Logger$debug("closeFile")
#Logger$debug("getFileDescriptor")
#Logger$debug("getLogFilepath")
#Logger$debug("getLofFileSize")
#Logger$debug("getOldLogFilePath")
#Logger$debug("getTime")
#Logger$debug("openFile")
#Logger$debug("printMessage")
#Logger$debug("rotateLogFile")
#Logger$debug("writeMessage")



# End Of File
