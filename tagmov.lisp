(defpackage :tagmov
  (:use :cl)
  (:export #:main))

(in-package :tagmov)

(defstruct text t1 t2 x y txt color font size)
(defstruct file name duration width)

(defparameter *files* nil)
(defparameter *result* nil)
(defparameter *fontfile* "IBMPlexMono-Regular.ttf")
(defparameter *fontsize* 32)
(defparameter *fontcolor* "white")
(defparameter *txts* nil)
(defparameter *bar* nil)
(defparameter *verbose* 0)

(defun get-video-duration (path)
  (read-line (uiop:process-info-output
              (uiop:launch-program (format
                                    nil
                                    "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ~A"
                                    path)
                                   :output :stream))))

(defun get-video-width (path)
  (read-line (uiop:process-info-output
              (uiop:launch-program (format
                                    nil
                                    "ffprobe -v error -show_entries stream=width -of default=noprint_wrappers=1:nokey=1 ~A"
                                    path)
                                   :output :stream))))

(defun add-input-file (x)
  (if (probe-file x)
      (let ((f (make-file :name x
                          :duration (get-video-duration x)
                          :width (get-video-width x))))
        (setf *files* (cons f *files*)))
      (error 'input-unreadable-error :name x))
  (when (= (length *files*) 1)
    (setf *result* (str:concat x ".mkv"))))

(defun input-file-parser (x)
  (add-input-file x))

(defun font-file-parser (x)
  (setf *fontfile* x))

(defun font-size-parser (x)
  (setf *fontsize* (parse-integer x)))

(defun font-color-parser (x)
  (setf *fontcolor* x))

(defun text-parser (x)
  (let* ((tokens (uiop:split-string x))
         (text (make-text :t1 (car tokens)
                          :t2 (cadr tokens)
                          :x (caddr tokens)
                          :y (cadddr tokens)
                          :txt (format nil "~{~A~^ ~}" (cddddr tokens))
                          :color *fontcolor*
                          :font *fontfile*
                          :size *fontsize*)))
    (setf *txts* (cons text *txts*))))

(defun text-to-command (txt command)
  (str:concat command
              (format nil
                      "drawtext=enable='between(t,~A,~A)':text='~A':x=~A:y=~A:fontfile=~A:fontsize=~A:fontcolor=~A"
                      (text-t1 txt)
                      (text-t2 txt)
                      (text-txt txt)
                      (text-x txt)
                      (text-y txt)
                      (text-font txt)
                      (text-size txt)
                      (text-color txt))))

(defun bar-parser (x)
  (setf *bar* x))

(defun verbose-parser (x)
  (setf *verbose* (parse-integer x)))

(defun max-duration ()
  (let ((result 0))
    (loop for x in *files*
          do (let ((dur (parse-float:parse-float (file-duration x))))
               (when (> dur result)
                 (setf result dur))))
    result))

(defun total-width ()
  (let ((result 0))
    (loop for x in *files*
          do (incf result (parse-integer (file-width x))))
    result))

(defun bar-to-command (x command video)
  (if x
      (str:concat command (format nil
                                  "color=c=~A:s=~Ax6[bar];[~A][bar]overlay=-w+(w/~A)*t:H-h:shortest=1"
                                  x
                                  (total-width)
                                  video
                                  (max-duration)))
      command))

(opts:define-opts
  (:name :help
   :description "Show this usage information and quit"
   :short #\h
   :long "help")
  (:name :verbose
   :description "Make verbose"
   :short #\v
   :long "verbose"
   :arg-parser #'verbose-parser)
  (:name :input
   :description "Set the input move file name"
   :short #\i
   :long "input"
   :arg-parser #'input-file-parser)
  (:name :font-file
   :description "Set the font to be used"
   :short #\f
   :long "font-file"
   :arg-parser #'font-file-parser)
  (:name :font-size
   :description "Set the font size to be used"
   :short #\s
   :long "font-size"
   :arg-parser #'font-size-parser)
  (:name :font-color
   :description "Set the font color to be used"
   :short #\c
   :long "font-color"
   :arg-parser #'font-color-parser)
  (:name :text
   :description "Render a text to the video"
   :short #\t
   :long "text"
   :arg-parser #'text-parser)
  (:name :bar
   :description "Render progress bar"
   :short #\b
   :long "bar"
   :arg-parser #'bar-parser))

(defun dbg (fmt &rest args)
  (when (> *verbose* 0)
    (format t fmt args)))

(defmacro when-option ((options opt) &body body)
  `(when (getf ,options ,opt)
     ,@body))

(defun run-single ()
  (let ((command (format nil "ffmpeg -y -i ~A -filter_complex \"" (file-name (car *files*)))))
    (setf command (bar-to-command *bar* command 0))
    (loop for txt in *txts*
          do  (setf command (str:concat command ", "))
              (setf command (text-to-command txt command)))
    (setf command (str:concat command (format nil "\" -acodec copy ~A" *result*)))
    (dbg "~%Command is: '~A'~%" command)
    (uiop:run-program command)))

(defun run-many ()
  (let ((command (format nil "ffmpeg -y ")))
    (loop for f in *files*
          do (setf command (str:concat command (format nil "-i ~A " (file-name f)))))
    (setf command (str:concat command " -filter_complex \""))
    (loop for i from 0 to (- (length *files*) 1)
          do (setf command (str:concat command (format nil "[~A:v]" i))))
    (setf command (str:concat command (format nil "hstack=inputs=~A" (length *files*))))
    (when (or *bar* *txts*)
      (setf command (str:concat command "[v];"))) ; No semicolon in last filter
    (setf command (bar-to-command *bar* command "v"))
    (loop for i from 0 to (- (length *txts*) 1)
          do  (when (or (> i 0) *bar*)
                (setf command (str:concat command ", ")))
              (setf command (text-to-command (nth i *txts*) command)))
    (setf command (str:concat command (format nil "\" -acodec copy ~A" *result*)))
    (dbg "~%Command is: '~A'~%" command)
    (uiop:run-program command)))

(defun run ()
  (if (> (length *files*) 1)
      (run-many)
      (run-single)))

(defun usage ()
  (opts:describe
   :prefix "Tag movie file with progress bar and texts"
   :usage-of "tagmov"))

(defun unknown-option (condition)
  (format t "Error: option '~A' is unknown~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun parser-error (condition)
  (format t "Argument Parse Error: ~A~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun main ()
  (let ((options (handler-case
                     (handler-bind ((opts:arg-parser-failed #'parser-error)
                                    (opts:unknown-option    #'unknown-option))
                       (opts:get-opts)))))
    (when-option (options :help)
                 (usage))
    (if (> (length *files*) 0)
        (run)
        (unless (getf options :help)
          (usage)))))
