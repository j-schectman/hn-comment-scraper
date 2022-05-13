(ql:quickload '("dexador" "plump" "lquery"))
(defparameter *hn-root-url* "https://news.ycombinator.com")
(defparameter *request* (dex:get (concatenate 'string *hn-root-url* "/news?p=" "1") ))
(defparameter *parsed-content* (lquery:$ (initialize *request*)))
(defparameter *results-file* "results.txt")
(defparameter *result-ids-file* "results-ids.txt")

(defparameter *comment-urls* 
  (remove-duplicates 
    (remove-if-not 
     #'(lambda (v) (search "item?" v))
     (lquery:$ *parsed-content* "td .subtext a" (attr :href)))
    :test 'equal
    ))

(format nil "~:[~a~;~:*~a~%~a~]" "word" "word")

(defun flatten-comment (comment)
  (let ((comment-queue (list comment)) (formatted-comment nil)) 
    (loop for to-flatten = (pop comment-queue)
          do (print formatted-comment)
          if (and 
               (plump:element-p to-flatten)
               (find (plump:tag-name to-flatten) (list "span" "p") :test 'equal))
          do (loop for n across (reverse (lquery-funcs:contents to-flatten)) 
                   do (push n comment-queue))
          else
          do (setf formatted-comment 
                   (format 
                     nil 
                     (if (equal (plump:tag-name (plump:parent to-flatten)) "p")
                         "~:[~a~;~:*~a~%~%~a~]" 
                         "~:[~a~;~:*~a ~a~]" 
                         )
                     formatted-comment 
                     (lquery-funcs:render-text to-flatten)))
          while (> (length comment-queue) 0))
    formatted-comment
    )
  )

(defun get-comments-for-id (id-string)
  (handler-case
    (let* ((result (dex:get (concatenate 'string *hn-root-url* "/" id-string)))
           (comments (lquery:$ (initialize result) ".commtext")))
      (loop for comment across comments
            collect (flatten-comment comment)
            )
      )
    ; Just ignore failed requests, we're lazy and it doesn't maatter
    (dex:http-request-failed (e)
                             (print (list "failed to get" id-string "with error" e) )
                             (vector))))

(defun get-already-scraped-ids ()
  (with-open-file 
    (results-ids-file 
      *result-ids-file*
      :if-does-not-exist :create)
    (loop for 
          line = (read-line results-ids-file nil)
          while line collect line))) 

(defun save-comments-to-output (comments id output)
  (if (> (length comments) 0) (format output "~{[comment-start]~a~^[comment-end]~%~}" comments))
  (with-open-file 
    (stream *result-ids-file*
            :direction :output 
            :if-does-not-exist :create
            :if-exists :append)
    (format stream "~a~%" id)))
  
(with-open-file
  (results-file *results-file*
   :direction :output 
   :if-does-not-exist :create
   :if-exists :append)
 (let ((already-scraped-ids (get-already-scraped-ids)))
    (loop for id across *comment-urls*
          for x in (list 1 2)
          unless (find id already-scraped-ids :test 'equal)
          do (progn 
               (sleep 3)
               (print (list "getting comment for" id))
               (push id already-scraped-ids)
               (save-comments-to-output (get-comments-for-id id) id results-file)))) )
