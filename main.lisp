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

(defun get-comments-for-id (id-string)
  (handler-case
    (let* ((result (dex:get (concatenate 'string *hn-root-url* "/" id-string)))
           (node-queue (lquery:$ (initialize result) ".commtext")))
      (reverse (loop for comm = (vector-pop node-queue)
             if (plump:text-node-p comm)
             collect (lquery-funcs:text comm)
             else
             do (loop for n across (lquery-funcs:contents comm) do (vector-push n node-queue))
             while (> (length node-queue) 0)))
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
  (format output "~{~a~^ ~}~%" comments)
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
          for _ in (list 1 3 4)
          unless (find id already-scraped-ids :test 'equal)
          do (progn 
               (sleep 1)
               (print (list "getting comment for" id))
               (push id already-scraped-ids)
               (save-comments-to-output (get-comments-for-id id) id results-file)))) )
