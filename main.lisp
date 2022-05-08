(ql:quickload '("dexador" "plump" "lquery" "lparallel"))
(defparameter *url* "https://news.ycombinator.com")
(defparameter *request* (dex:get *url*))
(defparameter *parsed-content* (lquery:$ (initialize *request*)))

; Find 
(defparameter *hrefs* 
  (remove-if-not 
    #'(lambda (v) (search "item?" v))
    (lquery:$ *parsed-content* "td .subtext a" (attr :href))))

(defun get-comments-for-item (item-string)
 (let* ((result (dex:get (concatenate 'string *url* "/" item-string)))
       (parsed-content (lquery:$ (initialize result))))
  (lquery:$ parsed-content ".commtext" (text))) )

(get-comments-for-item (aref *hrefs* 0))

