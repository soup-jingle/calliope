(in-package :calliope)

(defun delimiterp (char)
  (char= char #\Space))

(defun make-keyword (string)
  (intern (string-upcase string) "KEYWORD"))

(defun keyword->string (kw)
  (string-downcase (string kw)))

(defun split (string &key (delimiters " ") (max nil))
  (flet ((delimiterp (char) (position char delimiters)))
    (loop for count = 1
            then (1+ count)
          for start = (position-if-not #'delimiterp string)
            then (position-if-not #'delimiterp string :start (1+ end))
          for end = (if (not (equal count max))
			(and start
			     (position-if #'delimiterp string :start start)))
          when start
          collect (subseq string start end)
          while end)))

(defun split-tag (raw-tag)
  (let* ((delimiters "="))
    (split raw-tag :delimiters delimiters :max 2)))

(defun convert-split-tag (split-tag)
  (destructuring-bind (name &optional (value "")) split-tag
    (list (make-keyword name) value)))

(defun parse-tag (raw-tag)
  (convert-split-tag (split-tag raw-tag)))


(defun change-assoc (alist key value)
  (let ((cassoc (assoc key alist)))
    (if (null cassoc)
	(setf alist (acons key value alist))
	(setf (cdr cassoc) value)))
  alist)
