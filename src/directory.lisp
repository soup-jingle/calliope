(in-package :calliope)


(defun directory-pathname-p (name)
  (flet ((component-present-p (value)
	   (and value (not (eql value :unspecific)))))
    (and (not (component-present-p (pathname-name name)))
	 (not (component-present-p (pathname-type name)))
	 name)))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (if (not (directory-pathname-p name))
	(make-pathname :directory (append (or (pathname-directory pathname) (list :relative))
					  (list (file-namestring pathname)))
		       :name nil
		       :type nil
		       :defaults pathname)
	pathname)))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (name-and-type (pathname (first (last directory)))))
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))

(defun list-directory (root)
  (directory (make-pathname :name :wild
			    :type :wild
			    :defaults (pathname-as-directory root))))

(defun directory-recurse (root function &key directories (test (constantly t)))
  (labels ((walk (dir)
	     (cond ((directory-pathname-p dir)
		    (when (and directories (funcall test dir))
		      (funcall function dir))
		    (dolist (x (list-directory dir)) (walk x)))
		   ((funcall test dir) (funcall function dir)))))
    (walk (pathname-as-directory root))))
