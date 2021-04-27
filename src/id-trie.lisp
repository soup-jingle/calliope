(in-package :trie)

(defclass id-trie (trie)
  ((value
   :initarg :value
   :accessor value
   :type integer)))

;; add-child, add new ID corresponding to an key for a hash-table.
;;    generate a new ID if not exist, use ID as value

(defmethod add-child ((trie id-trie) key &optional value)
  (let* ((new-node (make-instance 'id-trie :key key :value value))
	 (children (children trie))
	 (existing-node (find key children :key #'key)))
    (if existing-node
	existing-node
	(progn (setf (children trie)
		     (sort (cons new-node children) #'char-lessp :key #'key))
	       new-node))))

(defmethod add-string ((trie id-trie) string id)
  (let ((n trie))
    (loop for c across string
	  do (setf n (add-child n c))
	  finally (progn (setf (suffixp n) t)
			 (setf (value n) id)))))
