;; uses cl-trie

(defun load-word-into-trie (trie word)
  (setf (cl-trie:lookup trie word) (1+ (cl-trie:lookup trie word 0))))

(defun load-list-into-trie (trie l)
  (mapc (lambda (word) (load-word-into-trie trie word)) l)
  trie)

(defun autocompletions (trie prefix)
  (mapcar (lambda (word)
	    (concatenate 'string prefix (subseq word 1)))
	  (cl-trie:all-keys (cl-trie:find-node trie prefix))))
