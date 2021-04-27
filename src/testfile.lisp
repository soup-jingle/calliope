(ql:quickload :cffi)

(defpackage #:flac-metadata
  (:use #:common-lisp #:cffi))

(in-package #:flac-metadata)

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


(define-foreign-library flac-library
  (:darwin (:or "libFLAC.dylib"
                "/opt/local/lib/libFLAC.dylib"))
  (:unix (:or "libFLAC.so.8"
              "/usr/lib/libFLAC.so"
              "/usr/local/lib/libFLAC.so"))
  (t (:default "libFLAC")))

(use-foreign-library flac-library)

(defcstruct stream-info
  (min-blocksize   :uint)
  (max-blocksize   :uint)
  (min-framesize   :uint)
  (max-framesize   :uint)
  (sample-rate     :uint)
  (channels        :uint)
  (bits-per-sample :uint)
  (total-samples   :uint64) ;; FLAC__uint64
  (md5sum          :char :count 16))

(defcstruct padding
  (dummy :int))

(defcstruct application
  (id :char :count 4)
  (data (:pointer :char)))

(defcstruct seek-point
  (sample-number :uint64)
  (stream-offset :uint64)
  (frame-samples :uint))

(defcstruct seek-table
  (num-points :uint)
  (points (:pointer (:struct seek-point))))

(defcstruct vorbis-comment-entry
  (length :uint32)
  (entry (:pointer :char)))

(defcstruct vorbis-comment
  (vendor-string (:struct vorbis-comment-entry))
  (num-comments :uint32)
  (comments (:pointer (:struct vorbis-comment-entry))))

(defcstruct cue-sheet-index
  (offset :uint64)
  (number :char))

(defcstruct cue-sheet-track
  (offset :uint64)
  (number :char)
  (isrc :char :count 13)
  (type :uint)  ;; 0 for audio, 1 for non-audio
  (pre-emphasis :uint) ; 0 for no pre-emphasis, 1 for pre-emphasis
  (num-indices :char)
  (indices (:pointer (:struct cue-sheet-index))))

(defcstruct cue-sheet
  (media-catalog-number :char :count 129)
  (lead-in :uint64)
  (is-cd   :int)
  (num-tracks :uint)
  (tracks (:pointer (:struct cue-sheet-track))))

(defcenum picture-type
  :FLAC__STREAM_METADATA_PICTURE_TYPE_OTHER                     ;; Other
  :FLAC__STREAM_METADATA_PICTURE_TYPE_FILE_ICON_STANDARD        ;; 32x32 pixels 'file icon' (PNG only)
  :FLAC__STREAM_METADATA_PICTURE_TYPE_FILE_ICON                 ;; Other file icon
  :FLAC__STREAM_METADATA_PICTURE_TYPE_FRONT_COVER               ;; Cover (front)
  :FLAC__STREAM_METADATA_PICTURE_TYPE_BACK_COVER                ;; Cover (back)
  :FLAC__STREAM_METADATA_PICTURE_TYPE_LEAFLET_PAGE              ;; Leaflet page
  :FLAC__STREAM_METADATA_PICTURE_TYPE_MEDIA                     ;; Media (e.g. label side of CD)
  :FLAC__STREAM_METADATA_PICTURE_TYPE_LEAD_ARTIST               ;; Lead artist/lead performer/soloist
  :FLAC__STREAM_METADATA_PICTURE_TYPE_ARTIST                    ;; Artist/performer
  :FLAC__STREAM_METADATA_PICTURE_TYPE_CONDUCTOR                 ;; Conductor
  :FLAC__STREAM_METADATA_PICTURE_TYPE_BAND                      ;; Band/Orchestra
  :FLAC__STREAM_METADATA_PICTURE_TYPE_COMPOSER                  ;; Composer
  :FLAC__STREAM_METADATA_PICTURE_TYPE_LYRICIST                  ;; Lyricist/text writer
  :FLAC__STREAM_METADATA_PICTURE_TYPE_RECORDING_LOCATION        ;; Recording Location
  :FLAC__STREAM_METADATA_PICTURE_TYPE_DURING_RECORDING          ;; During recording
  :FLAC__STREAM_METADATA_PICTURE_TYPE_DURING_PERFORMANCE        ;; During performance
  :FLAC__STREAM_METADATA_PICTURE_TYPE_VIDEO_SCREEN_CAPTURE      ;; Movie/video screen capture
  :FLAC__STREAM_METADATA_PICTURE_TYPE_FISH                      ;; A bright coloured fish
  :FLAC__STREAM_METADATA_PICTURE_TYPE_ILLUSTRATION              ;; Illustration
  :FLAC__STREAM_METADATA_PICTURE_TYPE_BAND_LOGOTYPE             ;; Band/artist logotype
  :FLAC__STREAM_METADATA_PICTURE_TYPE_PUBLISHER_LOGOTYPE        ;; Publisher/Studio logotype
  )

(defcstruct picture
  (type picture-type)
  (mime-type (:pointer :char))
  (description (:pointer :char))
  (width :uint32)
  (height :uint32)
  (depth :uint32)
  (colors :uint32)
  (data-length :uint32)
  (data (:pointer :char)))

(defcstruct unknown
  (data (:pointer :char)))

(defcunion flac-union
  (stream-info    (:struct stream-info))
  (padding        (:struct padding))
  (application    (:struct application))
  (seek-table     (:struct seek-table))
  (vorbis-comment (:struct vorbis-comment))
  (cue-sheet      (:struct cue-sheet))
  (picture        (:struct picture))
  (unknown        (:struct unknown)))

(defcenum flac-metadata-type
  :FLAC-METADATA-TYPE-STREAMINFO 	;; STREAMINFO block
  :FLAC-METADATA-TYPE-PADDING 		;; PADDING block
  :FLAC-METADATA-TYPE-APPLICATION 	;; APPLICATION block
  :FLAC-METADATA-TYPE-SEEKTABLE 	;; SEEKTABLE block
  :FLAC-METADATA-TYPE-VORBIS-COMMENT 	;; VORBISCOMMENT block (a.k.a. FLAC tags)
  :FLAC-METADATA-TYPE-CUESHEET 	;; CUESHEET block
  :FLAC-METADATA-TYPE-PICTURE 		;; PICTURE block
  :FLAC-METADATA-TYPE-UNDEFINED 	;; marker to denote beginning of undefined type range
					;; this number will increase as new metadata types are added
  :FLAC-MAX-METADATA-TYPE		;; No type will ever be greater than this.
                                        ;; There is not enough room in the protocol block. 
  )

(defcstruct metadata
  (type flac-metadata-type)
  (is-last :int)
  (length :uint)
  (data (:union flac-union)))


(defcfun ("FLAC__metadata_get_tags" flac-metadata-get-tags) :int
  (filename :string)
  (tags (:pointer (:pointer (:struct metadata)))))


(defun get-tags (file)
  (let (library
	(m (foreign-alloc '(:struct metadata))))
    (with-foreign-objects ((p :pointer)
			   (vc '(:struct vorbis-comment-entry)))
      (setf (mem-aref p :pointer) m)
      (let ((i (flac-metadata-get-tags file p)))
	(if (= i 1)
	    (with-foreign-slots ((type data) (mem-aref p :pointer) (:struct metadata))
	      (if (= (foreign-enum-value '(flac-metadata-type) type) 4)
		  (with-foreign-slots ((num-comments comments) data (:struct vorbis-comment))
		    (setf vc comments)
		    (loop for i below num-comments
			  do (with-foreign-slots ((entry) (mem-aptr vc '(:struct vorbis-comment-entry) i) (:struct vorbis-comment-entry))
				 (let ((e (foreign-string-to-lisp entry)))
				   (destructuring-bind (name value) (parse-tag e)
				     (let ((existing-values (getf library name)))
				       (setf (getf library name) (append existing-values (list value))))))))))))))
    (foreign-funcall "FLAC__metadata_object_delete" :pointer m :void)
    library))

; (cffi:close-foreign-library 'flac-library)
