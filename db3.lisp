;;;; http://xach.com/lisp/db3.lisp
;;
;; db3.lisp
#|

Database file structure

The structure of a dBASE III database file is composed of a header
and data records.  The layout is given below.


dBASE III DATABASE FILE HEADER:

+---------+-------------------+---------------------------------+
|  BYTE   |     CONTENTS      |          MEANING                |
+---------+-------------------+---------------------------------+
|  0      |  1 byte           | dBASE III version number        |
|         |                   |  (03H without a .DBT file)      |
|         |                   |  (83H with a .DBT file)         |
+---------+-------------------+---------------------------------+
|  1-3    |  3 bytes          | date of last update             |
|         |                   |  (YY MM DD) in binary format    |
+---------+-------------------+---------------------------------+
|  4-7    |  32 bit number    | number of records in data file  |
+---------+-------------------+---------------------------------+
|  8-9    |  16 bit number    | length of header structure      |
+---------+-------------------+---------------------------------+
|  10-11  |  16 bit number    | length of the record            |
+---------+-------------------+---------------------------------+
|  12-31  |  20 bytes         | reserved bytes (version 1.00)   |
+---------+-------------------+---------------------------------+
|  32-n   |  32 bytes each    | field descriptor array          |
|         |                   |  (see below)                    | --+
+---------+-------------------+---------------------------------+   |
|  n+1    |  1 byte           | 0DH as the field terminator     |   |
+---------+-------------------+---------------------------------+   |
                                                                    |
                                                                    |
A FIELD DESCRIPTOR:      <------------------------------------------+

+---------+-------------------+---------------------------------+
|  BYTE   |     CONTENTS      |          MEANING                |
+---------+-------------------+---------------------------------+
|  0-10   |  11 bytes         | field name in ASCII zero-filled |
+---------+-------------------+---------------------------------+
|  11     |  1 byte           | field type in ASCII             |
|         |                   |  (C N L D or M)                 |
+---------+-------------------+---------------------------------+
|  12-15  |  32 bit number    | field data address              |
|         |                   |  (address is set in memory)     |
+---------+-------------------+---------------------------------+
|  16     |  1 byte           | field length in binary          |
+---------+-------------------+---------------------------------+
|  17     |  1 byte           | field decimal count in binary   |
+---------+-------------------+---------------------------------+
|  18-31  |  14 bytes         | reserved bytes (version 1.00)   |
+---------+-------------------+---------------------------------+


The data records are layed out as follows:

     1. Data records are preceeded by one byte that is a space (20H) if the
        record is not deleted and an asterisk (2AH) if it is deleted.

     2. Data fields are packed into records with  no  field separators or
        record terminators.

     3. Data types are stored in ASCII format as follows:

     DATA TYPE      DATA RECORD STORAGE
     ---------      --------------------------------------------
     Character      (ASCII characters)
     Numeric        - . 0 1 2 3 4 5 6 7 8 9
     Logical        ? Y y N n T t F f  (? when not initialized)
     Memo           (10 digits representing a .DBT block number)
     Date           (8 digits in YYYYMMDD format, such as
                    19840704 for July 4, 1984)

More information, from
  http://www.independent-software.com/dbase-dbf-dbt-file-format.html

  The version byte is one of the following:

  Byte	Bits	Version
  0x02	0000	0010	FoxBase 1.0
  0x03	0000	0011	FoxBase 2.x / dBASE III
  0x83	1000	0011	FoxBase 2.x / dBASE III with memo file
  0x30	0011	0000	Visual FoxPro
  0x31	0011	0001	Visual FoxPro with auto increment
  0x32	0011	0010	Visual FoxPro with varchar/varbinary
  0x43	0100	0011	dBASE IV SQL Table, no memo file
  0x63	0110	0011	dBASE IV SQL System, no memo file
  0x8b	1000	1011	dBASE IV with memo file
  0xcb	1100	1011	dBASE IV SQL Table with memo file
  0xfb	1111	1011	FoxPro 2
  0xf5	1111	0101	FoxPro 2 with memo file

  Valid dBASE for DOS file; bits 0-2 indicate version number, bit 3
  indicates the presence of a dBASE for DOS memo file, bits 4-6 indicate the
  presence of a SQL table, bit 7 indicates the presence of any memo
  file (either dBASE m PLUS or dBASE for DOS)

  Memo file header structure

  Offset	Size	Type	Sample value	Description
  0x00		4	uint32	34	Index of next available data block
                                        (for appending data) (BIG ENDIAN)
  0x04		2	uint16	64	Block size in bytes
  0x06		506	 	 	(Reserved)


|#
(in-package :db3)

(defparameter *external-format* :ascii
  "External format of the DBF file Character data")

(defvar *code-page-list*
  '((#x00 :ascii "plain ol' ascii")
    (#x01 :cp437 "U.S. MS-DOS")
    (#x02 :cp850 "International MS-DOS")
    (#x03 :cp1252 "Windows ANSI")
    (#x04 :macroman "Standard Macintosh")
    (#x08 :cp865 "Danish OEM")
    (#x09 :cp437 "Dutch OEM")
    (#x0A :cp850 "Dutch OEM (secondary)")
    (#x0B :cp437 "Finnish OEM")
    (#x0D :cp437 "French OEM")
    (#x0E :cp850 "French OEM (secondary)")
    (#x0F :cp437 "German OEM")
    (#x10 :cp850 "German OEM (secondary)")
    (#x11 :cp437 "Italian OEM")
    (#x12 :cp850 "Italian OEM (secondary)")
    (#x13 :cp932 "Japanese Shift-JIS")
    (#x14 :cp850 "Spanish OEM (secondary)")
    (#x15 :cp437 "Swedish OEM")
    (#x16 :cp850 "Swedish OEM (secondary)")
    (#x17 :cp865 "Norwegian OEM")
    (#x18 :cp437 "Spanish OEM")
    (#x19 :cp437 "English OEM (Britain)")
    (#x1A :cp850 "English OEM (Britain) (secondary)")
    (#x1B :cp437 "English OEM (U.S.)")
    (#x1C :cp863 "French OEM (Canada)")
    (#x1D :cp850 "French OEM (secondary)")
    (#x1F :cp852 "Czech OEM")
    (#x22 :cp852 "Hungarian OEM")
    (#x23 :cp852 "Polish OEM")
    (#x24 :cp860 "Portuguese OEM")
    (#x25 :cp850 "Portuguese OEM (secondary)")
    (#x26 :cp866 "Russian OEM")
    (#x37 :cp850 "English OEM (U.S.) (secondary)")
    (#x40 :cp852 "Romanian OEM")
    (#x4D :cp936 "Chinese GBK (PRC)")
    (#x4E :cp949 "Korean (ANSI/OEM)")
    (#x4F :cp950 "Chinese Big 5 (Taiwan)")
    (#x50 :cp874 "Thai (ANSI/OEM)")
    (#x57 :cp1252 "ANSI")
    (#x58 :cp1252 "Western European ANSI")
    (#x59 :cp1252 "Spanish ANSI")
    (#x64 :cp852 "Eastern European MS-DOS")
    (#x65 :cp866 "Russian MS-DOS")
    (#x66 :cp865 "Nordic MS-DOS")
    (#x67 :cp861 "Icelandic MS-DOS")
    ;; (0x68 :None"Kamenicky (Czech) MS-DOS")
    ;; (0x69 :None"Mazovia (Polish) MS-DOS")
    (#x6a :cp737 "Greek MS-DOS (437G)")
    (#x6b :cp857 "Turkish MS-DOS")
    (#x78 :cp950 "Traditional Chinese (Hong Kong SAR, Taiwan) Windows")
    (#x79 :cp949 "Korean Windows")
    (#x7a :cp936 "Chinese Simplified (PRC, Singapore) Windows")
    (#x7b :cp932 "Japanese Windows")
    (#x7c :cp874 "Thai Windows")
    (#x7d :cp1255 "Hebrew Windows")
    (#x7e :cp1256 "Arabic Windows")
    (#xc8 :cp1250 "Eastern European Windows")
    (#xc9 :cp1251 "Russian Windows")
    (#xca :cp1254 "Turkish Windows")
    (#xcb :cp1253 "Greek Windows")
    (#x96 :maccyrillic "Russian Macintosh")
    (#x97 :maclatin2 "Macintosh EE")
    (#x98 :macgreek "Greek Macintosh")))

(defun language-driver-to-encoding (language-driver)
  (cadr (assoc language-driver *code-page-list*)))


;;; reading binary stuff
(defun read-uint32 (stream)
  (loop repeat 4
        for offset from 0 by 8
        for value = (read-byte stream)
          then (logior (ash (read-byte stream) offset) value)
        finally (return value)))

(defun read-uint16 (stream)
  (loop repeat 2
        for offset from 0 by 8
        for value = (read-byte stream)
          then (logior (ash (read-byte stream) offset) value)
        finally (return value)))



;;; objects

(defclass db3 ()
  ((filename :initarg :filename :accessor filename)
   (memo :accessor memo)
   (version-number :accessor version-number)
   (last-update :accessor last-update)
   (record-count :accessor record-count)
   (header-length :accessor header-length)
   (record-length :accessor record-length)
   (encoding :accessor encoding)
   (fields :accessor fields)))


(defclass db3-field ()
  ((name :accessor field-name)
   (type :accessor field-type)
   (data-address :accessor data-address)
   (field-length :accessor field-length)
   (field-count :accessor field-count)))


(defclass db3-memo ()
  ((filename :initarg :filename :accessor filename)
   (stream :accessor db3-memo-stream)
   (next-block :accessor next-block)
   (block-size :accessor block-size)))


(defclass db3-memo-block ()
  ((type :accessor block-type)
   (size :accessor block-size)
   (data :accessor block-data :initarg :data)))


(defun asciiz->string (array)
  (let* ((string-length (or (position 0 array)
                            (length array)))
         (string (make-string string-length)))
    (loop for i below string-length
          do (setf (schar string i) (code-char (aref array i))))
    string))

(defun ascii->string (array)
  (cond
    ((eq :ascii *external-format*) (map 'string #'code-char array))

    (t #+sbcl
       (sb-ext:octets-to-string array :external-format *external-format*)

       #+ccl
       (ccl:decode-string-from-octets array :external-format *external-format*))))


(declaim (inline unsigned-to-signed bytes->integer bytes->bigint))

(defun unsigned-to-signed (byte n)
  (declare (type fixnum n) (type unsigned-byte byte))
  (logior byte (- (mask-field (byte 1 (1- (* n 8))) byte))))

(defun bytes->integer (data)
  (declare ((simple-array (unsigned-byte 8) (4)) data))
  (let ((bits (logior (ash (aref data 3) 24)
                      (ash (aref data 2) 16)
                      (ash (aref data 1) 8)
                      (aref data 0))))
    (unsigned-to-signed bits 4)))

(defun bytes->bigint (data)
  (declare ((simple-array (unsigned-byte 8) (8)) data))
  (let ((bits (logior (ash (aref data 7) 56)
                      (ash (aref data 6) 48)
                      (ash (aref data 5) 40)
                      (ash (aref data 4) 32)
                      (ash (aref data 3) 24)
                      (ash (aref data 2) 16)
                      (ash (aref data 1) 8)
                      (aref data 0))))
    (unsigned-to-signed bits 8)))


(defun load-field-descriptor (stream)
  (let ((field (make-instance 'db3-field))
        (name (make-array 11 :element-type '(unsigned-byte 8))))
    (read-sequence name stream)
    (setf (field-name field) (asciiz->string name)
          (field-type field) (code-char (read-byte stream))
          (data-address field) (read-uint32 stream)
          (field-length field) (read-byte stream)
          (field-count field) (read-byte stream))
    (loop repeat 14 do (read-byte stream))
    field))


(defmethod field-count ((db3 db3))
  (length (fields db3)))


(defmethod load-header ((db3 db3) stream)
  (let ((version (read-byte stream)))
    (case version
      (#x03 nil)                        ; accepted version, nothing to do
      (#x31 nil)                        ; Visual FoxPro with auto_increment
      ((#x83 #x8b)
       (assert (not (null (filename db3))))
       (let ((memo (make-instance 'db3-memo)))
         (setf (filename memo)
               (or (probe-file
                    (make-pathname :defaults (filename db3) :type "dbt"))
                   (probe-file
                    (make-pathname :defaults (filename db3) :type "DBT")))
               (memo db3) memo)))
      (t (error "DB3: Can't handle DBF file with version 0x~x" version)))
    (let ((year (read-byte stream))
          (month (read-byte stream))
          (day (read-byte stream)))
      (setf (version-number db3) version
            (last-update db3) (list (if (< year 80) (+ 2000 year) (+ 1900 year))
                                    month
                                    day)
            (record-count db3) (read-uint32 stream)
            (header-length db3) (read-uint16 stream)
            (record-length db3) (read-uint16 stream))
      (file-position stream 29)
      (setf (encoding db3) (language-driver-to-encoding (read-byte stream)))
      (file-position stream 32)
      (setf (fields db3)
            (loop for read-ahead = (read-byte stream)
               ;; it's kind of a peek-byte here
               do (file-position stream (1- (file-position stream)))
               until (= read-ahead #x0D)
               collect (load-field-descriptor stream)))
      (assert (= (read-byte stream) #x0D))

      ;;
      ;; https://www.dbf2002.com/dbf-file-format.html
      ;;
      ;; Visual Foxpro only: A 263-byte range that contains the backlink,
      ;; which is the relative path of an associated database (.dbc) file,
      ;; information. If the first byte is 0x00, the file is not associated
      ;; with a database. Therefore, database files always contain 0x00.
      ;;
      (when (= #x31 version)
       (let ((backlink (make-array 263 :element-type '(unsigned-byte 8))))
         (read-sequence backlink stream)))

      (load-memo-header db3)
      db3)))

(defmethod load-memo-header ((db3 db3))
  (when (and (member (version-number db3) '(#x83 #x8b))
             (filename (memo db3)))     ; might be nil
    (setf (db3-memo-stream (memo db3))
          (open (filename (memo db3))
                :direction :input
                :element-type '(unsigned-byte 8)))
    (let ((stream (db3-memo-stream (memo db3))))
      ;;
      ;; https://www.clicketyclick.dk/databases/xbase/format/dbt.html#DBT_STRUCT
      ;;
      ;; Version 0x83 is dbase III+, meaning block size of 512
      ;; Version 0x8b is dbase IV, where size of block is per file
      ;;
      (setf (next-block (memo db3)) (read-uint32 stream)
            (block-size (memo db3)) (case (version-number db3)
                                      (#x83 512)
                                      (#x8b (file-position stream 20)
                                            (read-uint32 stream)))))))

(defmethod load-memo-record ((db3 db3) data)
  (unless (and (slot-boundp db3 'memo)
               (slot-boundp (memo db3) 'stream)
               (db3-memo-stream (memo db3)))
    ;; silently ignore memo files set to NIL
    (return-from load-memo-record nil))

  (let* ((stream (db3-memo-stream (memo db3)))
         (record (make-instance 'db3-memo-block :data nil))
         (block-index
          (case (length data)
            (4 (read-uint32 data))
            (t (parse-integer (ascii->string data) :junk-allowed t))))
         (position (when block-index
                     (* (block-size (memo db3)) block-index))))
    (when position
      (file-position stream position)
      (case (version-number db3)
        (#x83
         ;; dbase III+ memo record only have the data, no type, no size
         (let ((block-data (make-array (block-size (memo db3))
                                       :element-type '(unsigned-byte 8))))
           (read-sequence block-data stream)
           (let ((terminator (search #(#x1a #x1a) block-data)))
             (if terminator
                 (let ((user-data (subseq block-data 0 terminator)))
                   (setf (block-data record) (ascii->string user-data)))
                 (error "DB3: failed to find 0x1A 0x1A record terminator")))))

        (#x8b
         ;; first 4-bytes are FFh FFh 08h 00h
         (let* ((reserved   (make-array 4 :element-type '(unsigned-byte 8)))
                (dummy      (read-sequence reserved stream))
                (size       (read-uint32 stream))
                (block-data (make-array size :element-type '(unsigned-byte 8))))
           (declare (ignore dummy))

           (setf (block-size record) size)
           (read-sequence block-data stream)

           (let ((user-data (subseq block-data 0 (position #x1f block-data))))
             (setf (block-data record) (ascii->string user-data)))))))
    record))

(defmethod close-memo ((db3 db3))
  (let ((s (when (and (slot-boundp db3 'memo)
                      (memo db3)
                      (slot-boundp (memo db3) 'stream))
             (db3-memo-stream (memo db3)))))
    (when (and s (open-stream-p s))
      (close s))))


(defmethod convert-field (db3 type data)
  (declare (ignore db3 type))
  (ascii->string data))

(defmethod convert-field (db3 (type (eql #\0)) data)
  (declare (ignore db3))
  data)

(defmethod convert-field (db3 (type (eql #\I)) data)
  (declare (ignore db3)
           ((simple-array (unsigned-byte 8) (4)) data))
  (bytes->integer data))

(defmethod convert-field (db3 (type (eql #\+)) data)
  (declare (ignore db3)
           ((simple-array (unsigned-byte 8) (4)) data))
  (bytes->integer data))

(defmethod convert-field (db3 (type (eql #\Y)) data)
  (declare (ignore db3)
           ((simple-array (unsigned-byte 8) (8)) data))
  (bytes->bigint data))

(defmethod convert-field (db3 (type (eql #\F)) data)
  (declare (ignore db3))
  (let ((*read-default-float-format* 'double-float)
        (float-string (string-right-trim '(#\Space) (ascii->string data))))
    (if (string= "" float-string) nil (read-from-string float-string))))

(defmethod convert-field (db3 (type (eql #\C)) data)
  (declare (ignore db3))
  (ascii->string data))

(defmethod convert-field (db3 (type (eql #\M)) data)
  (let ((memo (load-memo-record db3 data)))
    (when memo
      (block-data memo))))


(defmethod load-field ((db3 db3) (field db3-field) stream)
  (let ((data
         (make-array (field-length field) :element-type '(unsigned-byte 8))))
    (read-sequence data stream)
    (convert-field db3 (field-type field) data)))

(defmethod load-record ((db3 db3) stream)
  (let ((*external-format* (or (encoding db3) *external-format*))
        (deleted-mark      (read-byte stream)))
    (loop with record = (make-array (field-count db3))
       for i below (field-count db3)
       for field in (fields db3)
       do (setf (svref record i) (load-field db3 field stream))
       finally (return (values record (= #. (char-code #\*) deleted-mark))))))


(defun write-record (record stream)
  (loop for field across record
        do
        (write-char #\" stream)
        (write-string field stream)
        (write-string "\"," stream))
  (terpri stream))


(defun dump-db3 (input output)
  (with-open-file (stream input :direction :input
                          :element-type '(unsigned-byte 8))
    (with-open-file (ostream output :direction :output
                             :element-type 'character)
      (let ((db3 (make-instance 'db3 :filename input)))
        (load-header db3 stream)
        (loop repeat (record-count db3)
              do (write-record (load-record db3 stream) ostream))
        db3))))

(defun sample-db3 (input ostream &key (sample-size 10))
  (with-open-file (stream input :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((db3 (make-instance 'db3)))
      (load-header db3 stream)
      (loop
	 :repeat (min sample-size (record-count db3))
	 :do (format ostream "~s~%" (load-record db3 stream)))
      db3)))

(defun inspect-db3 (input ostream &key (sample-size 3))
  (with-open-file (stream input :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((db3 (make-instance 'db3 :filename (truename stream))))
      (load-header db3 stream)

      (format ostream "Version:   ~a~%" (version-number db3))
      (format ostream "Records:   ~a~%" (record-count db3))
      (format ostream "Encoding:  ~a~%" (encoding db3))
      (when (memo db3)
        (format ostream "Memo file: ~a~%" (filename (memo db3))))
      (format ostream "Fields:~%~{  ~a~^~%~}~%"
              (mapcar (lambda (f) (format nil "~a ~12t~c ~3db"
                                          (field-name f)
                                          (field-type f)
                                          (field-length f)))
                      (fields db3)))
      (loop
	 :repeat (min sample-size (record-count db3))
	 :do (format ostream "~s~%" (load-record db3 stream)))
      db3)))
