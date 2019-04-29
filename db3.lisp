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
  (1- (/ (1- (header-length db3)) 32)))


(defmethod load-header ((db3 db3) stream)
  (let ((version (read-byte stream)))
    (case version
      (#x03 nil)                        ; accepted version, nothing to do
      (#x83
       (assert (not (null (filename db3))))
       (let ((memo (make-instance 'db3-memo)))
         (setf (filename memo)
               (or (probe-file
                    (make-pathname :defaults (filename db3) :type "dbt"))
                   (probe-file
                    (make-pathname :defaults (filename db3) :type "DBT")))
               (memo db3) memo)))
      (t (error "DB3: Can't handle DBF file with version ~x" version)))
    (let ((year (read-byte stream))
          (month (read-byte stream))
          (day (read-byte stream)))
      (setf (version-number db3) version
            (last-update db3) (list year month day)
            (record-count db3) (read-uint32 stream)
            (header-length db3) (read-uint16 stream)
            (record-length db3) (read-uint16 stream))
      (file-position stream 32)
      (setf (fields db3) (loop repeat (field-count db3)
                               collect (load-field-descriptor stream)))
      (assert (= (read-byte stream) #x0D))
      (load-memo-header db3)
      db3)))

(defmethod load-memo-header ((db3 db3))
  (when (= #x83 (version-number db3))
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
            (block-size (memo db3)) 512))))

(defmethod load-memo-record ((db3 db3) data)
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
      ;; dbase III+ memo record only have the data, no type, no size
      (let ((block-data (make-array (block-size (memo db3))
                                    :element-type '(unsigned-byte 8))))
        (read-sequence block-data stream)
        (let ((terminator (search #(#x1a #x1a) block-data)))
          (if terminator
              (let ((user-data (subseq block-data 0 terminator)))
                (setf (block-data record) (ascii->string user-data)))
              (error "DB3: failed to find 0x1A 0x1A record terminator")))))
    record))

(defmethod close-memo ((db3 db3))
  (let ((s (when (slot-boundp db3 'memo)
             (db3-memo-stream (memo db3)))))
    (when (and s (open-stream-p s))
      (close s))))


(defmethod convert-field (db3 type data)
  (declare (ignore db3 type))
  (ascii->string data))

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
  (read-byte stream)
  (loop with record = (make-array (field-count db3))
        for i below (field-count db3)
        for field in (fields db3)
        do (setf (svref record i) (load-field db3 field stream))
        finally (return record)))


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
	 :repeat sample-size
	 :do (format ostream "~s~%" (load-record db3 stream)))
      db3)))

