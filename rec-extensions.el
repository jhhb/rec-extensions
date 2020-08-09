;;; rec-extensions.el --- A set of extension for rec-mode

;;; Commentary:
;; I intend for this to eventually become a package of wrappers around the
;; GNU Recutils utilities and (possibly) the Emacs major mode.
;; It's probably more accurate to brand this as "rec-extensions" or similar
;; rather than "enh-rec-mode" since I'd like this to be useful additions
;; to GNU Recutils and rec-mode rather than a fork or replacement of
;; Recutils or rec-mode.

;; I use org-mode and org tables to log lots of arbitrary events +
;; data.  Rather than storing this data in org tables, I'd like a way
;; to easily define data types and insert them into a log file
;; somewhere.  I'd like inserting data with pre-defined types to be
;; the default, with the option to arbitrarily insert data.  It would
;; be amazing to be able to run a key-command, select a record type to
;; insert, and then have an interactive experience to fill in the data
;; for the selected record type, with highlighting for the field
;; metadata, telling me which fields are mandatory, the type of each
;; field, requirements for each field, if the field is auto-created or
;; not, etc.  I'd like to run something like C-c r and then go through
;; a set of menus to fill in my data.

;; I'd love to be able to support the following:

;; - Interaction with, exploration of, recfile schemas

;; - Easy creation of records with the ability to see the schema
;;   the record on insert.

;; - A centralized place for recfile storage

;; - Syncs between the the schemas declared in recfiles, and a master
;; schema, to power interactive features.

;; Necessarily, powering some of these features is most easily
;; accomplished with a very opinionated approach to managing and using recfiles.

(require 'eldoc)
(require 'cl-lib)
(require 'flycheck)

(require 'rec-mode)

;;; Code:

(defcustom rec-extensions-data-directory-path
  (expand-file-name "rec-extensions" user-emacs-directory)
  "The path to the directory for rec-extensions to store its data."
  :group 'rec-extensions
  :type 'string)

(defcustom rec-extensions-data-file-path
  (expand-file-name "data.rec" rec-extensions-data-directory-path)
  "The path to the data file used by rec-extensions."
  :group 'rec-extensions
  :type 'string)

(cl-defstruct field
  (identity)
  (position)
  (rec-annotation)
  (value))

(cl-defstruct record
  (identity)
  (position)
  (fields))

(cl-defstruct descriptor
  (identity)
  (record)
  (position))

(defun parse-descriptors ()
  "Parse each element of `rec-buffer-descriptors'."
  (mapcar #'parse-descriptor rec-buffer-descriptors))

(defun parse-descriptor (descriptor)
  "Parse a single DESCRIPTOR of form (descriptor record position)."
  (let ((identity (first descriptor))
	(record (second descriptor))
	(pos (third descriptor)))
    (make-descriptor :identity identity :record (parse-record record) :position pos)))

(defun parse-record (record)
  "Parse a single RECORD of form (record position fields)."
  (let ((identity (first record))
        (pos (second record))
	(fields (third record)))
    (make-record :identity identity :position pos :fields (parse-fields fields))))

(defun parse-fields (fields)
  "Parse a list of FIELDS."
  (mapcar #'parse-field fields))

(defun parse-field (field)
  "Map a FIELD input tuple of (identity position annotation value) and parse it."
  (let ((identity (first field))
	(pos (second field))
	(rec-annotation (third field))
	(value (fourth field)))
    (make-field :identity identity :position pos :rec-annotation rec-annotation :value value)))

;;; utilities
(defun before-commit ()
  "Run before commit."
  (checkdoc))

(provide 'rec-extensions)

;;; rec-extensions.el ends here
