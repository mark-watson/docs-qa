(in-package #:docs-qa)

(ql:quickload :sqlite)
(use-package :sqlite)

(use-package :parse-float)

;; define the environment variable "OPENAI_KEY" with the value of your OpenAI API key

(defun write-floats-to-string (lst)
  (with-output-to-string (out)
    (format out "( ")
    (loop for i in lst
          do (format out "~f " i))
    (format out " )")))

(defun decode-row (row)
  (let ((id (nth 0 row))
        (context (nth 1 row))
        (embedding (read-from-string (nth 2 row))))
    (list id context embedding)))

(defun qa (question)
  (let ((answer (openai:answer-question question 60)))
    (format t "~&~a~%" answer)))

;;(use-package :iter)
;;(use-package :sqlite-ffi)

(defvar *db* (connect ":memory:"))
(pprint *db*)

;;(defparameter *db* (sqlite3-open "test.db" *db*))
(execute-non-query *db* "CREATE TABLE documents (id INTEGER PRIMARY KEY, content TEXT, embedding TEXT);")
(execute-non-query *db* "CREATE INDEX idx_documents_id ON documents (id);")
(execute-non-query *db* "CREATE INDEX idx_documents_content ON documents (content);")
(execute-non-query *db* "CREATE INDEX idx_documents_embedding ON documents (embedding);")

(defun insert-document (id content embedding)
  (execute-non-query *db* "INSERT INTO documents (id, content, embedding) VALUES (?, ?, ?);" id content (write-floats-to-string embedding)))

(defun get-document-by-id (id)
  (mapcar #'decode-row
            (execute-to-list *db* "SELECT * FROM documents WHERE id = ?;" id)))

(defun get-document-by-content (content)
  (mapcar #'decode-row 
    (execute-to-list *db* "SELECT * FROM documents WHERE content LIKE ?;" content)))

(defun get-document-by-embedding (embedding)
 (mapcar #'decode-row 
   (execute-to-list *db* "SELECT * FROM documents WHERE embedding LIKE ?;" embedding)))

(defun all-documents ()
  (mapcar #'decode-row 
    (execute-to-list *db* "SELECT * FROM documents;")))

(defun create-document (content)
  (let ((embedding (openai::embeddings content)))
    (insert-document (random 1000000) content embedding)))

(create-document "John bought a new car")
(create-document "who bought what")

(defvar docs (all-documents))
(pprint docs)

