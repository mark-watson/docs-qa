(in-package #:docs-qa)

;; Copyright 2023 Mark Watson. All Rights Reserved. Apache 2 License

(ql:quickload :sqlite)
(use-package :sqlite)

;;(use-package :parse-float)

;; define the environment variable "OPENAI_KEY" with the value of your OpenAI API key

(defun write-floats-to-string (lst)
  (with-output-to-string (out)
    (format out "( ")
    (loop for i in lst
          do (format out "~f " i))
    (format out " )")))

(defun read-file (infile) ;; from Bing+ChatGPT
  (with-open-file (instream infile
                            :direction :input
                            :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun concat-strings (list)
  (apply #'concatenate 'string list))

(defun truncate-string (string length)
  (subseq string 0 (min length (length string))))

(defun break-into-chunks (text chunk-size)
  "Breaks TEXT into chunks of size CHUNK-SIZE."
  (loop for start from 0 below (length text) by chunk-size
        collect (subseq text start (min (+ start chunk-size) (length text)))))

(defun decode-row (row)
  (let ((id (nth 0 row))
        (context (nth 1 row))
        (embedding (read-from-string (nth 2 row))))
    (list id context embedding)))

(defun qa (question)
  (let ((answer (openai:answer-question question 60)))
    (format t "~&~a~%" answer)))

(defvar *db* (connect ":memory:"))
;;(defvar *db* (connect "test.db"))

(pprint *db*)
(handler-case
    (progn
      (execute-non-query
       *db*
       "CREATE TABLE documents (document_path TEXT, content TEXT, embedding TEXT);")
      (execute-non-query *db* "CREATE INDEX idx_documents_id ON documents (document_path);")
      (execute-non-query *db* "CREATE INDEX idx_documents_content ON documents (content);")
      (execute-non-query *db* "CREATE INDEX idx_documents_embedding ON documents (embedding);"))
 (error (c)
   (print "Database and indices is already created")))

(defun insert-document (document_path content embedding)
  ;;(format t "~%insert-document:~%  content:~A~%  embedding: ~A~%" content embedding)
  (format t "~%insert-document:~%  content:~A~%~%" content)
  (execute-non-query
   *db*
   "INSERT INTO documents (document_path, content, embedding) VALUES (?, ?, ?);"
   document_path content (write-floats-to-string embedding)))

(defun get-document-by-document_path (document_path)
  (mapcar #'decode-row
            (execute-to-list *db*
                             "SELECT * FROM documents WHERE document_path = ?;"
                             document_path)))

(defun get-document-by-content (content)
  (mapcar #'decode-row 
    (execute-to-list *db*
                     "SELECT * FROM documents WHERE content LIKE ?;" content)))

(defun get-document-by-embedding (embedding)
 (mapcar #'decode-row 
   (execute-to-list *db*
                    "SELECT * FROM documents WHERE embedding LIKE ?;" embedding)))

(defun all-documents ()
  (mapcar #'decode-row 
    (execute-to-list *db* "SELECT * FROM documents;")))

(defun create-document (fpath)
  (let ((contents (break-into-chunks (read-file fpath) 200)))
    (dolist (content contents)
      (handler-case	  
	  (let ((embedding (openai::embeddings content)))
	    (insert-document fpath content embedding))
	(error (c)
	       (format t "Error: ~&~a~%" c))))))

;;(defvar docs (all-documents))
;;(pprint docs)

(defun semantic-match (query &optional (cutoff 0.7))
  (let ((emb (openai::embeddings query))
        (ret))
    (dolist (doc (all-documents))
      (let ((context (nth 1 doc)) ;; ignore fpath for now
	    (embedding (nth 2 doc)))
	(let ((score (openai::dot-product emb embedding)))
	  (when (> score cutoff)
	    (push context ret)))))
    (let* ((context (concat-strings ret))
           (query-with-context (concat-strings (list context "  Question:  " query))))
      (openai:answer-question query-with-context 40))))

(defun QA (query)
  (let ((answer (semantic-match query)))
    (format t "~%~%** query: ~A~%** answer: ~A~%~%" query answer)
    answer))

(defun test()
  "Test code for Semantic Document Search Using OpenAI GPT APIs and local vector database"
  (create-document "data/sports.txt")
  (create-document "data/chemistry.txt")
  (QA "What is the history of the science of chemistry?")
  (QA "What are the advantages of engainging in sports?"))

;; test()
    
