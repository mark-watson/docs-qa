(in-package #:docs-qa)

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

(execute-non-query
 *db*
 "CREATE TABLE documents (document_path TEXT PRIMARY KEY, content TEXT, embedding TEXT);")
(execute-non-query *db* "CREATE INDEX idx_documents_id ON documents (document_path);")
(execute-non-query *db* "CREATE INDEX idx_documents_content ON documents (content);")
(execute-non-query *db* "CREATE INDEX idx_documents_embedding ON documents (embedding);")

(defun insert-document (document_path content embedding)
  (format t "~%insert-document:~%  content:~A~%  embedding: ~A~%" content embedding)
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
  (let* ((content  (truncate-string (read-file fpath) 40))
         (embedding (openai::embeddings content)))
    (insert-document fpath content embedding)))

(create-document "data/sports.txt")
(create-document "data/chemistry.txt")

;;(defvar docs (all-documents))
;;(pprint docs)

(defun semantic-match (query &optional (cutoff 0.7))
  (let ((emb (openai::embeddings query))
        (ret))
    (dolist (doc (all-documents))
      (let* ((emb2 (nth 2 doc))
             (similarity (openai:dot-product emb emb2)))
        (format t "simlilarity = ~A~%" similarity)
        (if (> similarity cutoff)
            (progn
	      (setf ret (cons (nth 1 doc) ret))
	      (setf ret (cons " " ret))))))
    (print ret)
    (let* ((context (concat-strings ret))
           (query-with-context (concat-strings (list context "  Question:  " query))))
      (print query-with-context)
      (openai:answer-question query-with-context 40))))

(print (semantic-match "What energy than photons can be absorbed by a molecule?"))

    
