;;;;
;;;; Простая программа для управления ежедневными делами
;;;;
;;;;
(in-package :net.viva-calman.lisptodo)



;;;
;;; Переменные
;;;
(defparameter *today* nil)
(defparameter *tomorrow* (date-to-string (get-current-date)))
(defparameter *filename* nil)



;;;
;;; Классы
;;;
(defclass todoentry ()
  ((id :initarg :id
       :accessor id
       :initform 0
       :documentation "ID записи")
   (title :initarg :title
	  :accessor title
	  :initform (error "Необходимо задать название записи")
	  :documentation "Название записи")
   (status :initarg :status
	   :accessor status
	   :initform 0
	   :documentation "Статус выполнения задачи")))

(defclass todolist ()
  ((current-id :initarg :current-id
	       :accessor current-id
	       :initform 0
	       :documentation "Текущий номер записи в файле")
   (current-todo :initarg :current-todo
		     :accessor current-todo
		     :initform nil
		     :documentation "Массив с текущим списком дел")))

;;;
;;; Обобщенные функции
;;;

(defgeneric add-new-do (todolist
			title)
  ;; Добавление новой записи
  (:documentation "Добавление новой записи"))


(defgeneric update-current-id (todolist)
  ;; Обновление текущего номера записи в файле
  (:documentation "Обновление текущего id записи, для поддержания актуальности данных в объекте")
			       


(defgeneric show-todolist (todolist
			   mode)
  ;; Отображение списка дел
  (:documentation "Отображение списка дел. Режим определяет, отображаются все дела или только невыполненные"))

(defgeneric change-status (todoentry
			   status)
  ;; Изменение статуса записи
  (:documentation "Изменение статуса записи. 
0 - не выполнено, 
1 - выполнено, 
2 - удалено. Удаленные записи не отображаются при выводе, но сохраняются в объекте до вызова функции чистки"))

(defgeneric show-todo-entry (todoentry)
  ;;Отображение записи
  (:documentation "Отображение отдельной записи"))

(defgeneric show-all-entries (todolist
			      mode)
  ;; Вывод всех записей списка
  (:documentation "Вывод всех записей списка, в разных режимах")

(defgeneric select-entry-by-id (todolist
				id)
  ;; Вывод записи по заданному id
  (:documentation "Вывод записи по заданному id"))

(defgeneric read-today (todolist)
  ;; Превращение lisp-формы в объект
  (:documentation "Преобразуем lisp-форму"))

(defgeneric write-today (todolist)
  ;; Преобразование объекта в lisp-форму, пригодную для сохранения
  (:documentation "Преобразуем объект в lisp-форму"))

(defgeneric deserialize-todo (todoentry)
  ;; Преобразование отдельной записи в lisp-форму
  (:documentation "Преобразование отдельной записи в lisp-форму"))

(defgeneric serialize-todo (todoentry)
  ;; Преобразование lisp-формы в объект todoentry
  (:documentation "Преобразование lisp-формы в объект"))



;;;
;;; Методы
;;;
(defmethod add-new-do ((todolist todolist)
		       title)
  ;;Добавление новой записи
  (with-accessors ((current-id current-id)
		   (current-todo current-todo)) todolist
    (setf current-id (+ current-id 1))
    (push (list :id current-id :obj (make-instance 'todoentry :title title :id current-id)) current-todo)))

(defmethod change-status ((todoentry todoentry)
			  stat-val)
  ;; Изменение статуса записи
  (with-accessors ((status status)) todoentry
    (setf status stat-val)))

(defmethod show-todo-entry ((todoentry todoentry))
  ;; Возвращает список с содержимым отдельной записи
  (with-slots ((id id)
	       (title title)
	       (status status)) todoentry
    (list id title status)))

(defmethod show-all-entries ((todolist todolist)
			     mode)
  ;; Возвращаем все записи в текущем списке, режим задает, отображается ли список полностью 
  ;; или только выполненные/не выполненные записи
  (with-slots ((current-todo current-todo)) todolist
    (cond 
      ((= mode 0)
       (loop for i in current-todo collect (show-todo-entry (getf i :obj))))
      ((= mode 1)
       (remove-if-not #'(lambda (x) (= (third x) 1))
		      (loop for i in current-todo collect (show-todo-entry (getf i :obj)))))
      (t 
       (remove-if-not #'(lambda (x) (= (third x) 0))
		      (loop for i in current-todo collect (show-todo-entry (getf i :obj))))))))

(defmethod select-entry-by-id ((todolist todolist)
			       id)
  ;; Выбор записи по указанному id. Возвращает объект
  (with-slots ((current-todo current-todo)) todolist
    (first (remove-if-not #'(lambda (x) (= id (getf x :id))) current-todo))))

(defmethod write-today ((todolist todolist))
  ;; Преобразование списка дел в lisp-форму
  (with-slots ((current-id current-id)
	       (current-todo current-todo)) todolist
    (list current-id (loop for i in current-todo collect (parce-todo i)))))

(defmethod deserialize-todo ((todoentry todoentry))
  ;; Преобразование отдельной todo-записи в lisp-форму.
  (with-slots ((id id)
	       (title title)
	       (status status)) todoentry
    (list id title status)))
	       
			     

;;;
;;; Функции
;;;
(defun get-current-date ()
  ;; Получение дня, месяца и года на текущий момент
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (+ (get-universal-time) (* 3600 24)))
    (if (< month 10)
	(setf hour (concatenate 'string (write-to-string 0) (write-to-string month)))
	(setf hour (write-to-string month)))
    (list (write-to-string date) 
	  hour 
	  (write-to-string year))))

(defun init-current-todo ()
  ;; Инициализация пустого списка дел
  (setf *today* (make-instance 'todolist)))

(defun print-list (todo-list)
  ;; Отображение списка
  (format t "~{~{~a:~10t~a~t~a~%~}~}~%" todo-list))

(defun date-select ()
  ;; Принимает пользовательский ввод и возвращает список с датой
  (list (read-input "День")
	(read-input "Месяц")
	(read-input "Год")))

(defun date-to-string (date)
  ;; Принимает список и возвращает строку
  (let (month)
    (if (< (parce-integer (second date)) 10)
	(setf month (concatenate 'string (write-to-string 0) (second date)))
	(setf month (second date)))
    (concatenate 'string 
		 (third date) "-" month "-" (first date))
)

(defun read-input (prompt)
  ;; Чтение пользовательского ввода
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun write-new-todo (todo)
  ;; Сохранение новой записи
  (with-open-file (out todo
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print (write-today *today*) out))))

(defun load-todo (filename)
  ;; Загрузка записи
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *today* (read-today (read in))))))

(defun parce-todo (todo)
  ;; Разбивка todo
  (list (getf todo :id) (deserialize-todo (getf todo :obj))))

