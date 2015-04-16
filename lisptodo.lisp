;;;;
;;;; Простая программа для управления ежедневными делами
;;;;
;;;;
(in-package :net.viva-calman.lisptodo)

;;;
;;; Переменные
;;;
(defparameter *today* nil)

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
  ((current-id :accessor current-id
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



;;;
;;; Функции
;;;
(defun get-current-date ()
  ;; Получение дня, месяца и года на текущий момент
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (list date month year)))
  

(defun init-current-todo ()
  ;; Инициализация пустого списка делo
  (setf *today* (make-instance 'todolist)))
