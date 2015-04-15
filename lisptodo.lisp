;
; Простая программа для управления ежедневными делами
;
;
(in-package :net.viva-calman.lisptodo)

;
; Переменные
;
(defparameter *today* nil)
(defvar *current-id*)

;
; Классы
;
(defclass todoentry ()
  ((id :accessor id
       :initform (+ *current-id* 1)
       :documentation "ID записи")
   (title :initarg :title
	  :accessor title
	  :initform (error "Необходимо задать название записи")
	  :documentation "Название записи")
   (status :initarg :status
	   :accessor status
	   :initform nil
	   :documentation "Статус выполнения задачи")))

(defclass todolist ()
  ((current-id :accessor current-id
	       :initform 0
	       :documentation "Текущий номер записи в файле")
   (current-todo :initarg :current-todo
		     :accessor current-todo
		     :initform nil
		     :documentation "Массив с текущим списком дел")))

;
; Обобщенные функции
;

(defgeneric add-new-do ((todolist todolist)
			title)
  (:documentation "Добавление новой записи"))

		      

;
; Методы
;

;
; Функции
;

  
