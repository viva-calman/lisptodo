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

;
; Добавление новой записи
(defgeneric add-new-do ((todolist todolist)
			title)
  (:documentation "Добавление новой записи"))

;
; Обновление текущего номера записи в файле
(defgeneric update-current-id ((todolist todolist))
  (:documentation "Обновление текущего id записи, для поддержания актуальности данных в объекте")
			       
;
; Отображение списка дел
(defgeneric show-todolist ((todolist todolist)
			   mode)
  (:documentation "Отображение списка дел. Режим определяет, отображаются все дела или только невыполненные"))
;
; Изменение статуса записи
(defgeneric change-status ((todoentry todoentry)
			   id
			   status)
  (:documentation "Изменение статуса записи. 0 - не выполнено, 1 - выполнено, 2 - удалено. Удаленные записи не отображаются при выводе, но сохраняются в объекте до вызова функции чистки"))


;
; Методы
;

;
; Функции
;

  
