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
  (:documentation "Обновление текущего id записи, для поддержания актуальности данных в объекте"))

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
  (:documentation "Вывод всех записей списка, в разных режимах"))

(defgeneric select-entry-by-id (todolist
				id)
  ;; Вывод записи по заданному id
  (:documentation "Вывод записи по заданному id"))

(defgeneric write-today (todolist)
  ;; Преобразование объекта в lisp-форму, пригодную для сохранения
  (:documentation "Преобразуем объект в lisp-форму"))

(defgeneric deserialize-todo (todoentry)
  ;; Преобразование отдельной записи в lisp-форму
  (:documentation "Преобразование отдельной записи в lisp-форму"))

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
(defun get-current-date (&optional (offset (* 3600 24)))
  ;; Получение дня, месяца и года на текущий момент
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (+ (get-universal-time) offset))
    (list (write-to-string date) 
	  (write-to-string month) 
	  (write-to-string year)
	  hour
	  minute)))

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
    (if (< (parse-integer (second date)) 10)
	(setf month (concatenate 'string (write-to-string 0) (second date)))
	(setf month (second date)))
    (concatenate 'string 
		 (third date) "-" month "-" (first date))))

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
  (restart-case
      (progn 
	(with-open-file (in filename)
	  (with-standard-io-syntax
	    (setf *today* (serialize-today (read in))))))
    (file-error () (if-not-exist))
    (create-new (day) (create-new day))))

(defun parce-todo (todo)
  ;; Разбивка todo
  (list (getf todo :id) (deserialize-todo (getf todo :obj))))

(defun serialize-today (today)
  ;; Превращение lisp-формы в объект todolist
  (make-instance 'todolist 
		 :current-id (first today) 
		 :current-todo (serialize-todo (second today))))

(defun serialize-todo (todo)
  ;; Преобразуем lisp-форму todo в соответствующий объект
  (loop for i in todo collect (list :id (first i) 
				    :obj (make-instance 'todoentry 
							:id (first (second i)) 
							:title (second (second i)) 
							:status (third (second i))))))
(defun if-not-exist ()
  ;; Функция обработки ошибки в случае несуществующего файла
  (show-message "Указанный файл не существует. Введите верное имя файла")
  (open-todo))
  
(defun create-new (day)
  ;; Создание нового файла todo, если файл не существовал
  (with-open-file (out day
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
	(print (write-today (make-instance 'todolist)) out ))))

;;
;; Функции пользовательского интерфейса
;;
(defun user-interface ()
  ;; Основной CLI-интерфейс
  (show-message "Выберите действие:~%1) Добавить новую запись в завтрашний todo~%2) Открыть todo")
  (answer-digit ">"
    (format t "~%")
    (cond 
      ((= 1 ans))
      ((= 2 ans))
)))

	
(defun open-todo ()
  ;; Функция предоставляющая интерфейс для выбора действия с существующими файлами
  (show-message "По умолчанию загрузится сегодняшний todo
введите 1, для того, чтобы открыть todo на завтра
введите 2, чтобы открыть todo по заданной дате")
  (handler-bind ((sb-int:simple-file-error #'(lambda (c)
				    (invoke-restart 'file-error))))
    (dialog-gen (answer-input ">")
		((1 (load-tomorrow))
		 (2 (load-date)))
		(load-today))))
       
(defun load-today ()
  ;; Загрузка сегодняшнего todo
  (let ((today (get-current-date 0)))
    (handler-bind ((sb-int:simple-file-error #'(lambda (c)
						 (invoke-restart 'create-new (date-to-string today)))))
      (if (and (> (fourth today) 23) (> (fifth today) 50))
	  (show-message "До конца дня осталось меньше 10 минут. После этого завтрашний todo станет сегодняшним"))
      (load-todo (date-to-string today))
      (select-action today)
      (format t "todo загружен~%"))))

(defun load-tomorrow ()
  ;; Загрузка завтрашнего todo
  (format t "Загрузка...")
  (let ((tomorrow (get-current-date)))
    (handler-bind ((sb-int:simple-file-error #'(lambda (c)
						  (invoke-restart 'create-new (date-to-string tomorrow)))))
      (if (and (> (fourth tomorrow) 23) (> (fifth tomorrow) 50))
	  (show-message "До конца дня осталось меньше 10 минут. После этого будет создан новый завтрашний todo"))
      (load-todo (date-to-string tomorrow))
      (select-action tomorrow)
      (format t "todo загружен"))))

(defun load-date ()
  ;; Загрузка конкретного todo
)

(defun select-action (today)
  ;; Выбор действия, производимого с загруженным todo
  (show-message "Выбор действия :)
1 - Добавление новой записи в текущий todo
2 - Просмотр загруженного todo
3 - Изменение статуса записи
4 - Сохранение записи (действие по умолчанию)")
  (dialog-gen (answer-input ">")
	      ((1 (add-new-do *today* (read-input "Заголовок")))
	       (2 (show-todo))
	       (3 (change-status-todo)))
	      (write-new-todo (date-to-string today))))
       
(defun show-todo ()
  ;; Вывод списка с применением фильтра при необходимости
  (show-message "По умолчанию выводится список целиком.
Для фильтрации записей введите:
1 - Только невыполненные
2 - Только выполненные")
  (dialog-gen (answer-input ">")
	      ((1 (print-list (show-all-entries *today* 2)))
	       (2 (print-list (show-all-entries *today* 1))))
	      (print-list (show-all-entries *today* 0))))

(defun change-status-todo ()
  ;; Изменение статуса записи
  (show-message "Для изменения статуса записи, введите ее номер из первой колонки")
  (print-list (show-all-entries *today* 0))
  (let ((ans (answer-input ">")))
    (if ans
	(change-status (getf (select-entry-by-id *today* ans) :obj) (input-status))
	(format t "Нет такой записи"))))

(defun show-message (mess)
  ;; Вывод сообщения
  (format t "~a~%" mess))

(defun answer-input (prompt)
  ;; Ввод ответа и проверка его приемлимости
  (or (parse-integer (read-input prompt) :junk-allowed t) 0))

(defun input-status ()
  ;; Ввод статуса
  (show-message "Для того, чтобы пометить задачу выполненной, введите '1'
Для того, чтобы удалить запись, введите '2'
Для отмены - введите любое другое значение")
  (dialog-gen (answer-input ">")
	      ((1 (return-from input-status 1))
	       (2 (return-from input-status 2)))
	       (return-from input-status 0)))


;;;
;;; Макросы и вспомогательные функции для них
;;;

(defmacro answer-digit (prompt &rest body)
  ;; Макрос, генерирующий запрос ввода
  `(let ((,(gensym) (or (parse-integer (read-input ,prompt) :junk-allowed t) 0)))
     ,@body))
  
(defmacro dialog-gen (inans options default)
  ;; Макрос, конструирующи[Bй обработчик ответа пользователя
  (let ((ans-var (gensym))
	(ans-act options)
	(ans-def default))
    `(let ((,ans-var ,inans )) (cond ,@(make-cond ans-var ans-act) ,@(make-default-cond ans-def)))))

(defun make-cond (var bod)
  ;; Вспомогательная функция, генерирующая COND-выражение
  (loop for i in bod collect (list (list '= var (first i)) (second i))))

(defun make-default-cond (default)
  ;; Вспомогательная функция, генерирует вариант по умолчанию для COND
  (list (list t default)))

      
