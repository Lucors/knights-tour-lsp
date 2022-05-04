;;; Задание 11, вариант 9
;;; Задача о ходе коня (упрощ.)
;;; Протестировано на GNU CLISP v2.49+


;; Генератор списка из чисел [1:size] 
(defun numlist (size)
    (cdr 
        (reverse 
            (let ((out nil))
                (dotimes (n (+ 1 size) out)
                (setq out (cons n out)))
            )
        )
    )
)
;; Определяет наличие x в l
(defun is-contains (x l)
    (dolist (el l nil)
        (when (equal el x) (return t))
    )
)
;; Объединяет все вложенные в l списки
(defun concat (l)
    (let ((out '()))
        (dolist (el (reverse l) out)
            (setq out (append el out))
        )
    )
)
;; Свертка списка вправо 
(defun foldr (f tail l)
    (if (null l) tail
        (funcall f 
            (car l) 
            (foldr f tail (cdr l))
        )
    )
)


;; Целевая функция
(defun knight (m n)
    ;; Задаем шахматное поле как 
    ;; список координат клеток
    (setq board 
        (concat 
            (mapcar 
                (lambda (i) 
                    (mapcar 
                        (lambda (j) (list i j))
                        (numlist n)
                    )
                ) 
                (numlist m)
            )
        )
    )

    ;; Проверяет возможность хода
    (defun is-visited (x p cb)
        (cond 
            ((null (cdr cb)) nil)
            ((and 
                (equal x (car cb))
                (equal p (cadr cb))
            ) t)
            (t (is-visited x p (cdr cb)))
        )
    )
 
    ;; Возвращает следующие возможные перемещения коня
    (defun next-step (cb)
        (setq p (car cb))
        (setq s 
            (remove-if-not  
                (lambda (x) 
                    (and 
                        (<= 1 (car x) m)
                        (<= 1 (cadr x) n)
                    )
                )

                (mapcar 
                    (lambda (x) 
                        (list 
                            (+ (car p) (car x))
                            (+ (cadr p) (cadr x))
                        )
                    )
                    '((2 1) (1 2) (-1 2) (-2 1)(-2 -1) (-1 -2) (1 -2) (2 -1)) 
                )
            )
        )

        (remove-if
            (lambda (x) 
                (is-visited x p cb)
            )
            s
        )
    )
 
    ;; Возвращает возможные ветки движения
    (defun next-branches (cb) 
        (mapcar 
            (lambda (x) (cons x cb)) 
            (next-step cb) 
        )
    )

    ;; Проверяет является ли шаг конечным
    (defun is-branch-full (cb)
        (and 
            (equal (car cb) '(1 1)) 
            (null 
                (remove-if-not 
                    (lambda (x) (not (is-contains x cb))) 
                    board
                )
            )
        )
    )
 
    ;; Точка входа
    (defun move (cb)
        (setq nbs (next-branches cb))
        (cond 
            ((is-branch-full cb) (reverse cb))
            ((null nbs) '())
            (t  (foldr 
                    (lambda (x a) 
                        (cond 
                            ((null a) (move x))
                            (t a)
                        )
                    )
                    nil
                    nbs
                )
            )
        )
    )
    (move '((1 1)))
)
 
;; Проверка для поля 4x4
(print (knight 4 4))