;;; thread-pool.el --- Thread Pool                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; Keywords: extensions, lisp, threads

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a straightforward implementation of the thread pool implementation
;; described in `PThreads Programming'.
;; https://www.oreilly.com/library/view/pthreads-programming/9781449364724/
;; https://resources.oreilly.com/examples/9781565921153/

;; It doesn't work as expected because the multi-threaded support is
;; still limited on the Emacs side.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (if t
      (defmacro printf (format &rest args)
        `(princ (funcall #'format ,format ,@args)))
    (defmacro printf (_format &rest _args))))

(cl-defstruct tpool-work
  routine
  arg
  next)

(cl-defstruct tpool
  num-threads
  max-queue-size
  do-not-block-when-full
  ;; pool state
  threads
  cur-queue-size
  queue-head
  queue-tail
  queue-closed
  shutdown
  ;; pool synchronization
  queue-lock
  queue-not-empty
  queue-not-full
  queue-empty)

(defmacro tpool--with-slots (obj &rest body)
  (declare (indent 1))
  `(cl-symbol-macrolet
       ,(mapcar (lambda (slot)
                  `(,slot (,(intern (concat "tpool-" (symbol-name slot))) ,obj)))
                (mapcar #'car (cdr (cl-struct-slot-info 'tpool))))
     ,@body))

(defun tpool-init (num-worker-threads max-queue-size &optional do-not-block-when-full)
  (let* ((queue-lock (make-mutex))
         (tpool (make-tpool :num-threads num-worker-threads
                            :max-queue-size max-queue-size
                            :do-not-block-when-full do-not-block-when-full
                            :threads (make-vector num-worker-threads nil)
                            :cur-queue-size 0
                            :queue-head nil
                            :queue-tail nil
                            :queue-closed nil
                            :shutdown nil
                            :queue-lock queue-lock
                            :queue-not-empty (make-condition-variable queue-lock)
                            :queue-not-full (make-condition-variable queue-lock)
                            :queue-empty (make-condition-variable queue-lock))))
    (dotimes (i num-worker-threads)
      (aset (tpool-threads tpool) i (make-thread (lambda () (tpool--thread tpool)))))
    tpool))

(defun tpool-add-work (tpool routine arg)
  (cl-block nil
    (tpool--with-slots tpool
      (with-mutex queue-lock
        ;; no space and this caller doesn't want to wait
        (when (and (= cur-queue-size max-queue-size)
                   do-not-block-when-full)
          (cl-return))

        (while (and (= cur-queue-size max-queue-size)
                    (not shutdown)
                    (not queue-closed))
          (condition-wait queue-not-full))

        ;; the pool is in the process of being destroyed
        (when (or shutdown queue-closed)
          (cl-return))

        ;; allocate work structure
        (let ((work (make-tpool-work :routine routine
                                     :arg arg
                                     :next nil)))
          (printf "adder: adding an item %s\n" routine) ;

          (if (not (zerop cur-queue-size))
              (setf (tpool-work-next queue-tail) work
                    queue-tail work)
            (setf queue-tail work
                  queue-head work)

            (printf "adder: queue == 0, waking all workers\n")

            (condition-notify queue-not-empty t))

          (cl-incf cur-queue-size)
          t)))))

(defun tpool-destroy (tpool &optional finish)
  (cl-block nil
    (tpool--with-slots tpool
      (with-mutex queue-lock
        ;; Is a shutdown already in progress?
        (when (or queue-closed shutdown)
          (cl-return))

        (setf queue-closed t)

        ;; If the finish flag is set, wait for workers to drain queue
        (when finish
          (while (not (zerop cur-queue-size))
            (condition-wait queue-empty)))

        (setf shutdown t)

        ;; Wake up any workers so they recheck shutdown flag
        (condition-notify queue-not-empty t)
        (condition-notify queue-not-full t))

      ;; Wait for workers to exit
      (dotimes (i num-threads)
        (thread-join (aref threads i)))

      ;; Now free pool structures
      )))

(defun tpool--thread (tpool)
  (cl-block nil
    (tpool--with-slots tpool
      (while t
        (let (my-work)
          ;; Check queue for work
          (with-mutex queue-lock
            (while (and (zerop cur-queue-size)
                        (not shutdown))
              (printf "worker %s: I'm sleeping again\n" (current-thread))
              (condition-wait queue-not-empty))

            (printf "worker %s: I'm awake\n" (current-thread)) ;

            ;; Has a shutdown started while i was sleeping?
            (when shutdown
              (cl-return))

            (setq my-work queue-head)
            (cl-decf cur-queue-size)
            (if (zerop cur-queue-size)
                (setf queue-head nil
                      queue-tail nil)
              (setf queue-head (tpool-work-next my-work)))

            (printf "worker %s: dequeing item %s\n" (current-thread) (tpool-work-next my-work))

            ;; Handle waiting add_work threads
            (when (and (not do-not-block-when-full)
                       (= cur-queue-size (1- max-queue-size)))
              (condition-notify queue-not-full t))

            ;; Handle waiting destroyer threads
            (when (zerop cur-queue-size)
              (condition-notify queue-empty)))

          ;; Do this work item
          (funcall (tpool-work-routine my-work) (tpool-work-arg my-work)))))))

(provide 'thread-pool)
;;; thread-pool.el ends here
