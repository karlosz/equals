(in-package #:equals)

(defgeneric equals (x y &rest keys &key recursive &allow-other-keys)
  (:documentation
   "The EQUALS generic functions defines methods to test for 'equality'
of two objects a and b. When two objects a and b are EQUALS under an
appropriate and context-dependent notion of 'equality', then the function
returns T as result; otherwise EQUALS returns NIL as result.

If the argument recursive is T, then EQUALS may recurse down the 'structure'
of a and b. The description of each known method contains the relevant
information about its recursive dependent behavior."))

(defmethod equals ((x t) (y t) &rest keys &key recursive &allow-other-keys)
  (declare (ignore keys recursive))
  (equalp x y))

(defmethod equals ((x number) (y number) &rest keys &key recursive epsilon &allow-other-keys)
  (declare (ignore keys recursive))
  (check-type epsilon (or null real))
  (if epsilon
      (< (abs (- x y)) epsilon)
      (= x y)))

(defmethod equals ((x cons) (y cons) &rest keys &key recursive &allow-other-keys)
  (declare (ignore keys recursive))
  (tree-equal x y :test #'equals))

(defmethod equals ((x character) (y character) &rest keys &key recursive (case-sensitive t) &allow-other-keys)
  (declare (ignore keys recursive))
  (if case-sensitive (char= x y) (char-equal x y)))

(defmethod equals ((x string) (y string) &rest keys &key recursive (case-sensitive t) &allow-other-keys)
  (declare (ignore keys recursive))
  (if case-sensitive (string= x y) (string-equal x y)))

(defmethod equals ((x array) (y array) &rest keys &key recursive &allow-other-keys)
  (declare (ignore recursive))
  (when (and (array-rank x) (array-rank y))
    (dotimes (axis (array-rank x) t)
      (unless (= (array-dimension x axis)
                 (array-dimension y axis))
        (return nil)))
    (dotimes (index (array-total-size x) t)
      (let ((x-el (row-major-aref x index))
            (y-el (row-major-aref y index)))
        (unless (or (eq x-el y-el)
                    (apply #'equals x-el y-el keys))
          (return nil))))))

(defmethod equals ((x structure-object) (y structure-object) &rest keys &key recursive &allow-other-keys)
  (declare (ignore keys recursive))
  (eq x y))

(defmethod equals ((x standard-object) (y standard-object) &rest keys &key recursive &allow-other-keys)
  (declare (ignore keys recursive))
  (eq x y))

(defmethod equals ((x hash-table) (y hash-table) &rest keys
                   &key recursive
                        (by-key t)
                        (by-value t)
                        (check-properties t)
                   &allow-other-keys)
  (declare (ignore recursive))
  (if (eq x y)
      t
      (when (= (hash-table-count x)
               (hash-table-count y))
        (let ((key-test
               (if by-key
                   (loop for x-keys being the hash-keys of x
                         for y-keys being the hash-keys of y
                         always (apply #'equals x-keys y-keys keys))
                   t))
              (value-test
               (if by-value
                   (loop for x-val being the hash-values of x
                         for y-val being the hash-values of y
                         always (apply #'equals x-val y-val keys))
                   t))
              (check-properties-test
               (if check-properties
                   (and (= (hash-table-size x)
                           (hash-table-size y))
                        (= (hash-table-rehash-threshold x)
                           (hash-table-rehash-threshold y))
                        (eq (hash-table-test x)
                            (hash-table-test y))
                        #+sbcl
                        (eq (hash-table-weakness x)
                            (hash-table-weakness y))
                        #+sbcl
                        (eq (hash-table-synchronized-p x)
                            (hash-table-synchronized-p y)))
                   t)))
          (and key-test value-test check-properties-test)))))

(defgeneric compare (x y &rest keys &key recursive &allow-other-keys)
  (:documentation
   "The generic function COMPARE defines methods to test the ordering of two
objects a and b, if such order exists. The result value returned by COMPARE is
one of the four symbols: <, >, =, or /=. The COMPARE function returns /= as
result by default; thus it can represent partial orders among objects. The
equality tests should be coherent with what the generic function EQUALS does.

If the argument recursive is T, then COMPARE may recurse down the 'structure'
of a and b. The description of each known method contains the relevant information
about its recursive dependent behavior. "))

(defmethod compare ((x t) (y t) &rest keys &key recursive &allow-other-keys)
  (declare (ignore keys))
  (if (equals x y :recursive recursive) '= '/=))

(defmethod compare ((x number) (y number) &rest keys &key recursive &allow-other-keys)
  (declare (ignore keys recursive))
  (cond ((< x y) '<)
        ((> x y) '>)
        ((= x y) '=)))

(defmethod compare ((x character) (y character) &rest keys &key recursive (case-sensitive t) &allow-other-keys)
  (declare (ignore keys recursive))
  (if case-sensitive
      (cond ((char< x y) '<)
            ((char> x y) '>)
            ((char= x y) '=))
      (cond ((char-lessp x y) '<)
            ((char-greaterp x y) '>)
            ((char-equal x y) '=))))

(defmethod compare ((x string) (y string) &rest keys &key recursive (case-sensitive t) &allow-other-keys)
  (declare (ignore keys recursive))
  (if case-sensitive
      (cond ((string< x y) '<)
            ((string> x y) '>)
            ((string= x y) '=))
      (cond ((string-lessp x y) '<)
            ((string-greaterp x y) '>)
            ((string-equal x y) '=))))

(defmethod compare ((x symbol) (y symbol) &rest key &key recursive &allow-other-keys)
  (declare (ignore key recursive))
  (if (eq x y) '= '/=))

(defun lt (x y &rest keys &key recursive &allow-other-keys)
  "Returns T when COMPARE returns <."
  (declare (ignore recursive))
  (let ((result (apply #'compare x y keys)))
    (when (eq result '/=)
      (error "Cannot order arguments ~A and ~A" x y))
    (eq result '<)))

(defun lte (x y &rest keys &key recursive &allow-other-keys)
  "Returns T when COMPARE returns < or =."
  (declare (ignore recursive))
  (let ((result (apply #'compare x y keys)))
    (when (eq result '/=)
      (error "Cannot order arguments ~A and ~A" x y))
    (or (eq result '<) (eq result '=))))

(defun gt (x y &rest keys &key recursive &allow-other-keys)
  "Returns T when COMPARE returns >."
  (declare (ignore recursive))
  (let ((result (apply #'compare x y keys)))
    (when (eq result '/=)
      (error "Cannot order arguments ~A and ~A" x y))
    (eq result '>)))

(defun gte (x y &rest keys &key recursive &allow-other-keys)
  "Returns T when COMPARE returns > or =."
  (declare (ignore recursive))
  (let ((result (apply #'compare x y keys)))
    (when (eq result '/=)
      (error "Cannot order arguments ~A and ~A" x y))
    (or (eq result '>) (eq result '=))))

(setf (fdefinition 'lessp) #'lt)
(setf (fdefinition 'not-lessp) #'gte)
(setf (fdefinition 'greaterp) #'gt)
(setf (fdefinition 'not-greaterp) #'lte)

(defgeneric hash-code (obj)
  (:documentation   
   "Associates a unique fixnum hash-code for an object that can
be compared with EQUALS for use as hash table functions. (equals x y)
should agree with (= (hash-code x) (hash-code y)). See SXHASH."))

(defmethod hash-code ((obj t))
  (sxhash obj))

(pushnew :cdr-8 *features*)
