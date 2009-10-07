;;; File automatically generated (Don't touch).

(meroon-define Pre-Class? (create-predicate Pre-Class-class))

(meroon-define Class? (create-predicate Class-class))

(meroon-define Handy-Class? (create-predicate Handy-Class-class))

(meroon-define MeroonV2-Class? (create-predicate MeroonV2-Class-class))

(meroon-define Applyable-Object? (create-predicate Applyable-Object-class))

(meroon-define Generic? (create-predicate Generic-class))

(meroon-define Generic-1? (create-predicate Generic-1-class))

(meroon-define Generic-N? (create-predicate Generic-N-class))

(meroon-define Pre-Field? (create-predicate Pre-Field-class))

(meroon-define Field? (create-predicate Field-class))

(meroon-define Mono-Field? (create-predicate Mono-Field-class))

(meroon-define Poly-Field? (create-predicate Poly-Field-class))

(meroon-define Dispatcher? (create-predicate Dispatcher-class))

(meroon-define Immediate-Dispatcher? (create-predicate Immediate-Dispatcher-class))

(meroon-define Subclass-Dispatcher? (create-predicate Subclass-Dispatcher-class))

(meroon-define Indexed-Dispatcher? (create-predicate Indexed-Dispatcher-class))

(meroon-define Linear-Dispatcher? (create-predicate Linear-Dispatcher-class))

(meroon-define Global-Dispatcher? (create-predicate Global-Dispatcher-class))

(meroon-define Tracing-Dispatcher? (create-predicate Tracing-Dispatcher-class))

(meroon-define Anomaly? (create-predicate Anomaly-class))

(meroon-define Warning? (create-predicate Warning-class))

(meroon-define View? (create-predicate View-class))

(meroon-define Virtual-Field? (create-predicate Virtual-Field-class))

;;; File automatically generated (Don't touch).





(meroon-define Pre-Class-name (Mono-Field-create-careful-reader Pre-Class-class 'name))

(meroon-define Pre-Class-number (Mono-Field-create-careful-reader Pre-Class-class 'number))

(meroon-define Pre-Class-fields (Mono-Field-create-careful-reader Pre-Class-class 'fields))




(meroon-define Class-name (Mono-Field-create-careful-reader Class-class 'name))

(meroon-define Class-number (Mono-Field-create-careful-reader Class-class 'number))

(meroon-define Class-fields (Mono-Field-create-careful-reader Class-class 'fields))

(meroon-define Class-depth (Mono-Field-create-careful-reader Class-class 'depth))

(meroon-define Class-super-number (Mono-Field-create-careful-reader Class-class 'super-number))

(meroon-define Class-subclass-numbers (Mono-Field-create-careful-reader Class-class 'subclass-numbers))
(meroon-define set-Class-subclass-numbers! (Mono-Field-create-careful-writer Class-class 'subclass-numbers))

(meroon-define Class-next (Mono-Field-create-careful-reader Class-class 'next))
(meroon-define set-Class-next! (Mono-Field-create-careful-writer Class-class 'next))

(meroon-define Class-allocator (Mono-Field-create-careful-reader Class-class 'allocator))

(meroon-define Class-immutable? (Mono-Field-create-careful-reader Class-class 'immutable?))

(meroon-define Class-views (Mono-Field-create-careful-reader Class-class 'views))

(meroon-define Class-suprel-length (Poly-Field-create-careful-lengther Class-class 'suprel))
(meroon-define Class-suprel (Poly-Field-create-careful-reader Class-class 'suprel))




(meroon-define Handy-Class-name (Mono-Field-create-careful-reader Handy-Class-class 'name))

(meroon-define Handy-Class-number (Mono-Field-create-careful-reader Handy-Class-class 'number))

(meroon-define Handy-Class-fields (Mono-Field-create-careful-reader Handy-Class-class 'fields))

(meroon-define Handy-Class-depth (Mono-Field-create-careful-reader Handy-Class-class 'depth))

(meroon-define Handy-Class-super-number (Mono-Field-create-careful-reader Handy-Class-class 'super-number))

(meroon-define Handy-Class-subclass-numbers (Mono-Field-create-careful-reader Handy-Class-class 'subclass-numbers))
(meroon-define set-Handy-Class-subclass-numbers! (Mono-Field-create-careful-writer Handy-Class-class 'subclass-numbers))

(meroon-define Handy-Class-next (Mono-Field-create-careful-reader Handy-Class-class 'next))
(meroon-define set-Handy-Class-next! (Mono-Field-create-careful-writer Handy-Class-class 'next))

(meroon-define Handy-Class-allocator (Mono-Field-create-careful-reader Handy-Class-class 'allocator))

(meroon-define Handy-Class-immutable? (Mono-Field-create-careful-reader Handy-Class-class 'immutable?))

(meroon-define Handy-Class-views (Mono-Field-create-careful-reader Handy-Class-class 'views))

(meroon-define Handy-Class-suprel-length (Poly-Field-create-careful-lengther Handy-Class-class 'suprel))
(meroon-define Handy-Class-suprel (Poly-Field-create-careful-reader Handy-Class-class 'suprel))




(meroon-define MeroonV2-Class-name (Mono-Field-create-careful-reader MeroonV2-Class-class 'name))

(meroon-define MeroonV2-Class-number (Mono-Field-create-careful-reader MeroonV2-Class-class 'number))

(meroon-define MeroonV2-Class-fields (Mono-Field-create-careful-reader MeroonV2-Class-class 'fields))

(meroon-define MeroonV2-Class-depth (Mono-Field-create-careful-reader MeroonV2-Class-class 'depth))

(meroon-define MeroonV2-Class-super-number (Mono-Field-create-careful-reader MeroonV2-Class-class 'super-number))

(meroon-define MeroonV2-Class-subclass-numbers (Mono-Field-create-careful-reader MeroonV2-Class-class 'subclass-numbers))
(meroon-define set-MeroonV2-Class-subclass-numbers! (Mono-Field-create-careful-writer MeroonV2-Class-class 'subclass-numbers))

(meroon-define MeroonV2-Class-next (Mono-Field-create-careful-reader MeroonV2-Class-class 'next))
(meroon-define set-MeroonV2-Class-next! (Mono-Field-create-careful-writer MeroonV2-Class-class 'next))

(meroon-define MeroonV2-Class-allocator (Mono-Field-create-careful-reader MeroonV2-Class-class 'allocator))

(meroon-define MeroonV2-Class-immutable? (Mono-Field-create-careful-reader MeroonV2-Class-class 'immutable?))

(meroon-define MeroonV2-Class-views (Mono-Field-create-careful-reader MeroonV2-Class-class 'views))

(meroon-define MeroonV2-Class-suprel-length (Poly-Field-create-careful-lengther MeroonV2-Class-class 'suprel))
(meroon-define MeroonV2-Class-suprel (Poly-Field-create-careful-reader MeroonV2-Class-class 'suprel))







(meroon-define Generic-name (Mono-Field-create-careful-reader Generic-class 'name))

(meroon-define Generic-default (Mono-Field-create-careful-reader Generic-class 'default))
(meroon-define set-Generic-default! (Mono-Field-create-careful-writer Generic-class 'default))

(meroon-define Generic-variables (Mono-Field-create-careful-reader Generic-class 'variables))

(meroon-define Generic-dispatcher (Mono-Field-create-careful-reader Generic-class 'dispatcher))
(meroon-define set-Generic-dispatcher! (Mono-Field-create-careful-writer Generic-class 'dispatcher))

(meroon-define Generic-top-classes (Mono-Field-create-careful-reader Generic-class 'top-classes))




(meroon-define Generic-1-name (Mono-Field-create-careful-reader Generic-1-class 'name))

(meroon-define Generic-1-default (Mono-Field-create-careful-reader Generic-1-class 'default))
(meroon-define set-Generic-1-default! (Mono-Field-create-careful-writer Generic-1-class 'default))

(meroon-define Generic-1-variables (Mono-Field-create-careful-reader Generic-1-class 'variables))

(meroon-define Generic-1-dispatcher (Mono-Field-create-careful-reader Generic-1-class 'dispatcher))
(meroon-define set-Generic-1-dispatcher! (Mono-Field-create-careful-writer Generic-1-class 'dispatcher))

(meroon-define Generic-1-top-classes (Mono-Field-create-careful-reader Generic-1-class 'top-classes))




(meroon-define Generic-N-name (Mono-Field-create-careful-reader Generic-N-class 'name))

(meroon-define Generic-N-default (Mono-Field-create-careful-reader Generic-N-class 'default))
(meroon-define set-Generic-N-default! (Mono-Field-create-careful-writer Generic-N-class 'default))

(meroon-define Generic-N-variables (Mono-Field-create-careful-reader Generic-N-class 'variables))

(meroon-define Generic-N-dispatcher (Mono-Field-create-careful-reader Generic-N-class 'dispatcher))
(meroon-define set-Generic-N-dispatcher! (Mono-Field-create-careful-writer Generic-N-class 'dispatcher))

(meroon-define Generic-N-top-classes (Mono-Field-create-careful-reader Generic-N-class 'top-classes))




(meroon-define Pre-Field-name (Mono-Field-create-careful-reader Pre-Field-class 'name))




(meroon-define Field-name (Mono-Field-create-careful-reader Field-class 'name))

(meroon-define Field-immutable? (Mono-Field-create-careful-reader Field-class 'immutable?))

(meroon-define Field-class-number (Mono-Field-create-careful-reader Field-class 'class-number))
(meroon-define set-Field-class-number! (Mono-Field-create-careful-writer Field-class 'class-number))

(meroon-define Field-initialized? (Mono-Field-create-careful-reader Field-class 'initialized?))

(meroon-define Field-initializer (Mono-Field-create-careful-reader Field-class 'initializer))
(meroon-define set-Field-initializer! (Mono-Field-create-careful-writer Field-class 'initializer))

(meroon-define Field-path-length (Poly-Field-create-careful-lengther Field-class 'path))
(meroon-define Field-path (Poly-Field-create-careful-reader Field-class 'path))




(meroon-define Mono-Field-name (Mono-Field-create-careful-reader Mono-Field-class 'name))

(meroon-define Mono-Field-immutable? (Mono-Field-create-careful-reader Mono-Field-class 'immutable?))

(meroon-define Mono-Field-class-number (Mono-Field-create-careful-reader Mono-Field-class 'class-number))
(meroon-define set-Mono-Field-class-number! (Mono-Field-create-careful-writer Mono-Field-class 'class-number))

(meroon-define Mono-Field-initialized? (Mono-Field-create-careful-reader Mono-Field-class 'initialized?))

(meroon-define Mono-Field-initializer (Mono-Field-create-careful-reader Mono-Field-class 'initializer))
(meroon-define set-Mono-Field-initializer! (Mono-Field-create-careful-writer Mono-Field-class 'initializer))

(meroon-define Mono-Field-path-length (Poly-Field-create-careful-lengther Mono-Field-class 'path))
(meroon-define Mono-Field-path (Poly-Field-create-careful-reader Mono-Field-class 'path))




(meroon-define Poly-Field-name (Mono-Field-create-careful-reader Poly-Field-class 'name))

(meroon-define Poly-Field-immutable? (Mono-Field-create-careful-reader Poly-Field-class 'immutable?))

(meroon-define Poly-Field-class-number (Mono-Field-create-careful-reader Poly-Field-class 'class-number))
(meroon-define set-Poly-Field-class-number! (Mono-Field-create-careful-writer Poly-Field-class 'class-number))

(meroon-define Poly-Field-initialized? (Mono-Field-create-careful-reader Poly-Field-class 'initialized?))

(meroon-define Poly-Field-initializer (Mono-Field-create-careful-reader Poly-Field-class 'initializer))
(meroon-define set-Poly-Field-initializer! (Mono-Field-create-careful-writer Poly-Field-class 'initializer))

(meroon-define Poly-Field-path-length (Poly-Field-create-careful-lengther Poly-Field-class 'path))
(meroon-define Poly-Field-path (Poly-Field-create-careful-reader Poly-Field-class 'path))




(meroon-define Dispatcher-method-finder (Mono-Field-create-careful-reader Dispatcher-class 'method-finder))




(meroon-define Immediate-Dispatcher-method-finder (Mono-Field-create-careful-reader Immediate-Dispatcher-class 'method-finder))

(meroon-define Immediate-Dispatcher-method (Mono-Field-create-careful-reader Immediate-Dispatcher-class 'method))




(meroon-define Subclass-Dispatcher-method-finder (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'method-finder))

(meroon-define Subclass-Dispatcher-class-number (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'class-number))

(meroon-define Subclass-Dispatcher-class-depth (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'class-depth))

(meroon-define Subclass-Dispatcher-no (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'no))
(meroon-define set-Subclass-Dispatcher-no! (Mono-Field-create-careful-writer Subclass-Dispatcher-class 'no))

(meroon-define Subclass-Dispatcher-yes (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'yes))
(meroon-define set-Subclass-Dispatcher-yes! (Mono-Field-create-careful-writer Subclass-Dispatcher-class 'yes))




(meroon-define Indexed-Dispatcher-method-finder (Mono-Field-create-careful-reader Indexed-Dispatcher-class 'method-finder))

(meroon-define Indexed-Dispatcher-class-number (Mono-Field-create-careful-reader Indexed-Dispatcher-class 'class-number))

(meroon-define Indexed-Dispatcher-class-depth (Mono-Field-create-careful-reader Indexed-Dispatcher-class 'class-depth))

(meroon-define Indexed-Dispatcher-no (Mono-Field-create-careful-reader Indexed-Dispatcher-class 'no))
(meroon-define set-Indexed-Dispatcher-no! (Mono-Field-create-careful-writer Indexed-Dispatcher-class 'no))

(meroon-define Indexed-Dispatcher-method-length (Poly-Field-create-careful-lengther Indexed-Dispatcher-class 'method))
(meroon-define Indexed-Dispatcher-method (Poly-Field-create-careful-reader Indexed-Dispatcher-class 'method))
(meroon-define set-Indexed-Dispatcher-method! (Poly-Field-create-careful-writer Indexed-Dispatcher-class 'method))




(meroon-define Linear-Dispatcher-method-finder (Mono-Field-create-careful-reader Linear-Dispatcher-class 'method-finder))

(meroon-define Linear-Dispatcher-no (Mono-Field-create-careful-reader Linear-Dispatcher-class 'no))
(meroon-define set-Linear-Dispatcher-no! (Mono-Field-create-careful-writer Linear-Dispatcher-class 'no))

(meroon-define Linear-Dispatcher-method (Mono-Field-create-careful-reader Linear-Dispatcher-class 'method))
(meroon-define set-Linear-Dispatcher-method! (Mono-Field-create-careful-writer Linear-Dispatcher-class 'method))

(meroon-define Linear-Dispatcher-signature (Mono-Field-create-careful-reader Linear-Dispatcher-class 'signature))
(meroon-define set-Linear-Dispatcher-signature! (Mono-Field-create-careful-writer Linear-Dispatcher-class 'signature))




(meroon-define Global-Dispatcher-method-finder (Mono-Field-create-careful-reader Global-Dispatcher-class 'method-finder))

(meroon-define Global-Dispatcher-method-length (Poly-Field-create-careful-lengther Global-Dispatcher-class 'method))
(meroon-define Global-Dispatcher-method (Poly-Field-create-careful-reader Global-Dispatcher-class 'method))
(meroon-define set-Global-Dispatcher-method! (Poly-Field-create-careful-writer Global-Dispatcher-class 'method))




(meroon-define Tracing-Dispatcher-method-finder (Mono-Field-create-careful-reader Tracing-Dispatcher-class 'method-finder))

(meroon-define Tracing-Dispatcher-dispatcher (Mono-Field-create-careful-reader Tracing-Dispatcher-class 'dispatcher))
(meroon-define set-Tracing-Dispatcher-dispatcher! (Mono-Field-create-careful-writer Tracing-Dispatcher-class 'dispatcher))

(meroon-define Tracing-Dispatcher-default (Mono-Field-create-careful-reader Tracing-Dispatcher-class 'default))




(meroon-define Anomaly-category (Mono-Field-create-careful-reader Anomaly-class 'category))

(meroon-define Anomaly-operator (Mono-Field-create-careful-reader Anomaly-class 'operator))

(meroon-define Anomaly-message (Mono-Field-create-careful-reader Anomaly-class 'message))

(meroon-define Anomaly-hint-length (Poly-Field-create-careful-lengther Anomaly-class 'hint))
(meroon-define Anomaly-hint (Poly-Field-create-careful-reader Anomaly-class 'hint))




(meroon-define Warning-category (Mono-Field-create-careful-reader Warning-class 'category))

(meroon-define Warning-operator (Mono-Field-create-careful-reader Warning-class 'operator))

(meroon-define Warning-message (Mono-Field-create-careful-reader Warning-class 'message))

(meroon-define Warning-hint-length (Poly-Field-create-careful-lengther Warning-class 'hint))
(meroon-define Warning-hint (Poly-Field-create-careful-reader Warning-class 'hint))




(meroon-define View-name (Mono-Field-create-careful-reader View-class 'name))

(meroon-define View-number (Mono-Field-create-careful-reader View-class 'number))

(meroon-define View-fields (Mono-Field-create-careful-reader View-class 'fields))

(meroon-define View-super-length (Poly-Field-create-careful-lengther View-class 'super))
(meroon-define View-super (Poly-Field-create-careful-reader View-class 'super))




(meroon-define Virtual-Field-name (Mono-Field-create-careful-reader Virtual-Field-class 'name))

(meroon-define Virtual-Field-view (Mono-Field-create-careful-reader Virtual-Field-class 'view))

(meroon-define Virtual-Field-index (Mono-Field-create-careful-reader Virtual-Field-class 'index))
(meroon-define set-Virtual-Field-index! (Mono-Field-create-careful-writer Virtual-Field-class 'index))



;;; File automatically generated (Don't touch).

(meroon-define make-Object (make-maker Object-class))

(meroon-define make-Pre-Class (make-maker Pre-Class-class))

(meroon-define make-Class (make-maker Class-class))

(meroon-define make-Handy-Class (make-maker Handy-Class-class))

(meroon-define make-MeroonV2-Class (make-maker MeroonV2-Class-class))

(meroon-define make-Applyable-Object (make-maker Applyable-Object-class))

(meroon-define make-Generic (make-maker Generic-class))

(meroon-define make-Generic-1 (make-maker Generic-1-class))

(meroon-define make-Generic-N (make-maker Generic-N-class))

(meroon-define make-Pre-Field (make-maker Pre-Field-class))

(meroon-define make-Field (make-maker Field-class))

(meroon-define make-Mono-Field (make-maker Mono-Field-class))

(meroon-define make-Poly-Field (make-maker Poly-Field-class))

(meroon-define make-Dispatcher (make-maker Dispatcher-class))

(meroon-define make-Immediate-Dispatcher (make-maker Immediate-Dispatcher-class))

(meroon-define make-Subclass-Dispatcher (make-maker Subclass-Dispatcher-class))

(meroon-define make-Indexed-Dispatcher (make-maker Indexed-Dispatcher-class))

(meroon-define make-Linear-Dispatcher (make-maker Linear-Dispatcher-class))

(meroon-define make-Global-Dispatcher (make-maker Global-Dispatcher-class))

(meroon-define make-Tracing-Dispatcher (make-maker Tracing-Dispatcher-class))

(meroon-define make-Anomaly (make-maker Anomaly-class))

(meroon-define make-Warning (make-maker Warning-class))

(meroon-define make-View (make-maker View-class))

(meroon-define make-Virtual-Field (make-maker Virtual-Field-class))

;;; end of Basics.scm
