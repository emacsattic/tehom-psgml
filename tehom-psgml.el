;;; tehom-psgml.el --- Psgml extensions

;; Copyright (C) 1999,2000 by Tom Breton

;; Author: Tom Breton <Tehom@localhost>
;; Keywords: hypermedia, extensions
;; Version: 1.5

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; These are extensions to psgml that I have found useful when working
;; with XML, SGML, and HTML.  I would be happy to see them
;; incorporated into psgml.  If not, they are offered for general use.

;; The difference between `tehom-psgml-add-element-to-element' etc and
;; `psgml-add-element-to-element' etc is that the new functions let
;; the caller stipulate values for the various attributes of the added
;; element, while the old version only allows the user to do that at
;; runtime.

;;; Changes:

;; In addition, new in version 1.3, the controller can stipulate
;; sub-elements, as long as they are valid optional elements.  It can
;; do this recursively as far as stack depth allows.

;; Also new in 1.3, tehom-psgml-insert-els lets you insert multiple
;; elements at once using the same controller format that
;; tehom-psgml-insert-element uses.

;; Also new in 1.3, numberpaths, which let you remember which branches
;; to take to "get to" an element.  This is useful when your code is
;; heavily editing the buffer.  If your code just saved the elements,
;; the positions of their stags and etags would quickly become
;; meaningless.

;; New in 1.4:

;; tehom-psgml-all-children returns a list of all children

;; tehom-psgml-get-child-by-test-recursive, which is like
;; tehom-psgml-get-child-by-test but explores subtrees that meet an
;; optional test.

;; tehom-psgml-get-dtd-name gets the current dtd name
;; tehom-psgml-pick-attribute interactively gets one attribute name
;; from the element at point.

;; New in 1.5

;; Functions to support hbmk: `tehom-end-of-whitespace',
;; `tehom-psgml-true-stag-end', `tehom-psgml-get-true-el-contents',
;; `tehom-psgml-next-el'

;;; Requirements

;; psgml, Lennart Staflin's SGML/XML package for emacs.

;; cl, Dave Gillespie's Common Lisp-alike in emacs.  I was asked to
;; remove the dependency on cl, and I did, but the new code was just
;; much easier to write with cl, so it came back in.

;; regress.el is needed for the test suite, but the package will run
;; without it.

;; New in 1.6

;; General iterator macro over children,
;; `tehom-psgml-iterate-children'.  It could be used to simplify many
;; functions in this file.

;;; Code:

(require 'cl)          
(require 'psgml)       ;;The code is based on this
(require 'psgml-edit)  ;;Part of psgml, needed for rewritten stuff.


;;;;;;;;;;;;
;;Utility functions

(defun tehom-string-middle (str)
  "Return STR minus any whitespace at the beginning or end."
  
  (string-match "^[ \t\n]*\\(.*\\)[ \t\n]*$"  str)
  (match-string 1 str))

(eval-when-compile
  (setf
    (get 'tehom-string-middle 'rtest-suite)
    '("tehom-string-middle"
       ((let* 
	  (
	    (test-string
	      "  A text string
"
	      ))
	  (tehom-string-middle test-string))

	 "A text string")

       ((let* 
	  (
	    (test-string
	      "\nA text string\n"
	      ))
	  (tehom-string-middle test-string))

	 "A text string"))))


(defun tehom-end-of-whitespace (pos)
  ""

  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \n\t")
    (point)))


(eval-when-compile
  (setf
    (get 'tehom-end-of-whitespace 'rtest-suite)
    '("tehom-end-of-whitespace"
       
       ( "Skips whitespace"
	 (with-buffer-containing
	   (list  "abc   def ghi jkl" "abc" nil t)
	   (goto-char
	     (tehom-end-of-whitespace (point)))
	   
	   (looking-at "def"))
	 t)
       
       ( "Skips whitespace evn from the middle"
	 (with-buffer-containing
	   (list  "abc   def ghi jkl" "abc  " nil t)
	   (goto-char
	     (tehom-end-of-whitespace (point)))
	   
	   (looking-at "def"))
	 t)

       ( "Works for strings that include tabs and linefeeds."
	 (with-buffer-containing
	   (list  "abc \n\t  def ghi jkl" "abc" nil t)
	   (goto-char
	     (tehom-end-of-whitespace (point)))
	   (looking-at "def"))
	 t)

       ( "Doesn't skip whitespace if not on whitespace"
	 (with-buffer-containing
	   (list  "abc \n\t  def ghi jkl" "a" nil t)
	   (goto-char
	     (tehom-end-of-whitespace (point)))
	   (looking-at "def"))
	 nil))))


;;;; 


(defun tehom-psgml-get-dtd-name ()
  "Return the current dtd name"
  
  (sgml-need-dtd)

  (let
    ((dtd
       (sgml-pstate-dtd sgml-buffer-parse-state)))
    (sgml-dtd-doctype dtd)))


(defun tehom-psgml-pick-attribute (el)
  "Interactively return one attribute-name from EL."
  
  ;;Make sure we are in a psgml buffer.
  (sgml-need-dtd)

  (let 
    (
      (attribute-list 
	(sgml-non-fixed-attributes (sgml-element-attlist el)))
      (completion-ignore-case 
	sgml-namecase-general))

    (completing-read
      "Attribute name: "
      (mapcar 
	#'(lambda (a) (list (sgml-attdecl-name a)))
	attribute-list)
      nil t)))


;;;;Functions to get ancestor elements.

(defun tehom-psgml-get-enclosing-el (el test)
  "Return the smallest element that passes TEST and encloses EL, if any.
Return nil otherwise.

The element EL counts as enclosing itself."

  (while
    (and
      el
      (not (funcall test el)))

    (setq el (sgml-element-parent el)))
  el)



(defun tehom-psgml-get-enclosing-element-by-name (el gi)
  "Return the smallest element named GI that encloses EL, if any.
Return nil otherwise."

  (tehom-psgml-get-enclosing-el
    el 
    ( function
      ( lambda (el)
	(string= (sgml-element-gi el) gi )))))

;;;;Functions to get direct child elements.


(defmacro* tehom-psgml-iterate-children 
   ((var el &optional test result) &rest body)
   "\(tehom-psgml-iterate-children \(VAR EL [TEST [RESULT]]\) BODY...\):
Loop over the children of the psgml node EL.
Evaluate BODY with VAR bound to each child element, in turn.
Then evaluate RESULT to get return value, default VAR."
   (let
      (
	 (result-1 (or result var))
	 (test-1   (or test 't)))
      `
      (let 
	 ((,var (sgml-element-content ,el)))
	 (while 
	    (and ,var ,test-1) 
	    ,@body
	    (setq ,var (sgml-element-next ,var)))
	 ,result-1)))


(defun tehom-psgml-get-child-by-test (el test)
  "Return the first child element of EL that passes TEST, if any.
Return nil otherwise."

  (let 
    ((c (sgml-element-content el)))

    (while 
      (and
	c
	(not (funcall test c)))
      
      (setq c (sgml-element-next c)))
    c))

(defun tehom-psgml-get-child-by-name (el gi)
  "Return a child element of EL named GI, if any."

  (tehom-psgml-get-child-by-test
    el 
    ( function
      ( lambda (el)
	(string= (sgml-element-gi el) (upcase gi))))))

(defun tehom-psgml-get-child-and-number-by-test (el test)
  "Return a cell of \(element . branch-number) or nil.

element is the first child element of EL that passes TEST,
branch-number is what number subelement it is.

Return nil if no element is found."

  (let 
    ((c (sgml-element-content el))
      (n 0))

    (while 
      (and
	c
	(not (funcall test c)))
      (incf n)
      (setq c (sgml-element-next c)))
    (cons c n)))

(defun tehom-psgml-get-child-and-number-by-name (el gi)
  "
Return a cell of \(element . branch-number) or nil.

element is the first child element of EL named GI
branch-number is what number subelement it is.

Return nil if no element is found."

  (tehom-psgml-get-child-and-number-by-test
    el 
    ( function
      ( lambda (el)
	(string= (sgml-element-gi el) (upcase gi))))))


;;;; Recursive search functions:

(defun tehom-psgml-get-child-by-test-recursive 
  (el test &optional recurse-test)
  "Return the first child element of EL that passes TEST, if any.  
Explore subtrees that pass RECURSE-TEST, which defaults to always
true. 

Return nil if no acceptable node is found."
  
  (setq recurse-test (or recurse-test #'identity))
  (let 
    ((c (sgml-element-content el))
      (found nil))

    (while 
      (and c (not found))

      (setq found
	(cond
	  ;;Try to use this element.
	  ((funcall test c)
	    c)
	
	  ;;If not, try to explore its subtree.
	  ((and
	     (funcall recurse-test c)
	     (tehom-psgml-get-child-by-test-recursive
	       c test recurse-test)))
	
	  ;;If not, we haven't found it.
	  (t nil)))

      (unless found
	(setq c (sgml-element-next c))))
    
    found))

;;;;Functions to get descendant elements by path.

(defun tehom-psgml-find-element-by-path (path &optional element)
  "Return the first sub-element of ELEMENT that matches PATH or nil.
Return nil if there is no such element.

PATH is a list of general identifiers \(GIs\) as strings.  NB: PATH
does *not* start with the name of the given element, but rather the
name of its first descendant.  Eg, if tehom-psgml-find-element-by-path
is called with no element, meaning to use the entire document, the car
of the path will not be the document's name.

If ELEMENT is not given, use the top element."

  (sgml-need-dtd)  
  
  (if (not element)
    (setq element (sgml-top-element)))
  

  (let* 
    ( (path-left path)
      (curr-element element))
      
    (while (and path-left curr-element)
      (setq
	curr-element
	(tehom-psgml-get-child-by-name curr-element (car path-left)))
      
      (setq path-left (cdr path-left)))
      
    ;;Return the final element found, which will be nil if nothing
    ;;was found. 
    curr-element))

(defun tehom-psgml-find-numberpath-by-path (path &optional element)
  "Return a cell of \(element . numberpath) corresponding to PATH or nil.

Similar to tehom-psgml-find-element-by-path.

Return the numerical path to the first sub-element of ELEMENT that
matches PATH.  Return nil if there is no such element.

PATH is a list of general identifiers \(GIs\) as strings.  NB: PATH
does *not* start with the name of the given element, but rather the
name of its first descendant.  Eg, if tehom-psgml-find-element-by-path
is called with no element, meaning to use the entire document, the car
of the path will not be the document's name.

If ELEMENT is not given, use the top element."

  (sgml-need-dtd)  
  
  (if (not element)
    (setq element (sgml-top-element)))
  

  (let* 
    ( (reversed-number-path '())
      (path-left path)
      cell
      (curr-element element))

    (while (and path-left curr-element)
      (setq cell
	(tehom-psgml-get-child-and-number-by-name 
	  curr-element (car path-left)))
      
      (setq curr-element (car cell) )
      (push (cdr cell) reversed-number-path)

      (setq path-left (cdr path-left)))
      
    ;;Return the element and numerical path to the final element if
    ;;found, or nil if nothing was found.
    (if
      curr-element
      (cons curr-element (reverse reversed-number-path))
      nil)))


;;;;;;
;;Write a function to get child element by numberpath.

(defun tehom-psgml-get-child-by-number (el number)
  "Return the Nth child element of EL, if any.
Return nil otherwise."

  (let 
    ((c (sgml-element-content el)))

    ;;Uses cl.
    (dotimes (i number)
      (setq c (sgml-element-next c)))

    c))


(defun tehom-psgml-find-element-by-numberpath (numberpath &optional element)
  "Return the first sub-element that matches NUMBERPATH.
Return nil if there is no such element.

NUMBERPATH is a list of numbers indicating which subbranches to take,
successively, in order to reach that element.

If ELEMENT is not given, use the top element."

  (sgml-need-dtd)  
  
  (if (not element)
    (setq element (sgml-top-element)))
  

  (let* 
    ( (path-left numberpath)
      (curr-element element))
      
    (while (and path-left curr-element)
      (setq
	curr-element
	(tehom-psgml-get-child-by-number curr-element (car path-left)))
      
      (setq path-left (cdr path-left)))
      
    ;;Return the final element found, which will be nil if nothing
    ;;was found. 
    curr-element))

;;;;;;;;;

(defun tehom-psgml-goto-element-stag-end (path &optional element)
  "Move point to the first sub-element of ELEMENT that matches PATH. 
Specifically, move point to just after its start tag.

PATH is a list of general identifiers \(GIs\) as strings.
If ELEMENT is not given, use the top element."

  (if (not element)
    (setq element (sgml-top-element)))

  (let* 
    ((body-element
       (tehom-psgml-find-element-by-path path)))
	
    (if body-element
      (goto-char (sgml-element-stag-end body-element)))
    
    ;;Return the element found, which will be nil if nothing was found.
    body-element))

;;;;;;;;;;;;;;;
;;Return a list of all children

(defun tehom-psgml-all-children (el)
  ""
  (loop
    for child = (sgml-element-content el) then (sgml-element-next child)
    while child
    collect child))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Function to grab text contents

(defun tehom-psgml-index-get-el-contents (el &optional cut)
  "Return the text contents of the given element.

If CUT is non-nil, also delete those contents.

This is usually not appropriate for elements that have sub-elements.
In that case, do nothing and return an explanatory string."

  (if (sgml-element-content el)
    "[No content, element was not a leaf]"

    (let*
      (
	(start (tehom-psgml-true-stag-end el))
	(end   (sgml-element-etag-start el))
	(text-content (buffer-substring start end)))

      (if cut
	(progn
	  (delete-region 
	    start
	    end)))

      (tehom-string-middle text-content))))


;;;;;;;;
;;Work around psgml's handling of `_'

;;Needs regression tests.
'(tehom-psgml-true-stag-end (sgml-find-element-of (point)))
(defun tehom-psgml-true-stag-end
  (el)
  "Position after start-tag of ELEMENT.
Works around a bug in psgml's handling of Netscape bookmark files.
psgml treats `_' as the end of the start-tag."
  (let*
    ((start-1
       (sgml-element-stag-end el)))
    (if
      (=
	(char-after start-1) 
	?_)
      (save-excursion
	(goto-char start-1)
	(skip-chars-forward "^>")
	(1+ (point)))
      start-1)))



;;Obsolete
;;$$REMOVE ME
'
(defun tehom-psgml-get-true-el-contents (el)
  ""

  (if
    (sgml-element-content el)
    "[No content, element was not a leaf]"

    (buffer-substring-no-properties
      (tehom-psgml-true-stag-end el)
      (sgml-element-etag-start el))))


;;;;;;;;;;;;;;;;;;;;

;;Needs regression tests.  Test code looks like:
'  
(let* 
  ((next-el (tehom-psgml-next-el (point))))
  (if next-el 
    (aref next-el 0) 
    "No element"))


(defun tehom-psgml-next-el (pos)
  "Find the next element after or starting at POS, if any.
If POS is in the start tag, that counts as starting at POS."
  
  (let
    (
      (easy-el (sgml-find-element-of pos)))

    (if
      ;;If pos is *in* the start tag, we use this element.
      (< pos (tehom-psgml-true-stag-end easy-el))
      easy-el

      ;;While pos is in the whitespace of an omitted tag, climb
      ;;parents until we include the element that should come next.
      (let*
	( 
	  (end-of-whitespace (tehom-end-of-whitespace pos))
	  (parent 
	    (tehom-psgml-get-enclosing-el 
	      easy-el 
	      #'(lambda (el)
		  (> (sgml-element-end el) end-of-whitespace)))))
	
	;;Now that pos is definitely inside the element, look for an
	;;immediate child element immediately after it.  If we have
	;;climbed several levels upwards, ISTM we can only have
	;;reached an immediate parent of an element we want to choose.
	(when
	  parent
	  ;;Find the first child whose start is at or after pos.
	  (tehom-psgml-get-child-by-test 
	    parent
	    #'(lambda (el)
		(>= (sgml-element-start el) pos))))))))




;;;;Functions to add elements.

(defun tehom-psgml-add-element-to-element (gi first controller &optional el)
  "Add the element named by GI to the end of EL.

If FIRST is nil, add it to the end, otherwise add it to the front.

EL defaults to the element that point is in.

If CONTROLLER is nil, prompt user for the values of the new element's
attributes.  Otherwise CONTROLLER should be an alist of
\(attribute-name . attribute-value \).  If attribute-name is a
string, that attribute will be given the value in
attribute-value-string.

The special attribute-name `sub-nodes' means to insert sub-nodes as
well as attributes.  In that case, attribute-value is a list of
elements usually of the form \(sub-node-gi . sub-controller\), where
sub-controller has the same format as CONTROLLER.  Elements of the
form \(\"#PCDATA\" string\), ie where sub-node-gi equals \"#PCDATA\",
insert string as text."

  (interactive
    (let 
      ((tab
	 (mapcar 
	   (lambda (et) (cons (sgml-eltype-name et) nil))
	   (sgml--all-possible-elements
	     (sgml-find-context-of (point))))))
      (cond 
	((null tab)
	  (error "No element possible"))
	(t
	  (let ((completion-ignore-case sgml-namecase-general))
	    (list 
	      (completing-read "Element: " tab nil t
		(and (null (cdr tab)) (caar tab)))
	      current-prefix-arg))))))

  ;;Assign a default value to EL if it needs one.
  (if
    (null el)
    (setq el (sgml-find-context-of (point))))
  

  (let
    ((insert-position 
       (tehom-psgml-find-insert-position el gi first)))

    (cond 
      (insert-position
	(goto-char insert-position)
	(tehom-psgml-insert-element gi nil nil controller))
	  
      (t
	(error 
	  "A %s element is not valid in current element"
	  gi)))))



(defun tehom-psgml-find-insert-position (el gi first)
  "Find an appropriate position where GI could be inserted into EL.

GI is a general identifier, but EL is an element.

If FIRST is nil, this will be last legal position, otherwise it will
be the first legal insert position."

  (let 
    ( 
      (et (sgml-lookup-eltype (sgml-general-case gi))))

    (let ( (c (sgml-element-content el))
	   (s (sgml-element-model el))
	   (tok (sgml-eltype-token et))
	   (last-pos nil)
	   done)

      (while 
	(not done)
	
	(let
	  ((can-add-before (sgml--add-before-p tok s c)))

	  ;;If we are at a valid position, record it.
	  (if
	    can-add-before 
	    (setq last-pos 
	      (if c 
		(sgml-element-start c)
		(sgml-element-etag-start el))))
    
    
	  (if 

	    ;;If we will look for more valid positions, either because
	    ;;we want to find the last one or because we haven't found
	    ;;one yet, and if C hasn't dropped off the end...
	    (and
	      (or
		(not first)
		(not can-add-before))
	      c)

	    ;;...advance to the next candidate
	    (progn
	      (setq s (sgml-element-pstate c))
	      (setq c (sgml-element-next c)))
      
	    (setq done t))))

      last-pos)))



(defun tehom-psgml-insert-controlled-subels (controller newpos)
  "Insert any optional tokens specified by the controller."
  
  (let*
    ( position
      (sub-nodes-cell
	(assoc 'sub-nodes controller))
      (sub-nodes-list
	(if sub-nodes-cell (cdr sub-nodes-cell))))

    (if (not (listp sub-nodes-list))
      (message "sub-nodes should be a list."))

    ;;
    (while 
      (and 
	(listp sub-nodes-list)
	sub-nodes-list)

      (let*
	(
	  (node-spec (car sub-nodes-list))
	  (sub-token-name (car node-spec))
	  (sub-controller (cdr node-spec))

	  ;;Get all possible tokens.
	  (opt-tokens
	    (append
	      (sgml-required-tokens sgml-current-state)
	      (sgml-optional-tokens sgml-current-state)))

	  is-member)
	
	(setq is-member
	  (member* sub-token-name opt-tokens 
	    :key 'symbol-name :test 'string=))
	
	;;The above, expanded to remove cl
	'(setq tmp-opt-tokens opt-tokens)
	'(while 
	   (and tmp-opt-tokens (not is-member))
	   (if (string= 
		 sub-token-name 
		 (symbol-name (car tmp-opt-tokens)))
	     (setq is-member t))
		   
	   (setq tmp-opt-tokens (cdr tmp-opt-tokens)))


	;;Check that the token we want is one of them.
	;;Because of the way obarrays are used in psgml, and
	;;because controller has a string, not a symbol, we
	;;test against symbol-name.
	(if
	  is-member

	  (progn
	    (if
	      (string= sub-token-name "#PCDATA")
	      ;;Treat text content specially.  The car of the
	      ;;sub-controller is the text to insert.
	      (let
		 ((start (point)))
		 (insert "\n" (car sub-controller))
		 ;;Make it pretty.  It should be possible to turn
		 ;;this off, perhaps using further parts of the
		 ;;controller as control data.
		 (indent-region start (point) nil)
		 (fill-region start (point))
		 (setq newpos (or newpos (point))))
		    
	      (progn
		(setq position
		  (tehom-psgml-insert-element 
		    sub-token-name t t sub-controller))
		(setq newpos (or newpos position))))
		    
	    (sgml-parse-to-here))))
	      
      (setq sub-nodes-list (cdr sub-nodes-list))))
  newpos)


(defun tehom-psgml-insert-required-subels (newpos element)
  "Insert any required sub-elements, as far as possible."
  
  (let*
    (
      position 
      req-tokens 
      (more-to-do t))

    (while 
      more-to-do

      ;;Get the possible tokens, some one of which is
      ;;required.
      (setq req-tokens 
	(sgml-required-tokens sgml-current-state))

      ;;Proceed if the list of alternatives is exactly one
      ;;element long...
      (setq more-to-do
	(equal 1 (length req-tokens)))

      (if more-to-do
	;;...insert the required element.
	(setq position
	  (sgml-insert-element (car req-tokens) t t))
	;;...remember the position unless we already got one.
	(setq newpos (or newpos position))
	(sgml-parse-to-here)))

    ;;If we reached a point with more than one alternative
    ;;before running out of required elements...
    (when req-tokens
      ;;...pretty up the text,
      (insert "\n")
      ;;...write the choices if the user wants that. 
      (when sgml-insert-missing-element-comment
	(insert (format "<!-- one of %s -->" req-tokens))
	(sgml-indent-line nil element)))

    ;;Return the position in the buffer.
    newpos))



(defun tehom-psgml-insert-element (name &optional after silent controller)
  "Insert start and end tags for the NAME element.

If AFTER is nil, move the point to an appropriate position in the
element.

If SILENT is non-nil, don't print messages explaining what we're
doing.

If CONTROLLER is nil, prompt user for the values of the new element's
attributes.  For the format of CONTROLLER, see
tehom-psgml-add-element-to-element."

  (interactive 
    (list 
      (sgml-read-element-name "Element: ")
      sgml-leave-point-after-insert))

  ;;Only proceed if we have a meaningful name for the element.
  (when
    (and name (not (equal name "")))

    (let 
      ( newpos				; position to leave cursor at
	element				; inserted element
	attribute-alist
	(sgml-show-warnings nil)

	(position-delta
	  (if (and sgml-xml-p (sgml-check-empty name))
	    2
	    1)))
      
      ;;Possibly insert a break.
      (when 
	(sgml-break-brefore-stag-p name)
	(sgml-insert-break))

      (sgml-insert-tag (sgml-start-tag-of name) 'silent)
      
      (backward-char position-delta)

      ;;Figure out which element we just inserted.
      (setq element (sgml-find-element-of (point)))

      (setq attribute-alist
	(or
	  controller
	  (funcall sgml-new-attribute-list-function
	    element)))

      ;;sgml-insert-attributes happily skips past the 'sub-nodes
      ;;symbol, so no worries there.
      (sgml-insert-attributes 
	attribute-alist
	(sgml-element-attlist element))

      (forward-char position-delta)

      (when (sgml-break-after-stag-p name)
	(sgml-insert-break))

      (when 
	(not (sgml-element-empty element))

	(when
	  (and
	    (or
	      sgml-auto-insert-required-elements
	      controller)
	    (sgml-model-group-p sgml-current-state))

	  (tehom-psgml-insert-controlled-subels controller newpos)
	  (tehom-psgml-insert-required-subels newpos element))
	

	;;If we haven't got a position yet, use the cursor position.
	(setq newpos (or newpos (point)))

	;;Write the end of the element.
	(when sgml-insert-end-tag-on-new-line
	  (insert "\n"))
	(sgml-insert-tag (sgml-end-tag-of name) 'silent)

	;;Possibly show the user how we stand now.
	(unless after
	  (goto-char newpos))
	(unless silent (sgml-show-context)))

      ;;Return the position in the buffer.
      newpos)))


(defun tehom-psgml-insert-els (sub-nodes &optional silent)
  "Insert multiple entries indicated by SUB-NODES at point.

SUB-NODES is a list of /(element-gi . controller/), as if it were
inside a sub-nodes in tehom-psgml-insert-element.

If SILENT is non-nil, don't print messages explaining what we're
doing."
  
  (dolist
    (cont sub-nodes)
    (tehom-psgml-insert-element (car cont) t silent (cdr cont))))


(defun tehom-psgml-add-els-to-element (sub-nodes &optional el first)
  "Insert SUB-NODES to EL.
See tehom-psgml-insert-els."
  
  (dolist
    (cont sub-nodes)

    (tehom-psgml-add-element-to-element (car cont) first (cdr cont) el)))

(defun tehom-psgml-add-els-to-numberpath (sub-nodes numberpath &optional first)
  "Insert SUB-NODES to an element found according to NUMBERPATH.
See tehom-psgml-insert-els."
  
  (let* 
    ((el (tehom-psgml-find-element-by-numberpath numberpath)))
    (dolist
      (cont sub-nodes)

      (tehom-psgml-add-element-to-element 
	(car cont) first (cdr cont) el))))


;;;;;;;;;;;;;;;;;;;;;
;;Tests

;;Some of these tests *cannot be run* if psgml is not set up for html.
(eval-when-compile
  

  (setf
    (get 'tehom-psgml-regress 'rtest-setup)
    '(
       ;;User: Configure this according to where you put the test file.
       (defconst tehom-psgml-regress-test-file 
	 "~/projects/test-psgml.html" 
	 "The location of the file containing the known document for
the tests." ) 

       (defvar tehom-psgml-rtest-knownbuf
	 (make-rtest-known-buffer
	   :filename tehom-psgml-regress-test-file
	   :setup-form '(html-mode))
	 "" )

       (defvar tehom-psgml-test-in-known-buffer
	 '(:around (with-buffer-containing
		     tehom-psgml-rtest-knownbuf)) 
	 "Inclusion to test this in a known buffer")

       	(defvar tehom-psgml-rtest-knownbuf
	  (make-rtest-known-buffer
	    :filename tehom-psgml-regress-test-file
	    :setup-form '(html-mode))
	  "" )

       (rtest-make-skip-summary)))
  

  (setf
    (get 'tehom-psgml-regress 'rtest-suite)
   '("tehom-psgml-regress"
      tehom-string-middle
      tehom-end-of-whitespace

      ;;tehom-psgml-find-element-by-path
      (	
	(sgml-element-gi 	
	  (tehom-psgml-find-element-by-path '("HEAD")))

	"HEAD"
	:include tehom-psgml-test-in-known-buffer)

      (	(with-buffer-containing tehom-psgml-rtest-knownbuf
	  (sgml-element-gi 	
	     (tehom-psgml-find-element-by-path '("HEAD" "TITLE"))))

	"TITLE")

      ;;tehom-psgml-find-element-by-numberpath
      (	(with-buffer-containing tehom-psgml-rtest-knownbuf
	  (sgml-element-gi 	
	     (tehom-psgml-find-element-by-numberpath '(0))))

	"HEAD")

      (	(with-buffer-containing tehom-psgml-rtest-knownbuf
	  (sgml-element-gi 	
	     (tehom-psgml-find-element-by-numberpath '(0 0))))

	"TITLE")

      (	(with-buffer-containing tehom-psgml-rtest-knownbuf
	  (sgml-element-gi 	
	     (tehom-psgml-find-element-by-numberpath '(1))))

	"BODY")

      ;;tehom-psgml-find-numberpath-by-path
      ((with-buffer-containing tehom-psgml-rtest-knownbuf
	  (let* 
	    ((cell (tehom-psgml-find-numberpath-by-path '("BODY"))))
	    (cons (sgml-element-gi (car cell)) (cdr cell))))

	'("BODY" 1))


      ((with-buffer-containing tehom-psgml-rtest-knownbuf
	  (tehom-psgml-find-numberpath-by-path '("MOOHA")))

	nil)

      ;;tehom-psgml-add-element-to-element
      (
	(tehom-psgml-add-element-to-element "a" nil 
	  '( ("NAME" . "here") 
	     ("HREF" . "over#there") 
	     )
	  (tehom-psgml-find-element-by-path '("BODY")))

	;;There is now an A element and its attributes are as
	;;specified.
	:test 
	(let* 
	  ((el (tehom-psgml-find-element-by-path '("BODY" "A"))))
	    
	  (and 
	    el
	    (string= (sgml-element-attval el "NAME") "here")
	    (string= (sgml-element-attval el "HREF") "over#there")
	    ))
	:include tehom-psgml-test-in-known-buffer)
      

      (
	(tehom-psgml-add-element-to-element "H5" nil 
	  '((sub-nodes 
	    ("A" ("NAME" . "there") ("HREF" . "#here"))))
	  (tehom-psgml-find-element-by-path '("BODY")))

	;;There is now an A element in an H5 element and its
	;;attributes are as specified.
	:test 
	(let
	  ((el (tehom-psgml-find-element-by-path '("BODY" "H5" "A"))))
	    
	  (and 
	    el
	    (string= (sgml-element-attval el "NAME") "there")
	    (string= (sgml-element-attval el "HREF") "#here")
	    ))

	:include tehom-psgml-test-in-known-buffer)
      
      (
	(tehom-psgml-add-element-to-element "H5" nil 
	  '(
	     (sub-nodes 
	       ("#PCDATA" "My text goes here")))
	  (tehom-psgml-find-element-by-path '("BODY")))

	;;There is now an H5 element and its text matches.
	:test 
	(let* 
	  ((el (tehom-psgml-find-element-by-path '("BODY" "H5"))))
	    
	  (and 
	    el
	    (string= 
	      (tehom-psgml-index-get-el-contents el)
	      "My text goes here")))
	:include tehom-psgml-test-in-known-buffer)

      ;;tehom-psgml-add-els-to-element should be tested similarly.

      ( 
	 (with-buffer-containing 
	   ;;Set the point after the end of body start-tag
	   (list tehom-psgml-rtest-knownbuf "<body>" 0 t)
	   (mapcar
	     #'sgml-element-gi
	     (tehom-psgml-all-children (sgml-find-element-of (point)))))
	
	'("H1" "H2" "H3" "H1" "H1"))

      )))




(provide 'tehom-psgml)

;;; tehom-psgml.el ends here