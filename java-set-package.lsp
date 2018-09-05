#!/usr/bin/env newlisp
;;;; @Maddix Seth - Aug 20 2018 - Set the package on a java or groovy file

;; A folder structure is represented as a list of two items.
;;	The first item is a string which is the name of the folder.
;;	The second item is a list which is the folder contents.
;;	For example:
;;
;;	(("folder-name"
;;		("file" "file"
;;			(("folder-name"
;;				("file"))))))

;; A folder is represented as the following:
;;		("folder-name" ("folder" "contents"))

;; Which is pretty simple. However when dealing with folders, you will receive them as the contents of a parent folder.
;; for example:
;;		("file" "file" ("folder-name" ("folder" "Contents")) "file")

;; This looks like this in practice when dealing with a single child folder inside a parent folder:
;;		(("folder-name" ("folder "contents")))

(define (dir-public str-path)
	(directory (unless str-path ".") "^[^.]"))

(define (join-path x y)
	(append x "/" y))

(define (tree-directory str-path)
	(map
		(fn (path-item)
			(let (path (join-path str-path path-item))
				(if (directory? path)
					(list path-item (tree-directory path))
					path-item)))
		(dir-public str-path)))

(define (tree-print dir(idx 0))
	(let (space (dup "  " idx))
		(dolist (item dir)
			(if (list? (unless (empty? item) (last item)))
				(begin
					(println space  "- " (first item))
					(tree-print (sort (last item)) (+ 1 idx)))
				(println space "+ " item)))))

(define (tree-sub-directory folder-name crawled)
	(catch
		(dolist (item crawled)
			(when (and (list? item) (not (empty? item)))
				(if (= (first item) folder-name)
					(throw (last item))
					(tree-sub-directory folder-name (last item)))))))

(define (folder-name lst)
	(when (folder? lst)
		(first lst)))

(define (folder-contents lst)
	(when (folder? lst)
		(last lst)))

(define (tree-get-folders lst)
	(when (folder? lst)
		(filter tree-folder? (last lst))))

(define (tree-get-files lst)
	(when (folder? lst)
		(clean tree-folder? (last lst))))

(define (folder? lst)
	(and
		(list? lst)
		(= 2 (length lst))
		(string? (first lst))
		(list? (last lst))))

(define (branch? lst)
	(and
		(folder? lst)
		(not (empty? (filter folder? (last lst))))))

(define (branch?-old lst)
	(when (list? lst)
		(not (empty? (filter folder? (rest lst))))))

(define (leaf? lst)
	(not (branch? lst)))

(define (tree-files-with-path tree (path '()))
	(let (files (list))
		(dolist (item tree)
			(if (folder? item)
				(setq files (extend (tree-files-with-path (folder-contents item) (append path (list (folder-name item)))) files))
				(push (list path item) files)))
	files))

; Rewrite this. Also make a function to get the extension (Minus the . oc)
(define (trim-extension file)
	(join (reverse (rest (reverse (parse file ".")))) "."))

;; Implementation
;; --------------

(setq data (tree-directory "project"))
(tree-print data)
(tree-files-with-path data)
;(println (tree-files-with-path data))

(dolist (item
	(map (fn (x)
		(join
			(flat
				(list (first x)
				(trim-extension (last x))))
			"."))
		(tree-files-with-path data)))
	(println "import " item))


(exit)
