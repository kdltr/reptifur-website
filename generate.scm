(use srfi-1 sxml-transforms files lowdown anaphora vector-lib)
(use imlib2)

(define max-thumb-height 250)
(define max-height 1500)

(define (scale img h)
  (let* ((width (image-width img))
         (height (image-height img))
         (factor (/ height h))
         (new-width (ceiling (/ width factor)))
         (new-height h))
    (image-scale img (inexact->exact new-width) (inexact->exact new-height))))

(define (scale-file path)
  (let ((source (image-load path))
        (destination (pathname-replace-extension
                      path
                      (string-append ".min." (pathname-extension path)))))
    (image-save (scale source max-thumb-height) destination)
    (unless (< (image-height source) max-height)
      (image-save (scale source max-height) path))))

(define lang (make-parameter 'fr))
(define (i18n-if en fr) (cond ((string=? (lang) "en") en)
                              ((string=? (lang) "fr") fr)))
(define (langify str #!optional (end ""))
  (string-append str "." (lang) end))

(define conversion-rules
  (cons* `(*PI* *preorder*
                . ,(lambda (tag args)
                     (let ((tag (car args))
                           (attrs (cdr args)))
                       (list #\< #\? tag
                             (map (lambda (p) (list #\space (car p) "=\"" (cdr p) "\"")) attrs)
                             #\? #\> #\newline))))
         `(*COMMENT* *preorder* .
                     ,(lambda (tag args)
                        ""))
         `(doctype-html *preorder* .
                        ,(lambda (tag args)
                           "<!DOCTYPE html>"))
         universal-conversion-rules*))

(define title (make-parameter ""))
(define contents (make-parameter '()))

(define (page-template)
  (define (selected page) (if (string=? page (title)) '(class "selected") '()))
  `((*PI* xml (version "1.0") (encoding "utf-8"))
    (*PI* xml-stylesheet (type "text/css") (href "/style.css"))
    (doctype-html)
    (html (@ (xmlns "http://www.w3.org/1999/xhtml")
             (xml:lang ,(lang)))
          (head
           (title "Reptifur - " ,(title))
           (meta (@ (name viewport) (content "initial-scale=1.0"))))
          (body
           (header
            (a (@ (href ,(langify "/index" ".xhtml")))
               (img (@ (src "logo.png"))))
            (nav
             (a (@ (href ,(langify "/index" ".xhtml"))
                   (id ,"index-link")
                   ,(selected (i18n-if "About" "À Propos")))
                ,(i18n-if "About" "À Propos"))
             (a (@ (href ,(langify "/gallery" ".xhtml"))
                   (id "gallery-link")
                   ,(selected "Portfolio"))
                "Portfolio")
             (a (@ (href ,(langify "/pricesheet" ".xhtml"))
                   (id "pricesheet-link")
                   ,(selected (i18n-if "Pricesheet" "Grille tarifaire")))
                ,(i18n-if "Pricesheet" "Grille tarifaire"))))
           (main
            ,(contents))
           (footer
            (a (@ (href ,(i18n-if "/index.fr.xhtml" "/index.en.xhtml"))
                  (xml:lang ,(i18n-if "fr" "en")))
               ,(i18n-if "Version française" "English version")))))))

(define (title-page)
  (parameterize
      ((title (i18n-if "About" "À Propos"))
       (contents `(article (div ,(call-with-input-file (langify "index" ".md") markdown->sxml)))))
    (page-template)))

(define (pricesheet-page)
  (parameterize
      ((title (i18n-if "Pricesheet" "Grille tarifaire"))
       (contents `(article (@ (id "pricesheet"))
                           (p (a (@ (href ,(langify "pricesheet" ".jpeg")))
                                 (img (@ (id "pricesheet-image")
                                         (src ,(langify "pricesheet" ".png"))
                                         (alt "pricesheet")
                                         (title ,(i18n-if
                                                  "Please, read my ToS below."
                                                  "Merci de lire mes conditions de service, plus bas."))))))
                           ,(call-with-input-file (langify "pricesheet" ".md") markdown->sxml))))
    (page-template)))

(define (gallery-page images)
  (parameterize
      ((title "Portfolio")
       (contents
        `(article (@ (id "gallery"))
                  (ul (@ (id "gallery-list"))
                      ,(map gallery-item images)))))
    (page-template)))

(define (gallery-item src)
  (let ((gif? (string=? "gif" (pathname-extension src))))
    (unless gif?
      (scale-file (make-pathname '("out" "gallery") src)))
    `(li (@ (class "gallery-item"))
         (a (@ (href "gallery/" ,(langify src ".xhtml")))
            (img (@ (class "gallery-image")
                    (id ,src)
                    (src "gallery/" ,(if gif? src (pathname-replace-extension src (string-append ".min." (pathname-extension src)))))))))))


(define (image-page src prev next)
  `((*PI* xml (version "1.0") (encoding "utf-8"))
    (*PI* xml-stylesheet (type "text/css") (href "/style.css"))
    (html (@ (xmlns "http://www.w3.org/1999/xhtml")
             (xml:lang "fr"))
          (head
           (title ,src)
           (meta (@ (name viewport) (content "initial-scale=1.0"))))
          (body (@ (style "overflow: visible;"))
           (main
            (a (@ (href ,(langify "../gallery") ".xhtml#" ,src))
               (img (@ (id "image") (src ,src)))))))))


(define (generate-page file data)
  (with-output-to-file file
    (lambda () (SRV:send-reply (pre-post-order* data conversion-rules)))))


(define (generate-website)
  (create-directory "out")
  (file-copy "style.css" (make-pathname "out" "style.css") #t)
  (file-copy "portrait.png" (make-pathname "out" "portrait.png") #t)
  (file-copy "logo.png" (make-pathname "out" "logo.png") #t)
  (file-copy (langify "pricesheet" ".png")
             (make-pathname "out" (langify "pricesheet" ".png")) #t)
  (file-copy (langify "pricesheet" ".jpeg")
             (make-pathname "out" (langify "pricesheet") "jpeg") #t)
  (generate-page (make-pathname "out" (langify "index") "xhtml")
                 (title-page))
  (generate-page (make-pathname "out" (langify "pricesheet") "xhtml")
                 (pricesheet-page))

  (let* ((images (directory "gallery"))
         (n (length images)))
    (create-directory (make-pathname "out" "gallery"))
    (let ((images (list->vector images)))
      (vector-for-each
       (lambda (i src)
         (let* ((dest (make-pathname '("out" "gallery") src)))
           (file-copy (make-pathname "gallery" src) dest #t))
         (generate-page (make-pathname '("out" "gallery") (langify src) "xhtml")
                        (image-page (string-append src)
                                    (and (not (zero? i)) (vector-ref images (sub1 i)))
                                    (and (< i (sub1 n)) (vector-ref images (add1 i))))))
       images))
    (generate-page (make-pathname "out" (langify "gallery") "xhtml")
                   (gallery-page images))))

(create-directory "out")
(delete-directory "out" #t)
(parameterize ((lang "fr")) (generate-website))
(parameterize ((lang "en")) (generate-website))
