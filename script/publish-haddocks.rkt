#!/usr/bin/env racket
#lang racket

(define cabal-exe (find-executable-path "cabal"))
(define git-exe (find-executable-path "git"))

(define (cabal! . args)
  (apply system*/exit-code-throw cabal-exe args))
(define (git! . args)
  (apply system*/exit-code-throw git-exe args))
(define (git . args)
  (apply system*/exit-code git-exe args))

(define (publish-haddocks/main)
  (let [(branch (get-current-branch))]
    (cabal! "configure" "--enable-tests")
    (cabal! "haddock" "--hyperlink-source")
    (dynamic-wind
      (λ ()
         (git! "checkout" "gh-pages"))
      (λ ()
         (git! "pull" "--rebase" "origin" "gh-pages")
         (unless (file-exists? ".gitignore")
           (error 'publish-haddocks "missing .gitignore in the documentation branch"))
         (for-each
           (λ (root)
              (for-each
                (λ (file)
                   (begin
                     (delete-directory/files file #:must-exist? #f)
                     (copy-directory/files (build-path root file) file)))
                (directory-list root)))
           (directory-list "dist/doc/html" #:build? #t))
         (git! "add" "--no-ignore-removal" ".")
         (git! "status")
         (unless (zero? (git "diff-index" "--quiet" "HEAD" "--"))
           (git! "commit" "--message" "Update haddock documentation")
           (git! "push" "origin" "gh-pages")))
      (λ ()
         (git! "checkout" branch)))))

(define (get-current-branch)
  (string-trim
    (with-output-to-string (λ () (git! "rev-parse" "--abbrev-ref" "HEAD")))
    "\n"
    #:left? #f))

(define (system*/exit-code-throw command . args)
  (let ([exit-code (apply system*/exit-code command args)])
    (if (zero? exit-code)
      exit-code
      (error
        'publish-haddocks
        "command ~a exited with non-zero exit code ~s"
        (cons command args) exit-code))))

(publish-haddocks/main)
'!
