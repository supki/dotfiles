#!/usr/bin/env racket
#lang racket

(require racket/match)
(require racket/path)

(define (get-value str)
  (match (string-split str) [(list k v) v]))

(define (read-values file)
  (cons (get-value (read-line file)) (get-value (read-line file))))

(define (system*/exit-code/error-out command . args)
  (printf "Running ~a\n" (cons command args))
  (let ([exit-code (apply system*/exit-code command args)])
    (if (zero? exit-code)
      '()
      (error 'uplodocus "command ~a exited with exit code ~s" (cons command args) exit-code))))

(define cabal (find-executable-path "cabal"))
(define cp (find-executable-path "cp"))
(define tar (find-executable-path "tar"))
(define curl (find-executable-path "curl"))

(define password (getenv "PASSWORD"))

(define (cabal-files-in-current-directory)
  (filter
    (λ (file)
       (let [(ext (filename-extension file))]
         (if ext
           (bytes=? ext  #"cabal")
           #f)))
    (directory-list)))

(define (uplodocus/main)
  (match (cabal-files-in-current-directory)
         [(list file) (match (call-with-input-file file read-values)
                             [(cons name version)
                              (uplodocus/upload name version)
                              '!]
                             [_ (error 'uplodocus(format "Bad .cabal file ~s" file))])]
         ['() (error 'uplodocus (format "No .cabal files found"))]
         [files (error (format "Multiple .cabal files found: ~s" (map path->string files)))]))

(define (uplodocus/upload name version)
  (let* [(name-version   (format "~a-~a" name version))
         (docs-directory (format "~a-docs" name-version))
         (tar-archive    (format "~a.tar.gz" docs-directory))]
    (system*/exit-code/error-out
      cabal "haddock" "--hyperlink-source"
      (format "--html-location=http://hackage.haskell.org/package/~a/docs" name)
      (format "--contents-location=http://hackage.haskell.org/package/~a" name))
    (dynamic-wind
      (λ ()
         (system*/exit-code/error-out
           cp "-R" (format "dist/doc/html/~a" name) docs-directory))
      (λ ()
         (dynamic-wind
           (λ ()
              (system*/exit-code/error-out
                tar "cz" "--format=ustar" "-f" tar-archive docs-directory))
           (λ ()
              (system*/exit-code/error-out
                curl "-X" "PUT"
                "-H" "Content-Type: application/x-tar"
                "-H" "Content-Encoding: gzip"
                "--data-binary" (format "@~a" tar-archive)
                (format "https://MatveyAksenov:~a@hackage.haskell.org/package/~a/docs" password name-version)))
           (λ ()
              (delete-file tar-archive))))
      (λ ()
         (delete-directory/files docs-directory)))))

(uplodocus/main)
