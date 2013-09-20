#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch)
(require web-server/http/response-structs)
(require web-server/http/request-structs)
(require srfi/1)


; A blog is a (blog posts)
; where posts is a (listof post)
(struct blog (posts) #:mutable)

; and post is a (post title body)
; where title is a string, and body is a string
(struct post (id title body comments) #:mutable)

; BLOG: blog
; The initial BLOG.
(define BLOG
  (blog
   (list (post "second-post" "Second Post"
               "This is another post"
               (list "1st comment on 2nd post" "2nd comment on 2nd post"))
         (post "first-post" "First Post"
               "This is my first post"
               (list "First comment!")))))

; blog-insert-post!: blog post -> void
; Consumes a blog and a post, adds the post at the top of the blog.
(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))

; post-insert-comment!: post string -> void
; Consumes a post and a comment string.  As a side-efect,
; adds the comment to the bottom of the post's list of comments.
(define (post-insert-comment! a-post a-comment)
  (set-post-comments!
   a-post
   (append (post-comments a-post) (list a-comment))))


; parse-post: bindings -> post
; Extracts a post out of the bindings.
(define (parse-post bindings)
  (post (extract-binding/single 'id bindings) (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)
        (extract-binding/single 'comment bindings)))





; rest: request -> doesn't return
; Consumes a request, activate blog-dispatch to decide which function to process the request
(define (rest request)
  (blog-dispatch request))

; the url format is: http://localhost:{port}/.../... (examples below)
(define-values (blog-dispatch blog-url)
  (dispatch-rules
   [("xml""blog") #:method "get" get-blog-xml] ; /xml/blog
   [("xml""blog") #:method "post" add-post-xml] ; /xml/blog
   [("xml""blog""post" (string-arg)) #:method "get" get-post-xml] ; /xml/blog/post/first-post
   [("posts" (string-arg)) review-post] 
   [("archive" (integer-arg) (integer-arg)) review-archive]
   [else render-not-implemented]))

(define (render-not-implemented req)
  (response 501 #"Not Implemented" (current-seconds) #f null void)  
  )

(define (render-under-construction req)
  (response/xexpr
   '(html
     (head (title "My Blog"))
     (body (h1 "Under construction"))))  
  )

(define (review-post req p) `(review-post ,p))
(define (review-archive req y m) `(review-archive ,y ,m))

(define (print-req-raw request)
  (printf (bindings-assq-all #"title" (request-bindings/raw request)))
  )

(define (print-req-params request)
  (match (bindings-assq #"title" (request-bindings/raw request))
    [(struct binding (id))
     (printf "I got a params " (bytes->string/utf-8 id))]
    [_ (struct binding (id)) (printf "I got no params ")])) 

(define (get-params request)
  (match
      (bindings-assq
       #"title"
       (request-bindings/raw request))
    [(? binding:form? b)
     (bytes->string/utf-8
      (binding:form-value b))]
    [_
     (print-req-raw request)]))
 

; insert a post into the BLOG list of post
(define (add-post-xml request)
  (blog-insert-post!
   BLOG (parse-post (request-bindings request)))
  (response/xexpr #:mime-type #"text/xml"   
                  `(status "Success")))


(define (find-post-by-id listp id) 
  (let ([px (car listp)])
    (if (equal? (post-id px) id)
        (identity px)
        (if (equal? (cdr listp) null)
            (identity null)
            (find-post-by-id (cdr listp) id)
            ))
    ))

(define (get-post-xml request p)
  (let ([a-post (find-post-by-id (blog-posts BLOG) p)])
    (if (null? a-post) 
        (response 404 #"Not Found" (current-seconds) #f null void)
        (response/xexpr #:mime-type #"text/xml"   
                        `(post (id ,(post-id a-post)) (title ,(post-title a-post))
                               (body ,(post-body a-post))
                               (comments ,@(map render-post-comments-xml (post-comments a-post)))))))  
  )

; reply-blog-xml: request -> doesn't return
; Produces an XML of the content of the BLOG.
(define (get-blog-xml request)  
  (response/xexpr #:mime-type #"text/xml"   
                  `(blog
                    (title "Restkit Blog")
                    ,(render-posts-xml)
                    ))  
  )

(define (render-post-comments-xml a-comment)
  `(comment ,(let ([x a-comment]) x))
  )

; render-post: post -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
(define (render-post-xml a-post)
  `(post (id ,(post-id a-post)) (title ,(post-title a-post))
         (body ,(post-body a-post))
         (comments ,@(map render-post-comments-xml (post-comments a-post)))))

; render-posts: -> xexpr
; Consumes a blog, produces an xexpr fragment
; of all its posts.
(define (render-posts-xml)
  `(posts ,@(map render-post-xml (blog-posts BLOG))))

; below define is not needed, it is use only if need to convert url to request
(define (url->request u)
  (make-request #"GET" (string->url u) empty
                (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))

(serve/servlet rest #:port 8080 #:servlet-regexp #rx"" #:launch-browser? #f)









