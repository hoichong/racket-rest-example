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

(define SVC_OUTCOME (mcons "status" "_"))

; blog-insert-post!: blog post -> void
; Consumes a blog and a post, adds the post at the top of the blog.
; error-checking here: TODO
; TRANSACTIONS ?
(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog (cons a-post (blog-posts a-blog)))
  )

(define (blog-insert-post-comment! a-blog a-post)
  (let ([apo (find-post-by-id (blog-posts a-blog) (post-id a-post))])
    (if (null? apo) (set-mcdr! SVC_OUTCOME "404") (post-insert-comment! apo (post-comments a-post))))
  )

; blog-update-post!: blog post -> void
; Consumes a blog and a post, add the post to the top of the blog if the post is a new one 
; or update an existing post title, post body
(define (blog-update-post! a-blog a-post pid)
  (let ([apo (find-post-by-id (blog-posts a-blog) pid)])
    (cond
      [(null? apo) (set-blog-posts! a-blog (cons a-post (blog-posts a-blog)))]
      [else (set-post-title! apo (post-title a-post)) (set-post-body! apo (post-body a-post))])
    )
  )


; post-insert-comment!: post string -> void
; Consumes a post and a comment string.  As a side-efect,
; adds the comment to the bottom of the post's list of comments.
; error-checking here: TODO
(define (post-insert-comment! a-post a-comment)
  (set-post-comments! a-post
                      (append (if (null? (post-comments a-post)) '() (post-comments a-post)) (list a-comment)))
  )


; parse-post: bindings -> post
; Extracts a post out of the bindings.
; error-checking here: TODO
(define (parse-post bindings)
  (post (extract-binding/single 'id bindings) (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)
        (list (extract-binding/single 'comment bindings))))

(define (parse-post-comment bindings)
  (post (extract-binding/single 'id bindings) "_" "_"
        (extract-binding/single 'comment bindings)))



; rest: request -> doesn't return
; Consumes a request, activate blog-dispatch to decide which function to process the request
(define (rest request)
  (blog-dispatch request))

; the url format is: http://localhost:{port}/.../... (examples below)
; "xm""blog" resolve to /xml/blog
; "xml""blog""post" (string-arg) resolve to /xml/blog/post/id-of-xyz-post
(define-values (blog-dispatch blog-url)
  (dispatch-rules
   [("xml""blog") #:method "get" get-blog-xml] ; /xml/blog .get all posts from the blog
   [("xml""blog""post") #:method "post" add-post-xml] ; /xml/blog/post .add a post into the blog
   [("xml""blog""post" (string-arg)) #:method "get" get-post-xml] ; /xml/blog/post/xyz-post-id
   [("xml""blog""post""comment") #:method "post" add-post-comment-xml] ; /xml/blog/post/comment
   [else render-not-implemented]))



(define (render-not-implemented req)
  (response 501 #"Not Implemented" (current-seconds) #f null void)  
  )

(define (render-under-construction req)
  (response/xexpr
   '(html
     (head (title "Restkit Blog"))
     (body (h1 "Under construction"))))  
  )


; insert or update a post into the BLOG list of post
(define (update-post-xml request p)
  (blog-update-post!
   BLOG (parse-post (request-bindings request)) p)
  (response/xexpr #:mime-type #"text/xml"   
                  `(status "Success")))

; insert a comment into a post
(define (add-post-comment-xml request)
  (blog-insert-post-comment!
   BLOG (parse-post-comment (request-bindings request)))
  (if (equal? (mcdr SVC_OUTCOME) "404") 
      (response 404 #"Not Found" (current-seconds) #f null void)
      (response/xexpr #:mime-type #"text/xml"   
                      `(status "Success"))))

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









