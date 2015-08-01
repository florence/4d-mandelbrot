#lang typed/racket

(require pict3d pict3d/universe math/flonum racket/flonum
         racket/unsafe/ops racket/fixnum typed/net/gifwrite)

(define-type Quat FlVector)
(: quat : Flonum Flonum Flonum Flonum -> Quat)
(define (quat a b c d) (flvector a b c d))

(define zero-quat (quat 0.0 0.0 0.0 0.0))

(: aprox-mandel : (Positive-Fixnum -> (Quat -> Flonum)))
(define ((aprox-mandel n) c)
  (let loop : Flonum ([n* : Nonnegative-Fixnum n]
                      [z_n : Quat zero-quat]
                      [rate : Flonum 0.0])
    (if (fx= n* 0)
        (fl/ rate (fl n))
        (let ([z_n+1 (mandel c z_n)])
          (loop (fx- n* 1)
                z_n+1
                (fl+ (quat-dist (quat- z_n z_n+1)) rate))))))

;(: mandel (Quat Quat -> Quat))
(define-syntax-rule (mandel c z)
  (let ([idz z]
        [idc c])
    (quat+ (quat^2 idz) idc)))

(: quat+ : Quat Quat -> Quat)
(define quat+ flvector+)


(: quat- : Quat Quat -> Quat)
(define quat- flvector-)

(: quat-dist : Quat -> Flonum)
(define (quat-dist q)
  (flsqrt
   (fl+ (flsqr (unsafe-flvector-ref q 0))
        (fl+ (flsqr (unsafe-flvector-ref q 1))
             (fl+ (flsqr (unsafe-flvector-ref q 2))
                  (flsqr (unsafe-flvector-ref q 3)))))))

;(: quat^2 : Quat -> Quat)
(define-syntax-rule (quat^2 q)
  (let ([idq q])
    (let ([a (unsafe-flvector-ref idq 0)]
          [b (unsafe-flvector-ref idq 1)]
          [c (unsafe-flvector-ref idq 2)]
          [d (unsafe-flvector-ref idq 3)])
      (define 2a (fl* 2.0 a))
      (let ([r (fl- (flsqr a)
                    (fl+ (flsqr b)
                         (fl+ (flsqr c)
                              (flsqr d))))]
            [x (fl* 2a b)]
            [y (fl* 2a c)]
            [z (fl* 2a d)])
        (quat r x y z)))))

(define-syntax-rule (flsqr x)
  (let ([id x]) (fl* id id)))

(provide render-at-passes)
(: render-at-passes : (∀ (A) (Positive-Fixnum
                              Flonum Flonum
                              Integer
                              (Flonum Pict3D A -> A)
                              A
                              -> A)))
(define (render-at-passes n s e samples make init)
  (define start (fl s))
  (define end (fl e))
  (define steps (fl samples))
  (define base
    (freeze
     (let ([seq (list (sub1 start) (add1 end))])
       (for*/fold ([p : Pict3D empty-pict3d])
                  ([x (in-list seq)]
                   [y (in-list seq)]
                   [z (in-list seq)])
         (combine (light (pos x y z)
                         (emitted "white" 5))
                  p)))))
  (define world
    (parameterize ([current-material
                    (material #:ambient 0.0
                              #:diffuse 0.6
                              #:specular 0.39
                              #:roughness 0.0)])
      (define step-size (/ (- end start) steps))
      (define cube-size (/ step-size 2))
      (: cache : (Vectorof Pict3D))
      (define cache
        (for/vector : (Vectorof Pict3D) #:length 100 ([k : Positive-Fixnum (in-range 1 101)])
          (define α (fl/ (fl k) 100.0))
          (define col (fl- 1.0 α))
          (define colv (flvector 1.0 #;col 0.0 0.0 α
                        ))
          (with-color (rgba colv)
            (with-emitted (emitted colv 1)
              (cube origin step-size)))))
      (define-syntax-rule (point α_raw)
        (let ()
          (define col (fllog α_raw))
          (define α (fl- 1.0 col))
          (and
           (fl>= α .01)
           (vector-ref cache
                       (if (fl>= α 1.0)
                           99
                           (fx- (fl->fx (floor (fl* 100.0 α)))
                                1))))))
      (define f (aprox-mandel n))
      (for/fold : A ([res : A init])
                ([x : Flonum (in-range start end step-size)])
        ;(printf "rendering timestep ~a\n" r)
        (make
         x
         (apply combine
                (for*/fold ([res : (Listof Pict3D) (list base)])
                           ([r : Flonum (in-range start end step-size)]
                            [y : Flonum (in-range start end step-size)]
                            [z : Flonum (in-range start end step-size)])
                  (define α_raw (f (quat r x y z)))
                  (define p (point α_raw))
                  (if (not p)
                      res
                      (cons (move p (dir r y z)) res))))
         res))))
  world)

(provide rotatory)
(: rotatory : (Positive-Fixnum
               Flonum Flonum
               Integer
               -> Void))
(define (rotatory n start end steps)
  (define worlds (map freeze (render-at-passes n start end steps
                                               (lambda (_ [a : Pict3D] [r : (Listof Pict3D)])
                                                 (cons a r))
                                               null)))

  (define camera-start (pos (* 2 (max (abs start) (abs end))) 0 0))
  (define camera (basis 'camera (point-at camera-start origin)))

  (current-pict3d-background (rgba "white"))

  (: draw : (Listof Pict3D) Integer Flonum -> Pict3D)
  (define (draw s n t)
    (combine
     (rotate-z/center (rotate-y/center (rotate-x/center (first s)
                                                        (/ t 11.0))
                                       (/ t 13.0))
                      (/ t 17.0))
     camera))
  (: frame : (Listof Pict3D) Integer Any -> (Listof Pict3D))
  (define (frame s n t)
    (if (even? n)
        s
        (let ([r (rest s)])
          (if (null? r)
              worlds
              r))))
  ((inst big-bang3d (Listof Pict3D))
   worlds
   #:on-draw draw
   #:on-frame frame)
  (void))


(define DELAY 10);/100ths of a second

(provide render!)
(: render! : (->*
              (Path-String
               Positive-Fixnum
               Flonum Flonum
               Integer)
              (Integer
               #:width Nonnegative-Integer
               #:height Nonnegative-Integer)
              Void))
(define (render! path n start end steps [delay DELAY]
                 #:width [width (current-pict3d-width)]
                 #:height [height (current-pict3d-height)])
  (parameterize ([current-pict3d-width width]
                 [current-pict3d-height height])
    (define outside (* 1 (max (abs start) (abs end))))
    (define camera-start (pos (- outside) outside outside))
    (define camera (basis 'camera (point-at camera-start origin)))
    (parameterize ([current-pict3d-background (rgba "white")])
      (call-with-output-file*
       path #:exists 'replace
       (lambda ([p : Output-Port])
         (define count 0)
         (define w (current-pict3d-width))
         (define h (current-pict3d-height))
         (define stream (gif-start p w h 0 #f))
         (gif-add-loop-control stream 0)
         (render-at-passes
          n start end steps
          (lambda ([r : Flonum] [pict : Pict3D] _)
            (define btmp (pict3d->bitmap (combine (rot (fl* 360.0 r) pict) camera)))
            (define-values (pxls colormap transparent)
              (let ([argb (make-bytes (* w h 4) 255)]
                    [mask (send btmp get-loaded-mask)])
                (send btmp get-argb-pixels 0 0 w h argb)
                (when mask
                  (send mask get-argb-pixels 0 0 w h argb #t))
                (quantize argb)))
            (gif-add-control stream 'any #f delay transparent)
            (gif-add-image stream 0 0 w h #f colormap pxls))
          (void))
         (gif-end stream))))))

(: rot : Flonum Pict3D -> Pict3D)
(define (rot t pict)
  pict
  #;
  (rotate-z/center (rotate-y/center (rotate-x/center pict
                                                     t)
                                    t)
                   t))
