#lang racket/base
(require "ffi.rkt"
         racket/match
         racket/control
         ffi/unsafe
         ffi/unsafe/cvector
         ffi/unsafe/atomic
         ffi/unsafe/alloc
         data/gvector)
(provide init-model! completion free-model-context)

(struct model-context (model context cache))

(define (free-model-context mc)
  (match-define (model-context model context _) mc)
  (llama_free context)
  (llama_free_model model)
  (void))

(define n-batch 2048)

(define alloc-sampler
  ((allocator llama_sampler_free) llama_sampler_chain_init))

(define free-sampler
  ((deallocator) llama_sampler_free))

(define (make-sampler model grammar)
  (define sparams (llama_sampler_chain_default_params))
  (set-llama_sampler_chain_params-no_perf! sparams #f)
  (define smpl (alloc-sampler sparams))
  (when grammar
    (llama_sampler_chain_add
     smpl
     (llama_sampler_init_grammar model (string->bytes/utf-8 grammar) #"root")))
  (llama_sampler_chain_add smpl (llama_sampler_init_top_k 40))
  (llama_sampler_chain_add smpl (llama_sampler_init_min_p 0.05 1))
  (llama_sampler_chain_add smpl (llama_sampler_init_temp 0.8))
  (llama_sampler_chain_add smpl (llama_sampler_init_dist LLAMA_DEFAULT_SEED))
  smpl)

(define (simple-sample model context)
  (define n-vocab (llama_n_vocab model))
  (define logits (llama_get_logits_ith context -1))
  (define-values (id logit)
    (for/fold ([id 0]
               [logit 0.0])
              ([i (in-range n-vocab)])
      (define f (ptr-ref logits _float i))
      (if (> f logit)
          (values i f)
          (values id logit))))
  #;
  (printf "prob ~a~%"
          (/
           1
           (for/fold ([sum 0.0])
                     ([i (in-range n-vocab)])
             (define f (ptr-ref logits _float i))
             (+ sum (exp (- f logit))))))
  id)

(define (make-context model n-context #:kvcache-quant? [kvcache-quant? #f])
  (define ctx-params (llama_context_default_params))
  (set-llama_context_params-n_ctx! ctx-params n-context)
  (set-llama_context_params-n_batch! ctx-params n-batch)
  (set-llama_context_params-flash_attn! ctx-params #t)
  (set-llama_context_params-no_perf! ctx-params #f)
  (when kvcache-quant?
    (set-llama_context_params-type_k! ctx-params 8)
    (set-llama_context_params-type_v! ctx-params 8))
  (define ctx (llama_new_context_with_model model ctx-params))
  (unless ctx
    (error 'main "failed to create the llama_context"))
  ctx)

(define (init-model! #:path model-path #:context [context 8192] #:n-gpu-layers [ngl #f] #:kvcache-quant? [kvcache-quant? #f])
  (define model-params (llama_model_default_params))
  (when ngl
    (set-llama_model_params-n_gpu_layers! model-params ngl))
  (define model (llama_load_model_from_file model-path model-params))
  (unless model
    (error 'main "unable to load model"))

  (define ctx (make-context model context #:kvcache-quant? kvcache-quant?))
  (model-context model ctx (make-gvector #:capacity context)))

(llama_backend_init)

(define (detoken model id buf)
  (define n (llama_token_to_piece model id buf 128 0 #t))
  (when (< n 0)
    (error 'decode-token "failed to convert token to piece"))
  n)

;;; avoid something like " " "[" vs " ["
(define (tokenize/cache model prompt-bytes cache buf)
  (define (make-new-tokens cached-c cached-i)
    (cond
      [(= cached-c 0) (tokenize model prompt-bytes)]
      [(< cached-i (bytes-length prompt-bytes))
       (define-values (rest-tokens rest-n) (tokenize model (subbytes prompt-bytes cached-i) #f))
       (define-values (cached-tokens _) (make-cached-tokens cached-c rest-n))
       (for ([y (in-range rest-n)])
         (cvector-set! cached-tokens (+ cached-c y) (cvector-ref rest-tokens y)))
       (values cached-tokens (+ cached-c rest-n))]
      [else
       (make-cached-tokens cached-c)]))
  (define (make-cached-tokens cached-c [more 0])
    (define tokens (make-cvector _llama_token (+ cached-c more)))
    (for ([x (in-range cached-c)]
          [t (in-gvector cache)])
      (cvector-set! tokens x t))
    (values tokens cached-c))
  (let loop ([i 0] [c (if (and (> (gvector-count cache) 0)
                               (= (gvector-ref cache 0) (llama_token_bos model)))
                          1
                          0)])
    (cond
      [(or (= i (bytes-length prompt-bytes)) (= c (gvector-count cache)))
       (make-new-tokens c i)]
      [else
       (define n (detoken model (gvector-ref cache c) buf))
       (cond
         [(> (+ i n) (bytes-length prompt-bytes))
          (make-new-tokens c i)]
         [(for/and ([a (in-bytes prompt-bytes i)]
                    [i (in-range n)])
            (eq? a (bytes-ref buf i)))
          (loop (+ i n) (+ c 1))]
         [else
          (make-new-tokens c i)])])))

(define (tokenize model prompt-bytes [whole? #t])
  (define attempt (make-cvector _llama_token (bytes-length prompt-bytes)))
  (define n-prompt (llama_tokenize model prompt-bytes (bytes-length prompt-bytes) (cvector-ptr attempt) (cvector-length attempt) whole? #t))
  (cond
    [(< n-prompt 0)
     (define prompt-tokens (make-cvector _llama_token (- n-prompt)))
     (when (< (llama_tokenize model prompt-bytes (bytes-length prompt-bytes) (cvector-ptr prompt-tokens) (- n-prompt) whole? #t) 0)
       (error 'tokenize "failed"))
     (values prompt-tokens (- n-prompt))]
    [else
     (values attempt n-prompt)]))

(define (use-prompt-cache! tokens n cache context)
  (define count (gvector-count cache))
  (define diff0
    (let loop ([i 0])
      (cond
        [(and (< i n) (< i count))
         (define token (cvector-ref tokens i))
         (define c-token (gvector-ref cache i))
         (cond
           [(= token c-token) (loop (+ i 1))]
           [else i])]
        [else i])))
  (define diff (max (if (= diff0 n) (- diff0 1) diff0) 0))
  (llama_kv_cache_seq_rm context 0 diff -1)
  (let loop ()
    (when (> (gvector-count cache) diff)
      (gvector-remove-last! cache)
      (loop)))
  diff)

(define (completion mc prompt output
                    #:n-predict [n-predict 256]
                    #:perf [perf #f]
                    #:grammar [grammar #f]
                    #:progress [progress #f])

  (match-define (model-context model ctx cache) mc)
  (define max-ctx (llama_n_ctx ctx))

  (define buf (make-bytes 128))
  (define cvec (make-cvector _llama_token 1))

  (define smpl (make-sampler model grammar))
  (define begin-time (current-inexact-monotonic-milliseconds))

  (define (decode cvec off n)
    (define batch (make-llama_batch n (ptr-add (cvector-ptr cvec) off _llama_token) #f #f #f #f #f))
    ;(define batch (llama_batch_get_one (ptr-add (cvector-ptr cvec) off _llama_token) n))
    (define r (llama_decode ctx batch))
    (unless (zero? r)
      (error 'completion "failed to eval"))
    (for ([i (in-range n)])
      (gvector-add! cache (cvector-ref cvec (+ off i)))))
  (define (event)
    ;; TODO better way to clear ticks
    (for ([i (in-range 10000)])
      (void)))
    
  (define prompt-bytes (string->bytes/utf-8 prompt))
  (define-values (prompt-tokens n-prompt) (tokenize/cache model prompt-bytes cache buf))
  (when (> n-prompt max-ctx)
    (error 'completion "too large"))

  (define off (use-prompt-cache! prompt-tokens n-prompt cache ctx))
  (define prompt-time (current-inexact-monotonic-milliseconds))
  (when progress
    (progress off n-prompt 'begin))
  (define init-token-id
    (let loop ([off off])
      (cond
        [(>= (+ off n-batch) n-prompt)
         (decode prompt-tokens off (- n-prompt off))
         (llama_sampler_sample smpl ctx -1)]
        [else
         (decode prompt-tokens off n-batch)
         (when progress
           (progress off n-prompt 'step))
         (event)
         (loop (+ off n-batch))])))
  (when progress
    (progress n-prompt n-prompt 'end))

  (define decode-time (current-inexact-monotonic-milliseconds))

  (define callback? (procedure? output))

  (define n-decode
    (let loop ([token-id init-token-id] [n-decode 1])
      (cond
        [(llama_token_is_eog model token-id)
         n-decode]
        [else
         (define n (detoken model token-id buf))
         (define finish?
           (cond
             [callback? (output buf n)]
             [else
              (write-bytes buf output 0 n)
              (flush-output output)
              #f]))
         (event)
         (cond
           [finish? n-decode]
           [(and (< (+ n-prompt n-decode) max-ctx)
                 (or (not n-predict) (<= n-decode n-predict)))
            (cvector-set! cvec 0 token-id)
            (decode cvec 0 1)
            (loop (llama_sampler_sample smpl ctx -1) (+ n-decode 1))]
           [else n-decode])])))
  (when callback? (output buf 0))
  (free-sampler smpl)
  
  (define end-time (current-inexact-monotonic-milliseconds))
  (when perf
    (perf n-prompt (- n-prompt off) (- decode-time prompt-time)
          n-decode (- end-time decode-time))))
