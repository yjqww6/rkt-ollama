#lang racket/base

(module llama-cpp racket/base
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/unsafe/cvector)
  (provide (all-defined-out))

  (define-ffi-definer define-llama (ffi-lib "libllama"))

  (define LLAMA_DEFAULT_SEED #xFFFFFFFF)

  (define _llama_token _int32)
  (define _llama_pos _int32)
  (define _llama_seq_id _int32)

  (define-cstruct _llama_model_params
    ([devices _pointer]
     [n_gpu_layers _int32]
     [split_mode _int32]
     [main_gpu _int32]
     [tensor_split _pointer]
     [rpc_servers _string]
     [progress_callback (_fun _float _pointer -> _stdbool)]
     [progress_callback_user_data _pointer]
     [kv_overrides _pointer]
     [vocab_only _stdbool]
     [use_mmap _stdbool]
     [use_mlock _stdbool]
     [check_tensors _stdbool]))

  (define-cstruct _llama_context_params
    ([n_ctx _uint32]
     [n_batch _uint32]
     [n_ubatch _uint32]
     [n_seq_max _uint32]
     [n_threads _int32]
     [n_threads_batch _int32]
     [rope_scaling_type _int32]
     [pooling_type _int32]
     [attention_type _int32]
     [rope_freq_base _float]
     [rope_freq_scale _float]
     [yarn_ext_factor _float]
     [yarn_attn_factor _float]
     [yarn_beta_fast _float]
     [yarn_beta_slow _float]
     [yarn_orig_ctx _uint32]
     [defrag_thold _float]
     [cb_eval (_fun _pointer _int -> _int)]
     [cb_eval_user_data _pointer]
     [type_k _int32]
     [type_v _int32]
     [logits_all _stdbool]
     [embeddings _stdbool]
     [offload_kqv _stdbool]
     [flash_attn _stdbool]
     [no_perf _stdbool]
     [abort_callback (_fun _pointer -> _stdbool)]
     [abort_callback_data _pointer]))

  (define-cstruct _llama_model_quantize_params
    ([nthread _int32]
     [ftype _int32]
     [output_tensor_type _int32]
     [token_embedding_type _int32]
     [allow_requantize _stdbool]
     [quantize_output_tensor _stdbool]
     [only_copy _stdbool]
     [pure _stdbool]
     [keep_split _stdbool]
     [imatrix _pointer]
     [kv_overrides _pointer]))

  (define-cstruct _llama_logit_bias
    ([token _llama_token]
     [bias _float]))

  (define-cstruct _llama_sampler_chain_params
    ([no_perf _stdbool]))

  (define-cstruct _llama_chat_message
    ([role _string]
     [content _string]))

  (define-cstruct _llama_token_data
    ([id _llama_token]
     [logit _float]
     [p _float]))

  (define-cstruct _llama_token_data_array
    ([data _pointer]
     [size _size]
     [selected _int64]
     [sorted _stdbool]))

  (define-cstruct _llama_batch
    ([n_tokens _int32]
     ;[token _pointer]
     [token _gcpointer]
     [embd _pointer]
     [pos _pointer]
     [n_seq_id _pointer]
     [seq_id _pointer]
     [logits _pointer]))

  (define-llama llama_backend_init (_fun -> _void))
  (define-llama llama_model_default_params (_fun -> _llama_model_params))
  (define-llama llama_context_default_params (_fun -> _llama_context_params))
  (define-llama llama_sampler_chain_default_params (_fun -> _llama_sampler_chain_params))
  (define-llama llama_load_model_from_file (_fun _string _llama_model_params -> _pointer))
  (define-llama llama_free_model (_fun _pointer -> _void))
  (define-llama llama_new_context_with_model (_fun _pointer _llama_context_params -> _pointer))
  (define-llama llama_free (_fun _pointer -> _void))
  (define-llama llama_time_us (_fun -> _int64))
  (define-llama llama_tokenize (_fun _pointer _bytes _int32 _pointer _int32 _stdbool _stdbool -> _int32))
  (define-llama llama_token_to_piece (_fun _pointer _llama_token _bytes _int32 _int32 _stdbool -> _int32))
  (define-llama llama_token_bos (_fun _pointer -> _llama_token))
  ;(define-llama llama_batch_get_one (_fun _pointer _int32 -> _llama_batch))
  (define-llama llama_decode (_fun _pointer _llama_batch -> _int32))
  (define-llama llama_sampler_chain_init (_fun _llama_sampler_chain_params -> _pointer))
  (define-llama llama_sampler_chain_add (_fun _pointer _pointer -> _void))
  (define-llama llama_sampler_init_greedy (_fun -> _pointer))
  (define-llama llama_sampler_init_dist (_fun _uint32 -> _pointer))
  (define-llama llama_sampler_init_top_k (_fun _int32 -> _pointer))
  (define-llama llama_sampler_init_min_p (_fun _float _size -> _pointer))
  (define-llama llama_sampler_init_temp (_fun _float -> _pointer))
  (define-llama llama_sampler_init_grammar (_fun _pointer _bytes/nul-terminated _bytes/nul-terminated -> _pointer))
  (define-llama llama_sampler_sample (_fun _pointer _pointer _int32 -> _llama_token))
  (define-llama llama_sampler_free (_fun _pointer -> _void))
  (define-llama llama_sampler_accept (_fun _pointer _llama_token -> _void))
  (define-llama llama_get_model (_fun _pointer -> _pointer))
  (define-llama llama_model_has_encoder (_fun _pointer -> _stdbool))
  (define-llama llama_model_has_decoder (_fun _pointer -> _stdbool))
  (define-llama llama_model_decoder_start_token (_fun _pointer -> _llama_token))
  (define-llama llama_model_is_recurrent (_fun _pointer -> _stdbool))
  (define-llama llama_token_is_eog (_fun _pointer _llama_token -> _stdbool))
  (define-llama llama_kv_cache_clear (_fun _pointer -> _void))
  (define-llama llama_kv_cache_seq_rm (_fun _pointer _llama_seq_id _llama_pos _llama_pos -> _stdbool))
  (define-llama llama_n_ctx (_fun _pointer -> _uint32))
  )

(require 'llama-cpp
         racket/match
         racket/control
         ffi/unsafe
         ffi/unsafe/cvector
         ffi/unsafe/atomic
         ffi/unsafe/alloc
         data/gvector)
(provide init-model! completion)

(struct model-context (model context cache))

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

(define (make-context model n-context)
  (define ctx-params (llama_context_default_params))
  (set-llama_context_params-n_ctx! ctx-params n-context)
  (set-llama_context_params-n_batch! ctx-params n-batch)
  (set-llama_context_params-flash_attn! ctx-params #t)
  (set-llama_context_params-no_perf! ctx-params #f)
  (define ctx (llama_new_context_with_model model ctx-params))
  (unless ctx
    (error 'main "failed to create the llama_context"))
  ctx)

(define (init-model! #:path model-path #:context [context 8192] #:n-gpu-layers [ngl #f])
  (define model-params (llama_model_default_params))
  (when ngl
    (set-llama_model_params-n_gpu_layers! model-params ngl))
  (define model (llama_load_model_from_file model-path model-params))
  (unless model
    (error 'main "unable to load model"))

  (define ctx (make-context model context))
  (model-context model ctx (make-gvector #:capacity context)))

(llama_backend_init)

(define (detoken model id buf)
  (define n (llama_token_to_piece model id buf 128 0 #t))
  (when (< n 0)
    (error 'decode-token "failed to convert token to piece"))
  n)

;;; avoid something like " " "[" vs " ["
(define (tokenize/cache model prompt-bytes cache buf)
  (define bos (llama_token_bos model))
  (let loop ([i 0] [c 0])
    (cond
      [(= c (gvector-count cache))
       (define b (subbytes prompt-bytes i))
       (match-define (cons tokens n-tokens) (tokenize model b #f))
       (define new-tokens (make-cvector _llama_token (+ n-tokens (gvector-count cache))))
       (for ([t (in-gvector cache)]
             [i (in-naturals)])
         (cvector-set! new-tokens i t))
       (for ([i (in-naturals (gvector-count cache))]
             [o (in-range n-tokens)])
         (cvector-set! new-tokens i (cvector-ref tokens o)))
       (cons
        new-tokens
        (cvector-length new-tokens))]
      [(= bos (gvector-ref cache c))
       (loop i (+ c 1))]
      [else
       (define n (detoken model (gvector-ref cache c) buf))
       (cond
         [(> (+ i n) (bytes-length prompt-bytes))
          (tokenize model prompt-bytes)]
         [(for/and ([a (in-bytes prompt-bytes i)]
                    [i (in-range n)])
            (eq? a (bytes-ref buf i)))
          (loop (+ i n) (+ c 1))]
         [else
          (tokenize model prompt-bytes)])])))

(define (tokenize model prompt-bytes [whole? #t])
  (define attempt (make-cvector _llama_token (* 2 (bytes-length prompt-bytes))))
  (define n-prompt (llama_tokenize model prompt-bytes (bytes-length prompt-bytes) (cvector-ptr attempt) (cvector-length attempt) whole? #t))
  (cond
    [(< n-prompt 0)
     (define prompt-tokens (make-cvector _llama_token (- n-prompt)))
     (when (< (llama_tokenize model prompt-bytes (bytes-length prompt-bytes) (cvector-ptr prompt-tokens) (- n-prompt) whole? #t) 0)
       (error 'tokenize "failed"))
     (cons prompt-tokens (- n-prompt))]
    [else
     (cons attempt n-prompt)]))

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
                    #:grammar [grammar #f])

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
  (match-define (cons prompt-tokens n-prompt) (tokenize/cache model prompt-bytes cache buf))
  (when (> n-prompt max-ctx)
    (error 'completion "too large"))

  (define off (use-prompt-cache! prompt-tokens n-prompt cache ctx))
  (define prompt-time (current-inexact-monotonic-milliseconds))
  (define init-token-id
    (let loop ([off off])
      (cond
        [(>= (+ off n-batch) n-prompt)
         (decode prompt-tokens off (- n-prompt off))
         (llama_sampler_sample smpl ctx -1)]
        [else
         (decode prompt-tokens off n-batch)
         (event)
         (loop (+ off n-batch))])))

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
