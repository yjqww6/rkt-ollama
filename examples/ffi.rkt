#lang racket/base
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
(define-llama llama_n_vocab (_fun _pointer -> _uint32))
(define-llama llama_get_logits_ith (_fun _pointer _int32 -> _pointer))
(define-llama llama_state_seq_set_data (_fun _pointer _pointer _size _int32 -> _size))
(define-llama llama_state_seq_get_size (_fun _pointer _int32 -> _size))
(define-llama llama_state_seq_get_data (_fun _pointer _pointer _size _int32 -> _size))
