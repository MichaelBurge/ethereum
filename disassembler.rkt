#lang typed/racket

(require "types.rkt")
(require "opcodes.rkt")
(require "instructions.rkt")

(require "typed/binaryio.rkt")
;(require "typed/dict.rkt")
(provide (all-defined-out))

(: disassemble-one (-> Bytes Integer EthInstruction))
(define (disassemble-one bs i)
  (let* ([ byte (cast (bytes-or-zero bs i 1) Byte)])
    (if (hash-has-key? *opcodes-by-byte* byte)
        (disassemble-opcode bs i (hash-ref *opcodes-by-byte* byte))
        (eth-bytes (bytes byte)))))

(: disassemble-opcode (-> Bytes Integer opcode EthInstruction))
(define (disassemble-opcode bs i op)
  (cond ((push-op? op) (disassemble-push bs i))
        (else          (eth-op (opcode-name op)))
        ))


(: disassemble-push (-> Bytes Integer EthInstruction))
(define (disassemble-push bs i)
  (let ([ op (hash-ref *opcodes-by-byte* (bytes-ref bs i)) ])
    (eth-push (op-extra-size op)
              (bytes-or-zero bs
                             (+ i 1) ; start position
                             (op-extra-size op))))) ; length

(: for/assembly-cont (-> Bytes (-> Integer EthInstruction (U #f Integer)) [#:error? Boolean] Void))
(define (for/assembly-cont bs act #:error? [error? #f])
  (: loop (-> Integer Void))
  (define (loop n)
    (let ([ ethi (disassemble-one bs n) ])
      (match (act n ethi)
        [ #f (void)]
        [ (? integer? m)
          (if (>= m (bytes-length bs))
              (if error?
                  (error "for-assembly/cont: Out of range" m bs)
                  (void))
              (loop m))])))
  (loop 0))

(: for/assembly (-> Bytes (-> Integer EthInstruction Void) Void))
(define (for/assembly bs act)
  (: loop (-> Integer EthInstruction Integer))
  (define (loop n ethi)
    (act n ethi)
    (+ n (instruction-size ethi)))
  (for/assembly-cont bs loop))

(: disassemble (-> Bytes EthInstructions))
(define (disassemble bs)
  (: ret EthInstructions)
  (define ret null)
  (for/assembly bs (λ (n x)
                     (set! ret (cons x ret))))
  (reverse ret))

(: print-disassembly (-> SymbolTable Bytes Void))
(define (print-disassembly symbol-table bs)
  (let ([ reverse-symbol-table (invert-hash symbol-table) ])
    (: loop (-> Integer EthInstruction Void))
    (define (loop n ethi)
      (fprintf (current-output-port) "~x" n)
      (write-char #\tab)
      (display (reverse-symbol-name reverse-symbol-table n))
      (write-char #\tab)
      (cond ((eth-push? ethi)
             (begin
               (define op (ethi->opcode ethi))
               (fprintf (current-output-port)
                        "Push~a 0x~x"
                        (op-extra-size op)
                        (eth-push-value ethi))))
            ((eth-op? ethi)
             (void (write-string (symbol->string (opcode-name (ethi->opcode ethi))))))
            ((eth-bytes? ethi)
             (fprintf (current-output-port)
                      "BYTE ~a"
                      (bytes->integer (eth-bytes-bytes ethi) #f)))
            (else (error "print-disassembly: Unknown ethi" ethi))))
    (for/assembly bs loop)
    ))

(: bytes-or-zero (-> Bytes Integer Integer Integer))
(define (bytes-or-zero bs i len)
  (if (>= i (bytes-length bs))
      0
      (bytes->integer bs #f #t i
                      (min (+ i len)
                           (bytes-length bs)))))

(: invert-hash (All (A B) (-> (Mutable-HashTable A B) (Mutable-HashTable B A))))
(define (invert-hash hash)
  (: elems (Listof (Pairof A B)))
  (define elems (hash->list hash))

  (: swap-pair (-> (Pairof A B) (Pairof B A)))
  (define (swap-pair p) (cons (cdr p) (car p)))
  (make-hash (map swap-pair elems)))

(: reverse-symbol-name (-> ReverseSymbolTable Integer Symbol))
(define (reverse-symbol-name reverse-symbol-table n)
  (hash-ref reverse-symbol-table n (λ () '||)))
