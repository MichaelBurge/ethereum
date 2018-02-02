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
        (eth-unknown byte))))

(: disassemble-opcode (-> Bytes Integer opcode EthInstruction))
(define (disassemble-opcode bs i op)
  (cond ((push-op? op) (disassemble-push bs i))
        (else          (eth-asm (opcode-name op)))
        ))

   
(: disassemble-push (-> Bytes Integer EthInstruction))
(define (disassemble-push bs i)
  (let ([ op (hash-ref *opcodes-by-byte* (bytes-ref bs i)) ])
    (eth-push (op-extra-size op)
              (bytes->integer bs
                              #f      ; signed?
                              #t      ; big-endian
                              (+ i 1) ; start position
                              (+ i 1 (op-extra-size op)))))) ; end

; Outputs 3 column TSV suitable for pasting into Google sheets
(: print-disassembly (-> SymbolTable Bytes Void))
(define (print-disassembly symbol-table bs)
  (let ([ reverse-symbol-table (invert-hash symbol-table) ])
    (: loop (-> Integer Void))
    (define (loop n)
      (fprintf (current-output-port) "~x" n)
      (write-char #\tab)
      (display (reverse-symbol-name reverse-symbol-table n))
      ;; (print `(,(bytes-ref bs n)
      ;;          ,(push-op? (hash-ref opcodes-by-byte (bytes-ref bs n)))
      ;;          ,(op-extra-size (hash-ref opcodes-by-byte (bytes-ref bs n)))))
      (write-char #\tab)
      (let ([ ethi (disassemble-one bs n) ])
        (cond ((eth-push? ethi)
               (begin
                 (define op (ethi->opcode ethi))
                 (fprintf (current-output-port)
                          "Push~a 0x~x"
                          (op-extra-size op)
                          (eth-push-value ethi))))
              ((eth-asm? ethi) (write-string (symbol->string (opcode-name (ethi->opcode ethi)))))
              ((eth-unknown? ethi)
               (fprintf (current-output-port)
                        "BYTE ~a"
                        (eth-unknown-byte ethi)))
              (else (error "print-disassembly: Unknown ethi" ethi)))
        (set! n (+ n (ethi-extra-size ethi))))
      (newline)
      (if (>= n (- (bytes-length bs) 1))
          (void)
          (loop (+ n 1))))
    (loop 0)))

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
