#lang typed/racket

(require "types.rkt")
(require "opcodes.rkt")

(provide (all-defined-out))

(define assumed-label-size 2)

(: ethi-extra-size (-> EthInstruction Byte))
(define (ethi-extra-size ethi)
  (if (eth-unknown? ethi)
      0
      (op-extra-size (ethi->opcode ethi))))

(: instruction-size (-> EthInstruction Integer))
(define (instruction-size i)
  (if (eth-push? i)
      (+ 1 (push-true-size i))
      1))

(: instructions-size (-> EthInstructions Integer))
(define (instructions-size is)
  (for/sum : Integer ([ i is ])
    (instruction-size i)))

(: lookup-push-opcode (-> eth-push opcode))
(define (lookup-push-opcode push)
  (: sz (U 'shrink Byte))
  (define sz (eth-push-size push))
  (if (byte? sz)
      (let ([ b : Byte (cast (+ #x5f sz) Byte)])
        (hash-ref *opcodes-by-byte* b))
      (error "lookup-push-opcode: Push must have a fixed size")))
      
(: ethi->opcode (-> (U eth-asm eth-push label) opcode))
(define (ethi->opcode ethi)
  (cond ((eth-asm? ethi) (lookup-opcode (eth-asm-name ethi)))
        ((eth-push? ethi) (lookup-push-opcode ethi))
        ((label? ethi) (lookup-opcode 'JUMPDEST))
        (else (error "ethi->opcode: Unknown case"))))

(: push-true-size (-> eth-push Byte))
(define (push-true-size push)
  (if (equal? (eth-push-size push) 'shrink)
      (let ([ val (eth-push-value push) ])
        (cond ((label? val) assumed-label-size)
              ((integer? val) (integer-bytes val))
              (else (error "push-true-size: Unexpected case" val))))
      (eth-push-size push)))

(: integer-bytes (-> Integer Byte))
(define (integer-bytes n)
  (cond ((< n 256)        1)
        ((< n 65536)      2)
        ((< n 16777216)   3)
        ((< n 4294967296) 4)
        (else            32)))
