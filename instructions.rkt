#lang typed/racket

(require "types.rkt")
(require "opcodes.rkt")

(require "typed/binaryio.rkt")

(provide (all-defined-out))

(define assumed-label-size 2)

(: expand-instruction (-> EthInstructionQ EthInstruction))
(define (expand-instruction x)
  (match x
    [`(push  'shrink         ,val) (eth-push 'shrink           (cast val EthWord))]
    [`(push  ,(? byte? size) ,val) (eth-push  (cast size Byte) (cast val EthWord))]
    [`(op    (quote ,x))           (eth-op    (cast x Symbol))]
    [`(byte  ,(? byte? val))       (eth-bytes (bytes (cast val Byte)))]
    [`(bytes ,(? exact-integer? size)
             ,(? exact-integer? val))
     (eth-bytes (integer->bytes val size #f))]
    [`(label (quote ,(? symbol? name))) (label-definition (cast name Symbol) 0  #f)]
    [`(label (quote ,(? symbol? name))
             ,(? exact-integer? os))    (label-definition (cast name Symbol) os #f)]
    [_ (error "expand-instruction: Unknown syntax" x)]))


(: shrink-instruction (-> EthInstruction EthInstructionQ))
(define (shrink-instruction i)
  (match i
    [(struct eth-push ('shrink value))           `(push 'shrink ,value)]
    [(struct eth-push (size value))              `(push ,size ,value)]
    [(struct eth-op  (sym))                     `(op (quote ,sym))]
    [(struct eth-bytes (bs)) (match (bytes-length bs)
                                   [ 0           `(byte ,(first (bytes->list bs)))]
                                   [ n           `(bytes ,(bytes->integer bs #f))])]
    [(struct label-definition (name 0  #f))      `(label (quote ,name))]
    [(struct label-definition (name os #f))      `(label (quote ,name) ,os)]
    [(struct label-definition (name os virtual)) `(label (quote ,name) ,os ,virtual)]
    [_ (error "shrink-instruction: Unknown syntax" i)]))

(: ethi-extra-size (-> EthInstruction Byte))
(define (ethi-extra-size ethi)
  (if (eth-bytes? ethi)
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
      
(: ethi->opcode (-> (U eth-op eth-push label) opcode))
(define (ethi->opcode ethi)
  (cond ((eth-op? ethi) (lookup-opcode (eth-op-name ethi)))
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
