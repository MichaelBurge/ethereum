#lang typed/racket

(provide (all-defined-out))

(struct opcode ([ byte : Byte ] [ name : Symbol ] [ num-reads : Integer ] [ num-writes : Integer ]) #:transparent)

(struct label ([ name : Symbol ]) #:transparent)
(struct label-definition label ([ offset : Integer ] [ virtual : Boolean ]) #:transparent)
(struct eth-op    ([ name : Symbol ]) #:transparent)
(struct eth-push  ([ size : (U 'shrink Byte) ] [ value : Integer ]) #:transparent)
(struct eth-bytes ([ bytes : Bytes ]) #:transparent)
(define-type EthInstruction     (U eth-op eth-push eth-bytes label-definition))
(define-type EthInstructions (Listof EthInstruction))
(define-type EthInstructionQ Any)
(define-type EthWord Integer)

(define-type SymbolTable (Mutable-HashTable Symbol Integer))
(: make-symbol-table (-> SymbolTable))
(define (make-symbol-table) (make-hash))
(define-type ReverseSymbolTable (Mutable-HashTable Integer Symbol))
