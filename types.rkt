#lang typed/racket

(provide (all-defined-out))

(struct opcode ([ byte : Byte ] [ name : Symbol ] [ num-reads : Integer ] [ num-writes : Integer ]) #:transparent)

(struct label ([ name : Symbol ]) #:transparent)
(struct label-definition label ([ offset : Integer ] [ virtual : Boolean ]) #:transparent)
(struct eth-asm     ([ name : Symbol ]) #:transparent)
(struct eth-push    ([ size : (U 'shrink Byte) ] [ value : (U Integer Symbol label) ]) #:transparent)
(struct eth-unknown ([ byte : Byte ]) #:transparent)
(define-type EthInstruction     (U eth-asm eth-push eth-unknown label-definition))
(define-type EthInstructions (Listof EthInstruction))

(define-type SymbolTable (Mutable-HashTable Symbol Integer))
(define-type ReverseSymbolTable (Mutable-HashTable Integer Symbol))
