#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))

(: *opcode-table* (Listof opcode))
(define *opcode-table*
  ; Byte Symbol StackReads StackWrites
  `(,(opcode #x00 'STOP       0 0 )
    ,(opcode #x01 'ADD        2 1 )
    ,(opcode #x02 'MUL        2 1)
    ,(opcode #x03 'SUB        2 1)
    ,(opcode #x04 'DIV        2 1)
    ,(opcode #x05 'SDIV       2 1)
    ,(opcode #x06 'MOD        2 1 )
    ,(opcode #x07 'SMOD       2 1)
    ,(opcode #x08 'ADDMOD     3 1)
    ,(opcode #x09 'MULMOD     2 1)
    ,(opcode #x0a 'EXP        2 1)
    ,(opcode #x0b 'SIGNEXTEND 2 1)

    ,(opcode #x10 'LT         2 1)
    ,(opcode #x11 'GT         2 1)
    ,(opcode #x12 'SLT        2 1)
    ,(opcode #x13 'LGT        2 1)
    ,(opcode #x14 'EQ         2 1)
    ,(opcode #x15 'ISZERO     1 1)
    ,(opcode #x16 'AND        2 1)
    ,(opcode #x17 'OR         2 1)
    ,(opcode #x18 'XOR        2 1)
    ,(opcode #x19 'NOT        1 1)
    ,(opcode #x1a 'BYTE       2 1)
    
    ,(opcode #x20 'SHA3       2 1)
    
    ,(opcode #x30 'ADDRESS      0 1)
    ,(opcode #x31 'BALANCE      1 1)
    ,(opcode #x32 'ORIGIN       0 1)
    ,(opcode #x33 'CALLER       0 1)
    ,(opcode #x34 'CALLVALUE    0 1)
    ,(opcode #x35 'CALLDATALOAD 1 1)
    ,(opcode #x36 'CALLDATASIZE 0 1)
    ,(opcode #x37 'CALLDATACOPY 3 1)
    ,(opcode #x38 'CODESIZE     0 1)
    ,(opcode #x39 'CODECOPY     3 0)
    ,(opcode #x3a 'GASPRICE     0 1)
    ,(opcode #x3b 'EXTCODESIZE  1 1)
    ,(opcode #x3c 'EXTCODECOPY  4 0)
    
    ,(opcode #x40 'BLOCKHASH    1 1)
    ,(opcode #x41 'COINBASE     0 1)
    ,(opcode #x42 'TIMESTAMP    0 1)
    ,(opcode #x43 'NUMBER       0 1)
    ,(opcode #x44 'DIFFICULTY   0 1)
    ,(opcode #x45 'GASLIMIT     0 1)
    
    ,(opcode #x50 'POP          1 0)
    ,(opcode #x51 'MLOAD        1 1)
    ,(opcode #x52 'MSTORE       2 0)
    ,(opcode #x53 'MSTORE8      2 0)
    ,(opcode #x54 'SLOAD        1 1)
    ,(opcode #x55 'SSTORE       2 0)
    ,(opcode #x56 'JUMP         1 0)
    ,(opcode #x57 'JUMPI        2 0)
    ,(opcode #x58 'PC           0 1)
    ,(opcode #x59 'MSIZE        0 1)
    ,(opcode #x5a 'GAS          0 1)
    ,(opcode #x5b 'JUMPDEST     0 0)
    
    ,(opcode #x60 'PUSH1        0 1)
    ,(opcode #x61 'PUSH2        0 1)
    ,(opcode #x62 'PUSH3        0 1)
    ,(opcode #x63 'PUSH4        0 1)
    ,(opcode #x64 'PUSH5        0 1)
    ,(opcode #x65 'PUSH6        0 1)
    ,(opcode #x66 'PUSH7        0 1)
    ,(opcode #x67 'PUSH8        0 1)
    ,(opcode #x68 'PUSH9        0 1)
    ,(opcode #x69 'PUSH10       0 1)
    ,(opcode #x6a 'PUSH11       0 1)
    ,(opcode #x6b 'PUSH12       0 1)
    ,(opcode #x6c 'PUSH13       0 1)
    ,(opcode #x6d 'PUSH14       0 1)
    ,(opcode #x6e 'PUSH15       0 1)
    ,(opcode #x6f 'PUSH16       0 1)
    ,(opcode #x70 'PUSH17       0 1)
    ,(opcode #x71 'PUSH18       0 1)
    ,(opcode #x72 'PUSH19       0 1)
    ,(opcode #x73 'PUSH20       0 1)
    ,(opcode #x74 'PUSH21       0 1)
    ,(opcode #x75 'PUSH22       0 1)
    ,(opcode #x76 'PUSH23       0 1)
    ,(opcode #x77 'PUSH24       0 1)
    ,(opcode #x78 'PUSH25       0 1)
    ,(opcode #x79 'PUSH26       0 1)
    ,(opcode #x7a 'PUSH27       0 1)
    ,(opcode #x7b 'PUSH28       0 1)
    ,(opcode #x7c 'PUSH29       0 1)
    ,(opcode #x7d 'PUSH30       0 1)
    ,(opcode #x7e 'PUSH31       0 1)
    ,(opcode #x7f 'PUSH32       0 1)
    
    ,(opcode #x80 'DUP1         1 2)
    ,(opcode #x81 'DUP2         2 3)
    ,(opcode #x82 'DUP3         3 4)
    ,(opcode #x83 'DUP4         4 5)
    ,(opcode #x84 'DUP5         5 6)
    ,(opcode #x85 'DUP6         6 7)
    ,(opcode #x86 'DUP7         7 8)
    ,(opcode #x87 'DUP8         8 9)
    ,(opcode #x88 'DUP9         9 10)
    ,(opcode #x89 'DUP10        10 11)
    ,(opcode #x8a 'DUP11        11 12)
    ,(opcode #x8b 'DUP12        12 13)
    ,(opcode #x8c 'DUP13        13 14)
    ,(opcode #x8d 'DUP14        14 15)
    ,(opcode #x8e 'DUP15        15 16)
    ,(opcode #x8f 'DUP16        16 17)
    
    ,(opcode #x90 'SWAP1        2 2)
    ,(opcode #x91 'SWAP2        3 3)
    ,(opcode #x92 'SWAP3        4 4)
    ,(opcode #x93 'SWAP4        5 5)
    ,(opcode #x94 'SWAP5        6 6)
    ,(opcode #x95 'SWAP6        7 7)
    ,(opcode #x96 'SWAP7        8 8)
    ,(opcode #x97 'SWAP8        9 9)
    ,(opcode #x98 'SWAP9        10 10)
    ,(opcode #x99 'SWAP10       11 11)
    ,(opcode #x9a 'SWAP11       12 12)
    ,(opcode #x9b 'SWAP12       13 13)
    ,(opcode #x9c 'SWAP13       14 14)
    ,(opcode #x9d 'SWAP14       15 15)
    ,(opcode #x9e 'SWAP15       16 16)
    ,(opcode #x9f 'SWAP16       17 17)
    
    ,(opcode #xa0 'LOG0 2 0)
    ,(opcode #xa1 'LOG1 3 0)
    ,(opcode #xa2 'LOG2 4 0)
    ,(opcode #xa3 'LOG3 5 0)
    ,(opcode #xa4 'LOG3 6 0)
    
    ,(opcode #xf0 'CREATE       3 1)
    ,(opcode #xf1 'CALL         7 1)
    ,(opcode #xf2 'CALLCODE     7 1)
    ,(opcode #xf3 'RETURN       2 0)
    ,(opcode #xf4 'DELEGATECALL 6 1)
    
    ,(opcode #xfd 'REVERT       0 0)
    ,(opcode #xfe 'INVALID      0 0)
    ,(opcode #xff 'SUICIDE      1 0)
    ))

(: *opcodes-by-sym* (Immutable-HashTable Symbol opcode))
(define *opcodes-by-sym*
  (make-immutable-hash (map (λ ([op : opcode]) : (Pairof Symbol opcode)
                               ((inst cons Symbol opcode) (opcode-name op) op))
                            *opcode-table*)))

(: *opcodes-by-byte* (Immutable-HashTable Byte opcode))
(define *opcodes-by-byte*
  (make-immutable-hash (map (λ ([op : opcode]) : (Pairof Byte opcode)
                               ((inst cons Byte opcode) (opcode-byte op) op)) *opcode-table*)))

(: lookup-opcode (-> Symbol opcode))
(define (lookup-opcode sym)
  (hash-ref *opcodes-by-sym* sym))

(: push-op? (-> opcode Boolean))
(define (push-op? op)
  (and
   (opcode? op)
   (>= (opcode-byte op) #x60)
   (<= (opcode-byte op) #x7f)))

(: op-extra-size (-> opcode Byte))
(define (op-extra-size op)
  (if (push-op? op)
      (cast (- (opcode-byte op) #x5f) Byte)
      0))