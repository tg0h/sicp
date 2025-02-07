#lang sicp

(define (stream-map proc . argstreams) (if (⟨??⟩ (car argstreams))
            the-empty-stream
(⟨??⟩
(apply proc (map ⟨??⟩ argstreams)) (apply stream-map
(cons proc (map ⟨??⟩ argstreams))))))
