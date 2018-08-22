#lang racket

(require 2htdp/image)

;; A vecimg is an image represented internally as a vector of colors:
;; vec : vector of color?
;; width : number
(define-struct vecimg (vec width))


(define (bitmap->vecimg x)
  (make-vecimg (list->vector (image->color-list x))
               (image-width x)))
 
(define (vecimg->image v)
  (color-list->bitmap (vector->list (vecimg-vec v))
                      (vecimg-width v)
                      (vecimg-height v)))
 
(define (vecimg-height v)
  (quotient (vector-length (vecimg-vec v))
            (vecimg-width v)))


#|  Exercise 1

Design (vecimg-ref x y) and (vecimg-set! x y color) for two-dimensional pixel references and pixel updates, respectively.

vecimg-ref returns a color, or an error if the x/y coordinate is out of bounds. vecimg-set! returns a void value.  |#





#|

Exercise 2

Design a procedure vecimg-copy that takes an image and returns a new image which is an exact copy of the given image.

Verify that the result of your copy function, if mutated, does not affect the original image.

(define butterfly1 ...)
(define butterfly2 (vecimg-copy ))
(vecimg-set! butterfly-too 10 40 yellow)
(color-equal? (vecimg-ref butterfly 10 40)
              (vecimg-ref butterfly-too 10 40))
  -> #f

|#



#|

Exercise 3

Design a procedure image-transpose that takes an image and returns the
image created by interchanging the rows and columns of the given image.

That is, the color at location (i ,j) of the resulting image is
 pulled from location (j ,i) of the given image.

|#

#|

2 Counting Literature

Exercise 1 Develop a data and structure definition for storing a Frequency,
  which combines a String and a Number, and
  representing that many uses of the specified string.

Exercise 2 Design a function count-word which consumes a ListOfFrequency
  and a String and adds 1 to the frequency for that string,
  producing a new ListOfFrequency. If there is no Frequency
  for that string, the resulting ListOfFrequency should have
  a new Frequency with that string and initialized with number 1.

Exercise 3 Download the text of Hamlet into the same
  folder as your file where you are working on this exercise.

Then use the 2htdp/batch-io library to read and create a list of words from the file.

Then compute the frequencies of all of the words in the file.

Hint: You could start by generalizing count-word, like count-all-words
 which takes a ListOfString and produces a ListOfFrequency with the appropriate frequencies.

|#