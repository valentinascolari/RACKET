;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Loja_Informatica_ValentinaScolari) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct material (tipo preco qtdd))
; Um elemento material do conjunto Material é uma estrutura
;  (make-material tipo preco qtdd)
;  tipo : TipoMat
;  preco: Numero é o preco do material
;  qtdd : Numero é a quantidade em estoque

(define-struct disco (ssd? capMax))
; Um elemento disco do conjunto Disco é uma estrutura
;  (make-disco ssd? capMax) onde:
;  ssd? : Boolean indica se o disco é um SSD
;  capMax: Numero indica a capacidade do disco (GB)

(define-struct mouse (tresBotoes?))
; Um elemento mouse do conjunto Mouse é uma estrutura
;  (make-mouse tresBotoes?) onde
;  tresBotoes? : Boolean indica se o mouse possui três botões

(define-struct data (dia mes ano))
; Um elemento data do conjunto Data é uma estrutura
; (make-data dia mes ano) onde:
; dia : Numero
; mes : Numero
; ano : Numero

(define-struct toner (data marca))
; Um elemento toner do conjunto Toner é uma estrutura
;  (make-toner uma-data marca) onde:
;  uma-data : Data é a data de validade do toner
;  marca : String é a marca do toner

; primeiro, discos:
(define disco1 (make-disco #t 512))
(define disco2 (make-disco #false 1024))

; depois, mouses:
(define mouse1 (make-mouse #t))
(define mouse2 (make-mouse #f))

; depois, toners:
(define toner1 (make-toner (make-data 31 12 2014) "HP"))
(define toner2 (make-toner (make-data 1 1 2023)"Canon"))

; e, finalmente, materiais:
(define mat1 (make-material disco1 200 20))
(define mat2 (make-material toner1 150 5))

;Função preço
(define (preco mat)
  (material-preco mat))

;; Função custo
(define (custo mat qtdd)
  (* (preco mat) qtdd))

;; Função em-estoque?
(define (em-estoque? mat qtdd)
  (cond [(>= (material-qtdd mat) qtdd) #t]
        [else #f]))

;; Função retorna-ppdd
(define (retorna-ppdd mat)
  (cond [(disco? (material-tipo mat)) (cond [(disco-ssd? (material-tipo mat)) "É SSD"]
                                             [else "Não é SSD"])]
        [(mouse? (material-tipo mat)) (cond [(mouse-tresBotoes? (material-tipo mat)) "Tem 3 botões"]
                                             [else "Não tem 3 botões"])]
        [(toner? (material-tipo mat)) (toner-data (material-tipo mat))]))

;; Função capacidade
(define (capacidade mat)
  (cond [(and (disco? (material-tipo mat)) (disco-ssd? (material-tipo mat)))
         (disco-capMax (material-tipo mat))]
        [(disco? (material-tipo mat))
         "Não se trata de SSD"]))
       
;; Função atualiza-estoque
(define (atualiza-estoque mat op qtdd)
  (cond [(and (equal? op "a") (>= qtdd 0))
         (+ (material-qtdd mat) qtdd)]
        [(and (equal? op "c") (>= qtdd 0) (>= (material-qtdd mat) qtdd))
         (- (material-qtdd mat) qtdd)]))
        
;; Função tem-marca?
(define (tem-marca? mat marca)
  (cond [(toner? (material-tipo mat)) (equal? marca (toner-marca (material-tipo mat)))]
        [else #f]))

;; Testes
(check-expect (preco mat1) 200)
(check-expect (custo mat1 5) 1000)
(check-expect (em-estoque? mat1 15) #t)
(check-expect (retorna-ppdd mat1) "É SSD")
(check-expect (capacidade mat1) 512)
(check-expect (atualiza-estoque mat1 "a" 10) 30)
(check-expect (tem-marca? mat2 "HP") #t)



