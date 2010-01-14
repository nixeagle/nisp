;;;; nisp-hello.lisp
;;; Copyright (c) 2010 Nixeagle
;;; Released under gplv3 or later
;;; Word list taken from file HELLO in GNU Emacs 23.1.91.1
(in-package :nisp-system)
(defpackage #:nisp.hello
  (:use :cl)
  (:export #:hello))

(in-package :nisp.hello)

(defvar +hello-words+ 
  '("ሠላም" "مكيلع مالّسلا" "নমস্কার" "⠓⠑⠇⠇⠕" "မင္‍ဂလာပာ" "printf (\"Hello, world!\n\");"
    "Dobrý den" "Hej" "Goddag" "Halløj" "Hallo" "Dag" "Hello" 
    "Saluton (Eĥoŝanĝo ĉiuĵaŭde)" "Tere päevast" "Tere õhtust"
    "Hei" "Hyvää päivää" "Bonjour" "Salut" "გამარჯობა" 
    "Guten Tag" "Grüß Gott" "Γειά σας" "નમસ્તે" "שלום" "Szép jó napot!"
    "नमस्ते" "नमस्कार ।" "Ciao" "Buon giorno" "System.out.println(\"Sugeng siang!\");"
    "ನಮಸ್ಕಾರ" "ជំរាបសួរ" "ສະບາຍດີ" "ຂໍໃຫ້ໂຊກດີ" "നമസ്കാരം" "Bonġu" "Saħħa"
    "∀ p ∈ world • hello p  □" "Hallo" "Dag" "Hei" "God dag" "ଶୁଣିବେ"
    "Dzień dobry!" "Cześć!" "Здра́вствуйте!" "ආයුබෝවන්" "Dobrý deň" "Pozdravljeni!"
    "¡Hola!" "Hej" "Goddag" "Hallå" "வணக்கம்" "నమస్కారం" "สวัสดีครับ" "สวัสดีค่ะ"
    "བཀྲ་ཤིས་བདེ་ལེགས༎" "ሰላማት" "Merhaba" "Вітаю" "Chào bạn" "こんにちは" "ｺﾝﾆﾁﾊ"
    "你好" "早晨, 你好" "안녕하세요" "안녕하십니까")
  "List of \"hello\" in many languages.")

(defun hello (&optional (word-list +hello-words+))
  "Return a random string saying hello from WORD-LIST."
  (alexandria:random-elt word-list))