(define gui #f)
(define gps-lat-label #f)
(define gps-lng-label #f)
(define status-label #f)
(define gps:host "server-4ld3.onrender.com")
(define gps:endpoint "/upload")
(define buf (make-u8vector 1024))


;;validation
(define (valid-gps-coords? lat lng)
  (and lat lng
       (number? lat)
       (number? lng)
       (not (eq? lat #f))
       (not (eq? lng #f))
       (> lat 0)
       (> lng 0)
       ))


(define (send-button g w t x y)
  (send-gps-data))

;; Update the GPS display labels
(define (update-gps-display)
  (let ((lat (gps-latitude))
        (lng (gps-longitude)))
    (glgui-widget-set! gui gps-lat-label 'label
      (string-append "Lat: " (float->choppedstring lat 8)))
    (glgui-widget-set! gui gps-lng-label 'label
      (string-append "Lng: " (float->choppedstring lng 8)))
  	))

(define (post-json host endpoint jsonstr)
  (let* ((request (string-append
                   "POST " endpoint " HTTP/1.0\r\n"
                   "Host: " host "\r\n"
                   "Content-Type: application/json\r\n"
                   "Content-Length: " (number->string (string-length jsonstr)) "\r\n\r\n"
                   jsonstr)))
    (if (fx= (httpsclient-open host 443) 1)
        (begin
          (httpsclient-send (string->u8vector request))
          (let loop ((n (httpsclient-recv buf)) (output (u8vector)))
            (if (fx<= n 0)
                (begin (httpsclient-close)
                       (u8vector->string output))
                (loop (httpsclient-recv buf)
                      (u8vector-append output (subu8vector buf 0 n)))))
        )
        (begin
          (glgui-widget-set! gui status-label 'label "Status: Connection Failed!")
          #f))))

(define (send-gps-data)
  (let* ((lat (gps-latitude))
         (lng (gps-longitude)))
    (if (valid-gps-coords? lat lng)
        (let* ((lat-str (number->string lat))
               (lng-str (number->string lng))
               (json (string-append
                      "{\"latitude\": \"" lat-str "\", "
                      "\"longitude\": \"" lng-str "\"}"))
               (host gps:host)
               (endpoint gps:endpoint)
               (response (post-json host endpoint json)))
          (if response
              (glgui-widget-set! gui status-label 'label "Status: Sent!")
              (glgui-widget-set! gui status-label 'label "Status: Failed!")))
        ;; else show invalid coordinates
        (glgui-widget-set! gui status-label 'label "Status: Invalid GPS Data!"))))


(main
  ;; INIT
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))

    ;; Title
    (glgui-label gui 150 (- (glgui-height-get) 30) 200 30 "GPS" ascii_18.fnt White)

    ;; Exit Button
    (glgui-button-string gui 85 180 150 30 "Exit" ascii_18.fnt
      (lambda (x y) (force-terminate)))

    ;; Send Data Button
      (glgui-button-string gui 85 230 150 30 "Send Data" ascii_18.fnt send-button)

    ;; GPS Labels
    (set! gps-lat-label (glgui-label gui 100 330 300 30 "Lat:" ascii_18.fnt White))
    (set! gps-lng-label (glgui-label gui 100 300 300 30 "Lng:" ascii_18.fnt White))

    ;; Status Label
    (set! status-label (glgui-label gui 100 270 300 30 "Status: Ready!" ascii_18.fnt White))

    (gps-enable))

  ;; EVENT
  (lambda (t x y)
    (if (= t EVENT_KEYPRESS)
        (if (= x EVENT_KEYESCAPE) (terminate)))
    (update-gps-display)
    (glgui-event gui t x y))

  ;; TERMINATE
  (lambda ()
    (gps-disable))

  ;; SUSPEND
  (lambda ()
    (glgui-suspend)
    (gps-disable))

  ;; RESUME
  (lambda ()
    (glgui-resume)
    (gps-enable)
    (update-gps-display)))
