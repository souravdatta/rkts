#lang typed/racket

(require typed/openssl)
(require typed/db)

(require/typed db
               (postgresql-connect (->* (#:user String
                                         #:database String)
                                        (#:allow-cleartext-password? Boolean
                                         #:debug? Any
                                         #:notice-handler (U 'output 'error Output-Port (-> String String Any))
                                         #:notification-handler (U 'output 'error Output-Port (-> String Any) (-> String String Any))
                                         #:server String
                                         #:port Integer
                                         #:socket (U Path-String 'guess False)
                                         #:password (U String False)
                                         #:ssl (U 'yes 'optional 'no)
                                         #:ssl-context SSL-Client-Context)
                                        Connection))
               (postgresql-guess-socket-path (-> Path-String)))
                                        

(provide postgresql-connect postgresql-guess-socket-path)
