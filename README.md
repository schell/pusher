Pusher
======
Scotty proxy to S3.

For generating a self signed certificate to run this server on TLS,
@see https://github.com/yesodweb/wai/tree/master/warp-tls

configuration
-------------
You can supply pusher with a config file at startup `pusher myconfig.cfg` with
the following variables:

    port = 31000
    users-file = "/some/initial/users.txt"
    log-file = "log.txt"
    tmp-dir = "/var/tmp/blah"
    server-key = "key.pem"
    server-crt = "certificate.pem"

Future
------
- [ ] cloudfront cache invalidation
- [ ] schedule s3 copies
- [x] upload tar.gz and expand at s3
