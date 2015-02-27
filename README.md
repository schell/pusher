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
    static-dir = "/some/place/to/put/static/files/likeanimage.png"
    sign-in-cookie-life = 3600 #in seconds

functions/routes
----------------

### user stuff

    GET /users - list users 
    GET /users.txt - spit out the users.txt 
    GET /user - show create a new user form
    POST /user - create a new user 
     
### uploading 

    GET /upload - show upload file form 
    POST /upload - upload a file 
    GET /upload-zip - show upload tarballed dir form
    POST /upload-zip - upload a tarballed dir 

### copying existing s3 things

    GET /copy - copy a single file to another location (supports across buckets)
    GET /copy-folder - copy a directory and contents (supports across buckets) 

### catchall with static dir lookup or 404

    GET {static-dir}/* 

### misc

    GET / - say hello 
    GET /log - display the log file
    GET /auth-check-lvl - check your authorization
    GET /auth-check-bucket - check your auth on a given bucket 
    GET /list - list the contents of a bucket 

Future
------
- [ ] cloudfront cache invalidation
- [ ] schedule s3 copies
- [x] upload tar.gz and expand at s3
