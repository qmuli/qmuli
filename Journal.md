2018-08-12 
----------

Having success with converting the codebase to use Eff. 
Converted enough stuff to compile and run the simple-s3-copy example, but it seems to crash when executed on Lambda with
```
HttpExceptionRequest Request {
host = "s3.amazonaws.com"
port = 443
secure = True
requestHeaders = [("Host","s3.amazonaws.com"),("X-Amz-Date","20180812T204840Z"),("X-Amz-Content-SHA256","e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),("X-Amz-Security-Token","FQoGZXIvYXdzEC4aDGW8AsTTHxGa2wMB7iLtAaGLdXw8vwhjqmKyZvHk6It3PJyl1YLyLkb4mqjnb+EneIZeIHTf1L+ka6drNV/ANpRfZAOfH+QYCdPRLwZtnybdkBOeHEKI60Ev//CfQmql0AswgTBrla/B5yU7slq77EDeGgroEJGnoUInyJIAhnlmDpmVndCUCH6WiegJPc67ciEN/Qhn++IZ8w5XN9Zq8kqpui6la7DWXhA/8tPRHeByP58l4oQUZdx+GxEiuZs2+AJiylmwkhGYFmBSJvKfNGvlRhtT5nE5OT80zc1ze3u3+JowFPcOGZcoIU1EYxqmiOHV7vqZ5DxOSrplxyj/rsLbBQ=="),("Authorization","<REDACTED>")]
path = "/efftest.incoming/best.jpg"
queryString = ""
method = "GET"
proxy = Nothing
rawBody = False
redirectCount = 0
responseTimeout = ResponseTimeoutMicro 70000000
requestVersion = HTTP/1.1
}
ConnectionClosed
```

6:39pm
Looks like the above problem happened because I ran `runAWST` and `runResourceT` before extracting the body using `sinkBody`


