<img src="https://arweave.net/dOpRkKrNNQ4HHebxZlPCo0BWfrjwJ-CEBQs2EPgrwbg" />

This repo contains an extremely simple implementation of an AO Scheduling Unit written in Erlang.

It is not meant to be used as a reference implementation or to be used in a production setting. It is simply a proof of concept to experiment with the speeds that a well-tuned SU implementation should be able to achieve.

This implementation reaches speeds of >450 TPS of sustained scheduling throughput _per AO process_ on a single Macbook Pro M2 laptop:

```
% ab -n 100000 -c 100 -T application/x-www-form-urlencoded -p item.bin http://127.0.0.1:8081/
This is ApacheBench, Version 2.3 <$Revision: 1903618 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking 127.0.0.1 (be patient)
Completed 10000 requests
Completed 20000 requests
Completed 30000 requests
Completed 40000 requests
Completed 50000 requests
Completed 60000 requests
Completed 70000 requests
Completed 80000 requests
Completed 90000 requests
Completed 100000 requests
Finished 100000 requests


Server Software:        Cowboy
Server Hostname:        127.0.0.1
Server Port:            8081

Document Path:          /
Document Length:        80 bytes

Concurrency Level:      100
Time taken for tests:   220.968 seconds
Complete requests:      100000
Failed requests:        0
Total transferred:      22300000 bytes
Total body sent:        123700000
HTML transferred:       8000000 bytes
Requests per second:    452.55 [#/sec] (mean)
Time per request:       220.968 [ms] (mean)
Time per request:       2.210 [ms] (mean, across all concurrent requests)
Transfer rate:          98.55 [Kbytes/sec] received
                        546.69 kb/s sent
                        645.24 kb/s total

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    3  63.7      0    1934
Processing:    12  218  56.1    202     570
Waiting:       12  218  56.1    202     570
Total:         12  221  83.0    202    2104
```

## Building and running

In order to play with the SuperSU implementation, you will need to have Erlang/OTP 26 or later installed, as well as the rebar3 build tool.

Once you have the dependencies installed, you can build the SuperSU implementation with:

    $ rebar3 compile

You can then run the SuperSU server with:

    $ rebar3 shell
