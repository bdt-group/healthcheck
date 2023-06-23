healthcheck
=====

Library for all-in-one infrastructure features for your application.
The library provides an interface for setting and getting the state.

Features
--------

* Own HTTP server with customizable healthcheck
* Prometeus metrics endpoint thru `meter` integration

Usage
-----
Once deployed, the library API will be available at http://127.0.0.1:8080

Setting the state after application launch and the client will receive a response with a 200 status code.
```
ok = healthcheck:set_state(200, <<"ok">>)
```
Checking the status of a service.
```
curl -v http://127.0.0.1:8080/healthcheck
```

Can pass any status code value to the set_state function, which is required by the business logic of the application.

Build
-----

    $ rebar3 compile
