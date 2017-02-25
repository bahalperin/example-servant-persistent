This is a fork of an example project for how to set up a web-server with
[servant-server](http://haskell-servant.readthedocs.io/) that uses
[persistent](https://www.stackage.org/package/persistent) for saving data to a
database.

This version adds serving static files and sending an index.html file when a GET
request is made to the root path.

Also, markdown documentation for the API will be sent if a request is made to
an invalid route.

You can build and run the project with [stack](http://haskellstack.org/), e.g.:

``` bash
stack build
stack exec example-servant-persistent
```

Then you can query the server from a separate shell:

``` bash
curl -H 'Content-type: application/json' localhost:3000/user/add --data '{"name": "Alice", "age": 42}'
curl -H 'Content-type: application/json' localhost:3000/user/get/Alice
curl localhost:300
curl localhost:300/static/index.html
curl localhost:300/help
```
