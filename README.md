### Barebones Erlang tcp chat server using OTP

###### Usage
* Build it with rebar or compile manually
* Run `application:load(chatserver)` and `application:start(chatserver)` in an erl shell
* Connect using `telnet <host, default=localhost> <port, default=5555>`

The port number can be changed in the `chatserver.app.src` file

###### Todo
- ~~Write client side connectors~~
- ~~Add a supervision tree~~
- ~~Add appfiles~~
- ~~Rebar~~
- ~~Comment~~
