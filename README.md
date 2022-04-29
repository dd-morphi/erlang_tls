erlang_tls
=====

Simple code to test connecting with tls

Generate certificate
-----

self signed for server
----

    $ openssl req -newkey rsa:2048 -keyout server_key.pem -x509 -days 365 -out server_certificate.pem -outform PEM -subj /CN=$(hostname)/O=server/ -nodes

self signed for client
----

    $ openssl req -newkey rsa:2048 -keyout client_key.pem -x509 -days 365 -out client_certificate.pem -outform PEM -subj /CN=$(hostname)/O=client/ -nodes



Build
-----

    $ rebar3 compile

Execute
-----

Server accepts anyone
----

    $ rebar3 shell
    > server:any().

    $ rebar3 shell
    > client:example_simple().

Server accepts only with certificates
----

    $ rebar3 shell
    > server:require_user_certificates().

    $ rebar3 shell
    > client:example_certificates().
