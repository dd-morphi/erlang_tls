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

Server with stateless session tickets
----

    $ rebar3 shell
    > server:stateless_session_tickets().
    > server:accept().

    $ rebar3 shell
    > client:example_session_tickets.
    > client:reconnect().

You should be able to see less messages in second connect.
    <<< TLS 1.3 Handshake, ClientHello
    >>> TLS 1.3 Handshake, ServerHello
    >>> Handshake, EncryptedExtensions
    >>> Handshake, CertificateRequest
    >>> Handshake, Certificate
    >>> Handshake, CertificateVerify
    >>> Handshake, Finished
    <<< Handshake, Certificate
    <<< Handshake, CertificateVerify
    <<< Handshake, Finished
    >>> Post-Handshake, NewSessionTicket
    >>> Post-Handshake, NewSessionTicket
    >>> Post-Handshake, NewSessionTicket

    <<< TLS 1.3 Handshake, ClientHello
    >>> TLS 1.3 Handshake, ServerHello
    >>> Handshake, EncryptedExtensions
    >>> Handshake, Finished
    <<< Handshake, Finished
    >>> Post-Handshake, NewSessionTicket
    >>> Post-Handshake, NewSessionTicket
    >>> Post-Handshake, NewSessionTicket