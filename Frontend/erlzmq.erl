%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%
%% Copyright (c) 2011 Yurii Rashkovskii, Evax Software and Michael Truog
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(erlzmq).
%% @headerfile "erlzmq.hrl"
-include_lib("erlzmq.hrl").
-export([context/0,
         context/1,
         socket/2,
         bind/2,
         connect/2,
         send/2,
         send/3,
         sendmsg/2,
         sendmsg/3,
         recv/1,
         recv/2,
         recvmsg/1,
         recvmsg/2,
         setsockopt/3,
         getsockopt/2,
         close/1,
         close/2,
         term/1,
         term/2,
         version/0]).
-export_type([erlzmq_socket/0, erlzmq_context/0]).

%% @equiv context(1)
-spec context() -&gt;
    {ok, erlzmq_context()} |
    erlzmq_error().
context() -&gt;
    context(1).

%% @doc Create a new erlzmq context with the specified number of io threads.
%% &lt;br /&gt;
%% If the context can be created an 'ok' tuple containing an
%% {@type erlzmq_context()} handle to the created context is returned;
%% if not, it returns an 'error' tuple with an {@type erlzmq_type_error()}
%% describing the error.
%% &lt;br /&gt;
%% The context must be later cleaned up calling {@link erlzmq:term/1. term/1}
%% &lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq-init"&gt;zmq_init&lt;/a&gt;&lt;/i&gt;
%% @end
-spec context(Threads :: pos_integer()) -&gt;
    {ok, erlzmq_context()} |
    erlzmq_error().
context(Threads) when is_integer(Threads) -&gt;
    erlzmq_nif:context(Threads).


%% @doc Create a socket.
%% &lt;br /&gt;
%% This functions creates a socket of the given
%% {@link erlzmq_socket_type(). type}, optionally setting it to active mode,
%% and associates it with the given {@link erlzmq_context(). context}.
%% &lt;br /&gt;
%% If the socket can be created an 'ok' tuple containing a
%% {@type erlzmq_socket()} handle to the created socket is returned;
%% if not, it returns an {@type erlzmq_error()} describing the error.
%% &lt;br /&gt;
%% In line with Erlang's socket paradigm,  a socket can be either active or
%% passive. Passive sockets tend to have lower latency and have a higher
%% throughput for small message sizes. Active sockets on the contrary give
%% the highest throughput for messages above 32k. A benchmarking tool is
%% included in the source distribution.&lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_socket"&gt;zmq_socket&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec socket(Context :: erlzmq_context(),
             Type :: erlzmq_socket_type() |
                     list(erlzmq_socket_type() |
                          {active, boolean()} |
                          {active_pid, pid()})) -&gt;
                    {ok, erlzmq_socket()} |
                    erlzmq_error().
socket(Context, Type) when is_atom(Type) -&gt;
    socket(Context, [Type]);
socket(Context, [H | T]) when is_atom(H) -&gt;
    case T of
        [] -&gt;
            % active is false by default
            % (to avoid latency on small messages (messages &lt; 32KB))
            socket(Context, H, {active, false});
        [Active] -&gt;
            socket(Context, H, Active)
    end;
socket(Context, [H | [Type]]) when is_tuple(H) -&gt;
    socket(Context, Type, H).

-spec socket(Context :: erlzmq_context(),
             Type :: erlzmq_socket_type(),
             {active, boolean()} | {active_pid, pid()}) -&gt;
                    {ok, erlzmq_socket()} |
                    erlzmq_error().
socket(Context, Type, {active, true}) -&gt;
    true = (Type =/= pub) and (Type =/= push) and (Type =/= xpub),
    erlzmq_nif:socket(Context, socket_type(Type), 1, self());
socket(Context, Type, {active_pid, Pid})
    when is_pid(Pid), node(Pid) =:= node() -&gt;
    true = (Type =/= pub) and (Type =/= push) and (Type =/= xpub),
    erlzmq_nif:socket(Context, socket_type(Type), 1, Pid);
socket(Context, Type, {active, false}) -&gt;
    erlzmq_nif:socket(Context, socket_type(Type), 0, self()).


%% @doc Accept connections on a socket.
%% &lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_bind"&gt;zmq_bind&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec bind(Socket :: erlzmq_socket(),
           Endpoint :: erlzmq_endpoint()) -&gt;
    ok |
    erlzmq_error().
bind({I, Socket}, Endpoint)
    when is_integer(I), is_list(Endpoint) -&gt;
    erlzmq_nif:bind(Socket, Endpoint);
bind({I, Socket}, Endpoint)
    when is_integer(I), is_binary(Endpoint) -&gt;
    bind({I, Socket}, binary_to_list(Endpoint)).

%% @doc Connect a socket.
%% &lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_connect"&gt;zmq_connect&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec connect(Socket :: erlzmq_socket(),
              Endpoint :: erlzmq_endpoint()) -&gt;
    ok |
    erlzmq_error().
connect({I, Socket}, Endpoint)
    when is_integer(I), is_list(Endpoint) -&gt;
    erlzmq_nif:connect(Socket, Endpoint);
connect({I, Socket}, Endpoint)
    when is_integer(I), is_binary(Endpoint) -&gt;
    connect({I, Socket}, binary_to_list(Endpoint)).

%% @equiv send(Socket, Msg, [])
-spec send(erlzmq_socket(),
           Binary :: binary()) -&gt;
    ok |
    erlzmq_error().
send(Socket, Binary) when is_binary(Binary) -&gt;
    send(Socket, Binary, []).

%% @doc Send a message on a socket.
%% &lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_send"&gt;zmq_send&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec send(erlzmq_socket(),
           Binary :: binary(),
           Flags :: erlzmq_send_recv_flags()) -&gt;
    ok |
    erlzmq_error().
send({I, Socket}, Binary, Flags)
    when is_integer(I), is_binary(Binary), is_list(Flags) -&gt;
    case erlzmq_nif:send(Socket, Binary, sendrecv_flags(Flags)) of
        Ref when is_reference(Ref) -&gt;
            receive
                {Ref, ok} -&gt;
                    ok;
                {Ref, {error, _} = Error} -&gt;
                    Error
            after case  erlzmq_nif:getsockopt(Socket,?'ZMQ_SNDTIMEO') of
                       {ok, -1} -&gt;
                           infinity;
                       {ok, Else} -&gt;
                           Else
                   end -&gt;
                    {error, eagain}
            end;
        Result -&gt;
            Result
    end.

%% @equiv send(Socket, Msg, [])
%% @doc This function exists for zeromq api compatibility and doesn't
%% actually provide any different functionality then what you get with
%% the {@link erlzmq:send/2} function. In fact this function just
%% calls that function. So there is a slight bit of additional
%% overhead as well.
-spec sendmsg(erlzmq_socket(),
           Binary :: binary()) -&gt;
    ok |
    erlzmq_error().
sendmsg(Socket, Binary) when is_binary(Binary) -&gt;
    send(Socket, Binary, []).

%% @equiv send(Socket, Msg, Flags)
%% @doc This function exists for zeromq api compatibility and doesn't
%% actually provide any different functionality then what you get with
%% the {@link erlzmq:send/3} function. In fact this function just
%% calls that function. So there is a slight bit of additional
%% overhead as well.
-spec sendmsg(erlzmq_socket(),
           Binary :: binary(),
           Flags :: erlzmq_send_recv_flags()) -&gt;
    ok |
    erlzmq_error().
sendmsg(Socket, Binary, Flags) -&gt;
    send(Socket, Binary, Flags).


%% @equiv recv(Socket, 0)
-spec recv(Socket :: erlzmq_socket()) -&gt;
    {ok, erlzmq_data()} |
    erlzmq_error().
recv(Socket) -&gt;
    recv(Socket, []).

%% @doc Receive a message from a socket.
%% &lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_recv"&gt;zmq_recv&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec recv(Socket :: erlzmq_socket(),
           Flags :: erlzmq_send_recv_flags()) -&gt;
    {ok, erlzmq_data()} |
    erlzmq_error().
recv({I, Socket}, Flags)
    when is_integer(I), is_list(Flags) -&gt;
    case erlzmq_nif:recv(Socket, sendrecv_flags(Flags)) of
        Ref when is_reference(Ref) -&gt;
            receive
                {Ref, {error, _} = Error} -&gt;
                    Error;
                {Ref, Result} -&gt;
                    {ok, Result}
            after case erlzmq_nif:getsockopt(Socket,?'ZMQ_RCVTIMEO') of
                      {ok, -1} -&gt;
                          infinity;
                      {ok, Else} -&gt;
                          Else
                  end -&gt;
                    {error, eagain}
            end;
        Result -&gt;
            Result
    end.

%% @equiv recv(Socket, 0)
%% @doc This function exists for zeromq api compatibility and doesn't
%% actually provide any different functionality then what you get with
%% the {@link erlzmq:recv/3} function. In fact this function just
%% calls that function. So there is a slight bit of additional
%% overhead as well.
-spec recvmsg(Socket :: erlzmq_socket()) -&gt;
    {ok, erlzmq_data()} |
    erlzmq_error().
recvmsg(Socket) -&gt;
    recv(Socket, []).

%% @equiv recv(Socket, Flags)
%% @doc This function exists for zeromq api compatibility and doesn't
%% actually provide any different functionality then what you get with
%% the {@link erlzmq:recv/3} function. In fact this function just
%% calls that function. So there is a slight bit of additional
%% overhead as well.
-spec recvmsg(Socket :: erlzmq_socket(),
           Flags :: erlzmq_send_recv_flags()) -&gt;
    {ok, erlzmq_data()} |
    erlzmq_error().
recvmsg(Socket, Flags) -&gt;
    recv(Socket, Flags).

%% @doc Set an {@link erlzmq_sockopt(). option} associated with a socket.
%% &lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_setsockopt"&gt;zmq_setsockopt&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec setsockopt(erlzmq_socket(),
                 Name :: erlzmq_sockopt(),
                 erlzmq_sockopt_value() | binary()) -&gt;
    ok |
    erlzmq_error().
setsockopt(Socket, Name, Value) when is_list(Value) -&gt;
    setsockopt(Socket, Name, erlang:list_to_binary(Value));
setsockopt({I, Socket}, Name, Value) when is_integer(I), is_atom(Name) -&gt;
    erlzmq_nif:setsockopt(Socket, option_name(Name), Value).

%% @doc Get an {@link erlzmq_sockopt(). option} associated with a socket.
%% &lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_getsockopt"&gt;zmq_getsockopt&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec getsockopt(Socket :: erlzmq_socket(),
                 Name :: erlzmq_sockopt()) -&gt;
    {ok, erlzmq_sockopt_value()} |
    erlzmq_error().
getsockopt({I, Socket}, Name) when is_integer(I), is_atom(Name) -&gt;
    erlzmq_nif:getsockopt(Socket, option_name(Name)).

%% @equiv close(Socket, infinity)
-spec close(Socket :: erlzmq_socket()) -&gt;
    ok |
    erlzmq_error().
close(Socket) -&gt;
    close(Socket, infinity).

%% @doc Close the given socket.
%% &lt;br /&gt;
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_close"&gt;zmq_close&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec close(Socket :: erlzmq_socket(),
            Timeout :: timeout()) -&gt;
    ok |
    erlzmq_error().
close({I, Socket}, Timeout) when is_integer(I) -&gt;
    case erlzmq_nif:close(Socket) of
        Ref when is_reference(Ref) -&gt;
            receive
                {Ref, Result} -&gt;
                    Result
            after
                Timeout -&gt;
                    {error, {timeout, Ref}}
            end;
        Result -&gt;
            Result
    end.

%% @equiv term(Context, infinity)
-spec term(Context :: erlzmq_context()) -&gt;
    ok |
    erlzmq_error().
term(Context) -&gt;
    term(Context, infinity).

%% @doc Terminate the given context waiting up to Timeout ms.
%% &lt;br /&gt;
%% This function should be called after all sockets associated with
%% the given context have been closed.&lt;br /&gt;
%% If not it will block the given Timeout amount of time.
%% &lt;i&gt;For more information see
%% &lt;a href="http://api.zeromq.org/master:zmq_term"&gt;zmq_term&lt;/a&gt;.&lt;/i&gt;
%% @end
-spec term(Context :: erlzmq_context(),
           Timeout :: timeout()) -&gt;
    ok |
    erlzmq_error() |
    {error, {timeout, reference()}}.

term(Context, Timeout) -&gt;
    case erlzmq_nif:term(Context) of
        Ref when is_reference(Ref) -&gt;
            receive
                {Ref, Result} -&gt;
                    Result
            after
                Timeout -&gt;
                    {error, {timeout, Ref}}
            end;
        Result -&gt;
            Result
    end.

%% @doc Returns the 0MQ library version.
%% @end
-spec version() -&gt; {integer(), integer(), integer()}.

version() -&gt; erlzmq_nif:version().

%% Private

-spec socket_type(Type :: erlzmq_socket_type()) -&gt;
    integer().

socket_type(pair) -&gt;
    ?'ZMQ_PAIR';
socket_type(pub) -&gt;
    ?'ZMQ_PUB';
socket_type(sub) -&gt;
    ?'ZMQ_SUB';
socket_type(req) -&gt;
    ?'ZMQ_REQ';
socket_type(rep) -&gt;
    ?'ZMQ_REP';
socket_type(dealer) -&gt;
    ?'ZMQ_DEALER';
socket_type(xreq) -&gt;
    ?'ZMQ_XREQ';
socket_type(router) -&gt;
    ?'ZMQ_ROUTER';
socket_type(xrep) -&gt;
    ?'ZMQ_XREP';
socket_type(pull) -&gt;
    ?'ZMQ_PULL';
socket_type(push) -&gt;
    ?'ZMQ_PUSH';
socket_type(xpub) -&gt;
    ?'ZMQ_XPUB';
socket_type(xsub) -&gt;
    ?'ZMQ_XSUB'.

-spec sendrecv_flags(Flags :: erlzmq_send_recv_flags()) -&gt;
    integer().

sendrecv_flags([]) -&gt;
    0;
sendrecv_flags([dontwait|Rest]) -&gt;
    ?'ZMQ_DONTWAIT' bor sendrecv_flags(Rest);
sendrecv_flags([sndmore|Rest]) -&gt;
    ?'ZMQ_SNDMORE' bor sendrecv_flags(Rest).

-spec option_name(Name :: erlzmq_sockopt()) -&gt;
    integer().

option_name(affinity) -&gt;
    ?'ZMQ_AFFINITY';
option_name(identity) -&gt;
    ?'ZMQ_IDENTITY';
option_name(subscribe) -&gt;
    ?'ZMQ_SUBSCRIBE';
option_name(unsubscribe) -&gt;
    ?'ZMQ_UNSUBSCRIBE';
option_name(rate) -&gt;
    ?'ZMQ_RATE';
option_name(recovery_ivl) -&gt;
    ?'ZMQ_RECOVERY_IVL';
option_name(sndbuf) -&gt;
    ?'ZMQ_SNDBUF';
option_name(rcvbuf) -&gt;
    ?'ZMQ_RCVBUF';
option_name(rcvmore) -&gt;
    ?'ZMQ_RCVMORE';
option_name(fd) -&gt;
    ?'ZMQ_FD';
option_name(events) -&gt;
    ?'ZMQ_EVENTS';
option_name(linger) -&gt;
    ?'ZMQ_LINGER';
option_name(reconnect_ivl) -&gt;
    ?'ZMQ_RECONNECT_IVL';
option_name(backlog) -&gt;
    ?'ZMQ_BACKLOG';
option_name(reconnect_ivl_max) -&gt;
    ?'ZMQ_RECONNECT_IVL_MAX';
option_name(maxmsgsize) -&gt;
    ?'ZMQ_MAXMSGSIZE';
option_name(sndhwm) -&gt;
    ?'ZMQ_SNDHWM';
option_name(rcvhwm) -&gt;
    ?'ZMQ_RCVHWM';
option_name(multicast_hops) -&gt;
    ?'ZMQ_MULTICAST_HOPS';
option_name(rcvtimeo) -&gt;
    ?'ZMQ_RCVTIMEO';
option_name(sndtimeo) -&gt;
    ?'ZMQ_SNDTIMEO';
option_name(ipv4only) -&gt;
    ?'ZMQ_IPV4ONLY'.
</pre></body></html>Ztext/plainUUTF-8_Fhttps://raw.githubusercontent.com/zeromq/erlzmq2/master/src/erlzmq.erl    ( ? Q g � � �??#?)             
              ?r