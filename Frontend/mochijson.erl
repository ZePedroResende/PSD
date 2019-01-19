%% @copyright 2006 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @doc Yet another JSON (RFC 4627) library for Erlang.
-module(mochijson).
-author('bob@mochimedia.com').
-export([encoder/1, encode/1]).
-export([decoder/1, decode/1]).
-export([binary_encoder/1, binary_encode/1]).
-export([binary_decoder/1, binary_decode/1]).

% This is a macro to placate syntax highlighters..
-define(Q, $\").
-define(ADV_COL(S, N), S#decoder{column=N+S#decoder.column}).
-define(INC_COL(S), S#decoder{column=1+S#decoder.column}).
-define(INC_LINE(S), S#decoder{column=1, line=1+S#decoder.line}).

%% @type json_string() = atom | string() | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = {array, [json_term()]}
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object()
%% @type encoding() = utf8 | unicode
%% @type encoder_option() = {input_encoding, encoding()} |
%%                          {handler, function()}
%% @type decoder_option() = {input_encoding, encoding()} |
%%                          {object_hook, function()}
%% @type bjson_string() = binary()
%% @type bjson_number() = integer() | float()
%% @type bjson_array() = [bjson_term()]
%% @type bjson_object() = {struct, [{bjson_string(), bjson_term()}]}
%% @type bjson_term() = bjson_string() | bjson_number() | bjson_array() |
%%                      bjson_object()
%% @type binary_encoder_option() = {handler, function()}
%% @type binary_decoder_option() = {object_hook, function()}

-record(encoder, {input_encoding=unicode,
                  handler=null}).

-record(decoder, {input_encoding=utf8,
                  object_hook=null,
                  line=1,
                  column=1,
                  state=null}).

%% @spec encoder([encoder_option()]) -&gt; function()
%% @doc Create an encoder/1 with the given options.
encoder(Options) -&gt;
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -&gt; json_encode(O, State) end.

%% @spec encode(json_term()) -&gt; iolist()
%% @doc Encode the given as JSON to an iolist.
encode(Any) -&gt;
    json_encode(Any, #encoder{}).

%% @spec decoder([decoder_option()]) -&gt; function()
%% @doc Create a decoder/1 with the given options.
decoder(Options) -&gt;
    State = parse_decoder_options(Options, #decoder{}),
    fun (O) -&gt; json_decode(O, State) end.

%% @spec decode(iolist()) -&gt; json_term()
%% @doc Decode the given iolist to Erlang terms.
decode(S) -&gt;
    json_decode(S, #decoder{}).

%% @spec binary_decoder([binary_decoder_option()]) -&gt; function()
%% @doc Create a binary_decoder/1 with the given options.
binary_decoder(Options) -&gt;
    mochijson2:decoder(Options).

%% @spec binary_encoder([binary_encoder_option()]) -&gt; function()
%% @doc Create a binary_encoder/1 with the given options.
binary_encoder(Options) -&gt;
    mochijson2:encoder(Options).

%% @spec binary_encode(bjson_term()) -&gt; iolist()
%% @doc Encode the given as JSON to an iolist, using lists for arrays and
%%      binaries for strings.
binary_encode(Any) -&gt;
    mochijson2:encode(Any).

%% @spec binary_decode(iolist()) -&gt; bjson_term()
%% @doc Decode the given iolist to Erlang terms, using lists for arrays and
%%      binaries for strings.
binary_decode(S) -&gt;
    mochijson2:decode(S).

%% Internal API

parse_encoder_options([], State) -&gt;
    State;
parse_encoder_options([{input_encoding, Encoding} | Rest], State) -&gt;
    parse_encoder_options(Rest, State#encoder{input_encoding=Encoding});
parse_encoder_options([{handler, Handler} | Rest], State) -&gt;
    parse_encoder_options(Rest, State#encoder{handler=Handler}).

parse_decoder_options([], State) -&gt;
    State;
parse_decoder_options([{input_encoding, Encoding} | Rest], State) -&gt;
    parse_decoder_options(Rest, State#decoder{input_encoding=Encoding});
parse_decoder_options([{object_hook, Hook} | Rest], State) -&gt;
    parse_decoder_options(Rest, State#decoder{object_hook=Hook}).

json_encode(true, _State) -&gt;
    "true";
json_encode(false, _State) -&gt;
    "false";
json_encode(null, _State) -&gt;
    "null";
json_encode(I, _State) when is_integer(I) -&gt;
    integer_to_list(I);
json_encode(F, _State) when is_float(F) -&gt;
    mochinum:digits(F);
json_encode(L, State) when is_list(L); is_binary(L); is_atom(L) -&gt;
    json_encode_string(L, State);
json_encode({array, Props}, State) when is_list(Props) -&gt;
    json_encode_array(Props, State);
json_encode({struct, Props}, State) when is_list(Props) -&gt;
    json_encode_proplist(Props, State);
json_encode(Bad, #encoder{handler=null}) -&gt;
    exit({json_encode, {bad_term, Bad}});
json_encode(Bad, State=#encoder{handler=Handler}) -&gt;
    json_encode(Handler(Bad), State).

json_encode_array([], _State) -&gt;
    "[]";
json_encode_array(L, State) -&gt;
    F = fun (O, Acc) -&gt;
                [$,, json_encode(O, State) | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$\] | Acc1]).

json_encode_proplist([], _State) -&gt;
    "{}";
json_encode_proplist(Props, State) -&gt;
    F = fun ({K, V}, Acc) -&gt;
                KS = case K of 
                         K when is_atom(K) -&gt;
                             json_encode_string_utf8(atom_to_list(K));
                         K when is_integer(K) -&gt;
                             json_encode_string(integer_to_list(K), State);
                         K when is_list(K); is_binary(K) -&gt;
                             json_encode_string(K, State)
                     end,
                VS = json_encode(V, State),
                [$,, VS, $:, KS | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$\} | Acc1]).

json_encode_string(A, _State) when is_atom(A) -&gt;
    json_encode_string_unicode(xmerl_ucs:from_utf8(atom_to_list(A)));
json_encode_string(B, _State) when is_binary(B) -&gt;
    json_encode_string_unicode(xmerl_ucs:from_utf8(B));
json_encode_string(S, #encoder{input_encoding=utf8}) -&gt;
    json_encode_string_utf8(S);
json_encode_string(S, #encoder{input_encoding=unicode}) -&gt;
    json_encode_string_unicode(S).

json_encode_string_utf8(S) -&gt;
    [?Q | json_encode_string_utf8_1(S)].

json_encode_string_utf8_1([C | Cs]) when C &gt;= 0, C =&lt; 16#7f -&gt;
    NewC = case C of
               $\\ -&gt; "\\\\";
               ?Q -&gt; "\\\"";
               _ when C &gt;= $\s, C &lt; 16#7f -&gt; C;
               $\t -&gt; "\\t";
               $\n -&gt; "\\n";
               $\r -&gt; "\\r";
               $\f -&gt; "\\f";
               $\b -&gt; "\\b";
               _ when C &gt;= 0, C =&lt; 16#7f -&gt; unihex(C);
               _ -&gt; exit({json_encode, {bad_char, C}})
           end,
    [NewC | json_encode_string_utf8_1(Cs)];
json_encode_string_utf8_1(All=[C | _]) when C &gt;= 16#80, C =&lt; 16#10FFFF -&gt;
    [?Q | Rest] = json_encode_string_unicode(xmerl_ucs:from_utf8(All)),
    Rest;
json_encode_string_utf8_1([]) -&gt;
    "\"".

json_encode_string_unicode(S) -&gt;
    [?Q | json_encode_string_unicode_1(S)].

json_encode_string_unicode_1([C | Cs]) -&gt;
    NewC = case C of
               $\\ -&gt; "\\\\";
               ?Q -&gt; "\\\"";
               _ when C &gt;= $\s, C &lt; 16#7f -&gt; C;
               $\t -&gt; "\\t";
               $\n -&gt; "\\n";
               $\r -&gt; "\\r";
               $\f -&gt; "\\f";
               $\b -&gt; "\\b";
               _ when C &gt;= 0, C =&lt; 16#10FFFF -&gt; unihex(C);
               _ -&gt; exit({json_encode, {bad_char, C}})
           end,
    [NewC | json_encode_string_unicode_1(Cs)];
json_encode_string_unicode_1([]) -&gt;
    "\"".

dehex(C) when C &gt;= $0, C =&lt; $9 -&gt;
    C - $0;
dehex(C) when C &gt;= $a, C =&lt; $f -&gt;
    C - $a + 10;
dehex(C) when C &gt;= $A, C =&lt; $F -&gt;
    C - $A + 10.

hexdigit(C) when C &gt;= 0, C =&lt; 9 -&gt;
    C + $0;
hexdigit(C) when C =&lt; 15 -&gt;
    C + $a - 10.

unihex(C) when C &lt; 16#10000 -&gt;
    &lt;&lt;D3:4, D2:4, D1:4, D0:4&gt;&gt; = &lt;&lt;C:16&gt;&gt;,
    Digits = [hexdigit(D) || D &lt;- [D3, D2, D1, D0]],
    [$\\, $u | Digits];
unihex(C) when C =&lt; 16#10FFFF -&gt;
    N = C - 16#10000,
    S1 = 16#d800 bor ((N bsr 10) band 16#3ff),
    S2 = 16#dc00 bor (N band 16#3ff),
    [unihex(S1), unihex(S2)].

json_decode(B, S) when is_binary(B) -&gt;
    json_decode(binary_to_list(B), S);
json_decode(L, S) -&gt;
    {Res, L1, S1} = decode1(L, S),
    {eof, [], _} = tokenize(L1, S1#decoder{state=trim}),
    Res.

decode1(L, S=#decoder{state=null}) -&gt;
    case tokenize(L, S#decoder{state=any}) of
        {{const, C}, L1, S1} -&gt;
            {C, L1, S1};
        {start_array, L1, S1} -&gt;
            decode_array(L1, S1#decoder{state=any}, []);
        {start_object, L1, S1} -&gt;
            decode_object(L1, S1#decoder{state=key}, [])
    end.

make_object(V, #decoder{object_hook=null}) -&gt;
    V;
make_object(V, #decoder{object_hook=Hook}) -&gt;
    Hook(V).

decode_object(L, S=#decoder{state=key}, Acc) -&gt;
    case tokenize(L, S) of
        {end_object, Rest, S1} -&gt;
            V = make_object({struct, lists:reverse(Acc)}, S1),
            {V, Rest, S1#decoder{state=null}};
        {{const, K}, Rest, S1} when is_list(K) -&gt;
            {colon, L2, S2} = tokenize(Rest, S1),
            {V, L3, S3} = decode1(L2, S2#decoder{state=null}),
            decode_object(L3, S3#decoder{state=comma}, [{K, V} | Acc])
    end;
decode_object(L, S=#decoder{state=comma}, Acc) -&gt;
    case tokenize(L, S) of
        {end_object, Rest, S1} -&gt;
            V = make_object({struct, lists:reverse(Acc)}, S1),
            {V, Rest, S1#decoder{state=null}};
        {comma, Rest, S1} -&gt;
            decode_object(Rest, S1#decoder{state=key}, Acc)
    end.

decode_array(L, S=#decoder{state=any}, Acc) -&gt;
    case tokenize(L, S) of
        {end_array, Rest, S1} -&gt;
            {{array, lists:reverse(Acc)}, Rest, S1#decoder{state=null}};
        {start_array, Rest, S1} -&gt;
            {Array, Rest1, S2} = decode_array(Rest, S1#decoder{state=any}, []),
            decode_array(Rest1, S2#decoder{state=comma}, [Array | Acc]);
        {start_object, Rest, S1} -&gt;
            {Array, Rest1, S2} = decode_object(Rest, S1#decoder{state=key}, []),
            decode_array(Rest1, S2#decoder{state=comma}, [Array | Acc]);
        {{const, Const}, Rest, S1} -&gt;
            decode_array(Rest, S1#decoder{state=comma}, [Const | Acc])
    end;
decode_array(L, S=#decoder{state=comma}, Acc) -&gt;
    case tokenize(L, S) of
        {end_array, Rest, S1} -&gt;
            {{array, lists:reverse(Acc)}, Rest, S1#decoder{state=null}};
        {comma, Rest, S1} -&gt;
            decode_array(Rest, S1#decoder{state=any}, Acc)
    end.

tokenize_string(IoList=[C | _], S=#decoder{input_encoding=utf8}, Acc)
  when is_list(C); is_binary(C); C &gt;= 16#7f -&gt;
    List = xmerl_ucs:from_utf8(iolist_to_binary(IoList)),
    tokenize_string(List, S#decoder{input_encoding=unicode}, Acc);
tokenize_string("\"" ++ Rest, S, Acc) -&gt;
    {lists:reverse(Acc), Rest, ?INC_COL(S)};
tokenize_string("\\\"" ++ Rest, S, Acc) -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\" | Acc]);
tokenize_string("\\\\" ++ Rest, S, Acc) -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\\ | Acc]);
tokenize_string("\\/" ++ Rest, S, Acc) -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 2), [$/ | Acc]);
tokenize_string("\\b" ++ Rest, S, Acc) -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\b | Acc]);
tokenize_string("\\f" ++ Rest, S, Acc) -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\f | Acc]);
tokenize_string("\\n" ++ Rest, S, Acc) -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\n | Acc]);
tokenize_string("\\r" ++ Rest, S, Acc) -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\r | Acc]);
tokenize_string("\\t" ++ Rest, S, Acc) -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\t | Acc]);
tokenize_string([$\\, $u, C3, C2, C1, C0 | Rest], S, Acc) -&gt;
    % coalesce UTF-16 surrogate pair?
    C = dehex(C0) bor
        (dehex(C1) bsl 4) bor
        (dehex(C2) bsl 8) bor 
        (dehex(C3) bsl 12),
    tokenize_string(Rest, ?ADV_COL(S, 6), [C | Acc]);
tokenize_string([C | Rest], S, Acc) when C &gt;= $\s; C &lt; 16#10FFFF -&gt;
    tokenize_string(Rest, ?ADV_COL(S, 1), [C | Acc]).
    
tokenize_number(IoList=[C | _], Mode, S=#decoder{input_encoding=utf8}, Acc)
  when is_list(C); is_binary(C); C &gt;= 16#7f -&gt;
    List = xmerl_ucs:from_utf8(iolist_to_binary(IoList)),
    tokenize_number(List, Mode, S#decoder{input_encoding=unicode}, Acc);
tokenize_number([$- | Rest], sign, S, []) -&gt;
    tokenize_number(Rest, int, ?INC_COL(S), [$-]);
tokenize_number(Rest, sign, S, []) -&gt;
    tokenize_number(Rest, int, S, []);
tokenize_number([$0 | Rest], int, S, Acc) -&gt;
    tokenize_number(Rest, frac, ?INC_COL(S), [$0 | Acc]);
tokenize_number([C | Rest], int, S, Acc) when C &gt;= $1, C =&lt; $9 -&gt;
    tokenize_number(Rest, int1, ?INC_COL(S), [C | Acc]);
tokenize_number([C | Rest], int1, S, Acc) when C &gt;= $0, C =&lt; $9 -&gt;
    tokenize_number(Rest, int1, ?INC_COL(S), [C | Acc]);
tokenize_number(Rest, int1, S, Acc) -&gt;
    tokenize_number(Rest, frac, S, Acc);
tokenize_number([$., C | Rest], frac, S, Acc) when C &gt;= $0, C =&lt; $9 -&gt;
    tokenize_number(Rest, frac1, ?ADV_COL(S, 2), [C, $. | Acc]);
tokenize_number([E | Rest], frac, S, Acc) when E == $e; E == $E -&gt;
    tokenize_number(Rest, esign, ?INC_COL(S), [$e, $0, $. | Acc]);
tokenize_number(Rest, frac, S, Acc) -&gt;
    {{int, lists:reverse(Acc)}, Rest, S};
tokenize_number([C | Rest], frac1, S, Acc) when C &gt;= $0, C =&lt; $9 -&gt;
    tokenize_number(Rest, frac1, ?INC_COL(S), [C | Acc]);
tokenize_number([E | Rest], frac1, S, Acc) when E == $e; E == $E -&gt;
    tokenize_number(Rest, esign, ?INC_COL(S), [$e | Acc]);
tokenize_number(Rest, frac1, S, Acc) -&gt;
    {{float, lists:reverse(Acc)}, Rest, S};
tokenize_number([C | Rest], esign, S, Acc) when C == $-; C == $+ -&gt;
    tokenize_number(Rest, eint, ?INC_COL(S), [C | Acc]);
tokenize_number(Rest, esign, S, Acc) -&gt;
    tokenize_number(Rest, eint, S, Acc);
tokenize_number([C | Rest], eint, S, Acc) when C &gt;= $0, C =&lt; $9 -&gt;
    tokenize_number(Rest, eint1, ?INC_COL(S), [C | Acc]);
tokenize_number([C | Rest], eint1, S, Acc) when C &gt;= $0, C =&lt; $9 -&gt;
    tokenize_number(Rest, eint1, ?INC_COL(S), [C | Acc]);
tokenize_number(Rest, eint1, S, Acc) -&gt;
    {{float, lists:reverse(Acc)}, Rest, S}.

tokenize([], S=#decoder{state=trim}) -&gt;
    {eof, [], S};
tokenize([L | Rest], S) when is_list(L) -&gt;
    tokenize(L ++ Rest, S);
tokenize([B | Rest], S) when is_binary(B) -&gt;
    tokenize(xmerl_ucs:from_utf8(B) ++ Rest, S);
tokenize("\r\n" ++ Rest, S) -&gt;
    tokenize(Rest, ?INC_LINE(S));
tokenize("\n" ++ Rest, S) -&gt;
    tokenize(Rest, ?INC_LINE(S));
tokenize([C | Rest], S) when C == $\s; C == $\t -&gt;
    tokenize(Rest, ?INC_COL(S));
tokenize("{" ++ Rest, S) -&gt;
    {start_object, Rest, ?INC_COL(S)};
tokenize("}" ++ Rest, S) -&gt;
    {end_object, Rest, ?INC_COL(S)};
tokenize("[" ++ Rest, S) -&gt;
    {start_array, Rest, ?INC_COL(S)};
tokenize("]" ++ Rest, S) -&gt;
    {end_array, Rest, ?INC_COL(S)};
tokenize("," ++ Rest, S) -&gt;
    {comma, Rest, ?INC_COL(S)};
tokenize(":" ++ Rest, S) -&gt;
    {colon, Rest, ?INC_COL(S)};
tokenize("null" ++ Rest, S) -&gt;
    {{const, null}, Rest, ?ADV_COL(S, 4)};
tokenize("true" ++ Rest, S) -&gt;
    {{const, true}, Rest, ?ADV_COL(S, 4)};
tokenize("false" ++ Rest, S) -&gt;
    {{const, false}, Rest, ?ADV_COL(S, 5)};
tokenize("\"" ++ Rest, S) -&gt;
    {String, Rest1, S1} = tokenize_string(Rest, ?INC_COL(S), []),
    {{const, String}, Rest1, S1};
tokenize(L=[C | _], S) when C &gt;= $0, C =&lt; $9; C == $- -&gt;
    case tokenize_number(L, sign, S, []) of
        {{int, Int}, Rest, S1} -&gt;
            {{const, list_to_integer(Int)}, Rest, S1};
        {{float, Float}, Rest, S1} -&gt;
            {{const, list_to_float(Float)}, Rest, S1}
    end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% testing constructs borrowed from the Yaws JSON implementation.

%% Create an object from a list of Key/Value pairs.

obj_new() -&gt;
    {struct, []}.

is_obj({struct, Props}) -&gt;
    F = fun ({K, _}) when is_list(K) -&gt;
                true;
            (_) -&gt;
                false
        end,    
    lists:all(F, Props).

obj_from_list(Props) -&gt;
    Obj = {struct, Props},
    case is_obj(Obj) of
        true -&gt; Obj;
        false -&gt; exit(json_bad_object)
    end.

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({struct, Props1}, {struct, Props2}) -&gt;
    equiv_object(Props1, Props2);
equiv({array, L1}, {array, L2}) -&gt;
    equiv_list(L1, L2);
equiv(N1, N2) when is_number(N1), is_number(N2) -&gt; N1 == N2;
equiv(S1, S2) when is_list(S1), is_list(S2)     -&gt; S1 == S2;
equiv(true, true) -&gt; true;
equiv(false, false) -&gt; true;
equiv(null, null) -&gt; true.

%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) -&gt;
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun({{K1, V1}, {K2, V2}}) -&gt;
        equiv(K1, K2) and equiv(V1, V2)
    end, Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_list([], []) -&gt;
    true;
equiv_list([V1 | L1], [V2 | L2]) -&gt;
    equiv(V1, V2) andalso equiv_list(L1, L2).

e2j_vec_test() -&gt;
    test_one(e2j_test_vec(utf8), 1).

issue33_test() -&gt;
    %% http://code.google.com/p/mochiweb/issues/detail?id=33
    Js = {struct, [{"key", [194, 163]}]},
    Encoder = encoder([{input_encoding, utf8}]),
    "{\"key\":\"\\u00a3\"}" = lists:flatten(Encoder(Js)).

test_one([], _N) -&gt;
    %% io:format("~p tests passed~n", [N-1]),
    ok;
test_one([{E, J} | Rest], N) -&gt;
    %% io:format("[~p] ~p ~p~n", [N, E, J]),
    true = equiv(E, decode(J)),
    true = equiv(E, decode(encode(E))),
    test_one(Rest, 1+N).

e2j_test_vec(utf8) -&gt;
    [
    {1, "1"},
    {3.1416, "3.14160"}, % text representation may truncate, trail zeroes
    {-1, "-1"},
    {-3.1416, "-3.14160"},
    {12.0e10, "1.20000e+11"},
    {1.234E+10, "1.23400e+10"},
    {-1.234E-10, "-1.23400e-10"},
    {10.0, "1.0e+01"},
    {123.456, "1.23456E+2"},
    {10.0, "1e1"},
    {"foo", "\"foo\""},
    {"foo" ++ [5] ++ "bar", "\"foo\\u0005bar\""},
    {"", "\"\""},
    {"\"", "\"\\\"\""},
    {"\n\n\n", "\"\\n\\n\\n\""},
    {"\\", "\"\\\\\""},
    {"\" \b\f\r\n\t\"", "\"\\\" \\b\\f\\r\\n\\t\\\"\""},
    {obj_new(), "{}"},
    {obj_from_list([{"foo", "bar"}]), "{\"foo\":\"bar\"}"},
    {obj_from_list([{"foo", "bar"}, {"baz", 123}]),
     "{\"foo\":\"bar\",\"baz\":123}"},
    {{array, []}, "[]"},
    {{array, [{array, []}]}, "[[]]"},
    {{array, [1, "foo"]}, "[1,\"foo\"]"},

    % json array in a json object
    {obj_from_list([{"foo", {array, [123]}}]),
     "{\"foo\":[123]}"},

    % json object in a json object
    {obj_from_list([{"foo", obj_from_list([{"bar", true}])}]),
     "{\"foo\":{\"bar\":true}}"},

    % fold evaluation order
    {obj_from_list([{"foo", {array, []}},
                     {"bar", obj_from_list([{"baz", true}])},
                     {"alice", "bob"}]),
     "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}"},

    % json object in a json array
    {{array, [-123, "foo", obj_from_list([{"bar", {array, []}}]), null]},
     "[-123,\"foo\",{\"bar\":[]},null]"}
    ].

-endif.
</pre></body></html>Ztext/plainUUTF-8_Mhttps://raw.githubusercontent.com/zesilva63/PSD/master/frontend/mochijson.erl    ( ? Q g � � �R}R�R�             
              R�