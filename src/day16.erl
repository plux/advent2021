-module(day16).
-compile([export_all]).
-include("aoc.hrl").

-define(sum, 0).
-define(product, 1).
-define(min, 2).
-define(max, 3).
-define(literal, 4).
-define(gt, 5).
-define(lt, 6).
-define(eq, 7).

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1({Packet, _}) ->
    versions(Packet).

part2({Packet, _}) ->
    eval(Packet).

eval({packet, _V, Id, SubPackets}) ->
    Vals = [eval(P) || P <- SubPackets],
    case {Id, Vals} of
        {?sum, _}                -> lists:sum(Vals);
        {?product, _}            -> lists:foldl(fun erlang:'*'/2, 1, Vals);
        {?min, _}                -> lists:min(Vals);
        {?max, _}                -> lists:max(Vals);
        {?gt, [A, B]} when A > B -> 1;
        {?lt, [A, B]} when A < B -> 1;
        {?eq, [A, A]}            -> 1;
        {_, [_, _]}              -> 0
    end;
eval({literal, _, Val}) ->
    Val.

versions({packet, V, _Id, SubPackets}) ->
    V + lists:sum([versions(P) || P <- SubPackets]);
versions({literal, V, _Literal}) ->
    V.

parse(Hex)  ->
    BinStr = lists:flatten([hex2bin(C) || C <- Hex]),
    parse_packet(<< <<($0 - B):1>> || B <- BinStr >>).

hex2bin(C) ->
    io_lib:format("~4.2.0B", [list_to_integer([C], 16)]).

parse_packet(<<Version:3, ?literal:3, Bin/bits>>) ->
    {Val, Rest} = parse_literal(Bin, <<>>),
    {{literal, Version, Val}, Rest};
parse_packet(<<Version:3, Id:3, 0:1, LengthPackets:15, Rest0/bits>>) ->
    {SubPackets, Rest} = sub_packets_bits(LengthPackets, [], Rest0),
    {{packet, Version, Id, SubPackets}, Rest};
parse_packet(<<Version:3, Id:3, 1:1, NumPackets:11, Rest0/bits>>) ->
    {SubPackets, Rest} = sub_packets_num(NumPackets, [], Rest0),
    {{packet, Version, Id, SubPackets}, Rest}.

parse_literal(<<1:1, X:4/bits, Rest/bits>>, Acc) ->
    parse_literal(Rest, <<Acc/bits, X/bits>>);
parse_literal(<<0:1, X:4/bits, Rest/bits>>, Acc) ->
    Size = bit_size(<<Acc/bits, X/bits>>),
    <<Val:Size/integer>> = <<Acc/bits, X/bits>>,
    {Val, Rest}.

sub_packets_num(0, Acc, Rest) ->
    {lists:reverse(Acc), Rest};
sub_packets_num(N, Acc, Bin) ->
    {SubPacket, Rest} = parse_packet(Bin),
    sub_packets_num(N-1, [SubPacket|Acc], Rest).

sub_packets_bits(0, Acc, Bin) ->
    {lists:reverse(Acc), Bin};
sub_packets_bits(N, Acc, Bin) ->
    {Packet, Rest} = parse_packet(Bin),
    Size = bit_size(Bin) - bit_size(Rest),
    sub_packets_bits(N-Size, [Packet|Acc], Rest).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual(9,  part1(parse("38006F45291200")))
    , ?_assertEqual(14, part1(parse("EE00D40C823060")))
    , ?_assertEqual(16, part1(parse("8A004A801A8002F478")))
    , ?_assertEqual(3,  part2(parse("C200B40A82")))
    , ?_assertEqual(12, part1(parse("620080001611562C8802118E34")))
    , ?_assertEqual(23, part1(parse("C0015000016115A2E0802F182340")))
    , ?_assertEqual(31, part1(parse("A0016C880162017C3686B18A3D4780")))
    , ?_assertEqual({934, 912901337844}, ?solve())
    ].
