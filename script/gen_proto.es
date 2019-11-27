#!/usr/bin/env escript
%% vim: set ft=erlang:
%% coding: utf-8

main([]) ->
    io:format("make proto begin~n", []),
    setup_code_path(),
    case filelib:is_file(filename:absname("./proto/game_proto.proto")) of
        true -> gpb_compile:c([{o_erl, "./src/"}, {o_hrl, "./include"}], ["./proto/game_proto.proto"]);
        false ->
            io:format("Error: ~s.~n", ["can not find file: ./proto/game_proto.proto"]),
            show_usage(),
            halt(1)
    end.

setup_code_path() ->
    ScriptName = escript:script_name(),
    %% check symbolic link
    RawFile = find_raw_file(ScriptName),

    BinDir = filename:dirname(RawFile),
    EBinDir = filename:join([BinDir, "..", "ebin"]),
    %% add the gpb ebin path to we can have access to gpb_compile
    case code:add_pathz(EBinDir) of
        true ->
            try gpb_compile:module_info()
            catch error:undef ->
                io:format(
                    "Found dir ~p, but no usable gpb_compile.beam,~n"
                    "please verify that gpb has been built properly.~n",
                    [EBinDir]),
                halt(1)
            end,
            ok;
        {error, bad_directory} ->
            io:format(
                "Cannot find dir ~p~n"
                "please verify that gpb has been built properly.~n",
                [EBinDir]),
            halt(1)
    end.

find_raw_file(Name) ->
    find_raw_file(Name, file:read_link(Name)).

find_raw_file(Name, {error, _}) ->
    Name;

find_raw_file(Name, {ok, Name1}) ->
    %% for relative symbolic link
    %% if Name1 is absolute, then AbsoluteName is Name1
    DirName = filename:dirname(Name),
    AbsoluteName = filename:join(DirName, Name1),
    find_raw_file(AbsoluteName, file:read_link(AbsoluteName)).

show_usage() ->
    io:format("usage: ~s [options] X.proto [...]~n",
        [filename:basename(escript:script_name())]),
    gpb_compile:show_args().