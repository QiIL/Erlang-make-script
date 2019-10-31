#!/usr/bin/env escript
%% vim: set ft=erlang:
%% coding: utf-8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 根据配置文件生成同名配置 .config -> .erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include_lib("kernel/include/file.hrl").
-define(M(A, Y), io:format( "[gen_config:~w]:" ++  A, [?LINE|Y])).
-define(E(A, Y), io:format( "[gen_config:~w]:" ++  A, [?LINE|Y])).
-define(P(A, Y), io:format(A, Y )).
-define(ERROR(S, A), begin ?E("gen config err:~p ;args ~p~n", [A, S]), halt(1) end).
-define(CONFIG_SRC_DIR, "config/src").
-define(CONFIG_OUTPUT_DIR, "config/erl/").
-define(EASY_MAKE_CONFIG, "./script/easy_make.config").

%% 输出配置erl的格式
erl_config_format(GenName, SrcBody) ->
    StrGenName = erlang:atom_to_list(GenName),
    Head = "%% coding: utf-8
-module(" ++ StrGenName ++ ").
-export([find/1]).
%% auto genetate do not edit
%% ============= make by script, config start ===========
",
    Tail = "find(_)->undefined.
%% ============== make by script, config end =============
",
    Head ++ SrcBody ++ Tail.

write_file(GenPath, Src) ->
    case file:write_file(GenPath, Src, []) of
        ok -> ok;
        {error, Reason} -> throw({error, Reason})
    end.

main([]) ->
    load_make_config(),
    load_meta_config(),
    case gen_config() of
        ok ->
            dump_meta(),
            ?M("gen config ok !!!!!!!!!! ~n", []),
            ok;
        _ ->
            dump_meta(),
            halt(1)
    end.

gen_config() ->
    AllFile = filelib:wildcard(filename:absname(?CONFIG_SRC_DIR) ++ "/*.config"),
    gen_erl_config(AllFile),
    ok.

gen_erl_config([]) -> ok;
gen_erl_config([File | T]) ->
    case get_file_status(File) of
        {need_compile, MTime, _OtherTime} ->
            case gen_erl_config2(File) of
                ok -> ets:insert(get_meta_config_ets(), {File, MTime});
                error -> error;
                _ -> error
            end;
        {need_compile, MTime} ->
            case gen_erl_config2(File) of
                ok -> ets:insert(get_meta_config_ets(), {File, MTime});
                error -> error;
                _ -> error
            end;
        _ -> ignore
    end,
    gen_erl_config(T).

gen_erl_config2(File) ->
    {ok, List} = file:consult(File),
    CfgFile = File -- (filename:absname(?CONFIG_SRC_DIR) ++ "/"),
    FileName = lists:sublist(CfgFile, 1, erlang:length(CfgFile) - 7),
    ?M("~s ~n", [FileName ++ ".erl"]),
    Body = [begin
        case Config of
            {K, V} ->
                io_lib:format("find(~w)->~w; ~n", [K, V]);
            _ -> ?ERROR(File, Config)
        end
    end || Config <- List],
    Src = erl_config_format(erlang:list_to_atom(FileName), "\n\n" ++ Body),
    write_file(filename:absname(?CONFIG_OUTPUT_DIR ++ "/" ++ FileName ++ ".erl"), Src),
    ok.

dump_meta() ->
    MetaFile = get_meta_config_filename(),
    ok = ets:tab2file(get_meta_config_ets(), MetaFile),
    ok.
%% =======================================================
load_make_config() ->
    ets:new(easy_make, [set, named_table, public, {read_concurrency, true}]),
    {ok, List} = file:consult(?EASY_MAKE_CONFIG),
    [ets:insert(easy_make, Tuple) || Tuple <- List].
%% 加载文件编译记录
load_meta_config() ->
    Filename = get_meta_config_filename(),
    case is_file_exists(Filename) of
        true ->
            case ets:file2tab(Filename) of
                {ok, _} ->
                    ok;
                _Error ->
                    ets:new(get_meta_config_ets(), [set, named_table, public, {read_concurrency, true}, {keypos, 1}])
            end;
        false ->
            ets:new(get_meta_config_ets(), [set, named_table, public, {read_concurrency, true}, {keypos, 1}])
    end.

%% 源文件的编译记录, 格式为：{File, Time}
get_meta_config_filename() ->
    filename:absname(get_meta_dir() ++ "/" ++ erlang:atom_to_list(get_meta_config_ets())).
get_meta_dir() ->
    case ets:lookup(easy_make, meta_dir) of
        [{_, Dir}] -> Dir;
        _ -> filename:absname("./script")
    end.

%% 记录源文件时间的ets
get_meta_config_ets() ->
    case ets:lookup(easy_make, meta_ets_file) of
        [{_, Atom}] -> Atom;
        _ -> ets_meta_config
    end.

is_file_exists(File) ->
    case file:read_file_info(File) of
        {ok, _} ->
            true;
        _Other ->
            false
    end.

%% 对比编译文件的最新改动时间
get_file_status(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} ->
            case ets:lookup(get_meta_config_ets(), File) of
                [{_, MTime}] -> ignore;
                [{_, OtherTime}] -> {need_compile, MTime, OtherTime};
                _ -> {need_compile, MTime}
            end;
        _ -> error
    end.