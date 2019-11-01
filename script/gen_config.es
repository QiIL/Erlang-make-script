#!/usr/bin/env escript
%% vim: set ft=erlang:
%% coding: utf-8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 根据配置需要生成新的配置 erl -> dyn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include_lib("kernel/include/file.hrl").
-define(M(A, Y), io:format( "[gen_config:~w]:" ++  A, [?LINE|Y])).
-define(C(A, Y), io:format( "[check_config]:" ++  A, [Y])).
-define(E(A, Y), io:format( "[gen_config:~w]:" ++  A, [?LINE|Y])).
-define(P(A, Y), io:format(A, Y )).
-define(ERROR(S, A), begin ?E("gen config err:~p ;args ~p~n", [A, S]), halt(1) end).
-define(CONFIG_SRC_DIR, "config/erl").
-define(CONFIG_OUTPUT_DIR, "config/dyn").
-define(CONFIG_INCLUDE_PATH, "include").
-define(EASY_MAKE_CONFIG, "./script/easy_make.config").

main([]) ->
    case gen_config() of
        ok ->
            ?M("gen config ok !!!!!!!!!! ~n", []),
            ok;
        _ ->
            halt(1)
    end.
%% ===================================================================
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

write_file(Name, Src) ->
    write_file2(filename:absname_join(?CONFIG_OUTPUT_DIR, Name), Src).
write_file2(GenPath, Src) ->
    case file:write_file(GenPath, Src, []) of
        ok -> ok;
        {error, Reason} -> throw({error, Reason})
    end.

%% 判断是否需要编译，输入文件的更新时间比输出文件更新时间大，就要编译
need_compile(InputList, OutputList) ->
    ABSInputList = [filename:absname_join(?CONFIG_SRC_DIR, File) || File <- InputList],
    ABSOutputList = [filename:absname_join(?CONFIG_OUTPUT_DIR, File) || File <- OutputList],
    need_compile2(ABSInputList, ABSOutputList).

need_compile2(SrcFileList, OutputFileList)->
    lists:max([ get_file_mtime(SrcFile) || SrcFile<- SrcFileList])
        >= lists:min([ get_file_mtime(OutputFile) || OutputFile<- OutputFileList]).

%% 获取文件上次修改时间
get_file_mtime(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime=MTime}}  ->
            MTime;
        _ ->
            0
    end.

gen_config_list(Name) ->
    ABSName = filename:absname_join(?CONFIG_SRC_DIR, Name),
    case filelib:is_file(ABSName) of
        true -> gen_config_list(ABSName, ?CONFIG_INCLUDE_PATH);
        _ -> ?E(" can't find file ~s in ~p", [Name, ?CONFIG_SRC_DIR])
    end.

gen_config_list(ConfigPathName, IncludesPath) ->
    case catch gen_config_list2(ConfigPathName, IncludesPath) of
        [_|_] = L -> [T|| {_,V}=T<- L, V=/=undefined];
        _Err -> ?E("file error:~s~n", [ConfigPathName]), _Err
    end.

gen_config_list2(PathName, IncludesPath) ->
    case filelib:is_file(PathName) of true -> ok; _ -> erlang:throw({error, 'not a file'}) end,
    case epp:parse_file(PathName, [{includes, IncludesPath}]) of
        {ok, TList1} ->
            TList2 = erl_expand_records:module(TList1, [strict_record_tests]),
            %% 有且只有一个function 是find
            case lists:keyfind(function, 1, TList2) of
                {_, _, find, _, [{clause, _, _, _,[{_,_,_, TList3}]}]} ->ok; %% 单个function多个case
                {_, _, find, _, TList3} -> ok; %% 多个function，没有case
                _Err ->
                    TList3 = undefined,
                    ?E("file error:~s~n", [PathName]),
                    ?ERROR({function, 1, TList1}, _Err)
            end,
            [begin
                {_, Key2, _} = erl_eval:exprs(Key, [{'_Other', undefined}]),
                {_, Val2, _} = erl_eval:exprs(Val, [{'_Other', undefined}]),
                {Key2, Val2}
            %% {erl_prettypr:format(Key, [{ribbon, 1000}, {paper, 1000}]), erl_prettypr:format(Val, [{ribbon, 1000}, {paper, 1000}])}
            end || {_,_,Key,_,Val} <-TList3, not is_var(Key) ];
        _Err ->
            erlang:throw({error, 1, _Err})
    end.


is_var([])->
    false;
is_var([H | T])->
    is_var(H) orelse is_var(T);
is_var({'var', _, _})->
    true;
is_var({'tuple', _, List}) ->
    is_var(List);
is_var({'cons', _, CDR, CDT})->
    is_var(CDR) orelse is_var(CDT);
is_var({'bin', _, BinElements})->
    is_var(BinElements);
is_var({'bin_element', _, T, _, _})->
    is_var(T);
is_var(_)-> %% nil atom
    false.

format_key_value(Key, Val) ->
    io_lib:format("find(~w)->~w;~n", [Key, Val]).

%% =========================================================================
gen_config() ->
    %% 自行实现的生成逻辑
    gen_test_config(),
    ok.

gen_test_config() ->
    OutFile = "cfg_test_aux.erl",
    InputFile = "cfg_test.erl",
    case need_compile([InputFile], [OutFile]) of
        true ->
            KVList = gen_config_list(InputFile),
            Body = [format_key_value(Key, Val) || {Key, Val} <- KVList],
            Src = erl_config_format(cfg_test_aux, Body),
            write_file(OutFile, Src),
            ?M("~s~n", [OutFile]);
        false ->
            ?C("~s~n", [OutFile])
    end.
