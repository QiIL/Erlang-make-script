%%============================
%% 编译所有文件
%%============================
-module(easy_make).
-include_lib("kernel/include/file.hrl").
-define(MSG(__Format, __Arg), io:format("[~s:~w]:" ++ __Format, [?MODULE,?LINE|__Arg])).
-define(EASY_MAKE_CONFIG, "./script/easy_make.config").
-define(DEFAULT_WORKER_NUM, 20).
-export([
    make_src/0,
    make_lib/0
]).
%% 根据Emakefile编译所有的源码输出到./bin
make_src() ->
    normal_make(meta_src),
    ?MSG("make src done ~n", []).

make_lib() ->
    ?MSG("make library ~n", []),
    normal_make(meta_lib),
    ?MSG("make library done ~n", []).

normal_make(Meta) ->
    load_make_config(),
    load_meta_config(Meta),
    load_make_queue(Meta),
    case catch make_all(Meta) of
        error ->
            dump_meta(Meta),
            error;
        {error, _} ->
            dump_meta(Meta),
            error;
        _ ->
            after_make(Meta)
    end.

%% 加载编译配置
load_make_config() ->
    ets:new(easy_make, [set, named_table, public, {read_concurrency, true}]),
    {ok, List} = file:consult(?EASY_MAKE_CONFIG),
    [ets:insert(easy_make, Tuple) || Tuple <- List].

%% 加载文件编译记录
load_meta_config(Meta) ->
    ensure_meta_dir(),
    Filename = get_meta_config_filename(Meta),
    case is_file_exists(Filename) of
        true ->
            case ets:file2tab(Filename) of
                {ok, _} ->
                    ok;
                _Error ->
                    ets:new(get_meta_config_ets(Meta), [set, named_table, public, {read_concurrency, true}, {keypos, 1}])
            end;
        false ->
            ets:new(get_meta_config_ets(Meta), [set, named_table, public, {read_concurrency, true}, {keypos, 1}])
    end.


%% 把需要编译的文件放进队列
%% lib 独立出来因为不常编译，不用放一起了
load_make_queue(meta_lib) ->
    ets:new(make_queue, [named_table, public, ordered_set, {write_concurrency, true}, {read_concurrency, true}]),
    case ets:lookup(easy_make, lib_emake) of
        [{_, List}] ->
            load_make_queue(List, 1, 1);
        _ -> erlang:throw(no_lib_make_config)
    end,
    ok;
load_make_queue(meta_src) ->
    %% 先放进去的先编译
    ets:new(make_queue, [named_table, public, ordered_set, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, List} = file:consult(get_emakefile()),
    load_make_queue(List, 1, 1),
    ok.


load_make_queue([], _, _) -> ok;
load_make_queue([{Rules, Option} | T], OptionIndex, Count) ->
%%    ?MSG("FILE RULES: ~s~n", [Rules]),
    Files = merge_rule_erl_file(Rules, []),
    %% 把文件的编译规则存一下
    ets:insert(make_queue, {{option, OptionIndex}, Option}),
    NewCount = insert_make_queue(Files, OptionIndex, Count),
    load_make_queue(T, OptionIndex + 1, NewCount).

%% 编译所有的源文件
make_all(Meta) ->
    %% spawn多个worker进行编译
    %% 没那么多文件都不要那么多个进程啦
    [{_, Count}] = ets:lookup(make_queue, count),
    Worker = get_worker_num(Count),
    Ref = make_ref(),
    PIDs = [start_worker(self(), Ref, Meta) || _ <- lists:seq(1, Worker)],
    do_wait_worker(length(PIDs), Ref).

do_wait_worker(0, _) -> ok;
do_wait_worker(N, Ref) ->
    receive
        {ack, Ref} ->
            do_wait_worker(N - 1, Ref);
        {error, Ref, F} ->
            throw({error, F});
        {'EXIT', _P, _Reason} ->
            %% 还是会退出的，随便了
            do_wait_worker(N, Ref);
        _Other ->
            ?MSG("receive unknown msg:~p~n", [_Other]),
            do_wait_worker(N, Ref)
    end.

start_worker(Parent, Ref, Meta) ->
    spawn_link(fun() -> worker_loop(Parent, Ref, Meta) end).

worker_loop(Parent, Ref, Meta) ->
    case pop_run_queue() of
        {ok, File, Opt} ->
            case recompile(File, Opt, Meta) of
                error ->
                    ?MSG("compile failed on ~s opts: ~p~n", [coerce_2_list(File), Opt]),
                    Parent ! {error, Ref, File},
                    exit(error);
                {error, _, _} ->
                    ?MSG("compile failed on ~s opts: ~p~n", [coerce_2_list(File), Opt]),
                    Parent ! {error, Ref, File},
                    exit(error);
                _ -> worker_loop(Parent, Ref, Meta)
            end;
        _ ->
            %% 没有要编译的文件了
            Parent ! {ack, Ref}
    end.

%% 根据文件的最新修改来编译文件
recompile(File, Option, Meta) ->
    case get_file_status(Meta, File) of
        {need_compile, MTime} ->
            ?MSG("newcompile: ~s ~n", [File]),
            case compile:file(File, Option) of
                error -> error;
                {error, _, _} = _Err -> _Err;
                _ ->
                    ets:insert(get_meta_config_ets(Meta), {File, MTime}),
                    ok
            end;
        {need_compile, MTime, _OtherTime} ->
            ?MSG("recompile: ~s: ~n", [File]),
            case compile:file(File, Option) of
                error -> error;
                {error, _, _} = _Err -> _Err;
                _ ->
                    ets:insert(get_meta_config_ets(Meta), {File, MTime}),
                    ok
            end;
        ignore -> ok;
        error -> error
    end.

%% 完成编译后的额外操作
%% 1.回写ets_meta_config
after_make(Meta) ->
    dump_meta(Meta),
    ok.

dump_meta(Meta) ->
    MetaFile = get_meta_config_filename(Meta),
    ok = ets:tab2file(get_meta_config_ets(Meta), MetaFile),
    ok.

%% =========================工具函数=================================
%% 根据规则读取erl文件，按顺序返回
merge_rule_erl_file([], List) -> List;
merge_rule_erl_file([Rule | T], List) ->
%%    ?MSG("FILE RULE: ~s~n", [Rule]),
    FileList = filelib:wildcard(Rule ++ ".erl"),
    merge_rule_erl_file(T, List ++ FileList).

is_file_exists(File) ->
    case file:read_file_info(File) of
        {ok, _} ->
            true;
        _Other ->
            false
    end.

%% 保存需要编译的文件队列
insert_make_queue([], _,  Count) ->
    ets:insert(make_queue, {count, Count}),
    Count;
insert_make_queue([F | T], OptionIndex, Index) ->
%%    ?MSG("INSERT FILE: ~s~n", [F]),
    ets:insert(make_queue, {Index, F, OptionIndex}),
    insert_make_queue(T, OptionIndex, Index + 1).

%% 弹出未编译的文件
%% return {ok, File}
pop_run_queue() ->
    [{_, Count}] = ets:lookup(make_queue, count),
    pop_run_queue(Count).
pop_run_queue(Count) ->
    Idx = ets:update_counter(make_queue, index, 1, {index, 0}),
    case ets:lookup(make_queue, Idx) of
        [{_, F, OptIndex}] ->
            [{_, Opt}] = ets:lookup(make_queue, {option, OptIndex}),
            {ok, F, Opt};
        [] when Count =< Idx ->
            done;
        [] ->
            %% 这一项任务丢失了?
            pop_run_queue(Count)
    end.

%% 对比编译文件的最新改动时间
get_file_status(Meta, File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} ->
            case ets:lookup(get_meta_config_ets(Meta), File) of
                [{_, MTime}] -> ignore;
                [{_, OtherTime}] -> {need_compile, MTime, OtherTime};
                _ -> {need_compile, MTime}
            end;
        _ -> error
    end.

%% 转换字符串
coerce_2_list(X) when is_atom(X) ->
    atom_to_list(X);
coerce_2_list(X) ->
    X.

%% =========================文件以及目录==============================
%% 源文件的编译记录, 格式为：{File, Time}
ensure_meta_dir() ->
    Dir = get_meta_dir(),
    case filelib:is_dir(filename:absname(Dir)) of
        true -> ok;
        false -> file:make_dir(filename:absname(Dir))
    end.

get_meta_config_filename(Meta) ->
    filename:absname(get_meta_dir() ++ "/" ++ erlang:atom_to_list(get_meta_config_ets(Meta))).
get_meta_dir() ->
    case ets:lookup(easy_make, meta_dir) of
        [{_, Dir}] -> Dir;
        _ -> filename:absname("./script/meta")
    end.

%% emakefile的路径
get_emakefile() ->
    case ets:lookup(easy_make, emakefile) of
        [{_, Dir}] -> Dir;
        _ -> filename:absname("./emakefile")
    end.

%% 记录源文件时间的ets
get_meta_config_ets(Meta) ->
    case ets:lookup(easy_make, Meta) of
        [{_, Atom}] -> Atom;
        _ -> erlang:throw(error)
    end.

%% 获取编译进程的数量
get_worker_num(Num) ->
    case ets:lookup(easy_make, worker_num) of
        [{_, CfgNum}] ->
            erlang:min(CfgNum, Num);
        _ -> erlang:min(?DEFAULT_WORKER_NUM, Num)
    end.
