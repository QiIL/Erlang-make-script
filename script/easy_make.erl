%%============================
%% 编译所有文件
%%============================
-module(easy_make).
-include_lib("kernel/include/file.hrl").
-define(MSG(__Format, __Arg), io:format("[~s:~w]:" ++ __Format, [?MODULE,?LINE|__Arg])).
-define(EASY_MAKE_CONFIG, "./script/easy_make.config").
-export([
    all/0
]).
%% 根据Emakefile编译所有的源码输出到./bin
all() ->
    init(),
    {ok, List} = file:consult(get_emakefile()),
    case make_all(List) of
        error ->
            dump_meta(),
            error;
        _ ->
            after_make(),
            ?MSG("cogratulation !! ~n", []),
            ?MSG("make done!! ~n", [])
    end.

%% 编译前的一些准备
init() ->
    load_make_config(),
    load_meta_config(),
    ok.

%% 加载编译配置
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

%% 编译所有的源文件
make_all([]) -> ok;
make_all([{FileRules, Option} | T]) ->
    Files = merge_rule_erl_file(FileRules, []),
%%    ?MSG("Files:~p", [Files]),
    case recompile(Files, Option) of
        ok -> make_all(T);
        error -> error;
        {error, _, _} -> error
    end.

%% 根据文件的最新修改来编译文件
recompile([], _) -> ok;
recompile([File | T], Option) ->
    case get_file_status(File) of
        {need_compile, MTime} ->
            ?MSG("recompile: ~s ~n", [File]),
            case compile:file(File, Option) of
                error -> error;
                {error, _, _} = _Err -> _Err;
                _ ->
                    ets:insert(ets_meta_config, {File, MTime}),
                    recompile(T, Option)
            end;
        {need_compile, MTime, OtherTime} ->
            ?MSG("recompile: ~s: ~p: ~p ~n", [File, MTime, OtherTime]),
            case compile:file(File, Option) of
                error -> error;
                {error, _, _} = _Err -> _Err;
                _ ->
                    ets:insert(ets_meta_config, {File, MTime}),
                    recompile(T, Option)
            end;
        ignore -> recompile(T, Option);
        error -> error
    end.

%% 完成编译后的额外操作
%% 1.回写ets_meta_config
after_make() ->
    move_lib_apps(),
    dump_meta(),
    ok.

dump_meta() ->
    MetaFile = get_meta_config_filename(),
    ok = ets:tab2file(ets_meta_config, MetaFile),
    ok.

%% 把依赖的apps移动到对应文件夹
move_lib_apps() ->
    AppDirs = get_lib_app_dirs(),
    move_app_file(AppDirs).

move_app_file([]) -> ok;
move_app_file([AppDir | T]) ->
    Apps = filelib:wildcard(AppDir + "/*.app"),
    BinDir = get_bin_dir(),
    [begin
        file:copy(File, BinDir ++ (File -- AppDir))
    end || File <- Apps],
    move_app_file(T).

%% =========================工具函数=================================
%% 根据规则读取erl文件，按顺序返回
merge_rule_erl_file([], List) -> List;
merge_rule_erl_file([Rule | T], List) ->
    FileList = filelib:wildcard(Rule ++ ".erl"),
    merge_rule_erl_file(T, List ++ FileList).

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

%% =========================文件以及目录==============================
%% 源文件的编译记录, 格式为：{File, Time}
get_meta_config_filename() ->
    filename:absname(get_meta_dir() ++ "/" ++ erlang:atom_to_list(get_meta_config_ets())).
get_meta_dir() ->
    case ets:lookup(easy_make, meta_dir) of
        [{_, Dir}] -> Dir;
        _ -> filename:absname("./script")
    end.

%% emakefile的路径
get_emakefile() ->
    case ets:lookup(easy_make, emakefile) of
        [{_, Dir}] -> Dir;
        _ -> filename:absname("./emakefile")
    end.

%% 记录源文件时间的ets
get_meta_config_ets() ->
    case ets:lookup(easy_make, meta_ets_file) of
        [{_, Atom}] -> Atom;
        _ -> ets_meta_config
    end.

%% 依赖的apps目录
get_lib_app_dirs() ->
    case ets:lookup(easy_make, app_dirs) of
        [{_, List}] -> [filename:absname(Dir) || Dir <- List];
        _ -> []
    end.

%% ebin目录
get_bin_dir() ->
    filename:absname("./ebin").