-module(hcr_deploy).

-export([reload_app/0,
         modules_for_app/1,
         reload_module/1,
         reload_module/2,
         add_paths/1,
         modified_deps/0,
         is_latest_version/1,
         module_modified/1,
         find_module_file/1]).


reload_app() ->
    ModifiedModules = lists:filter(fun module_modified/1, modules_for_app(hot_code_reload)),
    [] = [M || M <- ModifiedModules, code:soft_purge(M) =:= false],
    ok = add_paths("deps"),
    [code:load_file(M) || M <- ModifiedModules].


modules_for_app(App) when App =:= hot_code_reload ->
    case code:where_is_file(atom_to_list(App) ++ ".app") of
        {error, enoent} ->
            {error, no_app_file};
        Path ->
            case file:consult(Path) of
                {ok, [{application, App, Props}]} ->
                    proplists:get_value(modules, Props, []);
                {error, Error} ->
                    {error, Error}
            end
    end.


reload_module(M) ->
    code:soft_purge(M) andalso code:load_file(M).

reload_module(M, force) ->
    code:purge(M),
    code:load_file(M).


add_paths(BaseDir) ->
    case file:list_dir(BaseDir) of
        {ok, Filenames} ->
            lists:foreach(fun (Dep) ->
                              true == code:add_path(
                                          filename:join([BaseDir, Dep, "ebin"]))
                          end, Filenames),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc: List loaded modules from deps/ that have changed on disk,
%% will not notice new beams that have not yet been loaded.
modified_deps() ->
    {DepModules, _} = lists:unzip(lists:filter(
                        fun ({_, Beam}) ->
                                is_list(Beam) andalso
                                    lists:prefix(filename:absname("deps"), Beam)
                        end,
                        code:all_loaded())),
    lists:filter(fun (Module) -> not is_latest_version(Module) end, DepModules).

%% @doc: Check loaded vsn versus vsn on disk, do this since compile times changes
%% frequently for our deps even if source has not (because of delete-deps).
is_latest_version(Module) ->
    case code:get_object_code(Module) of
        {Module, _ObjectCode, Beam} ->
            {ok, {Module, VsnDisk}} = beam_lib:version(Beam),
            %% Can't use object code since it is read from disk
            VsnLoaded = proplists:get_value(vsn, Module:module_info(attributes)),
            VsnLoaded =:= VsnDisk;
        error ->
            %% Module not loaded
            false
    end.


%%
%% Snipped from Wings3d

module_modified(Module) ->
    case code:is_loaded(Module) of
        {file, preloaded} ->
            false;
        {file, Path} ->
            CompileOpts = proplists:get_value(compile, Module:module_info()),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            module_modified(Path, CompileTime, Src);
        _ ->
            true %% consider new modules to be modified so they get loaded
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
        false ->
            false;
        ModPath ->
            {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
            CompileOpts =  binary_to_term(CB),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            not ((CompileTime == PrevCompileTime) and (Src == PrevSrc))
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
        {ok, _} ->
            Path;
        _ ->
            %% may be the path was changed?
            case code:where_is_file(filename:basename(Path)) of
                non_existing ->
                    false;
                NewPath ->
                    NewPath
            end
    end.
