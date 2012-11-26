%%
%% Example model types, mentally substitute "hcr_model" for e.g. g8_tree,
%% hcr_config for g8_tree_config and hcr_model for g8_tree
%%

-type model() :: {hcr_model, hcr_config:type(), [hcr_model:property()]}.
-type any_model() :: model(). % | another_model() | ...

%% example composite state, e.g. User
-type state_property() :: {model, model()}. % | {another_model, another_model()} | ...
-type state() :: [state_property()].


%% Helper macros for specifying model getters/setters
-define(ATTR_READER(Ps),   attr_reader() -> Ps).
-define(ATTR_WRITER(Ps),   attr_writer() -> Ps).
-define(ATTR_ACCESSOR(Ps), attr_accessor() -> Ps).
