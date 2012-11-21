%%
%% Example model types, mentally substitute "model" for e.g. g8_tree,
%% hcr_config for g8_tree_config and hcr_model for g8_tree
%%

-type model() :: {model, hcr_config:type(), [hcr_model:property(),...]}.
