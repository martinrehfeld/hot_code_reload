-module(hcr_config).

-export([incr1/1]).

incr1({model, _} = M) -> hcr_model:v1(M).
