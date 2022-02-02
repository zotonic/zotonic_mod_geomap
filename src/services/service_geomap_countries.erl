%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2015 Marc Worrell
%% @doc Return the JSON for the country overview.

%% Copyright 2012-2015 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(service_geomap_countries).
-author("Marc Worrell <marc@worrell.nl>").

-svc_title("JSON for the country map, colours and country id per country.").
-svc_needauth(false).

-export([process_get/2]).

-export([
    get_data/1,
    get_country_coords/1
]).

-define(COUNT, 100).

-include_lib("zotonic_core/include/zotonic.hrl").

process_get(_ReqData, Context) ->
    {binary_json, get_data(Context)}.

get_data(Context) ->
    Coords = get_country_coords(Context),
    Ids = get_countries(Context),
    Data = lists:foldl(fun(Id, Acc) ->
                            case z_acl:rsc_visible(Id, Context) of
                                true ->
                                    case maybe_map_iso(m_rsc:p(Id, address_country, Context)) of
                                        undefined ->
                                            Acc;
                                        Iso ->
                                            [{Iso, {
                                                    Id,
                                                    z_trans:lookup_fallback(m_rsc:p(Id, title, Context), Context),
                                                    m_rsc:p(Id, map_color, Context), 
                                                    m_rsc:p(Id, map_value, Context)
                                                }
                                            } | Acc]
                                    end;
                                false ->
                                    Acc
                            end
                        end,
                        [],
                        Ids),
    set_values(Coords, Data).


get_countries(Context) ->
    case m_category:name_to_id(country, Context) of
        {error, _} -> 
            [];
        {ok, _} ->
            #search_result{result=R} = z_search:search({latest, [{cat, country}]}, {1,1000}, Context),
            R
    end.

get_country_coords(Context) ->
    {ok, #module_index{} = M} = z_module_indexer:find(lib, "data/internet_users_2005_choropleth_lowres.json", Context),
    {ok, Data} = file:read_file(z_convert:to_list(M#module_index.filepath)),
    mochijson:binary_decode(Data).


set_values({struct, [{<<"type">>,<<"FeatureCollection">>}, {<<"features">>, Cs}]}, Data) ->
    {struct, [
        {<<"type">>,<<"FeatureCollection">>}, 
        {<<"features">>, 
            lists:foldl(fun(C, Acc) -> 
                            {ok, C1} = set_value(C, Data),
                            [C1|Acc]
                        end,
                        [],
                        Cs)
    }]}.

set_value({struct, Fs}, Data) ->
    {struct, Properties} = proplists:get_value(<<"properties">>, Fs),
    Name = proplists:get_value(<<"name">>, Properties),
    P1 = case l10n_country2iso:country2iso(Name) of
        undefined when Name =/= <<>> ->
            ?LOG_INFO("[mod_geomap] country ~p is unknown - not mapped to an iso code.", [Name]),
            {<<"properties">>,
               {struct,[{<<"name">>,Name},
                        {<<"value">>,<<"">>},
                        {<<"colour">>,<<"#ccc">>}
                ]}
            };
        undefined ->
            {<<"properties">>,
               {struct,[{<<"name">>,Name},
                        {<<"value">>,<<"">>},
                        {<<"colour">>,<<"#ccc">>}
                ]}
            };
        Iso ->
            case proplists:get_all_values(Iso, Data) of
                [] ->
                    {<<"properties">>,
                       {struct,[{<<"name">>,l10n_iso2country:iso2country(Iso)},
                                {<<"value">>,<<"">>},
                                {<<"colour">>,<<"#ccc">>}
                        ]}
                    };
                Cs ->
                    [{_RscId, _Title, Colour, Value}|_] = Cs,
                    Titles = [ element(2,C) || C <- Cs ],
                    RscIds = [ element(1,C) || C <- Cs ],
                    Title = iolist_to_binary(lists:join(<<"<br/>">>, Titles)),
                    {<<"properties">>,
                       {struct,[{<<"rsc_ids">>, RscIds},
                                {<<"name">>, Title},
                                {<<"value">>,value(Value)},
                                {<<"colour">>, color(Colour)}
                        ]}
                    }
            end
    end,
    {ok, {struct, [ P1 | proplists:delete(<<"properties">>, Fs) ]}}.


% Some countries don't have a map representation. Combine them with a close other country.
maybe_map_iso(<<"sx">>) -> <<"mf">>;       % Saint Martin - other half of the island
maybe_map_iso(Iso) -> Iso.


color(undefined) -> <<"#ccc">>;
color(<<>>) -> <<"#ccc">>;
color(C) -> C.

value(undefined) -> <<"">>;
value(V) -> V.

