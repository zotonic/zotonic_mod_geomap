%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2023 Marc Worrell
%% @doc GeoMap model, expose functions and configurations as an API.

%% Copyright 2012-2023 Marc Worrell
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

-module(m_geomap).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "model", "geolocation", "configuration",
        "api_and_integration"
    ]
}).
-moduledoc("
Expose GeoMap configuration and country-map data to templates and model GET
requests.

## Model paths

| Path | Result |
| --- | --- |
| `provider` | Configured map provider. |
| `google_api_key` | Configured Google Maps API key. |
| `location_lat` | Configured initial latitude. |
| `location_lng` | Configured initial longitude. |
| `zoomlevel` | Configured initial zoom. |
| `countries` | Country GeoJSON enriched with visible country resources. |
| `nearby` | Empty map; resource lookup is not implemented on this path. |
| `locations` | Empty map; resource lookup is not implemented on this path. |

Configuration reads return the stored `mod_geomap` value without a model-level
fallback. Templates choose their own defaults, for example:

```django
{{ m.geomap.location_lat|default:0 }}
```

These reads have no administrator-only ACL check, including `google_api_key`.
Unknown paths return `{error, unknown_path}`. GET model callbacks return
`{ok, {Value, Rest}}`, retaining unused path segments.

## Country data

`countries` reads `priv/data/internet_users_2005_choropleth_lowres.json` from the
`zotonic_mod_geomap` application. The decoded geometry is cached in the site's
depcache for one day, or until the cache is flushed. Resource values and
visibility checks are applied separately on every request. No copy in the site
directory is needed. After replacing the data file, flush the site's depcache
to load the new geometry immediately.

The model searches up to 1,000 resources in category `country`, checks their
visibility, and groups them by `address_country`. It combines their IDs and
titles with `map_color` and `map_value` for matching country features. Missing
colors default to `#ccc`, and missing values to an empty binary. If the category
does not exist, no resource values are added. Sint Maarten's `sx` country code
is grouped with Saint Martin's `mf` geometry.

For nearby resource queries, use the `geo_nearby` search supplied by `mod_geomap`;
the `nearby` and `locations` model paths do not implement that search.
").

-behaviour(zotonic_model).

-export([
    m_get/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_get([ <<"countries">> | Rest ], _Msg, Context) ->
    {ok, {get_countries_json(Context), Rest}};
m_get([ <<"locations">> | Rest ], _Msg, Context) ->
    {ok, {get_locations(Context), Rest}};
m_get([ <<"nearby">> | Rest ], _Msg, Context) ->
    {ok, {get_nearby(Context), Rest}};
m_get([ <<"location_lng">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_geomap, location_lng, Context), Rest}};
m_get([ <<"location_lat">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_geomap, location_lat, Context), Rest}};
m_get([ <<"zoomlevel">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_geomap, zoomlevel, Context), Rest}};
m_get([ <<"provider">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_geomap, provider, Context), Rest}};
m_get([ <<"google_api_key">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_geomap, google_api_key, Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.



%% --------------------------------------------------------------------------------------------------------------------

get_nearby(Context) ->
    #{}.

%% --------------------------------------------------------------------------------------------------------------------


get_locations(Context) ->
    #{}.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Return a JSON with country coordinates and color/value coding.
%% Geometry is cached from the module's priv/data; resource values are added per request.
-spec get_countries_json( z:context() ) -> map().
get_countries_json(Context) ->
    Coords = get_country_coords(Context),
    Ids = get_countries(Context),
    Data = lists:foldl(fun(Id, Acc) ->
                            case z_acl:rsc_visible(Id, Context) of
                                true ->
                                    case maybe_map_iso(m_rsc:p(Id, address_country, Context)) of
                                        undefined ->
                                            Acc;
                                        Iso ->
                                            Acc#{
                                                Iso => [
                                                    {
                                                        Id,
                                                        z_trans:lookup_fallback(m_rsc:p(Id, title, Context), Context),
                                                        m_rsc:p(Id, map_color, Context),
                                                        m_rsc:p(Id, map_value, Context)
                                                    }
                                                    | maps:get(Iso, Acc, [])
                                                ]
                                            }
                                    end;
                                false ->
                                    Acc
                            end
                        end,
                        #{},
                        Ids),
    set_countries_values(Coords, Data).


get_countries(Context) ->
    case m_category:name_to_id(country, Context) of
        {error, _} ->
            [];
        {ok, _} ->
            #search_result{result=R} = z_search:search({latest, [{cat, country}]}, {1,1000}, Context),
            R
    end.

%% @doc Return the base json coordinates for countries
-spec get_country_coords( z:context() ) -> map().
get_country_coords(Context) ->
    z_depcache:memo(
        fun() ->
            JsonFile = filename:join([
                code:priv_dir(zotonic_mod_geomap),
                "data",
                "internet_users_2005_choropleth_lowres.json"
            ]),
            {ok, Data} = file:read_file(JsonFile),
            #{ <<"type">> := <<"FeatureCollection">>, <<"features">> := _ } = Coords = z_json:decode(Data),
            Coords
        end,
        {?MODULE, country_coords},
        ?DAY,
        Context).


set_countries_values(#{ <<"type">> := <<"FeatureCollection">>, <<"features">> := Cs } = Map, Data) ->
    Map#{
        <<"type">> => <<"FeatureCollection">>,
        <<"features">> =>
            lists:foldl(fun(C, Acc) ->
                            [ set_countries_value(C, Data) | Acc ]
                        end,
                        [],
                        Cs)
    }.

set_countries_value(#{ <<"properties">> := Properties } = Feature, Data) ->
    Name = maps:get(<<"name">>, Properties),
    case l10n_country2iso:country2iso(Name) of
        undefined when Name =/= <<>> ->
            ?LOG_INFO("[mod_geomap] country ~p is unknown - not mapped to an iso code.", [Name]),
            Feature#{
                <<"properties">> => #{
                    <<"name">> => Name,
                    <<"value">> => <<>>,
                    <<"colour">> => <<"#ccc">>
                }
            };
        undefined ->
            Feature#{
                <<"properties">> => #{
                    <<"name">> => Name,
                    <<"value">> => <<>>,
                    <<"colour">> => <<"#ccc">>
                }
            };
        Iso ->
            case maps:find(Iso, Data) of
                {ok, [ {_RscId, Title, Colour, Value} | _ ] = Cs} ->
                    Titles = [ T || {_, T, _, _} <- Cs ],
                    RscIds = [ Id || {Id, _, _, _} <- Cs ],
                    Title = iolist_to_binary(lists:join(<<"<br/>">>, Titles)),
                    Feature#{
                        <<"properties">> => #{
                            <<"rsc_ids">> => RscIds,
                            <<"name">> => Title,
                            <<"value">> => value(Value),
                            <<"colour">> => color(Colour)
                        }
                    };
                error ->
                    Feature#{
                        <<"name">> => l10n_iso2country:iso2country(Iso),
                        <<"value">> => <<>>,
                        <<"colour">> => <<"#ccc">>
                    }
            end
    end.


% Some countries don't have a map representation. Combine them with a close other country.
maybe_map_iso(<<"sx">>) -> <<"mf">>;       % Saint Martin - other half of the island
maybe_map_iso(Iso) -> Iso.

color(undefined) -> <<"#ccc">>;
color(<<>>) -> <<"#ccc">>;
color(C) -> C.

value(undefined) -> <<>>;
value(V) -> V.
