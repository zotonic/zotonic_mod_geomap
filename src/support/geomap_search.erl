%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014-2025 Arjan Scherpenisse
%% @doc Geo search functions
%% @end

%% Copyright 2014-2025 Arjan Scherpenisse
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

-module(geomap_search).

-export([search_query/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(DEFAULT_DISTANCE, 10.0).

%% @doc Geo-related searches
search_query(#search_query{
            name = <<"geo_nearby">>,
            args = #{
                <<"q">> := Terms
            }
        }, Context) ->
    Cat = term(<<"cat">>, Terms, []),
    Distance = z_convert:to_float(term(<<"distance">>, Terms, ?DEFAULT_DISTANCE)),
    Lat = term(<<"latitude">>, Terms, undefined),
    Lng = term(<<"longitude">>, Terms, undefined),
    Id = term(<<"id">>, Terms, undefined),
    query(Cat, Distance, Lat, Lng, Id, Context);
search_query(#search_query{ search={geo_nearby, Args} }, Context) ->
    % Old search format
    Cats = proplists:get_all_values(cat, Args),
    Distance = z_convert:to_float(proplists:get_value(distance, Args, ?DEFAULT_DISTANCE)),
    Lat = proplists:get_value(latitude, Args),
    Lng = proplists:get_value(longitude, Args),
    Id = proplists:get_value(id, Args),
    query(Cats, Distance, Lat, Lng, Id, Context);
search_query(_Q, _Context) ->
    undefined. %% fall through

term(_Name, [], Default) ->
    Default;
term(Name, [#{ <<"term">> := T, <<"value">> := V } | _ ], _Default) when T =:= Name ->
    V;
term(Name, [_|Ts], Default) ->
    term(Name, Ts, Default).


query(undefined, Distance, Lat, Lng, Id, Context) ->
    query([], Distance, Lat, Lng, Id, Context);
query(Cat, Distance, Lat, Lng, Id, Context) ->
    Args1 = if
        Cat =:= [] -> [];
        Cat =:= undefined -> [];
        is_list(Cat) -> [{"r", Cat}];
        true -> [{"r", [Cat]}]
    end,
    case get_query_center(Lat, Lng, Id, Context) of
        {ok, {LatC, LngC}} ->
            {LatMin, LngMin, LatMax, LngMax} = geomap_calculations:get_lat_lng_bounds(LatC, LngC, Distance),
            #search_sql{
                select = "r.id",
                from = "rsc r",
                where = "$1 < pivot_location_lat AND $2 < pivot_location_lng AND pivot_location_lat < $3 AND pivot_location_lng < $4",
                cats = Args1,
                order = "(pivot_location_lat-$5)*(pivot_location_lat-$5) + (pivot_location_lng-$6)*(pivot_location_lng-$6), id",
                args = [ LatMin, LngMin, LatMax, LngMax, LatC, LngC ],
                tables = [ {rsc,"r"} ]
              };
        {error, Reason} ->
            ?LOG_WARNING(#{
                in => zotonic_mod_geomap,
                text => <<"Error in geo_nearby query">>,
                result => error,
                reason => Reason,
                cat => Cat,
                distance => Distance,
                latitude => Lat,
                longitude => Lng,
                id => Id
            }),
            undefined
    end.

get_query_center(undefined, undefined, undefined, _Context) ->
    {error, missing_geo_search_parameters};
get_query_center(Lat, Lng, _Id, _Context) when Lat =/= undefined; Lng =/= undefined ->
    try
        {ok, {z_convert:to_float(Lat), z_convert:to_float(Lng)}}
    catch
        _:_ ->
            {error, not_floats}
    end;
get_query_center(_Lat, _Lng, Id, Context) ->
    Lat = m_rsc:p(Id, <<"pivot_location_lat">>, Context),
    Lng = m_rsc:p(Id, <<"pivot_location_lng">>, Context),
    if
        is_float(Lat), is_float(Lng) -> {ok, {Lat, Lng}};
        true -> {error, rsc_without_location}
    end.
