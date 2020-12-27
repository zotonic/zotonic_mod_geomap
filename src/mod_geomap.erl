%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2019 Marc Worrell
%% @doc Geo mapping support using OpenStreetMaps and GoogleMaps

%% Copyright 2012-2019 Marc Worrell
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

-module(mod_geomap).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("GeoMap services").
-mod_description("Maps, mapping, geocoding and geo calculations.").
-mod_prio(520).
-mod_depends([mod_l10n]).

-export([
    event/2,

    observe_rsc_get/3,
    observe_search_query/2,
    observe_pivot_update/3,
    observe_pivot_fields/3,

    observe_postback_notify/2,

    find_geocode/3,
    find_geocode_api/3,

    openstreetmap/2,
    googlemaps/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


observe_postback_notify(#postback_notify{ message="geomap_cluster", target=TargetId }, Context) ->
    Ids = [ z_context:get_q(<<"id">>, Context) | z_context:get_q(<<"ids">>, Context, []) ],
    Ids1 = [ m_rsc:rid(Id, Context) || Id <- Ids ],
    Ids2 = lists:filter(fun (Id) -> is_integer(Id) andalso m_rsc:is_visible(Id, Context) end,
                        Ids1),
    z_render:update(TargetId, #render{template="_geomap_popup_cluster.tpl", vars=[{ids, Ids2}]}, Context);
observe_postback_notify(_, _Context) ->
    undefined.

%% @doc Popup the geomap information
event(#postback_notify{ message = <<"geomap_popup">>, target = TargetId }, Context) ->
    Ids = [ z_context:get_q(<<"id">>, Context) | z_context:get_q(<<"ids">>, Context, []) ],
    Ids1 = [ m_rsc:rid(Id, Context) || Id <- Ids ],
    Ids2 = lists:filter(fun (Id) -> is_integer(Id) andalso m_rsc:is_visible(Id, Context) end,
                        Ids1),
    z_render:update(TargetId, #render{template="_geomap_popup.tpl", vars=[{ids, Ids2}]}, Context);

%% @doc Handle an address lookup from the admin.
event(#postback_notify{ message = <<"address_lookup">> }, Context) ->
    %% TODO: Maybe add check if the user is allowed to use the admin.
    {ok, Type, Q} = q( #{
            <<"address_street_1">> => z_context:get_q(<<"street">>, Context),
            <<"address_city">> => z_context:get_q(<<"city">>, Context),
            <<"address_state">> => z_context:get_q(<<"state">>, Context),
            <<"address_postcode">> => z_context:get_q(<<"postcode">>, Context),
            <<"address_country">> => z_context:get_q(<<"country">>, Context)
        }, Context),
    case find_geocode(Q, Type, Context) of
        {error, _} ->
            z_render:wire({script, [ {script, <<"map_mark_location_error();">>} ]}, Context);
        {ok, {Lat, Long}} ->
            z_render:wire({script, [ {script, io_lib:format("map_mark_location(~p,~p, 'lookup');", [Long, Lat])} ]}, Context)
    end.

%% @doc Append computed latitude and longitude values to the resource.
observe_rsc_get(#rsc_get{}, Props, _Context) ->
    case maps:get(<<"pivot_geocode">>, Props, undefined) of
        undefined ->
            Lat = maps:get(<<"location_lat">>, Props, undefined),
            Long = maps:get(<<"location_lng">>, Props, undefined),
            case is_number(Lat) andalso is_number(Long) of
                true ->
                    Props#{
                        <<"computed_location_lat">> => Lat,
                        <<"computed_location_lng">> => Long
                    };
                false ->
                    Props
            end;
        Quadtile ->
            {Lat, Long} = geomap_quadtile:decode(Quadtile),
            Props#{
                <<"computed_location_lat">> => Lat,
                <<"computed_location_lng">> => Long
            }
    end.

observe_search_query(#search_query{}=Q, Context) ->
    geomap_search:search_query(Q, Context).

%% @doc Check if the latitude/longitude are set, if so the pivot the pivot_geocode.
%%      If not then try to derive the lat/long from the rsc's address data.
observe_pivot_update(#pivot_update{}, KVs, _Context) ->
    case {catch z_convert:to_float(maps:get(<<"location_lat">>, KVs, undefined)),
          catch z_convert:to_float(maps:get(<<"location_lng">>, KVs, undefined))}
    of
        {Lat, Long} when is_float(Lat), is_float(Long) ->
            KVs#{
                <<"pivot_geocode">> => geomap_quadtile:encode(Lat, Long),
                <<"pivot_geocode_qhash">> => undefined
            };
        _ ->
            case z_utils:is_empty(maps:get(<<"address_country">>, KVs, undefined)) of
                true ->
                    KVs#{
                        <<"pivot_geocode">> => undefined,
                        <<"pivot_geocode_qhash">> => undefined
                    };
                false ->
                    KVs
            end
    end.


%% @doc Check if the latitude/longitude are set, if so then pivot the pivot_geocode.
%%      If not then try to derive the lat/long from the rsc's address data.
observe_pivot_fields(#pivot_fields{ raw_props = R }, PivotFields, Context) ->
    case {has_geoloc(R), has_pivot_geoloc(PivotFields)} of
        {true, _} ->
            % Directly derive from the hard coded location
            Lat = z_convert:to_float(maps:get(<<"location_lat">>, R)),
            Long = z_convert:to_float(maps:get(<<"location_lng">>, R)),
            PivotFields#{
                <<"pivot_geocode">> => geomap_quadtile:encode(Lat, Long),
                <<"pivot_geocode_qhash">> => undefined,
                <<"pivot_location_lat">> => Lat,
                <<"pivot_location_lng">> => Long
            };
        {false, true} ->
            % Some other module derived a pivot location - keep that location
            Lat = z_convert:to_float(maps:get(<<"pivot_location_lat">>, R)),
            Long = z_convert:to_float(maps:get(<<"pivot_location_lng">>, R)),
            PivotFields#{
                <<"pivot_geocode">> => geomap_quadtile:encode(Lat, Long),
                <<"pivot_geocode_qhash">> => undefined
            };
        {false, false} ->
            % Optionally geocode the address in the resource.
            case optional_geocode(R, Context) of
                reset ->
                    PivotFields#{
                        <<"pivot_geocode">> => undefined,
                        <<"pivot_geocode_qhash">> => undefined,
                        <<"pivot_location_lat">> => undefined,
                        <<"pivot_location_lng">> => undefined
                    };
                {ok, Lat, Long, QHash} ->
                    PivotFields#{
                        <<"pivot_geocode">> => geomap_quadtile:encode(Lat, Long),
                        <<"pivot_geocode_qhash">> => QHash,
                        <<"pivot_location_lat">> => Lat,
                        <<"pivot_location_lng">> => Long
                    };
                ok ->
                    PivotFields
            end
    end.


has_geoloc(#{ <<"location_lat">> := Lat, <<"location_lng">> := Lng }) ->
    is_numerical(Lat) andalso is_numerical(Lng);
has_geoloc(_) ->
    false.

has_pivot_geoloc(#{ <<"pivot_location_lat">> := Lat, <<"pivot_location_lng">> := Lng }) ->
    is_numerical(Lat) andalso is_numerical(Lng);
has_pivot_geoloc(_) ->
    false.

is_numerical(N) when is_number(N) -> true;
is_numerical(undefined) -> false;
is_numerical(<<>>) -> false;
is_numerical(N) ->
    try
        is_number( z_convert:to_float(N) )
    catch
        _:_ -> false
    end.



%% @doc Check if we should lookup the location belonging to the resource.
%%      If so we store the quadtile code into the resource without a re-pivot.
optional_geocode(R, Context) ->
    %% TODO: use the sha(qhash) to check known locations, this prevents multiple lookups
    %%       for the same address. (need to be placed in separate lookup table, so
    %%       that we can refresh after some time).
    Lat = maps:get(<<"location_lat">>, R, undefined),
    Long = maps:get(<<"location_long">>, R, undefined),
    case z_utils:is_empty(Lat) andalso z_utils:is_empty(Long) of
        false ->
            reset;
        true ->
            case q(R, Context) of
                {ok, _, <<>>} ->
                    reset;
                {ok, Type, Q} ->
                    LocHash = crypto:hash(md5, Q),
                    case maps:get(<<"pivot_geocode_qhash">>, R, undefined) of
                        LocHash ->
                            % Not changed since last lookup
                            ok;
                        _ ->
                            % Changed, and we are doing automatic lookups
                            case find_geocode(Q, Type, Context) of
                                {error, _} ->
                                    reset;
                                {ok, {NewLat,NewLong}} ->
                                    {ok, NewLat, NewLong, LocHash}
                            end
                    end
            end
    end.


find_geocode(Q, Type, Context) ->
    case geomap_precoded:find_geocode(Q, Type) of
        {ok, {_, _}} = OK ->
            OK;
        {error, not_found} ->
            Q1 = maybe_expand_country(Q, Type, Context),
            find_geocode_api(Q1, Type, Context)
    end.

%% @doc Check with Google and OpenStreetMap if they know the address
%% TODO: cache the lookup result (max 1 req/sec for Nominatim)
find_geocode_api(<<>>, _Type, _Context) ->
    {error, not_found};
find_geocode_api(Q, country, Context) ->
    Qq = z_url:url_encode(Q),
    openstreetmap(Qq, Context);
find_geocode_api(Q, _Type, Context) ->
    Qq = z_url:url_encode(Q),
    case googlemaps_check(Qq, Context) of
        {error, _} ->
            openstreetmap(Qq, Context);
        {ok, {_Lat, _Long}} = Ok->
            Ok
    end.

openstreetmap(Q, Context) ->
    Url = "https://nominatim.openstreetmap.org/search?format=json&limit=1&addressdetails=0&q="
        ++ z_convert:to_list(Q),
    case get_json(Url, Context) of
        {ok, [ #{ <<"lat">> := LatText, <<"lon">> := LonText } | _ ] } ->
            case {z_convert:to_float(LatText), z_convert:to_float(LonText)} of
                {Lat, Lng} when is_float(Lat), is_float(Lng) ->
                    {ok, {Lat, Lng}};
                _ ->
                    {error, not_found}
            end;
        {ok, []} ->
            {error, not_found};
        {ok, JSON} ->
            lager:error("OpenStreetMap unknown JSON ~p on ~p", [JSON, Q]),
            {error, unexpected_result};
        {error, Reason} = Error ->
            lager:warning("OpenStreetMap returns ~p for ~p", [Reason, Q]),
            Error
    end.

googlemaps_check(Q, Context) ->
    case z_depcache:get(googlemaps_error, Context) of
        undefined ->
            case googlemaps(Q, Context) of
                {error, ratelimit} = Error ->
                    lager:warning("Geomap: Google reached query limit, disabling for 900 sec"),
                    z_depcache:set(googlemaps_error, Error, 900, Context),
                    Error;
                {error, denied} = Error ->
                    lager:warning("Geomap: Google denied the request, disabling for 3600 sec"),
                    z_depcache:set(googlemaps_error, Error, 3600, Context),
                    Error;
                Result ->
                    Result
            end;
        {ok, Error} ->
            lager:debug("Geomap: skipping Google lookup due to ~p", [Error]),
            Error
    end.

googlemaps(Q, Context) ->
    googlemaps(m_config:get_value(mod_geomap, google_api_key, Context), Q, Context).

googlemaps(undefined, _Q, _Context) ->
    {error, apikey};
googlemaps(<<>>, _Q, _Context) ->
    {error, apikey};
googlemaps("", _Q, _Context) ->
    {error, apikey};
googlemaps(ApiKey, Q, Context) ->
    Url = "https://maps.googleapis.com/maps/api/geocode/json?address="
        ++ z_convert:to_list(Q)
        ++ "&key=" ++ z_convert:to_list(ApiKey),
    case get_json(Url, Context) of
        {ok, #{ <<"status">> := <<"OK">> } = Props } ->
            case maps:get(<<"results">>, Props) of
                [ Result ] ->
                    case maps:get(<<"geometry">>, Result, null) of
                        null ->
                            lager:info("Google maps result without geometry: ~p", [Props]),
                            {error, no_result};
                        #{ <<"location">> := Ls } ->
                            case {z_convert:to_float(maps:get(<<"lat">>, Ls, undefined)),
                                  z_convert:to_float(maps:get(<<"lng">>, Ls, undefined))}
                            of
                                {Lat, Long} when is_float(Lat), is_float(Long) ->
                                    {ok, {Lat, Long}};
                                _ ->
                                    {error, not_found}
                            end;
                        _ ->
                            lager:info("Google maps geometry without location: ~p", [Props]),
                            {error, no_result}
                    end;
                [] ->
                    lager:info("Google maps result without results: ~p", [Props]),
                    {error, no_result}
            end;
        {ok, #{ <<"status">> := <<"ZERO_RESULTS">> } } ->
            {error, not_found};
        {ok, #{ <<"status">> := <<"OVER_QUERY_LIMIT">> } = Props } ->
            lager:info("GoogleMaps api error: 'OVER_QUERY_LIMIT', message is ~p",
                       [ maps:get(<<"error_message">>, Props, <<>>) ]),
            {error, ratelimit};
        {ok, #{ <<"status">> := <<"REQUEST_DENIED">> } = Props } ->
            lager:warning("GoogleMaps api error: 'REQUEST_DENIED', message is ~p",
                          [ maps:get(<<"error_message">>, Props, <<>>) ]),
            {error, denied};
        {ok, #{ <<"status">> := Status } } ->
            lager:warning("Google maps status ~p on ~p", [Status, Q]),
            {error, unexpected_result};
        {ok, JSON} ->
            lager:error("Google maps unknown JSON ~p on ~p", [JSON, Q]),
            {error, unexpected_result};
        {error, Reason} = Error ->
            lager:warning("Google maps returns ~p on ~p", [Reason, Q]),
            Error
    end.


get_json(Url, Context) ->
    Hs = [
        {"Referer", z_convert:to_list(z_context:abs_url("/", Context))},
        {"User-Agent", "Zotonic"}
    ],
    case httpc:request(
            get,
            {Url, Hs},
            [ {autoredirect, true}, {relaxed, true}, {timeout, 10000} ],
            [ {body_format, binary} ])
    of
        {ok, {
            {_HTTP, 200, _OK},
            Headers,
            Body
        }} ->
            case proplists:get_value("content-type", Headers) of
                "application/json" ++ _ ->
                    try
                        {ok, z_json:decode(Body)}
                    catch
                        _:_ -> {error, json}
                    end;
                CT ->
                    {error, {unexpected_content_type, CT}}
            end;
        {ok, {{_, 503, _}, _, _}} ->
            {error, no_service};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, Other} ->
            lager:warning("Unexpected result from ~p: ~p",
                          [Url, Other]),
            {error, unexpected_result};
        {error, _Reason} = Err ->
            Err
    end.


q(R, Context) ->
    case iolist_to_binary(p(<<"address_country">>, <<>>, R)) of
        <<>> ->
            {ok, country, <<>>};
        Country ->
            Fs = iolist_to_binary([
                p(<<"address_street_1">>, $,, R),
                p(<<"address_city">>, $,, R),
                p(<<"address_state">>, $,, R),
                remove_ws(p(<<"address_postcode">>, $,, R))
            ]),
            case Fs of
                <<>> ->
                    {ok, country, Country};
                _ ->
                    Country1 = iolist_to_binary(country_name(Country, Context)),
                    {ok, full, <<Fs/binary, Country1/binary>>}
            end
    end.

remove_ws(V) ->
    binary:replace( iolist_to_binary(V), <<" ">>, <<>>, [global] ).

p(F, Sep, R) ->
    case maps:get(F, R, undefined) of
        <<>> -> <<>>;
        V when is_binary(V) -> [V, Sep];
        _ -> <<>>
    end.

maybe_expand_country(Country, country, Context) ->
    country_name(Country, Context);
maybe_expand_country(Address, full, _Context) ->
    Address.

country_name(undefined, _Context) -> <<>>;
country_name("", _Context) -> <<>>;
country_name(<<>>, _Context) -> <<>>;
country_name(<<"gb-nir">>, _Context) -> <<"Northern Ireland">>;
country_name(Iso, Context) ->
    m_l10n:country_name(Iso, en, Context).

