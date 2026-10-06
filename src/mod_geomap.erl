%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2025 Marc Worrell
%% @doc Geo mapping support using OpenStreetMaps and GoogleMaps
%% @end

%% Copyright 2012-2025 Marc Worrell
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
-moduledoc(#{
    zotonic_keywords => [
        "reference", "site_administrator", "module", "geolocation", "search_and_discovery",
        "configuration", "api_and_integration"
    ]
}).
-moduledoc("
Add resource locations, map display, geocoding, and geographic searches to
Zotonic. Enable `mod_geomap` on the site; it depends on `mod_l10n`.

## Resource locations

The admin location editor stores `location_lat`, `location_lng`, and
`location_zoom_level`. Coordinates are latitude and longitude in degrees.
Resource pivoting updates `pivot_location_lat`, `pivot_location_lng`, and the
quadtile `pivot_geocode`. Explicit coordinates take precedence over a location
derived from address fields. An already supplied pivot location is retained
when no explicit coordinates are present.

Resource reads expose `computed_location_lat` and `computed_location_lng` from
the quadtile, or from numeric explicit coordinates when no quadtile is available.
Use these computed properties for map display and the `geomap_distance` filter.

Address geocoding uses the resource's street, city, state, postcode, and country.
Full address lookups try Google Maps and fall back to OpenStreetMap Nominatim;
country lookups can use the module's precoded country coordinates. Address
lookup can therefore send address information to an external service.

## Configuration

Set these per-site keys under `mod_geomap`:

| Key | Purpose |
| --- | --- |
| `is_auto_geocode` | Automatically use external geocoding for resource addresses; defaults to `true`. |
| `provider` | `googlemaps` selects Google Maps; otherwise the bundled map templates use OpenLayers. |
| `google_api_key` | Key used for Google geocoding and the browser Maps script. |
| `location_lat` | Initial latitude when the resource has no location; the admin template falls back to `0`. |
| `location_lng` | Initial longitude when the resource has no location; the admin template falls back to `0`. |
| `zoomlevel` | Initial zoom when no resource-specific zoom is available. |

The OpenLayers admin map defaults to zoom `2` without a location and to `15`
for a located resource without its own zoom setting. The static-map tag has its
own default zoom of `14`. The provider setting selects map presentation; it does
not change the geocoding fallback order. The Google key is exposed to map
scripts through `m.geomap.google_api_key` and is not a server-only secret.

Set `mod_geomap.is_auto_geocode` to `false` to prevent resource pivoting from
sending addresses to Google Maps or Nominatim. Existing coordinates are retained
when a lookup is skipped. Explicit coordinates and local precoded lookups still
work. The admin's **Set to entered address** button and explicit Erlang geocoding
calls remain available and can contact external services.

## Nearby search

The `geo_nearby` search accepts a resource `id` with pivoted coordinates or an
explicit `latitude` and `longitude` pair. `distance` is in kilometers and defaults
to `10`. Use `cat` to restrict resource categories.

```django
{% with m.search.geo_nearby::%{ id: id, distance: 10 } as results %}
    {% for location_id in results %}
        <a href=\"{{ location_id.page_url }}\">{{ location_id.title }}</a>
    {% endfor %}
{% endwith %}
```

The search selects a latitude/longitude bounding box and orders results by
squared coordinate differences from the center. It is an approximate nearby
search, not an exact circular-distance filter. The center resource is not
excluded. Missing or unusable center coordinates do not produce a valid query.
The normal Zotonic search pipeline applies resource visibility restrictions.

The `has_geo` search term selects resources with both pivot coordinates present;
`has_geo=false` selects resources missing either coordinate.

## Admin panels

Enabling the module adds a **Geolocation** panel to resource editing. It starts
collapsed and loads the map lazily. The category's **Show geo data on edit page**
feature controls its visibility; if unset, it follows **Show address**, defaulting
to enabled. Resources with explicit latitude or longitude also show the panel.

Expand the panel to edit latitude, longitude, and zoom level (`0` through `29`).
Click the map to select a location, or use these controls:

* **Set to current location** requests the browser's location, subject to the
  visitor's permission and browser availability.
* **Set to entered address** geocodes the address in the edit form. This button
  is hidden when the form has no address-country field.
* **Clear** removes the explicit coordinates from the form.
* **Reset** restores the resource's saved coordinates.

Save the resource to persist the edited values. The indexed coordinates shown
below the inputs are the computed location; pivoting updates the searchable
location after a save. Clearing explicit coordinates can allow address-derived
geocoding to determine the location again.

Country resources also receive a **World Map** panel with **Color** and **Value**
fields. These store `map_color` and `map_value` for the country data returned by
`m.geomap.countries`; they do not set the resource's coordinates.

## Showing a map

### Static tile map

Render a resource's computed location without interactive-map JavaScript:

```django
{% geomap_static id=id zoom=14 n=2 %}
```

Or pass explicit coordinates:

```django
{% geomap_static latitude=52.34322 longitude=4.33423 zoom=14 %}
```

The tag renders `_geomap_static.tpl`, a grid of OpenStreetMap tile images with a
marker. `n` defaults to two rows and columns; `rows` and `cols` can override each
dimension, and `size` sets the displayed tile size (default `256` pixels).
Override this template in the site to change its markup or styling.

For a single remotely rendered map image, include:

```django
{% include \"_geomap_static_simple.tpl\" id=id zoom=14 width=440 height=280 %}
```

This template uses the resource's computed location and the external
`staticmap.openstreetmap.de` image service. Its default image dimensions are
`220` by `220` pixels. Both static approaches require a usable location.

### Interactive OpenLayers map

The `do_geomap` widget supports panning, zooming, and markers. With the map
provider unset or set to `openlayers`, load `_js_geomap.tpl` after the site's
standard jQuery/Zotonic scripts and before widget initialization. Keep the
standard `{% all include \"_html_head.tpl\" %}` hook in the page head to load the
module's OpenLayers CSS. Include the map scripts once per page.

```django
{% include \"_js_geomap.tpl\" %}
<div class=\"do_geomap\" style=\"width: 100%; height: 400px;\"
     data-geomap='{\"location_lat\":52.34322,\"location_lng\":4.33423,\"zoom\":14,\"marker\":true}'>
</div>
```

The map container needs an explicit height. `location_lat` and `location_lng`
set its center, `zoom` sets the initial zoom, and `marker=true` adds a marker
there. For resource coordinates, read `id.computed_location_lat` and
`id.computed_location_lng`, check that they are numeric, and serialize the
options as JSON with HTML-attribute escaping rather than concatenating strings.

For multiple locations, supply a `locations` array of objects containing `id`,
`lat`, and `lng`, with optional `icon` and `data`. The OpenLayers widget clusters
nearby markers; `clusterDistance`, `clusterBgColor`, and `clusterColor` control
cluster presentation. Marker clicks invoke the `map_infobox` wired event with
IDs and associated data, which the site can handle to display details.

The older `_geomap.tpl` is experimental and uses an older OpenLayers API; use
the widget with the bundled current assets for new templates.

### Related components

`m.geomap.countries` reads the bundled
`priv/data/internet_users_2005_choropleth_lowres.json` from the
`zotonic_mod_geomap` application using `code:priv_dir/1`. No copy in the site
directory is needed. Decoded geometry is cached per site for one day, or until
the site's depcache is flushed. Resource values and visibility checks are
applied on every request. Flush the cache after replacing the data file to
load the new geometry immediately.

Use `geomap_distance` for distances between resource locations or coordinate
maps. `m.geomap` exposes map configuration and country-map data. Its `nearby` and
`locations` paths currently return empty maps; use `geo_nearby` for resource
searches instead of relying on the legacy service descriptions in the README.

The module observes resource reads, pivot updates/fields, search queries and
terms, and map popup postbacks. Popup resource lists are filtered for visibility.
").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("GeoMap services").
-mod_description("Maps, mapping, geocoding and geo calculations.").
-mod_prio(520).
-mod_depends([mod_l10n]).

-mod_config([
    #{
        key => is_auto_geocode,
        type => boolean,
        default => true,
        description => "Automatically look up resource addresses using external geocoding services. "
                       "Disable to keep automatic lookups local; manual lookups remain available."
    },
    #{
        key => provider,
        type => string,
        default => "openlayers",
        description => "Map presentation provider: openlayers or googlemaps. "
                       "Does not change the geocoding services used for address lookups."
    },
    #{
        key => google_api_key,
        type => string,
        default => undefined,
        description => "Google Maps API key for server-side geocoding and browser maps. "
                       "Exposed to browser scripts through m.geomap.google_api_key."
    },
    #{
        key => location_lat,
        type => float,
        default => 0,
        description => "Initial map latitude in degrees when the resource has no location. "
                       "The admin map falls back to 0; does not set resource coordinates."
    },
    #{
        key => location_lng,
        type => float,
        default => 0,
        description => "Initial map longitude in degrees when the resource has no location. "
                       "The admin map falls back to 0; does not set resource coordinates."
    },
    #{
        key => zoomlevel,
        type => integer,
        default => 2,
        description => "Initial admin map zoom (0..29) when the resource has no location "
                       "or zoom setting. Located resources default to 15; "
                       "static maps have a separate default of 14."
    }
]).


-export([
    event/2,

    observe_rsc_get/3,

    observe_search_query/2,
    observe_search_query_term/2,

    observe_pivot_update/3,
    observe_pivot_fields/3,
    observe_postback_notify/2,

    find_geocode/4,
    find_geocode_api/4,

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
    R = #{
        <<"address_street_1">> => z_context:get_q(<<"street">>, Context),
        <<"address_city">> => z_context:get_q(<<"city">>, Context),
        <<"address_state">> => z_context:get_q(<<"state">>, Context),
        <<"address_postcode">> => z_context:get_q(<<"postcode">>, Context),
        <<"address_country">> => z_context:get_q(<<"country">>, Context)
    },
    {ok, Type, Q} = q(R, any, Context),
    case find_geocode(Q, Type, R, Context) of
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

observe_search_query_term(#search_query_term{ term = <<"has_geo">>, arg = Arg }, _Context) ->
    case z_convert:to_bool(Arg) of
        true ->
            #search_sql_term{
                where = [
                    <<
                        "(rsc.pivot_location_lat is not null ",
                        "and rsc.pivot_location_lng is not null)"
                    >>
                ]
            };
        false ->
            #search_sql_term{
                where = [
                    <<
                        "(rsc.pivot_location_lat is null ",
                        "or rsc.pivot_location_lng is null)"
                    >>
                ]
            }
    end;
observe_search_query_term(#search_query_term{}, _Context) ->
    undefined.

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
observe_pivot_fields(#pivot_fields{ id = Id, raw_props = R }, PivotFields, Context) ->
    try
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
                Lat = z_convert:to_float(maps:get(<<"pivot_location_lat">>, PivotFields)),
                Long = z_convert:to_float(maps:get(<<"pivot_location_lng">>, PivotFields)),
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
        end
    catch
        _Type:Err:S ->
            ?LOG_ERROR(#{
                in => zotonic_mod_geomap,
                text => <<"Error in mod_geomap pivot">>,
                result => error,
                reason => Err,
                id => Id,
                stack => S
            }),
            PivotFields
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
            case q(R, all, Context) of
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
                            IsAutoGeocode = m_config:get_boolean(mod_geomap, is_auto_geocode, true, Context),
                            case find_geocode(Q, Type, R, IsAutoGeocode, Context) of
                                {error, disabled} ->
                                    % Keep existing coordinates when external lookup is disabled.
                                    ok;
                                {error, _} ->
                                    reset;
                                {ok, {NewLat,NewLong}} ->
                                    {ok, NewLat, NewLong, LocHash}
                            end
                    end
            end
    end.


%% @doc Explicit lookups remain available regardless of the automatic lookup setting.
find_geocode(Q, Type, R, Context) ->
    find_geocode(Q, Type, R, true, Context).

%% Local precoded coordinates do not disclose an address to an external service.
find_geocode(Q, Type, R, IsExternalAllowed, Context) ->
    case geomap_precoded:find_geocode(Q, Type) of
        {ok, {_, _}} = OK ->
            OK;
        {error, not_found} when not IsExternalAllowed ->
            {error, disabled};
        {error, not_found} ->
            Q1 = maybe_expand_country(Q, Type, Context),
            find_geocode_api(Q1, Type, R, Context)
    end.

%% @doc Check with Google and OpenStreetMap if they know the address
%% TODO: cache the lookup result (max 1 req/sec for Nominatim)
find_geocode_api(<<>>, _Type, _R, _Context) ->
    {error, not_found};
find_geocode_api(Q, country, _R, Context) ->
    Qq = z_url:url_encode(Q),
    openstreetmap(Qq, Context);
find_geocode_api(Q, _Type, R, Context) ->
    Qq = z_url:url_encode(Q),
    case googlemaps_check(Qq, Context) of
        {error, _} ->
            {ok, _, QOSM} = q(R, osm, Context),
            openstreetmap(z_url:url_encode(QOSM), Context);
        {ok, {_Lat, _Long}} = Ok->
            Ok
    end.

openstreetmap(<<>>, _Context) ->
    {error, not_found};
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
            ?LOG_ERROR(#{
                in => zotonic_mod_geomap,
                text => <<"OpenStreetMap unknown JSON result">>,
                result => error,
                reason => unexpected_result,
                json => JSON,
                q => Q
            }),
            {error, unexpected_result};
        {error, Reason} = Error ->
            ?LOG_WARNING(#{
                in => zotonic_mod_geomap,
                text => <<"OpenStreetMap error">>,
                result => error,
                reason => Reason,
                q => Q
            }),
            Error
    end.

googlemaps_check(Q, Context) ->
    case z_depcache:get(googlemaps_error, Context) of
        undefined ->
            case googlemaps(Q, Context) of
                {error, ratelimit} = Error ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_geomap,
                        text => <<"Geomap: Google reached query limit, disabling for 900 sec">>,
                        result => error,
                        reason => ratelimit
                    }),
                    z_depcache:set(googlemaps_error, Error, 900, Context),
                    Error;
                {error, denied} = Error ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_geomap,
                        text => <<"Geomap: Google denied the request, disabling for 3600 se">>,
                        result => error,
                        reason => denied
                    }),
                    Error;
                Result ->
                    Result
            end;
        {ok, Error} ->
            ?LOG_DEBUG(#{
                in => zotonic_mod_geomap,
                text => <<"Geomap: skipping Google lookup due to googlemaps error">>,
                reason => error,
                result => Error,
                query => Q
            }),
            Error
    end.

googlemaps(<<>>, _Context) ->
    {error, not_found};
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
                            ?LOG_INFO(#{
                                in => zotonic_mod_geomap,
                                text => <<"Google maps result without geometry">>,
                                result => error,
                                reason => geometry,
                                props => Props
                            }),
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
                            ?LOG_INFO(#{
                                in => zotonic_mod_geomap,
                                text => <<"Google maps geometry without location">>,
                                result => error,
                                reason => no_location,
                                props => Props
                            }),
                            {error, no_result}
                    end;
                [] ->
                    ?LOG_INFO(#{
                        in => zotonic_mod_geomap,
                        text => <<"Google maps geometry without results">>,
                        result => error,
                        reason => no_result,
                        props => Props
                    }),
                    {error, no_result}
            end;
        {ok, #{ <<"status">> := <<"ZERO_RESULTS">> } } ->
            {error, not_found};
        {ok, #{ <<"status">> := <<"OVER_QUERY_LIMIT">> } = Props } ->
            ?LOG_INFO(#{
                in => zotonic_mod_geomap,
                text => <<"GoogleMaps api error: 'OVER_QUERY_LIMIT'">>,
                result => error,
                reason => ratelimit,
                message => maps:get(<<"error_message">>, Props, <<>>),
                q => Q
            }),
            {error, ratelimit};
        {ok, #{ <<"status">> := <<"REQUEST_DENIED">> } = Props } ->
            ?LOG_WARNING(#{
                in => zotonic_mod_geomap,
                text => <<"GoogleMaps api error: 'REQUEST_DENIED'">>,
                result => error,
                reason => denied,
                message => maps:get(<<"error_message">>, Props, <<>>),
                q => Q
            }),
            {error, denied};
        {ok, #{ <<"status">> := Status } = Props } ->
            ?LOG_WARNING(#{
                in => zotonic_mod_geomap,
                text => <<"GoogleMaps api error with unexpected status'">>,
                result => error,
                reason => unexpected_result,
                status => Status,
                message => maps:get(<<"error_message">>, Props, <<>>),
                q => Q
            }),
            {error, unexpected_result};
        {ok, JSON} ->
            ?LOG_ERROR(#{
                in => zotonic_mod_geomap,
                text => <<"GoogleMaps api error with unknown JSON">>,
                result => error,
                reason => unexpected_result,
                json => JSON,
                q => Q
            }),
            {error, unexpected_result};
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_mod_geomap,
                text => <<"GoogleMaps api error with error">>,
                result => error,
                reason => Reason,
                q => Q
            }),
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
            ?LOG_WARNING(#{
                in => zotonic_mod_geomap,
                text => <<"HTTP request returned unexpected result">>,
                result => error,
                reason => unexpected_result,
                url => Url,
                response => Other
            }),
            {error, unexpected_result};
        {error, _Reason} = Err ->
            Err
    end.


q(R, Service, Context) ->
    case iolist_to_binary(p(<<"address_country">>, <<>>, R)) of
        <<>> ->
            {ok, country, <<>>};
        Country ->
            Fs = iolist_to_binary([
                p(<<"address_street_1">>, $,, R),
                p(<<"address_city">>, $,, R),
                p(<<"address_state">>, $,, R),
                case Service of
                    osm ->
                        % The OSM postal code data is incomplete.
                        % Adding a postal code can result in an empty result.
                        <<>>;
                    _ ->
                        remove_ws(p(<<"address_postcode">>, $,, R))
                end
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



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Exercise the automatic path and the explicit API without real HTTP requests.
automatic_geocode_disabled_test() ->
    meck:new(m_config, [non_strict]),
    meck:new(z_context, [non_strict]),
    meck:new(z_depcache, [non_strict]),
    meck:new(httpc, [unstick, non_strict]),
    try
        meck:expect(z_context, abs_url, fun("/", _) -> <<"https://example.test/">> end),
        meck:expect(z_depcache, get, fun(googlemaps_error, _) -> undefined end),
        meck:expect(m_config, get_boolean, fun(mod_geomap, is_auto_geocode, true, _) -> false end),
        meck:expect(m_config, get_value, fun(mod_geomap, google_api_key, _) -> <<"test-key">> end),
        meck:expect(httpc, request, fun(get, _, _, _) ->
            {ok, {{"HTTP/1.1", 200, "OK"}, [{"content-type", "application/json"}],
                <<"{\"status\":\"OK\",\"results\":[{\"geometry\":{\"location\":{\"lat\":52.0,\"lng\":4.0}}}]}">>}}
        end),
        Context = #context{},
        ok = optional_geocode(#{<<"address_country">> => <<"zz">>}, Context),
        Address = #{
            <<"address_country">> => <<"gb-nir">>,
            <<"address_street_1">> => <<"Example address">>
        },
        ok = optional_geocode(Address, Context),
        {ok, 51.0834196, 10.4234469, _} = optional_geocode(#{<<"address_country">> => <<"de">>}, Context),
        ?assertEqual(0, meck:num_calls(httpc, request, '_')),
        ?assertEqual({ok, {52.0, 4.0}}, find_geocode(<<"Example address">>, full, #{}, Context)),
        ?assertEqual(1, meck:num_calls(httpc, request, '_')),
        meck:expect(m_config, get_boolean, fun(mod_geomap, is_auto_geocode, true, _) -> true end),
        ?assertMatch({ok, 52.0, 4.0, _}, optional_geocode(Address, Context)),
        ?assertEqual(2, meck:num_calls(httpc, request, '_')),
        ?assert(meck:validate(m_config)),
        ?assert(meck:validate(httpc))
    after
        meck:unload(httpc),
        meck:unload(z_depcache),
        meck:unload(z_context),
        meck:unload(m_config)
    end.
-endif.
