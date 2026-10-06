%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Show a location's map using static images from OpenStreetMap

%% Copyright 2012 Marc Worrell
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

-module(scomp_geomap_geomap_static).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "scomp", "geolocation", "template"
    ]
}).
-moduledoc("
Render a static OpenStreetMap tile grid with a marker at a location.

```django
{% geomap_static latitude=52.34322 longitude=4.33423 zoom=14 %}
```

Alternatively, use a resource's computed coordinates:

```django
{% geomap_static id=id %}
```

| Argument | Description | Default |
| --- | --- | --- |
| `latitude`, `longitude` | Explicit center coordinates in degrees. | Resource coordinates when `latitude` is absent. |
| `id` | Resource with `computed_location_lat` and `computed_location_lng`. | None. |
| `zoom` | Tile zoom level. | `14` |
| `n` | Default number of rows and columns. | `2` |
| `rows` | Number of tile rows. | `n` |
| `cols` | Number of tile columns. | `n` |
| `size` | Display size of each tile in pixels. | `256` |

When explicit latitude is present, both explicit coordinates are used; a missing
longitude does not fall back to the resource. A resource need not belong to a
particular category, but must supply usable computed coordinates. The tag emits
an empty result when it cannot resolve a pair of floating-point coordinates.

The component renders `_geomap_static.tpl` with tile coordinates, center
coordinates, grid dimensions, and marker offsets. The default template loads
tiles from `https://tile.openstreetmap.org` and displays a marker. Override it in
the site to customize presentation. The component declares `nocache` and does
not generate or store a combined map image.
").
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(zotonic_scomp).

-export([
    vary/2, render/3
]).

-define(TILE_SIZE, 256).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    case get_latlong(Params, Context) of
        {Latitude, Longitude} when is_float(Latitude), is_float(Longitude) ->
            Zoom = z_convert:to_integer(proplists:get_value(zoom, Params, geomap_tiles:zoom())),
            N = z_convert:to_integer(proplists:get_value(n, Params, 2)),
            Cols = z_convert:to_integer(proplists:get_value(cols, Params, N)),
            Rows = z_convert:to_integer(proplists:get_value(rows, Params, N)),
            Size = z_convert:to_integer(proplists:get_value(size, Params, ?TILE_SIZE)),
            {ok, Tiles, {MarkerX,MarkerY}} = geomap_tiles:map_tiles(Latitude, Longitude, Cols, Rows, Zoom),
            Vars = [
                {n, N},
                {rows, Rows},
                {cols, Cols},
                {size, Size},
                {location_lat, Latitude},
                {location_lng, Longitude},
                {tiles, Tiles},
                {marker, {MarkerX,MarkerY}},
                {marker_px, {round(MarkerX*Size), round(MarkerY*Size)}},
                {marker_perc, {(MarkerX / N) * 100, (MarkerY / N) * 100}}
                | Params
            ],
            {Html, _Context} = z_template:render_to_iolist("_geomap_static.tpl", Vars, Context),
            {ok, Html};
        _ ->
            {ok, <<>>}
    end.



get_latlong(Params, Context) ->
    case proplists:get_value(latitude, Params) of
        undefined ->
            case proplists:get_value(id, Params) of
                undefined ->
                    {undefined, undefined};
                Id -> 
                    case m_rsc:rid(Id, Context) of
                        undefined ->
                            {undefined, undefined};
                        RId ->
                            {m_rsc:p(RId, computed_location_lat, Context),
                             m_rsc:p(RId, computed_location_lng, Context)}
                    end
            end;
        Lat ->
            {catch z_convert:to_float(Lat),
             catch z_convert:to_float(proplists:get_value(longitude, Params))}
    end.



