-module(filter_geomap_distance).

-export([
    geomap_distance/3,
    geomap_distance/4
]).


geomap_distance(undefined, _LatB, _LngB, _Context) ->
    undefined;
geomap_distance(_LocA, undefined, _LngB, _Context) ->
    undefined;
geomap_distance(_LocA, _LatB, undefined, _Context) ->
    undefined;
geomap_distance(LocA, LatB, LngB, Context) when is_number(LatB), is_number(LngB) ->
    case get_loc(LocA, Context) of
        {ok, {LatA, LngA}} ->
            distance(LatA, LngA, LatB, LngB);
        {error, _} ->
            undefined
    end;
geomap_distance(LocA, LatB, LngB, Context) ->
    {LatB1, LngB1} = try
        {z_convert:to_float(LatB), z_convert:to_float(LngB)}
    catch
        _:_ -> {undefined, undefined}
    end,
    geomap_distance(LocA, LatB1, LngB1, Context).


geomap_distance(undefined, _LocB, _Context) ->
    undefined;
geomap_distance(_LocA, undefined, _Context) ->
    undefined;
geomap_distance(LocA, LocB, Context) ->
    case get_loc(LocB, Context) of
        {ok, {LatB, LngB}} ->
            geomap_distance(LocA, LatB, LngB, Context);
        {error, _} ->
            undefined
    end.

get_loc(Id, Context) when is_integer(Id); is_atom(Id); is_binary(Id) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, nolocation};
        RscId ->
            Lat = m_rsc:p(RscId, computed_location_lat, Context),
            Lng = m_rsc:p(RscId, computed_location_lng, Context),
            case is_number(Lat) andalso is_number(Lng) of
                true ->
                    {ok, {Lat, Lng}};
                false ->
                    {error, nolocation}
            end
    end;
get_loc(#{ <<"lat">> := Lat, <<"lng">> := Lng }, _Context) when is_number(Lat), is_number(Lng) ->
    {ok, {Lat, Lng}};
get_loc(#{ <<"location_lat">> := Lat, <<"location_lng">> := Lng }, _Context) when is_number(Lat), is_number(Lng) ->
    {ok, {Lat, Lng}};
get_loc(#{ <<"computed_location_lat">> := Lat, <<"computed_location_lng">> := Lng }, _Context) when is_number(Lat), is_number(Lng) ->
    {ok, {Lat, Lng}};
get_loc(_Loc, _Context) ->
    {error, nolocation}.


distance(LatA, LngA, LatB, LngB) ->
    geomap_calculations:distance(LatA, LngA, LatB, LngB).
