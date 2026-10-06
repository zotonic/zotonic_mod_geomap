mod_geomap
==========

Support for displaying maps and adding geographical locations to
:term:`resources <resource>` in the admin.

Calculates quadtile codes from lat/long location stored in Zotonic
resources, and other geo-related functionality.

Uses OpenStreetMap for display of maps in the admin and on Zotonic
websites.


Configuration
-------------

This modules has the following configuration keys:

 * `mod_geomap.is_auto_geocode` Automatically geocode resource addresses using external services (default `true`). Set to `false` to disable automatic Google Maps/Nominatim requests. Existing coordinates and local precoded lookups are retained; manual address lookups remain available.
 * `mod_geomap.provider` Either `openlayers` (default) or `googlemaps`
 * `mod_geomap.google_api_key` The API Key for Google Maps requests
 * `mod_geomap.location_lat` Default latitude for map views
 * `mod_geomap.location_lng` Default longitude for map views
 * `mod_geomap.zoomlevel` Default zoomlevel for map views (0..29)


Country map data
----------------

`m.geomap.countries` reads the bundled
`priv/data/internet_users_2005_choropleth_lowres.json` from the
`zotonic_mod_geomap` application, located using `code:priv_dir/1`.
No copy in the site directory is needed.

The decoded country geometry is cached per site for one day, or until the
cache is flushed. Country resource values and visibility checks are applied
on every request. After replacing the data file, flush the site's depcache
to load the new geometry immediately.

Search query: geo_nearby
------------------------

The module exposes a new search query type called `geo_nearby`, which is used like this:

    {% with m.search[{geo_nearby id=1306 distance=10}] as results %}

Required parameters are `id` or (`lat`+`lng`), and `distance` (which
specifies the search radius in kilometers).

The results are ordered, the nearest location is given first. (When
the `id` parameter is given, the first result is thus the id itself).

Optional parameters are `cat`, which can be a list of categories to
which to restrict the resulting resources to.


Service: /api/geomap/nearby
---------------------------

Retrieve a list of resources with (basic) information about them, all
of which are in the vicinity of the given resource or lat/lng pair.

Internally uses the `geo_nearby` search mechanism, and has the same parameters.

It returns a list of JSON objects with for each resource the following
resource properties: id, title, summary, location_lat, location_lng,
location_zoom_level, created, modified, publication_start, image_url.

Service: /api/geomap/locations
------------------------------

The module exposes an API service at `/api/geomap/locations`, which
returns a list of locations for the search or id given.


Custom tag: geomap_static
-------------------------

Shows a location’s map using static images from OpenStreetMap.

    {% geomap_static latitude=52.34322 longitude=4.33423 %}

The location is taken from the tag’s `latitude` + `longitude`
parameters, or, when absent, from the `id` parameter which is supposed
to be a Zotonic resource of category `location` (e.g. having an
address).

It displays the `_geomap_static.tpl` template.

Other parameters:

`zoom`
  Zoom level (defaults to 14)

`n`
  How many rows and cols to display (defaults for the `rows` and `cols` parameters), defaults to 2.

`cols`
  How many grid columns to display

`rows`
  How many grid rows to display

`size`
  The size in pixels of each tile, defaults to 256.
