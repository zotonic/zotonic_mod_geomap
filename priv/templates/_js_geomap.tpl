{# JS files for maps #}

{% if m.geomap.provider == 'googlemaps' %}
    <script src="https://maps.googleapis.com/maps/api/js?key={{ m.geomap.google_api_key|escape }}&amp;libraries=places&amp;language={{ z_language }}&amp;v=3"></script>
    {% lib
        "js/googlemaps/markerclusterer.js"
        "js/googlemaps/z.geomap.js"
    %}
{% else %}
    {% lib
        "js/openlayers/ol.js"
        "js/openlayers/z.geomap.js"
    %}
{% endif %}
