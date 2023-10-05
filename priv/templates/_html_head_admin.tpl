{# CSS files for maps #}

{% if m.geomap.provider == 'googlemaps' %}
{% else %}
    {% lib "css/ol.css" %}
{% endif %}
