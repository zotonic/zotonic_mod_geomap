{# Used by the geomap_static scomp to generate a static map #}

<div class="geomap-static" style="position: relative">
    <img src="/lib/images/marker-default.png" width="21" height="25" 
         style="position: absolute; z-index: 100; left: {{ marker_px[1]-10 }}px; top: {{ marker_px[2]-25 }}px"/>
{% for row in tiles %}
    <div class="geomap-row goemap-row-{{ forloop.counter }} {% if forloop.first %}geomap-row-first{% endif %} {% if forloop.last %}geomap-row-last{% endif %}" style="width: {{ size * row|length }}px; height: {{ size }}px">{% for x,y,z in row %}<img class="geomap-tile" width="{{size}}" height="{{size}}" src="https://tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png" />{% endfor %}</div>
{% endfor %}
</div>
