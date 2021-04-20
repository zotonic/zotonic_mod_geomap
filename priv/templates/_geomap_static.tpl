{# Used by the geomap_static scomp to generate a static map #}

<div class="geomap-static" style="position: relative; max-width:{{ n * size }}px">
    <img src="/lib/images/marker-default.png" width="21" height="25"
         style="position: absolute; z-index: 100; left: calc({{ marker_perc[1] }}% - 10px); top: calc({{ marker_perc[2] }}% - 25px)">
    <table style="border-spacing:0;border-collapse:collapse;max-width:100%;">
        {% for row in tiles %}
            <tr class="geomap-row goemap-row-{{ forloop.counter }} {% if forloop.first %}geomap-row-first{% endif %} {% if forloop.last %}geomap-row-last{% endif %}">
                {% for x,y,z in row %}
                    <td><img class="geomap-tile" width="{{size}}" height="{{size}}" src="https://tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png" style="width:100%;height:auto;"></td>
                {% endfor %}
            </tr>
        {% endfor %}
    </table>
</div>


