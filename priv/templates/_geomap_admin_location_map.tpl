{# This template is lazy loaded from the _geomap_admin_location.tpl #}

{% with m.rsc[id].computed_location_lat as latitude %}
{% with m.rsc[id].computed_location_lng as longitude %}
<div class="row">
    <div class="form-group col-md-4 label-floating">
		<input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" class="form-control" placeholder="{_ Latitude _}" pattern="^\s*[\\+\\-]?[0-9]+(\.[0-9]+)?\s*$">
        <label for="location_lat" class="control-label">{_ Latitude _}</label>
        {% if id %}
            <span class="text-muted">{_ indexed _}: {{ latitude|default:"-" }}</span>
        {% endif %}
    </div>

    <div class="form-group col-md-4 label-floating">
		<input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" class="form-control" placeholder="{_ Longitude _}" pattern="^\s*[\\+\\-]?[0-9]+(\.[0-9]+)?\s*$">
        <label for="location_lng" class="control-label">{_ Longitude _}</label>
        {% if id %}
    		<span class="text-muted">{_ indexed _}: {{ longitude|default:"-" }}</span>
        {% endif %}
    </div>

    <div class="form-group col-md-4 label-floating">
        <input id="location_zoom_level" type="number" name="location_zoom_level" min="0" max="29"
               value="{{ m.rsc[id].location_zoom_level }}" class="form-control" placeholder="{_ Zoom Level _} (0 … 29)">
        <label for="location_zoom_level" class="control-label">{_ Zoom Level _} (0 … 29)</label>
    </div>
</div>

<div class="form-group">
	<button class="btn btn-default" id="location_me"><i class="icon-screenshot"></i> {_ Set to current location _}</button>
	<button type="button" class="btn btn-default" id="location_manual" title="{_ Show the map at your current location, then click on the map to set the location _}">{_ Manual _}</button>
	<button class="btn btn-default" id="location_address"><i class="icon-screenshot"></i> {_ Set to entered address _}</button>
	<button class="btn btn-default" id="location_clear">{_ Clear _}</button>
	<button class="btn btn-default" id="location_reset">{_ Reset _}</button>
</div>

<div id="{{ #geomap }}" class="admin-geomap map" style="height: 480px; margin-bottom:8px">
    <div class="geomap-manual-note" hidden>{_ Click to set location _}</div>
</div>

<p class="text-muted"><span class="fa fa-info-circle"></span> {_ Please click on the map to select the location. _}</p>

{% javascript %}
{# Hide the 'entered address' button if there is no address select field on the edit page #}
if ($('#address_country').length == 0) {
    $('#location_address').hide();
}

{# Initialize the map, with a timeout function so that the interface works before the map is loaded #}

var map;
var map_geolocate;
var map_pin_layer;
var map_pin_layer_feature;

setTimeout(function() {

    /* Center the map on the current (calculated) position */
    {% if longitude|is_defined and latitude|is_defined %}
        var map_location = [
            parseFloat("{{ longitude }}"),
            parseFloat("{{ latitude }}")
        ];
        var has_pin = true;
        var map_zoom = parseFloat("{{ m.rsc[id].location_zoom_level|default:15 }}");
    {% else %}
        var map_location = [
            parseFloat("{{ m.geomap.location_lng|default:0 }}"),
            parseFloat("{{ m.geomap.location_lat|default:0 }}")
        ];
        var has_pin = false
        {% if id.location_zoom_level as zoom %}
            var map_zoom = parseFloat("{{ zoom }}");
        {% elseif m.geomap.zoomlevel as zoom %}
            var map_zoom = parseFloat("{{ zoom }}");
        {% else %}
            var map_zoom = parseFloat("2");
        {% endif %}
    {% endif %}

    map_location = ol.proj.transform(map_location, 'EPSG:4326', 'EPSG:3857');

    var mapOptions = {
        target: '{{ #geomap }}',
        theme: '/lib/css/ol.css',
        view: new ol.View({
            center: map_location,
            zoom: map_zoom
        }),
        layers: [
            new ol.layer.Tile({
              source: new ol.source.OSM()
            })
          ]
      };

    map = new ol.Map(mapOptions);

    /* The map can be made while it is hidden (for example in a closed widget); then it has no
     * size and stays blank. Let it measure itself again whenever its size changes. */
    if (window.ResizeObserver) {
        new ResizeObserver(function() { map.updateSize(); })
            .observe(document.getElementById('{{ #geomap }}'));
    }

    if (has_pin) {
        map_pin_layer_feature = new ol.Feature(new ol.geom.Point(map_location))
        map_pin_layer = new ol.layer.Vector ({
              source: new ol.source.Vector ({
                features: [ map_pin_layer_feature ]
              }),
              style: new ol.style.Style ({
                image: new ol.style.Icon({
                  src: '/lib/images/marker-default.png',
                  anchor: [ 0.5, 0.98 ]
                })
              })
            });
        map.addLayer( map_pin_layer );
    }

    /* Geolocate handler - to show current location */
    map_geolocate = new ol.Geolocation({
        trackingOptions: {
            enableHighAccuracy: true
        },
        projection: mapOptions.view.getProjection()
    });
    map_geolocate.on('change:position', function() {
        // 52.319567933090994
        // 4.864136531278204
        var coordinate = map_geolocate.getPosition();
        map_geolocate.setTracking(false);
        coordinate = ol.proj.transform(coordinate, 'EPSG:3857', 'EPSG:4326');
        map_mark_location(coordinate[0], coordinate[1], 'gps');
    });
    map_geolocate.on('error', function() {
        map_geolocate.setTracking(false);
        map_locating(false);
        $('#location_me').removeClass('disabled');
        z_growl_add("{_ Could not find your current location. _}");
    });

    /* Handle click on map to set the position */
    map.on('singleclick', function(evt) {
        var coordinate = evt.coordinate;
        coordinate = ol.proj.transform(coordinate, 'EPSG:3857', 'EPSG:4326');
        map_mark_location(coordinate[0], coordinate[1], 'click');
    });

    var is_first_moveend = true;
    map.on('moveend', function(e) {
        var newZoom = map.getView().getZoom();
        if (!is_first_moveend || newZoom != map_zoom) {
            $("#location_zoom_level").val( Math.round(newZoom) );
            is_first_moveend = false;
        }
    });

}, 100);

$('#location_me').click(function(ev) {
    let zoom = parseFloat( $("#location_zoom_level").val() );
    if (isNaN(zoom) || zoom < 12) {
        $("#location_zoom_level").val("12");
        map.getView().setZoom(12);
    }
    map_geolocate.setTracking(true);
    map_locating(true);
    $(this).addClass('disabled');
    ev.preventDefault();
});
$('#location_lat, #location_lng').on("change keyup", function() {
    var latitude = parseFloat( $('#location_lat').val() );
    var longitude = parseFloat( $('#location_lng').val() );
    if (!isNaN(latitude) && !isNaN(longitude)) {
        map_mark_location(longitude, latitude, 'typing');
    }
});
$('#location_zoom_level').on("change keyup", function() {
    if ($("#location_zoom_level").val() !== "") {
        map.getView().setZoom( parseFloat( $("#location_zoom_level").val() ) );
    }
});
$('#location_address').click(function(ev) {
    if ($('#address_country').length > 0) {
        var args = {
            street: $('#address_street_1').val(),
            city: $('#address_city').val(),
            postcode: $('#address_postcode').val(),
            state: $('#address_state').val(),
            country: $('#address_country').val(),
            z_delegate: 'mod_geomap'
        };
        z_notify("address_lookup", args);
        map_locating(true);
        $(this).addClass('disabled');
    }
    ev.preventDefault();
});
$('#location_clear').click(function(ev) {
    map_manual(false);
    $('#location_lat').val('');
    $('#location_lng').val('');
    if (map_pin_layer) {
        map_pin_layer.setVisible(false);
    }
    ev.preventDefault();
});
$('#location_reset').click(function(ev) {
    $('#location_lat').val('{{ m.rsc[id].location_lat }}');
    $('#location_lng').val('{{ m.rsc[id].location_lng }}');
    var latitude = parseFloat('{{ m.rsc[id].computed_location_lat }}');
    var longitude = parseFloat('{{ m.rsc[id].computed_location_lng }}');
    if (!isNaN(latitude) && !isNaN(longitude)) {
        map_mark_location(longitude, latitude, 'reset');
    }
    ev.preventDefault();
});

window.map_mark_location_error = function() {
    map_locating(false);
    z_growl_add("{_ Could not find the location. _}");
    $('#location_address').removeClass('disabled');
}

window.map_mark_location = function(longitude, latitude, method) {
    longitude = Math.round(longitude*1000000) / 1000000;
    latitude = Math.round(latitude*1000000) / 1000000;

    var location = [ longitude, latitude ];
    location = ol.proj.transform(location, 'EPSG:4326', 'EPSG:3857');
    if (!map_pin_layer_feature) {
        map_pin_layer_feature = new ol.Feature(new ol.geom.Point(location)),
        map_pin_layer = new ol.layer.Vector ({
              source: new ol.source.Vector ({
                features: [ map_pin_layer_feature ]
              }),
              style: new ol.style.Style ({
                image: new ol.style.Icon({
                  src: '/lib/images/marker-default.png',
                  anchor: [ 0.5, 0.98 ]
                })
              })
            });
        map.addLayer( map_pin_layer );
    } else {
        map_pin_layer_feature.setGeometry(new ol.geom.Point(location));
        map_pin_layer.setVisible(true);
    }
    if (method != 'typing' && method != 'reset') {
        $('#location_lat').val(latitude.toString());
        $('#location_lng').val(longitude.toString());
    }
    if (method != 'click') {
        map.getView().setCenter(location);
    }
    if (method == 'reset') {
        $('#location_zoom_level').val("{{ id.location_zoom_level }}");
        {% if id.location_zoom_level as zoom %}
            let map_zoom = parseFloat("{{ zoom }}");
        {% elseif longitude|is_defined and latitude|is_defined %}
            let map_zoom = parseFloat("15");
        {% elseif m.config.mod_geomap.zoomlevel.value as zoom %}
            let map_zoom = parseFloat("{{ zoom }}");
        {% else %}
            let map_zoom = parseFloat("2");
        {% endif %}
        map.getView().setZoom(map_zoom);
    } else if (method == 'click') {
        var zoom = Math.round( map.getView().getZoom() );
        $('#location_zoom_level').val( zoom.toString() );
    }
    if (method == 'gps' || method == 'lookup') {
        z_growl_add("{_ Location has been set. _}");
    }
    $('#location_address').removeClass('disabled');
    $('#location_me').removeClass('disabled');
    map_locating(false);
}

/* While the current location or the entered address is looked up: a spinner in the Latitude
 * field (spinner.svg, or spinner.gif when there is no svg). Stops when the location is set, on
 * an error, or after 20 seconds without an answer. */
var map_spinner_url = '/lib/images/spinner.gif';
(function() {
    var img = new Image();
    img.onload = function() { map_spinner_url = '/lib/images/spinner.svg'; };
    img.src = '/lib/images/spinner.svg';
})();
var map_locating_timer;

function map_locating(is_locating) {
    var lat = document.getElementById('location_lat');
    clearTimeout(map_locating_timer);
    if (!lat) {
        return;
    }
    if (is_locating) {
        lat.style.backgroundImage = 'url(' + map_spinner_url + ')';
        lat.classList.add('geomap-locating');
        map_locating_timer = setTimeout(function() {
            /* No answer at all (no location, no error): stop and let the user try again */
            map_locating(false);
            $('#location_me, #location_address').removeClass('disabled');
        }, 20000);
    } else {
        lat.style.backgroundImage = '';
        lat.classList.remove('geomap-locating');
    }
}

/* Manual: the map shows the current location, without setting it; the note "Click to set
 * location" stays on the map until Clear. A click on the map sets the location, as always. */
var map_is_manual = false;

function map_manual(is_manual) {
    map_is_manual = is_manual;
    $('#{{ #geomap }} .geomap-manual-note').prop('hidden', !is_manual);
}

$('#location_manual').click(function(ev) {
    ev.preventDefault();
    map_manual(true);
    if (map && navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(
            function(pos) {
                if (!map_is_manual) {
                    return;
                }
                var location = ol.proj.transform(
                    [ pos.coords.longitude, pos.coords.latitude ], 'EPSG:4326', 'EPSG:3857');
                map.getView().setCenter(location);
                if (!(map.getView().getZoom() >= 12)) {
                    map.getView().setZoom(12);
                }
            },
            function() {
                z_growl_add("{_ Could not find your current location. _}");
            },
            { enableHighAccuracy: true, timeout: 10000 });
    }
});

{% endjavascript %}
{% endwith %}
{% endwith %}
