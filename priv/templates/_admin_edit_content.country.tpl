{% extends "admin_edit_widget_std.tpl" %}

{# Set color/value for a world map #}

{% block widget_title %}{_ World Map _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<div class="notification notice">
	{_ Here you can define the color and display value for the world map of countries. _}
</div>

<div class="row">
	<div class="col-sm-6">
		<label class="control-label" for="map_color">{_ Color _}</label>
		<div class="controls">
			<input id="map_color" type="color" name="map_color" value="{{ r.map_color }}" class="form-control" placeholder="#00ffcc" />
		</div>
	</div>

	<div class="col-sm-6">
		<label class="control-label" for="map_value">{_ Value _}</label>
		<div class="controls">
			<input id="map_value" type="text" name="map_value" value="{{ r.map_value }}" class="form-control" />
		</div>
	</div>
</div>
{% endwith %}
{% endblock %}
