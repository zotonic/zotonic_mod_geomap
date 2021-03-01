<div class="col-md-12">
	<label class="checkbox">
        <input value="1" type="checkbox"
               name="is_feature_show_geodata"
               {% if id.is_feature_show_geodata
                        |if_undefined:(id.is_feature_show_address)
                        |if_undefined:true
                %}checked{% endif %}
               >
        {_ Show geo data on edit page _}
    </label>
</div>
