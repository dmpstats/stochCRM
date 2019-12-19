
// searchInput bindings //
// by VP 4 dreamRs //


var exportsSearch = window.Shiny = window.Shiny || {};
var $escapeSearch = exportsSearch.$escape = function(val) {
  return val.replace(/([!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~])/g, '\\$1');
};

var searchInputBinding = new Shiny.InputBinding();
$.extend(searchInputBinding, {
  find: function(scope) {
  	return $(scope).find('.search-text');
  },
  getId: function(el) {
  	//return InputBinding.prototype.getId.call(this, el) || el.name;
  	return $(el).attr('id');
  },
  getValue: function(el) {
  	//return el.value;
  	return $('#' + $escapeSearch(el.id) + '_text').val();
  },
  setValue: function(el, value) {
  	$('#' + $escapeSearch(el.id) + '_text').val(value);
  },
  subscribe: function(el, callback) {
   $('#' + $escapeSearch(el.id) + '_text').on('keyup.searchInputBinding input.searchInputBinding', function(event) {
     if(event.keyCode == 13) { //if enter
  	  callback();
     }
   });
   $('#' + $escapeSearch(el.id) + '_search').on('click', function(event) { // on click
      callback();
   });
   $('#' + $escapeSearch(el.id) + '_reset').on('click', function(event) { // on click
      var reset = $('#' + $escapeSearch(el.id)).data('reset');
      if (reset == 'TRUE') {
        var resetValue = $('#' + $escapeSearch(el.id)).data('reset-value');
        $('#' + $escapeSearch(el.id) + '_text').val(resetValue);
      }
      callback();
   });
  },
  unsubscribe: function(el) {
  	$(el).off('.searchInputBinding');
  },
  receiveMessage: function(el, data) {
  	if (data.hasOwnProperty('value')) this.setValue(el, data.value);

    if (data.hasOwnProperty('label')) {
      // console.log(el);
      $(el).parent().find('label[for="' + el.id + '"]').text(data.label);
    }

    if (data.hasOwnProperty('placeholder')) {
      $('#' + el.id + '_text')[0].placeholder = data.placeholder;
    }

    if (data.trigger) {
      $('#' + el.id + '_search').click();
    }

    $(el).trigger('change');
  },
  getState: function(el) {
  	return {
    	//label: $(el).parent().find('label[for=' + el.id + ']').text(),
    	value: this.getValue(el)//el.value
  	};
  },
  getRatePolicy: function() {
  	return {
  	policy: 'debounce',
  	delay: 250
  	};
  }
});
Shiny.inputBindings.register(searchInputBinding, 'shiny.searchInput');
