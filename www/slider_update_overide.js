var sliderInputBinding = Shiny.inputBindings.bindingNames['shiny.sliderInput'].binding;

var lazySliderInputBinding = $.extend({}, sliderInputBinding, {
  subscribe: function(el, callback) {
	var $el = $(el);
	var slider = $el.data('ionRangeSlider');

	var handleChange = function() {
	  if (!inputsInitialized) return;
	  callback(!$el.data('immediate') && !$el.data('animating'));
	};

	slider.update({
	  onUpdate: handleChange,
	  onFinish: handleChange
	});
  },

  unsubscribe: function(el, callback) {
	var slider = $(el).data('ionRangeSlider');
	slider.update({
	  onUpdate: null,
	  onFinish: null
	});
  }
});

//Shiny.inputBindings.register(lazySliderInputBinding, 'shiny.lazySliderInput');

var inputsInitialized = false;
$(document).one('shiny:connected', function() {
  inputsInitialized = true;
});
