function px(x) {
  if (typeof(x) === 'number')
    return x + 'px';
  else
    return x;
}

HTMLWidgets.widget({
  name: 'trelliscopejs_widget',
  type: 'output',
  factory: function(el, width, height) {
    // define shared variables for this instance...
    return {
      renderValue: function(x) {
        el.innerHTML = '';
        var dv = document.createElement('div');
        dv.id = x.id;
        var fullSize = width === window.innerWidth && height === window.innerHeight;
        if (!x.spa) {
          dv.style.width = px(width);
          dv.style.height = px(height);
          dv.className = 'trelliscope-not-spa';
        } else if (el.parentNode.id === 'htmlwidget_container') {
          el.parentNode.style.width = '100%';
          el.parentNode.style.height = '100%';
        }
        el.appendChild(dv);

        if (x.in_knitr) {
          el.style.marginTop = '30px';
          el.style.marginBottom = '30px';
        }

        var scrpt = document.createElement('script');
        scrpt.text= "(function() { trelliscopeApp('" + x.id + "', " + x.config_info + "); })();";
        el.appendChild(scrpt);
      },
      resize: function(width, height) {
        // code to re-render the widget with a new size...
      }
    };
  }
});
