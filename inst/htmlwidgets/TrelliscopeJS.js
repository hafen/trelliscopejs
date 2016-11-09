HTMLWidgets.widget({
  name: 'TrelliscopeJS',
  type: 'output',
  factory: function(el, width, height) {
    // define shared variables for this instance...
    return {
      renderValue: function(x) {
        var dv = document.createElement('div');
        dv.id = x.id;
        var fullSize = width === window.innerWidth && height === window.innerHeight;
        if (!fullSize) {
          dv.style.width = `${width}px`;
          dv.style.height = `${height}px`;
          dv.className = 'trelliscope-not-spa';
        }
        el.appendChild(dv);

        var scrpt = document.createElement('script');
        scrpt.text= `(function() { trelliscopeApp('${x.id}', '${x.url}'); })();`;
        el.appendChild(scrpt);
      },
      resize: function(width, height) {
        // code to re-render the widget with a new size...
      }
    };
  }
});
