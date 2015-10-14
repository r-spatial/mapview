HTMLWidgets.widget({

  name: 'slideView2',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      // TODO: add instance fields as required
    }

  },

  renderValue: function(el, x, instance) {

    var div_photos = document.createElement("div");
    div_photos.className ="photos";

    var div_separator = document.createElement("div");
    div_separator.className ="separator";
    div_photos.appendChild(div_separator);

    var div_before = document.createElement("div");
    div_before.className ="before";
    div_separator.appendChild(div_before);

    var div_after = document.createElement("div");
    div_after.className ="after";
    div_separator.appendChild(div_after);

    var div_layer1 = document.createElement("div");
    div_layer1.className ="layer";
    div_layer1.id = "photo-after";
    div_photos.appendChild(div_layer1);

    var div_layer2 = document.createElement("div");
    div_layer2.className ="layer";
    div_layer2.id = "photo-before";
    div_photos.appendChild(div_layer2);

    el.appendChild(div_photos);


    filename1 = document.getElementById("test-1-attachment").href;
    filename2 = document.getElementById("test-2-attachment").href;



    initD3();
    updatePhotos(filename1, filename2);


    el.style.width = "100%";
    //el.style.height = "100%";

  },

  resize: function(el, width, height, instance) {

  }

});

function updatePhotos(b,a) {
  document.getElementById('photo-before').style.background="url("+b+") no-repeat";
  document.getElementById('photo-after').style.background="url("+a+") no-repeat";
}

function initD3() {
  /* Gettin down to D3 business */
  var beforeLayer = d3.select('#photo-before').node();
  d3.select('.photos')
      .on('mousemove', function() {
          var pos = d3.mouse(this);
          d3.timer.flush();
          d3.timer(function() {
              d3.select('.separator').style("left", pos[0] + 'px');
              beforeLayer.style.clip = 'rect(0px ' + pos[0] + 'px 9999999px 0px)';
              return true;
          }, 0);
      });
}
