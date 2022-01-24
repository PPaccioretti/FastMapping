$( document ).ready(function() {
 shinyjs.init = function() {
  $('#PanelTabSet li a[data-value="DatasetTab"]').hide();
  $('#PanelTabSet li a[data-value="DepurationTab"]').hide();
  $('#PanelTabSet li a[data-value="PredictionTab"]').hide();
  $('#PanelTabSet li a[data-value="ResultsTab"]').hide();
  $('#PanelTabSet li a[data-value="ClusterTab"]').hide();
  $('#PanelTabSet li a[data-value="ReportTab"]').hide();
};

});

$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  var zip = $el.data("zip");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    zip: zip,
    nonce: Math.random()
  });
});
