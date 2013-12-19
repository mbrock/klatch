(function () {

  var source = new EventSource("/");
  source.onmessage = function (e) {
    var element = $("<li/>");
    var data = JSON.parse(e.data);
    var time = moment(data.eventTimestamp);
    element.append(time.format() + " " + data.eventData.tag + " " + JSON.stringify(data.eventData.contents));
    $(log).append(element);
  };

})();
