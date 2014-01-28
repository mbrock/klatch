/** @jsx React.DOM */

(function () {
  var Replaying = Klatch.Replaying;
  var AreaSplitter = Klatch.AreaSplitter;

  Klatch.Viewer = React.createClass({
    getInitialState: function () {
      var state = {};
      for (var key in Klatch.Projections) {
        state[key] = Klatch.Projections[key]();
        console.log("Running projection %o with state %o",
                    key, state[key].state);
      }
      return state;
    },

    recordMessage: function (data) {
      var message = Klatch.MessageModel(data);
      var state = this.state;
      var postponed = [];

      Object.keys(state).forEach(function (key) {
        var projection = state[key];
        projection.update(state, message, function (f) {
          postponed.push(f.bind(projection));
        });
      });

      postponed.forEach(function (f) { f() });

      this.setState(state);
    },

    render: function () {
      var replayStatus = this.state.ReplayStatus.state;
      if (replayStatus.replaying > 0) {
        var progress = 
          replayStatus.replayed / 
          replayStatus.replaying;

        return <Replaying progress={progress} />;
      } else
        return (
          <AreaSplitter messages={this.state.Messages.state}
                        topics={this.state.Topics.state}
                        areaMinimization={this.state.AreaMinimization.state}
                        online={this.state.Online.state} />
        );
    }
  });
})();
