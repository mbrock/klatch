/** @jsx React.DOM */

(function () {
  Klatch.Replaying = React.createClass({
    render: function () {
      var curve = Math.pow(this.props.progress, 1.8);
      var percentage = curve * 100;
      var style = {
        width: percentage + "%"
      };
      return (
        <div className="replaying">
          <span className="replaying" style={style}></span>
        </div>
      );
    }
  });
})();
