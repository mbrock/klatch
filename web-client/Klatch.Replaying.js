/** @jsx React.DOM */

(function () {
  Klatch.Replaying = React.createClass({
    render: function () {
      var curve = Math.pow(this.props.progress, 1.7);
      var ratio = curve * 100;
      var style = {
        width: (100 - ratio) + "%",
        opacity: (1 - curve)
      };
      return (
        <div className="replaying">
          <span className="replaying" style={style}></span>
        </div>
      );
    }
  });
})();
