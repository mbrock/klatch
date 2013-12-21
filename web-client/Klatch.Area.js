/** @jsx React.DOM */

(function () {
  var IRCMessage = Klatch.IRCMessage;
  var Message = Klatch.Message;
  var MarkedAsRead = Klatch.MarkedAsRead;

  var Area = Klatch.Area = React.createClass({
    render: function () {
      var source;
      var newSource;
      var sourceDiffers = true;
      var i = 0;

      var messages = this.props.messages.map(function (message) {
        newSource = message.getUserNick();

        if (newSource) {
          sourceDiffers = source !== newSource;
          source        = newSource;
        }

        if (message.payload.tag === "Received")
          return <IRCMessage message={message}
                             sourceDiffers={sourceDiffers || !(i++ % 5)}
                             key={message.sequence} />;
        else if (message.clientEventTag === 'mark-as-read')
          return <MarkedAsRead />;
        else
          return <Message message={message} key={message.sequence} />;
      });

      var isChannel = this.props.name.match(/^#/);
      return isChannel ?
                 <Channel name={this.props.name}
                          minimized={this.props.minimized}
                          messages={messages} />
               : <Boring name={this.props.name}
                         minimized={this.props.minimized}
                         messages={messages} />;
    }
  });

  Klatch.AreaSplitter = React.createClass({
    render: function () {
      var areas = [];
      var source, messages;

      for (source in this.props.messages) {
        messages = this.props.messages[source];
        areas.push(<Area name={source}
                         messages={this.props.messages[source]}
                         minimized={this.props.areaMinimization[source]} />);
      }

      return <div className="area-splitter">{areas}</div>;
    }
  });

  var AreaHeader = Klatch.AreaHeader = React.createClass({
    render: function () {
      var style = { "background-color": this.calculateColor() };
      var name = this.props.name;

      if (name.match(/^\w/))
        name = " " + name;

      return <h1 style={style}>
        <a href="#" rel="toggle-area-minimization" title="Hide/show"></a>
        <a href="#" rel="mark-as-read" title="Mark as read">✔︎</a>
        <span>{name}</span>
      </h1>;
    },

    calculateColor: function () {
      return Klatch.Clever.assignColor(this.props.name);
    },

    componentDidMount: function (node) {
      var self = this;
      $("a[rel=mark-as-read]", node).click(function () {
        Klatch.saveClientEvent({
          tag: 'mark-as-read',
          area: self.props.name,
          server: self.props.server
        });
      });

      $("a[rel=toggle-area-minimization]", node).click(function () {
        Klatch.saveClientEvent({
          tag: 'toggle-area-minimization',
          area: self.props.name
        });
      });
    }
  });

  var Channel = Klatch.Channel = React.createClass({
    render: function () {
      var visibility = this.props.minimized ? ' minimized' : '';

      var messages;

      if (!this.props.minimized)
        messages = <div>
                    <section className="area-content">
                     {this.props.messages}
                    </section>
                    <input className="input" type="text" />
                   </div>;

      return (<article className={"area channel" + visibility}>
               <AreaHeader name={this.props.name} />
               {messages}
              </article>);
    },

    componentDidMount: function (node) {
      $("input", node).focus();

      var lastReadMarker = $("hr", node).last();
      if (lastReadMarker[0]) {
        // how to make this work properly?
        $("section", node).scrollTop(100000);
      }
    }
  });

  var Boring = Klatch.Boring = React.createClass({
    render: function () {
      var visibility = this.props.minimized ? ' minimized' : '';
      var messages;

      if (!this.props.minimized)
        messages = <div>
                    <section className="area-content">
                     {this.props.messages}
                    </section>
                   </div>;

      return (<article className={"area boring" + visibility}>
               <AreaHeader name={this.props.name}
                           minimized={this.props.minimized} />
               {messages}
              </article>);
    },
  });
})();
