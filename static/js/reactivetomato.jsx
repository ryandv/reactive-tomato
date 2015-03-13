define(['react'], function(React) {

  var STATUSES = {
    IDLE: "idle",
    INPROGRESS: "inprogress",
    BREAKPENDING: "breakpending",
    BREAK: "break",
    COMPLETED: "completed",
    ABORTED: "aborted"
  };

  /********************************************************************************
  *
  * ReactiveTomato view
  *
  /*******************************************************************************/

  var ReactiveTomato = React.createClass({

    getInitialState: function() {
      return {
        id: -1,
        workstarttime: -1,
        workendtime: null,
        breakstarttime: null,
        breakendtime: null,
        worklength: 1500000,
        breaklength: 300000,
        status: STATUSES.IDLE,
        tags: []
      };
    },

    handleTagInput: function(tags) {
      switch(this.state.status) {
        case STATUSES.IDLE:
        case STATUSES.COMPLETED:
        case STATUSES.ABORTED:
          this.setState({
            tags: tags
          });
          break;
      }
    },

    requestNewModel: function() {
      $.ajax({
        url: "http://localhost:8080/elmodoro/",
        dataType: "json",
        type: "POST",
        data: JSON.stringify({
          breaklength: 300000,
          worklength: 1500000,
          tags: this.state.tags
        }),
        success: function(data) {
          this.setState(data);
        }.bind(this),
        error: function(xhr, status, err) {
          console.log(status, err.toString());
        }.bind(this)
      });
    },

    requestServerUpdate: function() {
      $.ajax({
        url: "http://localhost:8080/elmodoro/" + this.state.id,
        dataType: "json",
        type: "PUT",
        success: function(data) {
          this.setState(data);
        }.bind(this),
        error: function(xhr, status, err) {
          console.log(status, err.toString());
        }.bind(this)
      });
    },

    handleTimerEnd: function(status) {
      this.displayEndNotification(status);
      this.requestServerUpdate();
    },

    chooseNotificationMessage: function(status) {
      switch(status) {
        case STATUSES.INPROGRESS:
          return "Finished working!";
        case STATUSES.BREAK:
          return "Break completed!";
      }
    },

    createNotification: function(message) {
      return new Notification(message);
    },

    displayEndNotification: function(status) {
      if (!("Notification" in window)) {
        console.log("This browser does not support desktop notification");
      } else if (Notification.permission === "granted") {
        this.createNotification(this.chooseNotificationMessage(status));
      } else if (Notification.permission !== "granted") {
        Notification.requestPermission(function (permission) {
          if (permission === "granted") {
            this.createNotification(this.chooseNotificationMessage(status));
          }
        });
      }

    },

    handleStart: function() {
      switch(this.state.status) {
        case STATUSES.IDLE:
        case STATUSES.COMPLETED:
        case STATUSES.ABORTED:
          this.requestNewModel();
          break;
        case STATUSES.BREAKPENDING:
          this.requestServerUpdate();
          break;
      }
    },

    handleStop: function() {
      switch(this.state.status) {
        case STATUSES.INPROGRESS:
        case STATUSES.BREAK:
          this.requestServerUpdate();
          break;
      }
    },

    calcTimeRemaining: function() {
      switch(this.state.status) {
        case STATUSES.IDLE:
        case STATUSES.COMPLETED:
        case STATUSES.ABORTED:
          return 0;
        case STATUSES.INPROGRESS:
          return this.state.workstarttime + this.state.worklength - Date.now();
        case STATUSES.BREAK:
          return this.state.breakstarttime + this.state.breaklength - Date.now();
        case STATUSES.BREAKPENDING:
          return this.state.breaklength;
      }
    },

    render: function() {
      return (
        <section id="elmodoro-app">
          <Timer onTimerEnd={this.handleTimerEnd}
                 initialStatus={this.state.status}
                 initialTimeRemaining={this.calcTimeRemaining()}/>
          <div id="options">
            <TagEntry onTagInput={this.handleTagInput}/>
            <Controls onStart={this.handleStart} onStop={this.handleStop}/>
          </div>
        </section>
      );
    }

  });

  /********************************************************************************
  *
  * Controls view
  *
  /*******************************************************************************/

  var Controls = React.createClass({

    getInitialState: function() {
      return {
        status: STATUSES.IDLE
      };
    },

    handleStartClick: function() {
      this.props.onStart();
    },

    handleStopClick: function() {
      this.props.onStop();
    },

    render: function() {
      return (
        <div id="elmodoro-controls">
          <button className="button-green" onClick={this.handleStartClick}>Start</button>
          <button className="button-red" onClick={this.handleStopClick}>Stop</button>
        </div>
      );
    }

  });

  /********************************************************************************
  *
  * TagEntry view
  *
  /*******************************************************************************/

  var TagEntry = React.createClass({

    handleChange: function(e) {
      this.props.onTagInput(e.target.value.split(","));
    },

    render: function() {
      return (
        <div id="elmodoro-tags">
          <input className="tag-field" placeholder="Enter a list of tags for this Reactive Tomato" onChange={this.handleChange}/>
        </div>
      );
    }

  });

  /********************************************************************************
  *
  * Timer view
  *
  /*******************************************************************************/

  var Timer = React.createClass({

    getInitialState: function() {
      return {
        status: this.props.initialStatus,
        timeRemaining: this.props.initialTimeRemaining
      };
    },

    startTimer: function() {
      clearInterval(this.interval);
      this.interval = setInterval(this.tick, 1000);
    },

    componentDidMount: function() {
      clearInterval(this.interval);
      this.startTimer();
    },

    componentWillUnmount: function() {
      clearInterval(this.interval);
    },

    componentWillReceiveProps: function(nextProps) {
      this.setState({
        status: nextProps.initialStatus,
        timeRemaining: nextProps.initialTimeRemaining
      });
    },

    componentDidUpdate: function() {
      clearInterval(this.interval);
      this.startTimer();
    },

    tick: function() {

      if (this.state.status !== STATUSES.INPROGRESS && this.state.status !== STATUSES.BREAK) {
        return;
      }

      if (this.state.timeRemaining - 1000 <= 0) {
        clearInterval(this.interval);
        this.props.onTimerEnd(this.state.status);
      } else {
        this.setState({
          timeRemaining: this.state.timeRemaining - 1000
        });
      }

    },

    statusToCSSClass: function(status) {
      switch(status) {
        case STATUSES.IDLE:
          return "idle-timer";
        case STATUSES.INPROGRESS:
          return "in-progress-timer";
        case STATUSES.BREAKPENDING:
          return "break-pendingtimer";
        case STATUSES.BREAK:
          return "break-timer";
        case STATUSES.COMPLETED:
          return "completed-timer";
        case STATUSES.ABORTED:
          return "aborted-timer";
      }
    },

    formatTime: function(epochMilliseconds) {
      return this.paddedMinutesRemaining(epochMilliseconds) + ":" + this.paddedSecondsRemaining(epochMilliseconds);
    },

    zeroPad: function(number) {
      var unpadded = number.toString();
      if (unpadded.length < 2) {
        return "0" + unpadded;
      } else {
        return unpadded;
      }
    },

    paddedMinutesRemaining: function(epochMilliseconds) {
      return this.zeroPad(Math.floor(epochMilliseconds / 60000));
    },

    paddedSecondsRemaining: function(epochMilliseconds) {
      return this.zeroPad(Math.floor(epochMilliseconds / 1000) % 60);
    },

    render: function() {
      return (
        <div id="elmodoro-timer">
          <span className={this.statusToCSSClass(this.state.status)}>
            {this.formatTime(this.state.timeRemaining)}
          </span>
        </div>
      );
    }
  });

  return {
    ReactiveTomato: ReactiveTomato
  };
});
