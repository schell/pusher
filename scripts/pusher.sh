#!/bin/bash
#
# pusher    Starts the pusher project deployment service
#
# chkconfig: - 85 15
# description: Pusher provides an easy way to deploy projects to amazon s3
# processname: pusher
# pidfile: /var/run/pusher.pid

DAEMONPATH=/home/synapse
DAEMON=pusher
DAEMONOPTS="pusher.cfg"
PIDFILE=/var/run/pusher.pid

start() {
    echo "Starting $DAEMON"
    cd $DAEMONPATH
    PID=`$DAEMON $DAEMONOPTS > /dev/null 2>&1 & echo $!`

    if [ -z $PID ]; then
        printf "%s\n" "Fail"
    else
        echo $PID > $PIDFILE
        printf "%s\n" "Ok"
    fi
}

stop() {
    echo "Stopping $DAEMON"
    PID=`cat $PIDFILE`
    cd $DAEMONPATH
    if [ -f $PIDFILE ]; then
        kill -HUP $PID
        printf "%s\n" "Ok"
        rm -f $PIDFILE
    else
        printf "%s\n" "pidfile not found"
    fi
}

reload() {
    $0 restart
}

restart() {
    $0 stop
    $0 start
}

case "$1" in
  start)
    start
    ;;
  stop)
    stop
    ;;
  restart)
    restart
    ;;
  *)
    echo "Usage: $0 start|stop|restart" >&2
    exit 3
    ;;
esac
