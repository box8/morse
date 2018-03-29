morse
=====

**M**y **O**wn **R**adio **Se**rver is push-to-talk service for small groups. It runs on your own dedicated server. It's intended for Android devices, but works with any modern browser.

Installation
------------

### Server requirements

* erlang, git, valid server SSL certificates
* rebar v2: https://github.com/rebar/rebar

### Building

```sh
$ git clone git://github.com/box8/morse.git
$ cd morse
$ rebar prepare-deps
```

### Setup

Edit __morse.config__:
* port
* certificates
* secret_prefix

### Run in shell

```sh
$ ./morse.sh
```

### Run as service

Edit __morse.service__:
* WorkingDirectory
* ExecStart (path to erl)
* ExecStop (port)
* Group (should have read access to SSL certificates)

```sh
$ sudo cp morse.service /etc/systemd/system
$ sudo chown root:root /etc/systemd/system/morse.service
$ sudo systemctl enable morse
$ sudo systemctl start morse
```

Using morse
-----------

* In browser navigate to https://YOURSERVER:12345/q/w/e/r/t/y (or what your port and secret_prefix is).
* Press top button to connect (user action required to unlock \<audio\> element on Android).
* Press top button, allow using microphone for site.
* Push top button to talk, press bottom button to replay last incoming message.
