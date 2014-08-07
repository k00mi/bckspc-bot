# bckspc-bot

## Description

bckspc-bot is an IRC bot for [backspace][1]'s channel on freenode (#backspace).
It provides various features to interact with space infrastructure and other
handy things.

## Startup

bckspc-bot is a daemon and supports the usual start/stop/restart commands.

## Commands

<dl>
  <dt>!inspace</dt>
  <dd>Shortcut: i. List the members currently present.</dd>

  <dt>!pizza NUM{s,m,h}</dt>
  <dd>Shortcut: p. Alert you in NUM seconds/minutes/hours. Default is 15 minutes.</dd>

  <dt>NICK +1</dt>
  <dd>Give NICK one karma point.</dd>

  <dt>!karma [NICKS]</dt>
  <dd>Shortcut: k. List each NICK's karma score.
      If no NICK is supplied, list the issuer's score.</dd>

  <dt>!karmatop NUM</dt>
  <dd>List the top NUM karma scorers. Default is 3, maximum is 5.</dd>

  <dt>!alarm MSG</dt>
  <dd>Shortcut: a. Broadcasts MSG in the local network.
      Other services may display MSG on the LED-board or similar.<\dd>

  <dt>!echo [ARGS]</dt>
  <dd>Echo the arguments.</dd>
</dl>

## Other features

* Append "open"/"close" to the topic, depending on whether someone is currently
  present.

* Voice members who are currently in the space.

* Give karma for completing redmine issues.

## Configuration

bckspc-bot requires a JSON configuration file whose location is read from the
BOT_CONFIG environment variable.
The following fields are accepted:

<dl>
  <dt>statusUrl</dt>
  <dd>URL of the JSON space status information</dd>

  <dt>karmaFile</dt>
  <dd>Path to the file in which karma scores are saved.
      The file must exist.</dd>

  <dt>channel</dt>
  <dd>The channel the bot should join.</dd>

  <dt>pidDir</dt>
  <dd>Optional. The directory in which the PID file is created.
      Default is /var/run.</dd>

  <dt>nick</dt>
  <dd>Optional. The nickname the bot should use. Defaults to "bckspc-bot".</dd>

  <dt>password</dt>
  <dd>Optional. The password to register with.</dt>

  <dt>server</dt>
  <dd>The IRC server to connect to.</dd>

  <dt>port</dt>
  <dd>The port on which to connect to the server.</dd>

  <dt>redmine</dt>
  <dd>Optional. Object containing field "url" (URL of a Redmine project, should end with "/issues.json?"),
      "user" and "password" (both optional, used for basic HTTP authentication).</dd>

  <dt>mqttHost</dt>
  <dd>Host address of the MQTT broker.</dd>

  <dt>pizzaTopic</dt>
  <dd>Topic to which elapsed pizza timer is to be published.</dd>

  <dt>alarmTopic</dt>
  <dd>Topic to which the content of an !alarm command is to be published.</dd>
</dl>

See [the provided example file](cfg.json).


[1]: http://www.hackerspace-bamberg.de/Hauptseite
