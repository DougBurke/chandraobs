
## Heroku

Set up for Heroku

    %  heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git

    % heroku apps
    === My Apps
    chandraobservatory
    peaceful-mountain-6182

This will then use the peaceful-mountain-6182 URL, which can be re-named:

    % heroku apps:rename chandraobs-devel
    Renaming peaceful-mountain-6182 to chandraobs-devel... done
    http://chandraobs-devel.herokuapp.com/ | git@heroku.com:chandraobs-devel.git
    Git remote heroku updated

so that the web server will be accessed via:

    http://chandraobs-devel.herokuapp.com/

Make sure that the Procfile is sensible; the example server looks
for the PORT environment variable so we can just have:

    % cat Procfile
    web: cabal run webserver

Information on the status and memory usage:

    % heroku apps:info
    === chandraobs-devel
    Git URL:       git@heroku.com:chandraobs-devel.git
    Owner Email:   dburke.gw@gmail.com
    Region:        us
    Repo Size:     252k
    Slug Size:     187M
    Stack:         cedar
    Web URL:       http://chandraobs-devel.herokuapp.com/

Push to Heroku

    % git push heroku master

Check Heroku

    % heroku ps
    % heroku logs
    % heroku logs -t
    % heroku releases
    % heroku status

