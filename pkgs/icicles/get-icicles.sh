#!/usr/bin/env bash

dir=icicles

if [ -d "$dir" ]; then
  typeset -i i=0
  while [ -d "${dir}_OLD$i" ]; do
    i="$i + 1"
  done
  mv "$dir" "${dir}_OLD$i"
fi


for url in http://www.emacswiki.org/cgi-bin/wiki/download/icicles.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-doc1.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-doc2.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-cmd.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-face.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-fn.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mac.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mcmd.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-menu.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mode.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-opt.el \
           http://www.emacswiki.org/cgi-bin/wiki/download/icicles-var.el \
           http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/icomplete+.el \
           http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/hexrgb.el
do
#    wget -nd -P $dir $url
    echo Getting $url
    curl $url >`basename $url`
    # Sleep for 2 seconds so as not to overload www.emacswiki.org
    sleep 2
done
