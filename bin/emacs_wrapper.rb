#!/usr/bin/env ruby
if ARGV.length == 0
  exec "emacsclient -c"
else
  exec "emacsclient -a=vim #{ARGV.join(" ")}"
end
