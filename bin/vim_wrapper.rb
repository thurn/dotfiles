#!/usr/bin/env ruby
if ARGV.length == 0
  exec "vimpager"
else
  exec "vim -M #{ARGV.join(" ")}"
end
