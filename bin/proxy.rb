#!/usr/bin/env ruby
# Implements a proxy local command to some command on a server stored in a 
# file called .proxy_server in the same directory. Symlink this script to 
# the name of the binary you want to call on the server (and make sure 
# it is in your non-interactive path on the server)

path = File.expand_path(File.dirname(__FILE__))
server = File.open(path + "/.proxy_server").readline.chomp
program =  File.basename(__FILE__)
exec "ssh #{server} '#{program} #{ARGV.join(' ')}'"
