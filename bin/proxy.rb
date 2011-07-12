#!/usr/bin/env ruby
# Implements a proxy local command to some command on a server stored in a
# custom system environment variable named $ds. Symlink this script to 
# the name of the binary you want to call on the server (and make sure 
# it is in your non-interactive path on the server). The script will
# *attempt* to switch to a directory with the same name as your current
# directory on the server before running, but will not fail if it does not
# exist.

server = ENV["ds"]
program =  File.basename(__FILE__)
relative_path = `pwd`.sub(File.expand_path("~"), "~").chomp
exec "ssh #{server} 'cd #{relative_path} ; #{program} #{ARGV.join(' ')}'"
