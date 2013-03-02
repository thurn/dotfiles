#!/usr/bin/env ruby

# Pushes the oldest commit in a git repository which has not yet been pushed
# to origin/master

if ARGV.length == 0
  puts "Usage: gitPushFirstUnpushed.rb path/to/git/repo"
else
  Dir.chdir(ARGV[0])
end

commitSha = `git log origin/master..HEAD --format="%H" | tail -n 1`.chomp

if commitSha.length > 5
  puts "pushing #{commitSha}"
  exec "git push origin #{commitSha}:master"
else
  puts "no commits found"
end
