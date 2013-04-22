#!/usr/bin/env ruby

# Pushes the oldest commit in a git repository which has not yet been pushed
# to origin/master (and updates its date)

if ARGV.length == 0
  puts "Usage: gitPushFirstUnpushed.rb path/to/git/repo"
  exit
else
  Dir.chdir(ARGV[0])
end

system "git checkout develop"

# If there are any local modifications, first fold them into HEAD
system "git commit --amend -a -C HEAD"

# Get the SHA1 of the last commit
commitFile = File.open(File.expand_path("~/Dropbox/commitSha.txt"), "r")
prevCommitSha = commitFile.read.chomp
commitFile.close()

# Get the SHA1 of the next commit to push
commitSha = `git log #{prevCommitSha}..HEAD --format='%H' | tail -n 1`.chomp

if commitSha.length > 5
  puts "found #{commitSha} to push"
else
  puts "no commits found"
  exit
end

puts "switching to master"
exit unless system "git checkout master"

puts "cherry-picking commit"
exit unless system "git cherry-pick #{commitSha} --strategy recursive -X theirs"

date = `/usr/local/bin/gdate -R`.chomp
puts "updating commit time to #{date}"
exit unless system "git commit -a --amend --date='#{date}' -C HEAD"

newCommitSha = `git log HEAD --format="%H" | head -n 1`.chomp

puts "pushing commit"
exit unless system "git push origin #{newCommitSha}:master"

newCommitFile = File.open(File.expand_path("~/Dropbox/commitSha.txt"), "w")
newCommitFile.write(commitSha)
newCommitFile.close()

system "git checkout develop"
