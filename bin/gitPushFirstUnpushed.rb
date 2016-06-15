#!/usr/bin/env ruby

# Pushes the oldest commit in a git repository which has not yet been pushed
# to origin/master (and updates its date)

if ARGV.length == 0
  puts "Usage: gitPushFirstUnpushed.rb path/to/git/repo [search_string] [replace_string]"
  exit
else
  Dir.chdir(ARGV[0])
end

puts "switching to develop"
exit unless system "git checkout develop"

puts "tracking all untracked files"
exit unless system "git add -A"

puts "rolling local changes into last commit"
exit unless system "git commit -a --amend -C HEAD"

# Get the SHA1 of the last commit
commitFile = File.open(File.expand_path("~/Dropbox/commitSha.txt"), "r")
prevCommitSha = commitFile.read.chomp
commitFile.close()

# Create backup
puts "Creating backup at #{ENV["BACKUP"]}"
exit unless system "rsync -a . #{ENV["BACKUP"]}"
puts "Backed up to #{ENV["BACKUP"]}"

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
# Day names: "Mon" / "Tue" / "Wed" / "Thu" / "Fri" / "Sat" / "Sun"

if ARGV.length == 3
  puts "replacing #{ARGV[1]} with #{ARGV[2]}"
  date = date.sub(ARGV[1], ARGV[2])
end

puts "updating commit time to #{date}"

exit unless system "git commit -a --amend --date='#{date}' -C HEAD"

newCommitSha = `git log HEAD --format="%H" | head -n 1`.chomp

puts "pushing commit #{newCommitSha}"
exit unless system "git push origin #{newCommitSha}:master"

puts "writing #{commitSha} to commitSha.txt"
newCommitFile = File.open(File.expand_path("~/Dropbox/commitSha.txt"), "w")
newCommitFile.write(commitSha)
newCommitFile.close()

puts "switching back to develop"
system "git checkout develop"

puts "restoring local changes"
