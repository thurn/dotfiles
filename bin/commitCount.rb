#!/usr/bin/env ruby

# Get the SHA1 of the last commit
commitFile = File.open(File.expand_path("~/Dropbox/commitSha.txt"), "r")
prevCommitSha = commitFile.read.chomp
commitFile.close()

exec "git log #{prevCommitSha}..HEAD --pretty=oneline | wc -l"
