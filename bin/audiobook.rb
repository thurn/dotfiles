#!/usr/bin/env ruby
# Audiobook.rb
# A script to produce apple-compatible m4b audiobooks from mp3 files.
# Requires the 'mpg321' and 'faac' programs to be on the PATH.

require 'yaml'
require 'fileutils'
require 'optparse'
include FileUtils

$DataFile = "audiobook.yaml"

options = OptionParser.new
options.on('-c', '--create', "Finds all #{$DataFile} files from the current directory down and produces .m4b audiobooks for them.") { $create = true }
options.on('-a', '--author AUTHOR', "Creates an #{$DataFile} file with the specified author") do |author|
  $author = author
end
options.on('-t', '--title TITLE', "Creates an #{$DataFile} file with the specified title") do |title|
  $title = title
end
options.on_tail('h', '--help', "Prints this message") do 
  puts options
  exit
end
options.parse!(ARGV)

if $create
  Dir.glob(File.join('.', '**', $DataFile)) do |file|
    path = File.dirname(file)
    cd path do
      File.open($DataFile) do |file|
        yaml = YAML.load(file)
        warn "YAML Parse Error!" if not yaml
        author = yaml["author"]
        title = yaml["title"]
        outfile = title.gsub(/\s/,"_").gsub(/[^a-zA-Z0-9_]/,"")
        if File.exists? "#{outfile}.m4b"
          warn "#{outfile}.m4b exists, skipping."
        else
          result = system "mpg321 -q -w - *.mp3 | faac -w --artist '#{author}' --writer '#{author}' --title '#{title}' --album '#{title}' -o #{outfile}.m4b -"
          warn "System call failed: #{$?}" if not result
        end
      end
    end
  end
elsif $author and $title
  File.open($DataFile, 'w') do |file|
    file.puts(YAML.dump({"title" => $title, "author" => $author}))
  end
else
  puts options
  exit
end
