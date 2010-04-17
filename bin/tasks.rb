#!/usr/bin/env ruby1.9

require 'yaml'
require 'fileutils'
include FileUtils

# CONFIGURATION
$task_duration = 25 * 60 # in seconds
$break_duration = 25 * 5 # in seconds
$default_preference = 5
$upvote = +3
$downvote = -3
$storage = "~/Dropbox/tasks/tasks.yaml"

if not File.exists?($storage)
  touch $storage
end

File.open($storage) do |f|
  yaml = YAML.load(f)
  if yaml
    $tasks = yaml["tasks"]
    $descriptions = yaml["descriptions"]
  else
    $tasks = {}
    $descriptions = {}
  end
end

def sum_upto(hash, search_key)
  result = 0
  hash.each_key do |key|
    result += hash[key]
    break if key == search_key
  end
  return result
end

def suggest_task()
  total_preference = $tasks.values.inject(:+)
  random = rand(total_preference)
  temp_tasks = {}
  $tasks.each_key do |key|
    temp_tasks[key] = sum_upto($tasks, key)
  end
  temp_tasks.each_key do |key|
    return key if random < temp_tasks[key]
  end
  throw "error in suggest task"
end

def add_task()
  puts "Task name?"
  name = STDIN.gets.chomp.to_sym
  puts "Task description?"
  desc = STDIN.gets.chomp
  puts "Task added."
  $tasks[name] = $default_preference
  $descriptions[name] = desc
end

def remove_task()
  puts "Task name?"
  name = STDIN.gets.chomp.to_sym
  $tasks.delete(name)
  puts "Task removed."
end

def start_task()
  task = suggest_task()
  puts "We suggest #{task}"
  puts "Description: #{$descriptions[task]}"
  puts "Would you like to do this task? (y/n)"
  answer = STDIN.gets.chomp
  if answer.start_with?('y')
    puts "Ok. Start doing it!"
    $tasks[task] += $upvote
  else
    puts "Ok, let's try again."
    $tasks[task] += $downvote
    start_task()
  end
end

def list_tasks()
  puts $tasks.keys.join("\n")
end

if ARGV.length != 1
  puts "use 'tasks add' to add a task, 'tasks remove' to remove a task, 'tasks start' to start a task, 'tasks list' to list tasks."
  exit
end

case ARGV[0]
  when "add" then add_task()
  when "remove" then remove_task()
  when "start" then start_task()
  when "list" then list_tasks()
  else puts "There is no action called #{ARGV[0]}"
end

yaml = YAML.dump({"tasks" => $tasks, "descriptions" => $descriptions})
rm $storage
File.open($storage, "w") do |f|
  f.puts(yaml)
end
