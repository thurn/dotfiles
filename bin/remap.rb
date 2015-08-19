#!/usr/bin/env ruby

require "yaml"

def get_config()
  config_file = File.expand_path "~/.remap_config.yaml"

  if File.exists? config_file
    return YAML::load(File.open(config_file))
  else
    puts "ERROR: Config file #{config_file} not found!"
    exit
  end
end

def get_active_window()
  window_id = `xdotool getactivewindow`.chomp
  active_window_classname = `xprop -id #{window_id} WM_CLASS`.chomp
  classname_regex = /WM_CLASS\(STRING\) = ".*?", "(.*?)"/
  classname_match = classname_regex.match(active_window_classname)

  if classname_match
    return classname_match[1]
  else
    puts "ERROR: Unable to discover active window name!"
    exit
  end
end

def send_keystroke(str)
  exec "xvkbd -xsendevent -text '#{str}' > /dev/null 2> /dev/null"
end

if ARGV.length == 1
  input_keystroke = ARGV[0]
  config = get_config()
  classname = get_active_window()

  if config.has_key? classname and config[classname].has_key? input_keystroke
    send_keystroke(config[classname][input_keystroke])
  else
    send_keystroke(input_keystroke)
  end

else
  puts "USAGE: remap.rb <input-keystroke>"
  puts "Select a window in 10 seconds..."
  sleep 10
  puts "Current Window Name: #{get_active_window()}"
end
