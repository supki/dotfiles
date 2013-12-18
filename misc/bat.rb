#!/usr/bin/env ruby
# coding: utf-8

def main
  notify(charge) unless online?
  print_status(charge)
end

def online?
  file_read_int("/sys/class/power_supply/ADP1/online") == 1
end

def charge
  now  = "/sys/class/power_supply/BAT0/energy_now"
  full = "/sys/class/power_supply/BAT0/energy_full"
  100 * file_read_int(now) / file_read_int(full)
end

def notify(charge)
  options = ->(severity) {
    case severity
    when :error   then "-i dialog-error -u critical"
    when :warning then "-i dialog-warning"
    end
  }

  { 30 => [:error,   "Battery is EXTREMELY low!"],
    70 => [:warning, "Battery is low!"] }.each do |upper_bound, (severity, message)|
    if charge <= upper_bound
      `notify-send "Warning!" "#{message}" #{options[severity]}`
      break
    end
  end
end

def starify(charge)
  filled = (charge.fdiv(100) * 5).round
  empty  = 5 - filled
  "★" * filled + "☆" * empty
end

def print_status(charge)
  puts "#{starify(charge)} #{online? ? "↑" : "↓"}"
end

def file_read_int(fp)
  File.open(fp).read.to_i
end

if __FILE__ == $0
  main
end
