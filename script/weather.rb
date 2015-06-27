#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'nokogiri'
require 'open-uri'
require 'timeout'


MAX_RETRIES = 3

def message
  retries = 0
  begin
    Timeout::timeout(10) do
      page = Nokogiri::HTML(open 'http://pogoda.yandex.ru/zelenograd/')
      now, = page.search('div.b-thermometer__now').first.content.match(/(.*)(?= °C)/).captures
      kind = page.search('div.b-info-item_type_fact-big').first.content
      "#{now}°C #{symbol(kind)}"
    end
  rescue NoMethodError # Parcing failed or html retrieved is broken
   if retries < MAX_RETRIES
     retries += 1
     retry
   else
     raise NoMethodError
   end
  end
end

def symbol(kind)
  case kind
  when /ясно/ then '☀'
  when /облачно/, /туман/ then '☁'
  when /дождь/, /гроза/ then '☂'
  when /снег/, /метель/ then '☃'
  end
end


puts message if $0 == __FILE__
