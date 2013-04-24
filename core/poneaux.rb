PONEAUX[:download] = "#{ENV['HOME']}/.torrents/files"
PONEAUX[:normalfg] = 173
PONEAUX[:errorbg]  = 9
PONEAUX[:keybindings] = {
  ?K => ->(o) { o.execute :move, :up, 5 },
  ?J => ->(o) { o.execute :move, :down, 5 },
  ?; => ->(o) { o.keybindings[o.focus][?:].call(o) }
}
