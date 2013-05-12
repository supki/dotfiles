PONEAUX[:download] = "~/.torrents/files"
PONEAUX[:normal_fg] = 173
PONEAUX[:error_bg] = 9
PONEAUX[:keybindings] = {
  ?K => ->(o) { o.execute :move, :up, 5 },
  ?J => ->(o) { o.execute :move, :down, 5 },
  ?; => ->(o) { o.keybindings[o.focus][?:].call(o) }
}
PONEAUX[:alignment] = :right
