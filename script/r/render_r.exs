#!/usr/bin/env elixir

[template, data] = System.argv
{reading, []} = Code.eval_file(data)
IO.puts(EEx.eval_file(template, assigns: [reading: reading]))
