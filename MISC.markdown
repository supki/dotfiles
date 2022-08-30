# Miscellaneous tweaks/tricks

(Very) rarely useful bits of knowledge that I will certainly forget
unless I write them down. (Probably even then.)

## SSH

### Using `Match` and `ProxyCommand` to connect to servers unavailable on public network

This will connect to all "hidden" domains under `exmaple.com` using
the proxy at `proxy.example.com`:

```
Match Host *.example.com Exec "[[ -z $(dig +short %h) ]]"
  ProxyCommand ssh -q -W %h:%p proxy.example.com
```
