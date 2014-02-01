module {{ module.name }}
  (
  ) where

{% for import in module.imports %}
import {{ import }}
{% endfor %}
