module {{ module.name }}
  ( spec
  ) where
{% for import in module.imports %}
import {{ import }}
{% endfor %}

spec :: Spec
spec = undefined
