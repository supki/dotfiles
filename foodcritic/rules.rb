class ::Chef
  class Provider
    class Deploy
    end
  end
end

require 'chef/resources'

rule 'EBLO001', 'Specified action is the default' do
  tags %w(style recipe eblo)
  recipe do |ast|
    find_resources(ast).find_all do |resource|
      command = resource.xpath("command/ident[@value != 'action']/@value").to_s
      action = resource_attribute(resource, 'action').to_s

      next unless action && command

      chef_const = command.split('_').map(&:capitalize).join
      begin
        chef_resource = ::Object.const_get("Chef::Resource::#{chef_const}")
      rescue ::NameError
        next
      end
      default_action = chef_resource.new('M').action.to_s

      action == default_action
    end
  end
end
