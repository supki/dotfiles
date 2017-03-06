class ::Chef
  class Provider
    class Deploy
    end
    class Link
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

rule 'EBLO002', 'Stop doing this' do
  tags %w(style recipe eblo)
  resource_types = %(cookbook_file directory file link remote_file template)
  recipe do |ast|
    find_resources(ast).find_all do |resource|
      resource_types.include?(resource_type(resource)) && resource_name(resource).start_with?("~")
    end
  end
end

rule "EBLO003", "Missing template (for real)" do
  tags %w{correctness eblo}
  recipe do |ast, filename|
    find_resources(ast, type: :template).reject do |resource|
      resource_attributes(resource)["local"] || resource_attributes(resource)["cookbook"]
    end.select do |resource|
      source = template_file(resource_attributes(resource, return_expressions: true))

      break if source.respond_to?(:xpath)

      templates = "#{::Pathname.new(filename).dirname.dirname}/templates"
      template_paths(filename).none? do |path|
        ["#{templates}/#{source}", "#{templates}/default/#{source}"].include?(path)
      end
    end
  end
end
