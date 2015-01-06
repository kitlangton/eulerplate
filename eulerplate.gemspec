# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'eulerplate/version'

Gem::Specification.new do |spec|
  spec.name          = "eulerplate"
  spec.version       = Eulerplate::VERSION
  spec.authors       = "Kit Langton"
  spec.email         = "kit.langton@gmail.com"
  spec.summary       = "API and command line tool for Project Euler"
  spec.description   = "An API and command line tool for querying Project Euler and creating folders and tests for working on each problem."
  spec.homepage      = "https://github.com/sudokill/eulerplate"
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.7"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "rspec"
  spec.add_runtime_dependency "nokogiri"
  spec.add_runtime_dependency "thor"
end
