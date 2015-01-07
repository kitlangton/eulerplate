require 'thor'

module Eulerplate
  class CLI < Thor

    desc 'new NUMBER', 'creates a new file structure for problem NUMBER'
    def new(number)
      problem = Eulerplate::Problems.get(number)
      Eulerplate::Generators::ProblemKit.start([problem])
    end

    def self.source_root
      File.dirname(__FILE__) + "/templates"
    end

  end
end
