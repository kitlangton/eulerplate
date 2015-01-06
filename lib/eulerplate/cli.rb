require 'thor'

module Eulerplate
  class CLI < Thor

    desc 'new NUMBER', 'creates a new file structure for problem NUMBER'
    def new(number)
      problem = Eulerplate::FolderCreator.new.for_problem(number)
      say "Created the folder:"
      say "./#{ problem.folder_name }", :green
      say "And the files:"
      say "#{ problem.ruby_file_name }", :green
      say "#{ problem.test_name }", :green
      say "for Problem #{ problem.number }: #{ problem.name }"
    rescue
      say "HEY: That folder already exists!", :bold
    end

  end
end
