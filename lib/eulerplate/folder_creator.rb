require 'fileutils'
require 'tmpdir'

module Eulerplate
  class FolderCreator
    attr_reader :dir
    def initialize(opts = {})
      @dir = opts.fetch(:dir) { Dir.new(Dir.pwd) }
    end

    def for_problem(number)
      problem = Problems.new.problem(number)
      Dir.mkdir(File.join(@dir.path, problem.folder_name))
      dir = Dir.new(File.join(@dir.path, problem.folder_name))
      File.open(File.join(dir.path, problem.test_name), "w") do |file|
        file.puts problem.spec_body
      end
      File.open(File.join(dir.path, problem.ruby_file_name), "w") do |file|
        file.puts problem.ruby_body
      end
      problem
    end
  end
end
