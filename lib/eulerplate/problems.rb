require 'open-uri'
require 'nokogiri'

module Eulerplate
  class Problems
    def self.get(number)
      page = Nokogiri::HTML(open("https://projecteuler.net/problem=#{number}"))
      name = page.css('h2')[0].text
      number = page.css('#problem_info')[0].text.scan(/\d+/)[0].to_i
      problem = Eulerplate::ProblemParser.parse(page.css('.problem_content'))
      Problem.new(name: name, number: number, problem: problem)
    end
  end
end

module Eulerplate
  class Problem
    attr_reader :name, :number, :problem

    def initialize(opts = {})
      @name = opts.fetch(:name)
      @number = opts.fetch(:number)
      @problem = opts.fetch(:problem)
    end

    def folder_name
      result = []
      result << number
      result << name.downcase.split
      result.join("-")
    end

    def spec_file_name
      snake_name + "_spec.rb"
    end

    def class_file_name
      snake_name + ".rb"
    end

    def snake_name
      result = []
      result << name.downcase.split
      result.join("_")
    end

    def spec_description
      header = []
      header << "# PROBLEM #{number}: #{name}"
      header << "# https://projecteuler.net/problem=#{number}"
      header << "#"
      header << problem.lines.map { |line| "# #{line}" }.join
      header << "#"
      header << "# WRITE YOUR TESTS!"
      header.join("\n")
    end

    def class_name
      returned_name = name.split.map(&:capitalize).join("").scan(/\w/).join("")
      if returned_name[0] =~ /\d/
        return "Euler" + returned_name
      end
      returned_name
    end

  end
end

module Eulerplate
  class ProblemParser
    def self.parse(problem)
      result = []
      problem[0].traverse do |node|
        case
        when node.parent.name == "sup"
          result
        when node.name == "sup"
          result << "^#{node.text}"
        when node.text?
          result << node.text
        when node.name == "br"
          result << "\n"
        end
      end
      result.join.strip.gsub("\r","").gsub(/ *\n */,"\n")
    end
  end
end
