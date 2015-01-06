module Eulerplate
  module Generators
    class ProblemKit
      include Thor::Actions

      attr_reader :problem

      def self.source_root
        File.dirname(__FILE__) + "/templates"
      end

      def initialize(problem)
        @problem = problem
      end

      def generate
        create_folder
        create_class_file
        create_spec_file
      end

      def create_folder
        empty_directory(problem.folder_name)
      end

      def create_class_file
        template("class_file.rb", "#{problem.folder_name}/#{probelm.class_file_name}")
      end

      def create_spec_file
        template("spec_file.rb", "#{problem.folder_name}/#{probelm.spec_file_name}")
      end
    end
  end
end
