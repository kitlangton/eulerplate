module Eulerplate
  module Generators
    class ProblemKit < Thor::Group
      include Thor::Actions

      argument :problem

      def self.source_root
        File.dirname(__FILE__) + "/templates"
      end

      def create_folder
        empty_directory(problem.folder_name)
      end

      def create_class_file
        template("class_file.rb", "#{problem.folder_name}/#{problem.class_file_name}")
      end

      def create_spec_file
        template("spec_file.rb", "#{problem.folder_name}/#{problem.spec_file_name}")
      end
    end
  end
end
