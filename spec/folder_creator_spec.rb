require 'spec_helper'
require 'tmpdir'

describe Eulerplate::FolderCreator do
  it 'creates a folder for a problem' do
    Dir.mktmpdir do |dir|
      test_dir = Dir.new(dir)
      fc = Eulerplate::FolderCreator.new(dir: test_dir)
      fc.for_problem(9)
      expect(test_dir.to_a).to include "9-special-pythagorean-triplet"
    end
  end

  it 'creates a spec file for the problem' do
    Dir.mktmpdir do |dir|
      test_dir = Dir.new(dir)
      test_file = File.open(File.join(__dir__,"/temp","/example_test.rb")).read
      example_ruby_file = File.open(File.join(__dir__,"/temp","/special_pythagorean_triplet.rb")).read
      fc = Eulerplate::FolderCreator.new(dir: test_dir)
      problem = fc.for_problem(9)
      spec_file = File.read(File.join(dir, problem.folder_name, problem.test_name))
      ruby_file = File.read(File.join(dir, problem.folder_name, problem.ruby_file_name))
      expect(spec_file).to eq test_file
      expect(ruby_file).to eq example_ruby_file
    end
  end
end
