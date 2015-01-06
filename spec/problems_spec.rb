require 'spec_helper'

describe Eulerplate::Problems do
  let (:yooler) { Eulerplate::Problems.new }

  it 'knows the problem names' do
    name = yooler.problem(9).name
    expect(name).to eq "Special Pythagorean triplet"
  end

  it 'knows the problem numbers' do
    number = yooler.problem(15).number
    expect(number).to eq 15
  end

  it 'knows the problem content' do
    problem = yooler.problem(9).problem
    expect(problem).to eq ['A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,',
                          'a^2 + b^2 = c^2',
                          'For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.',
                          'There exists exactly one Pythagorean triplet for which a + b + c = 1000.',
                          'Find the product abc.'].join("\n")
  end


  describe '#class_name' do
    it 'strips the class name of special symbols' do
      class_name = yooler.problem(43).class_name
      expect(class_name).to eq "SubstringDivisibility"
    end

    it 'knows the class name' do
      class_name = yooler.problem(7).class_name
      expect(class_name).to eq "Euler10001stPrime"
    end
  end

end
