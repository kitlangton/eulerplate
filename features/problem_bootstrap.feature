Feature: Generating problem bootstraps
  In order to easily create Euler problems
  As a User
  I want eulerplate to create folders and specs

  Scenario: Folders
    When I run `eulerplate new 3`
    Then the following files should exist:
      | 3-largest-prime-factor/largest_prime_factor.rb |
    Then the file "3-largest-prime-factor/largest_prime_factor.rb" should contain:
    """
    class LargestPrimeFactor

    end
    """
    Then the file "3-largest-prime-factor/largest_prime_factor_spec.rb" should contain:
    """
    # PROBLEM 3: Largest prime factor
    # https://projecteuler.net/problem=3
    #
    # The prime factors of 13195 are 5, 7, 13 and 29.
    # What is the largest prime factor of the number 600851475143 ?
    #
    # WRITE YOUR TESTS!

    require_relative 'largest_prime_factor'

    describe LargestPrimeFactor do
      it 'verifies the example' do
        skip
      end
    end
    """
