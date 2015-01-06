# Eulerplate

A command line gem for creating kata boilerplate—specs and folders—for Project Euler problems.

## Installation

    $ gem install eulerplate

## Usage

To create a folder in the present working directory just type:

    $ eulerplate new PROBLEM_NUMBER

for Problem 3, you will get a new directory:

 - **3-largest-prime-factor**

And two files in that directory:

 - **largest_prime_factor_spec.rb** with the code:
```ruby
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
        skip 'write code to verify the example'
    end
end
```
 - **largest_product_in_a_series_spec.rb** with the code:
```ruby
class LargestPrimeFactor

end
```

## Contributing

1. Fork it ( https://github.com/[my-github-username]/eulerplate/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request
