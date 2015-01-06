# Eulerplate

An an API for querying Project Euler AND a command line tool for creating test folders.

This is a fairly simple gem.

## Installation

    $ gem install eulerplate

## Usage

To create a folder in the present working directory just type:

    $ eulerplate new PROBLEM_NUMBER

for Problem 8, you will get a new directory:

 - **8-largest-product-in-a-series**

And two files in that directory:

 - **largest_product_in_a_series.rb**
 - **largest_product_in_a_series_spec.rb**

The spec file will come with a pending RSpec test requiring the ruby file.


## Contributing

1. Fork it ( https://github.com/[my-github-username]/eulerplate/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request
