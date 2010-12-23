QuickTest
=========

QuickTest inspects Haskell source files for unit tests, and runs all tests found through GHCi with a dynamically generated test harness. This means you can simply write properties about your code, and QuickTest will test these properties *en masse* using QuickCheck.

QuickTest currently supports QuickCheck properties, and will soon support HUnit tests. QuickTest identifies QuickCheck properties by the prefix "prop_", so be sure to name your properties accordingly.

QuickTest is based on [quickcheck-script](http://hackage.haskell.org/package/quickcheck-script) by Koen Claessen and John Hughes.

### Example

I've created a module (`Demo.hs`) with some tests:

    module Demo where
    
    -- Classic QuickCheck sample property:
    prop_reverse_is_reversible xs = (reverse . reverse) xs == xs
    
    -- This property is falsifiable:
    prop_all_integers_are_odd x = odd x

To run the tests, I simply call quicktest:

    $ quicktest Demo.hs 
    Demo.hs:prop_reverse_is_reversible:4 +++ OK, passed 100 tests.
    Demo.hs:prop_all_integers_are_odd:7 *** Failed! Falsifiable (after 1 test):  
    0

### Requirements

 * haskell-platform

### Installation

    $ git clone git://github.com/davidsiegel/quicktest.git
    $ cd quicktest
    $ cabal install

