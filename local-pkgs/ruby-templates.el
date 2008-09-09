;;; *****************************************************************
;;; Ruby Code Templates
;;; Date:     3/Dec/98
;;; Author:  Jim Weirich
;;; Purpose: This file provides several templates for Ruby programming.
;;; *****************************************************************

(provide 'ruby-templates)
(require 'jw-templates)
(setq tempo-interactive t)

;;; ==================================================================
;;; Ruby Specific helper functions
;;; ==================================================================


;;; ==================================================================
;;; Declare template package.  Name should be a the major mode that
;;; gets the C-C I binding.
;;; ==================================================================

(jw-template-package "ruby")


;;; ==================================================================
;;; Templates
;;; ==================================================================

(setq jw-ruby-copyright-list
 '(
   "# Copyright " (current-year-string) " by " (jw-get-author-name)
   " (" (jw-get-author-email) ").  All rights reserved." n
   "# Permission is granted for use, modification and distribution as" n
   "# long as the above copyright notice is included." n
   ))

(jw-template "copyright" jw-ruby-copyright-list)

(defun jw-ruby-file-start ()
  (if (string-match "\\.rb$" (buffer-file-name))
      '("#!/usr/bin/env ruby" n n)
    '("#!/usr/bin/env ruby" n
      "# -*- ruby -*-" n n) ))

(setq jw-test-case-code
      '(                    
        "class " (p "Test Class Name: " tcname) " < Test::Unit::TestCase" n
        n
        "  def test_initial_conditions" n
        p "    assert false" n
        "  end" n
        "end" n
        n
        ))

(jw-template
 "test-file" 
 (append                       
  (jw-ruby-file-start)
  '(
    "require 'test/unit'" n
    n
    )
  jw-test-case-code) )


;;; Testing ==========================================================

(jw-template
 "test-case"
 jw-test-case-code )

(jw-template
 "def-test"
 '(
   > "def test_" (p "Test Name: ") n
   > p > n
   "end" > n
   n
   )
 )

(jw-template
 "assert-block"
 '( > "assert_block do" n
      > p n
      "end" > n
      )
 )

(jw-template
 "assert-true"
 '( > "assert " (p "Condition: ") > )
 )

(jw-template
 "assert-equal"
 '( > "assert_equal " (p "Expected: ") ", " (p "Actual: ") > )
 )

(jw-template
 "assert-raise"
 '( > "assert_raise " (p "Exception: ") " do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "assert-instance-of"
 '( > "assert_instance_of " (p "Class: ") ", " (p "Actual: ") > )
 )

(jw-template
 "assert-nil"
 '( > "assert_nil " (p "Actual: ") > )
 )

(jw-template
 "assert-kind-of"
 '( > "assert_kind_of " (p "Root class: ") ", " (p "Actual: ") > )
 )

(jw-template
 "assert-respond-to"
 '( > "assert_respond_to " (p "Object: ") ", :" (p "Method: :") > )
 )

(jw-template
 "assert-match"
 '( > "assert_match /" (p "Pattern: /") "/, " (p "Actual: ") > )
 )

(jw-template
 "assert-same"
 '( > "assert_same " (p "Expected: ") ", " (p "Actual: ") > )
 )

(jw-template
 "assert-operator"
 '( > "assert_operator " (p "Expected: ") ", :" (p "Operator: :") ", " (p "Actual: ") > )
 )

(jw-template
 "assert-nothing-raised"
 '( > "assert_nothing_raised do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "flunk"
 '( > "flunk" > )
 )

(jw-template
 "assert-not-same"
 '( > "assert_not_same " (p "Expected: ") ", " (p "Actual: ") > )
 )

(jw-template
 "assert-not-equal"
 '( > "assert_not_equal " (p "Expected: ") ", " (p "Actual: ") > )
 )

(jw-template
 "assert-not-nil"
 '( > "assert_not_nil " (p "Actual: ") > )
 )

(jw-template
 "assert-not-match"
 '( > "assert_no_match /" (p "Pattern: /") "/, " (p "Actual: ") > )
 )

(jw-template
 "assert-throws"
 '( > "assert_throws :" (p "Expected symbol: :") ", do " > n
      > p n
      "end" > n
      )
 )

(jw-template
 "assert-not-thrown"
 '( > "assert_nothing_thrown do " > n
      > p n
      "end" > n
      )
 )

(jw-template
 "assert-in-delta"
 '( > "assert_in_delta " (p "Expected float: ") ", " (p "Actual float: ") ", " (p "Delta: ") > )
 )

(jw-template
 "assert-send"
 '( > "assert_send [" (p "Receiver: ")
      ", :" (p "Method symbol: :")
      ", " (p "List of args (comma separated): ") "]" >
      )
 )
                       
;;; RSpec ============================================================

(jw-template
 "describe-description"
 '( > "describe \"" (p "Context: \"") "\" do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "describe-class"
 '( > "describe " (p "Class: ") ", \"" (p "When: when \"") "\" do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "describe-shared"
 '( > "describe \"" (p "Shared behavior name: \"") "\", :shared => true do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "before-each"
 '( > "before(:each) do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "before-all"
 '( > "before(:all) do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "before-all"
 '( > "before(:all) do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "after-each"
 '( > "after(:each) do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "after-all"
 '( > "after(:all) do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "it-should"
 '( > "it \"should " (p "Description:  it \"should ") "\" do" > n
      > p n
      "end" > n
      )
 )

(jw-template
 "it-should-behave-like"
 '( > "it_should_behave_like \"" (p "Shared behavior: \"") "\"" >  )
 )

(jw-template
 "should-be"
 '( > (p "Target: ") ".should be_" (p "Predicate: ") > )
 )

(jw-template
 "should-not-be"
 '( > (p "Target: ") ".should_not be_" (p "Predicate: ") > )
 )

(jw-template
 "should-have"
 '( > (p "Target: ") ".should have_" (p "Predicate: have_") > )
 )

(jw-template
 "should-not-have"
 '( > (p "Target: ") ".should_not have_" (p "Predicate: have_") > )
 )

(jw-template
 "should-be-true"
 '( > (p "Target: ") ".should be_true" > )
 )

(jw-template
 "should-be-false"
 '( > (p "Target: ") ".should be_false" > )
 )

(jw-template
 "should-be-nil"
 '( > (p "Target: ") ".should be_nil" > )
 )

(jw-template
 "should-not-be-nil"
 '( > (p "Target: ") ".should_not be_nil" > )
 )

(jw-template
 "should-be-close"
 '( > (p "Target: ") ".should be_close(" (p "Expected float: ") ", " (p "Delta: ") ")" > )
 )

(jw-template
 "should-not-be-close"
 '( > (p "Target: ") ".should_not be_close(" (p "Expected float: ") ", " (p "Delta: ") ")" > )
 )

(jw-template
 "should-change"
 '( > "lambda {" > n
      (p "Statment: ") > n
      > "}.should change(" (p "Change target: ") ", :" (p "Attribute symbol: :")
      ").from(" (p "From: ") ").to(" (p "To: ") ")" > n
      )
 )

(jw-template
 "should-not-change"
 '( > "lambda {" > n
      (p "Statment: ") > n
      > "}.should_not change(" (p "Change target: ") ", :" (p "Attribute symbol: :") ")" > n
      )
 )

(jw-template
 "should-="
 '( > (p "Target: ") ".should == " (p "Expected value: ")  > )
 )

(jw-template
 "should-equal-value"
 '( > (p "Target: ") ".should eql(" (p "Expected value: ") ")"  > )
 )

(jw-template
 "should-not-equal-value"
 '( > (p "Target: ") ".should_not eql(" (p "Expected value: ") ")"  > )
 )

(jw-template
 "should-be-same-object"
 '( > (p "Target: ") ".should equal(" (p "Expected object: ") ")"  > )
 )

(jw-template
 "should-not-be-same-object"
 '( > (p "Target: ") ".should_not equal(" (p "Expected object: ") ")"  > )
 )

(jw-template
 "should-have-items"
 '( > (p "Target: ") ".should have(" (p "Expected count: ") ")." (p "Item name: ")  > )
 )

(jw-template
 "should-not-have-items"
 '( > (p "Target: ") ".should_not have(" (p "Expected count: ") ")." (p "Item name: ")  > )
 )

(jw-template
 "should-have-at-least-items"
 '( > (p "Target: ") ".should have_at_least(" (p "Minimum count: ") ")." (p "Item name: ")  > )
 )

(jw-template
 "should-have-at-most-items"
 '( > (p "Target: ") ".should have_at_most(" (p "Maximum count: ") ")." (p "Item name: ")  > )
 )

(jw-template
 "should-include"
 '( > (p "Target: ") ".should include(" (p "Should include: ") ")" > )
 )

(jw-template
 "should-not-include"
 '( > (p "Target: ") ".should_not include(" (p "Should not include: ") ")" > )
 )

(jw-template
 "should-match"
 '( > (p "Target: ") ".should match(/" (p "Should match: /") "/)" > )
 )

(jw-template
 "should-not-match"
 '( > (p "Target: ") ".should_not match(/" (p "Should not match: /") "/)" > )
 )

(jw-template
 "should-raise"
 '( > "lambda {" > n
      > (p "Target statement: ") > n
      > "}.should raise(" (p "Expected exception: ")
      ", " (p "Message string or regex: ") ")" > )
 )
(jw-template
 "should-not-raise"
 '( > "lambda {" > n
      > (p "Target statement: ") > n
      > "}.should raise(" (p "Expected exception: ") ")" > )
 )

(jw-template
 "should-respond-to"
 '( > (p "Target: ") ".should respond_to(:" (p "Method name(s): :") ")" > )
 )

(jw-template
 "should-not-respond-to"
 '( > (p "Target: ") ".should_not respond_to(:" (p "Method name(s): :") ")" > )
 )

(jw-template
 "should-satisfy"
 '( > (p "Target: ") ".should satisfy { |" (p "Args: ") "|" > n
      > (p "Condition: ") > n
      > "}" > n
      )
 )

(jw-template
 "should-not-satisfy"
 '( > (p "Target: ") ".should_not satisfy { |" (p "Args: ") "|" > n
      > (p "Condition: ") > n
      > "}" > n
      )
 )

(jw-template
 "should-throw"
 '( > "lambda {" > n
      > (p "Target statement: ") > n
      > "}.should throw_symbol(:" (p "Symbol: :") ")" > )
 )
(jw-template
 "should-not-throw"
 '( > "lambda {" > n
      > (p "Target statement: ") > n
      > "}.should_not throw_symbol(:" (p "Symbol: :") ")" > )
 )


;;; File Structure ===================================================

(setq jw-ruby-main-test
 '(
    "if __FILE__ == $0 then" n
    "  main(ARGV)" n
    "end" n
   ))

(jw-template "preface" (jw-ruby-file-start))
                       
(jw-template "ifmain" jw-ruby-main-test)

(jw-template
 "mainfile" 
 (append
  (jw-ruby-file-start)
  '(
    n
    "# Main Program =======================================================" n
    n
    "def main(args)" n
    "end" n
    n
    )
  jw-ruby-main-test
  )
 )

;;; Debugging ========================================================

(jw-template
 "breakpoint"
 '( "require 'breakpoint'; breakpoint" > n ))
                       
;;; Objects and classes ==============================================

(jw-template 
 "class" 
 '(
   n
   "class " (p "Class Name: ") n
   p n
   "end" n
   n
   n
   )
 )

;;; Option Parsing ===================================================

(jw-template
 "optparse"
 '(
   "def handle_options(args)" > n>
   "options = OpenStruct.new" > n>
   n
   "opts = OptionParser.new do |opts|" > n>
   "opts.banner = \"Usage: #$0 [options]\"" > n>
   "opts.separator \"\"" > n>
   "opts.separator \"Where:\"" > n>
   n
   "opts.on_tail('-h', '--help', 'Show this message') do" > n>
   "puts opts" > n>
   "exit" > n>
   "end "  > n>
   n
   "opts.on_tail('--version', 'Show version') do" > n>
   "puts \"Version: #{" (p "Version Expression: ") "}\"" > n>
   "exit" > n>
   "end " > n>
   "end " > n>
   n
   "opts.parse!(args)" > n
   "options" > n>
   "end " > n>
   )
 )

(jw-template
 "option"
 '(
   "opts.on('-" (p "Option Char: ") "', "
   "'--" (p "Option Name: " name) "', "
   "'" (p "Description: " desc) "') do |value|" > n>
   "  options." (s name) " = value" > n>
   "  # Handle option" > n>
   "end " > n>
   )
 )

