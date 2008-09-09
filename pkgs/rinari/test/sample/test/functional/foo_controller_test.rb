require File.dirname(__FILE__) + '/../test_helper'
require 'foo_controller'

# Re-raise errors caught by the controller.
class FooController; def rescue_action(e) raise e end; end

class FooControllerTest < Test::Unit::TestCase
  def setup
    @controller = FooController.new
    @request    = ActionController::TestRequest.new
    @response   = ActionController::TestResponse.new
  end

  # Replace this with your real tests.
  def test_truth
    assert true
  end
end
