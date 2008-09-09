class FooController < ApplicationController
  def foo
    @foo = Foo.find params[:id]
  end

  def bar
    @bar = Bar.find params[:id]
    @bar.foo = Foo.find(:first)
  end
end
