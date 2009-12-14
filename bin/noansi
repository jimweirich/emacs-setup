#!/usr/bin/env ruby

def emit(ch)
  $stdout.write(ch)
  $stdout.flush
end

state = :copy
while ch = $stdin.read(1)
  case state
  when :copy
    if ch == "\033"
      state = :skip
    elsif ch != "\r"
      emit(ch)
    end
  when :skip
    state = :copy if ch =~ /^[a-zA-Z]$/
  end
end
