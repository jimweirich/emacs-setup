#!/usr/bin/env ruby

module Tags
  PROG = ENV['TAGS'] || 'ctags'
  ELISP_FILES = FileList['**/*.el']
end

namespace "tags" do
  desc "Generate an Emacs TAGS file"
  file 'TAGS' => Tags::ELISP_FILES do
    puts "Making Emacs TAGS file"
    verbose(false) do
      sh "#{Tags::PROG} -e #{Tags::ELISP_FILES}"
    end
  end
end

desc "Generate the TAGS file"
task :tags => ["TAGS"]
