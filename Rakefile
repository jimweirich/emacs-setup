#!/usr/bin/env ruby

require 'rake/clean'

EMACS_CMD='/Applications/Aquamacs\ Emacs.app/Contents/MacOS/Aquamacs\ Emacs'

EL_FILES = FileList['ini/*.el', 'local-pkgs/**/*.el']
ELC_FILES = EL_FILES.ext('.elc')

CLOBBER.include(ELC_FILES)
CLOBBER.include('html')

task :elc => ELC_FILES

rule ".elc" => ".el" do |t|
  sh "#{EMACS_CMD} -batch -f batch-byte-compile #{t.source}" do |ok, status|
    puts "Compile failed: #{status}" unless ok
  end  
end
