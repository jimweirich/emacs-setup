#!/usr/bin/env ruby
# -*- ruby -*-

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


task :send_to, [:dest] do |t, args|
  if args.dest.nil?
    puts "Usage: rake send_to[DEST]"
    puts "   DEST is an ssh destination, e.g. jim@some.host"
  else
    d = args.dest
    sh "ssh #{d} mkdir -p .elisp"
    %w(
      Rakefile README
      load-ini.el dot.emacs 
      ini local-pkgs pkgs snippets
    ).each do |fn|
      sh "scp -r '#{fn}' #{d}:.elisp"
    end
  end
    
end  
