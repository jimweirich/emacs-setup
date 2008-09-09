#!/usr/bin/env ruby

PKG_FILES = FileList['pkgs/**/*']

file "third_party_pkgs.tgz" do |t|
  sh "tar zcvf #{t.name} pkgs"
end

desc "Pack the third party libraries into a tgz file."
task :pack => "third_party_pkgs.tgz"

desc "Unpack the third party libraries"
task :unpack do
  mv "pkgs", "pkgs.old"
  sh "tar zxvf third_party_pkgs.tgz"
end
