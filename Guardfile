# A sample Guardfile
# More info at https://github.com/guard/guard#readme

# Generating JavaScript files with no file-specific namespaces. Conflicts may
# occur. TODO: fix.

guard 'shell' do
  watch(%r{^(.+)\.hs$}) do |m|
    filename = m[1]
    filename['Specs'] = '' if filename['Specs']
    puts "Running #{filename}Specs.hs"
    puts `runhaskell #{filename}Specs.hs`
  end
end

