require 'irb/completion'
require 'pp'
require 'rubygems'
require 'katakata_irb' rescue nil
IRB.conf[:USE_READLINE] = true
IRB.conf[:SAVE_HISTORY] = 999999
IRB.conf[:AUTO_INDENT] = true
IRB.conf[:PROMPT][:FUMIYAS] = {
  :PROMPT_I=>"#{RUBY_VERSION}.#{RUBY_PATCHLEVEL} %N(%m):%03n:%i> ",
  :PROMPT_N=>"#{RUBY_VERSION}.#{RUBY_PATCHLEVEL} %N(%m):%03n:%i> ",
  :PROMPT_S=>"#{RUBY_VERSION}.#{RUBY_PATCHLEVEL} %N(%m):%03n:%i%l ",
  :PROMPT_C=>"#{RUBY_VERSION}.#{RUBY_PATCHLEVEL} %N(%m):%03n:%i* ",
  :RETURN=>"=> %s\n"
}
IRB.conf[:PROMPT_MODE] = :FUMIYAS

require 'wirble'
Wirble.init({ :skip_prompt => true })
Wirble.colorize

