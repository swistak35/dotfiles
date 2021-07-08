#!/usr/bin/env ruby

require 'parser/current'
require 'unparser'

input_code = STDIN.read

ast = Parser::CurrentRuby.parse(input_code)

raise "Top-level ast is not a block" if ast.type != :block
let_send, _block_args, block_content = ast.children
_obj_receiver, method_name, arg = let_send.children
let_name = arg.children[0]


new_ast = Parser::AST::Node.new(:lvasgn, [let_name, block_content])
output_code = Unparser.unparse(new_ast)

# pp new_ast
puts output_code
