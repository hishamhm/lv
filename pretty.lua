#!/usr/bin/env lua

local inp = io.stdin:read("*a")

local function out(c)
   io.stdout:write(c)
end

local indent = 0
local stack = {}
for c in inp:gmatch(".") do
   if c == '"' then
      out(c)
      if stack[#stack] ~= '"' then
         table.insert(stack, c)
      else
         table.remove(stack)
      end
   elseif c == "(" or c == "[" then
      out(c)
      if stack[#stack] ~= '"' then
         out("\n")
         indent = indent + 1
         out(("   "):rep(indent))
         table.insert(stack, c)
      end
   elseif c == ")" or c == "]" then
      if stack[#stack] ~= '"' then
         out("\n")
         indent = indent - 1
         out(("   "):rep(indent))
         table.remove(stack)
      end
      out(c)
   elseif c == "," then
      if stack[#stack] == '"' then
         out(",")
      else
         out(", ")
         if stack[#stack] == "[" then
            if indent > 0 then
               out("\n")
               out(("   "):rep(indent))
            end
         end
      end
   else
      out(c)
   end
end
