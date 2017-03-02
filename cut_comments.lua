#!/usr/bin/lua

local skip_full_line = false
if arg[1] == "--lhs2tex" then
   skip_full_line = true
end   

local skipping = false
for line in io.stdin:lines() do
   if skipping then
      if line:match("^%-%- %*%* END CUT") then
         skipping = false
      end
   else
      if line:match("^%-%- %*%* BEGIN CUT") then
         skipping = true
      else
         if (not skip_full_line) or (not (line:match("^%-%- %*%*") or line:match("^ *trc %b() $ *$"))) then
            print((line:gsub("%-%- %*%*.*$", "")
                       :gsub("{%- %*%*.*%-} *", "")
                       :gsub("%f[%a_']trc %b() $ *", "")
                 ))
         end
      end
   end
end
