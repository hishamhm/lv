
local re = require("relabel")
local inspect = require("inspect")

local types = [[

   LvNodeType <- 'LvN' / 'LvC' / 'LvI'

   LvValue <- 'LvDBL ' Double
            / 'LvI32 ' Int
            / 'LvSTR ' String
            / 'LvBool ' Boolean
            / 'LvCluster ' LvValue_list
            / 'LvArr ' LvValue_list

   Int <- { [0-9]+ } / '(' { '-' [0-9]+ } ')'
   Double <- { posdouble } / '(' { '-' posdouble } ')'
   posdouble <- [0-9]+ '.' [0-9]+ ( 'e' '-'? [0-9]+ )?
   String <- '"' { [^"]* } '"'
   Boolean <- { 'True' / 'False' }
   LvValue_list <- '[' {| (LvValue (',' LvValue)* )? |} ']'

]]

local state_grammar = re.compile([[
   LvState <- 'LvState {' {|
      {:ts:               sTs :} ', '
      {:sched:            sSched :} ', '
      {:node_states:      sNodeStates :} ', '
      {:control_values:   sControlValues :} ', '
      {:indicator_values: sIndicatorValues :}
   |} '}'
   
   sTs              <- 'sTs = ' Int
   sSched           <- 'sSched = ' LvNodeAddr_list
   sNodeStates      <- 'sNodeStates = ' Seq_LvNodeState
   sControlValues   <- 'sControlValues = ' Seq_Maybe_LvValue
   sIndicatorValues <- 'sIndicatorValues = ' Seq_Maybe_LvValue

   LvNodeAddr_list    <- '[' {| (LvNodeAddr    (',' LvNodeAddr)*    )? |} ']'
   LvNodeState_list   <- '[' {| (LvNodeState   (',' LvNodeState)*   )? |} ']'
   Maybe_LvValue_list <- '[' {| (Maybe_LvValue (',' Maybe_LvValue)* )? |} ']'

   Seq_LvNodeState   <- 'fromList ' LvNodeState_list
   Seq_Maybe_LvValue <- 'fromList ' Maybe_LvValue_list

   LvNodeAddr <- { '{' {:type: LvNodeType :} ' ' {:port: Int :} '}' }

   LvNodeState <- 'LvNodeState {' {|
      {:name:   nsName :} ', '
      {:cont:   nsCont :} ', '
      {:inputs: nsInputs :}
   |} '}'

   nsName <- 'nsName = ' String
   nsCont <- 'nsCont = ' Maybe_LvCont
   nsInputs <- 'nsInputs = ' Seq_Maybe_LvValue

   Maybe_LvCont  <- {| 'Nothing' |} / 'Just ' LvCont
   Maybe_LvValue <- { 'Nothing' } / 'Just (' LvValue ')'

   LvCont <- 'KFunction(' LvValue_list ')'
           / 'KState[' LvState ']'

]] .. types)

local vi_grammar = re.compile([[
   LvVI <- 'LvVI {' {|
      {:controls:   vControls :} ', '
      {:indicators: vIndicators :} ', '
      {:nodes:      vNodes :} ', '
      {:wires:      vWires :}
   |} '}'
   
   vControls   <- 'vControls = '   String_LvControl_pair_list
   vIndicators <- 'vIndicators = ' String_LvIndicator_pair_list

   String_LvControl_pair_list   <- '[' {| (String_LvControl_pair   (',' String_LvControl_pair)*   )? |} ']'
   String_LvIndicator_pair_list <- '[' {| (String_LvIndicator_pair (',' String_LvIndicator_pair)* )? |} ']'
   String_LvNode_pair_list      <- '[' {| (String_LvNode_pair      (',' String_LvNode_pair)*      )? |} ']'
   LvWire_list                  <- '[' {| (LvWire                  (',' LvWire)*                  )? |} ']'

   String_LvControl_pair   <- '(' {| {:name: String :} ',' LvControl   |} ')'
   String_LvIndicator_pair <- '(' {| {:name: String :} ',' LvIndicator |} ')'
   String_LvNode_pair      <- '(' {| {:name: String :} ',' LvNode      |} ')'

   LvControl <- {:type: 'LvControl'      :} ' (' LvValue ')'
              / {:type: 'LvAutoControl'  :}
              / {:type: 'LvTunControl'   :}
              / {:type: 'LvTunSRControl' :}
              / {:type: 'LvSRControl'    :} ' (' LvValue ')'
   
   LvIndicator <- {:type: 'LvIndicator'    :} ' (' LvValue ')'
                / {:type: 'LvTunIndicator' :} ' ' LvTunnelMode
                / {:type: 'LvSRIndicator'  :} ' ' Int
   
   LvTunnelMode <- 'LvAutoIndexing'
                 / 'LvLastValue'

   vNodes <- 'vNodes = ' String_LvNode_pair_list
   vWires <- 'vWires = ' LvWire_list

   LvNode <- {:type: 'LvSubVI'        :} ' '  sub_vi
           / {:type: 'LvFunction'     :} ' '  {:fname: String      :}
           / {:type: 'LvConstant'     :} ' (' {:value: LvValue     :} ')'
           / {:type: 'LvWhile'        :} ' '  sub_vi
           / {:type: 'LvFor'          :} ' '  sub_vi
           / {:type: 'LvSequence'     :} ' '  sub_vi
           / {:type: 'LvCase'         :} ' '  {:vis:   LvVI_list   :}
           / {:type: 'LvFeedbackNode' :} ' (' {:value: LvValue     :} ')'

   LvVI_list <- '[' {| (LvVI (',' LvVI)* )? |} ']'
   
   sub_vi <- {:vi: '(' LvVI ')' :}

   LvWire <- 'LvWire {' {|
      {:src: 'wSrc = ' LvPortAddr :} ', '
      {:dst: 'wDst = ' LvPortAddr :}
   |} '}'

   LvPortAddr <- {| '{' {:type: LvNodeType :} ' ' {:nidx: Int :} ', ' {:pidx: Int :} '}' |}

]] .. types)

local function parse(grammar, line)
   local object, err, max = grammar:match(line)
   if not object then
      print(object, err, max)
      print("FAILED PARSING: Is grammar out of date?")
      os.exit(1)
      return nil
   end
   --print(inspect(object))
   return object
end

local functions = {
   ["+"] = { inputs = {"x", "y"}, outputs = {"="} },
   ["-"] = { inputs = {"x", "y"}, outputs = {"="} },
   ["*"] = { inputs = {"x", "y"}, outputs = {"="} },
   ["/"] = { inputs = {"x", "y"}, outputs = {"="} },
   ["<"] = { inputs = {"x", "y"}, outputs = {"="} },
   ["ArrayMax&Min"] = { inputs = {"in"}, outputs = {"max","min"} },
   ["Bundle"] = { inputs = {"a1","a2"}, outputs = {"aout"} },
   ["InsertIntoArray"] = { inputs = {"arr", "data"}, outputs = {"arr"} },
   ["RandomNumber"] = { inputs = {}, outputs = {"rnd"} },
   ["WaitUntilNextMs"] = { inputs = {"ms"}, outputs = {} },
}

local function label_for_function(name)
   if not functions[name] then
      error("Unknown function '"..name.."'")
   end
   local ins, outs = functions[name].inputs, functions[name].outputs
   local trs = math.max(#ins, #outs)
   local function cell(ports, i, prefix)
      if i <= #ports then
         local is_last = ''
         if i == #ports and i < trs then
            is_last = ('rowspan="%d"'):format(trs - #ports + 1)
         end
         return ('<td %s port="%s%d">%s</td>'):format(is_last, prefix, i-1, ports[i])
      end
      return ''
   end
   local out = { '<<table border="0" cellborder="1" cellspacing="0">' }
   table.insert(out, '<tr>')
   local colspan = (#ins > 0 and #outs > 0) and 'colspan="2"' or ''
   table.insert(out, '<td '..colspan..'>'..name:gsub("&", "&amp;"):gsub(">", "&gt;"):gsub("<", "&lt;")..'</td>')
   table.insert(out, '</tr>')
   for i = 1, trs do
      table.insert(out, '<tr>')
      table.insert(out, cell(ins, i, "in"))
      table.insert(out, cell(outs, i, "out"))
      table.insert(out, '</tr>')
   end
   table.insert(out, '</table>>')
   return table.concat(out)
end

local function map(fn, l)
   local ret = {}
   for i, v in ipairs(l) do ret[i] = fn(v) end
   return ret
end

local function show(val)
   if type(val) == "table" then
      return "[" .. table.concat(map(show, val), " | ") .. "]"
   else
      return val
   end
end

local function label_for_edge(name, type, value)
   local val = show(value)
   local out = { '<<table border="2" cellborder="1" cellspacing="0" cellpadding="4">' }
   table.insert(out, '<tr><td>'..type..': '..name..'</td></tr>')
   table.insert(out, '<tr><td>'..(val ~= "" and val or "&nbsp;")..'</td></tr>')
   table.insert(out, '</table>>')
   return table.concat(out)
end

local function match_node(graph, node)
   return function(cases)
      if type(node) == "string" then
         if node:match("^Function_") then
            return cases.LvFunction(graph.nodes[node])
         else
            return cases.LvSimpleNode(graph.nodes[node])
         end
      else
         assert(type(node) == "table")
         if node[1] then
            return cases.LvCase(node)
         else
            assert(node.iindex)
            return cases.LvStructure(node)
         end
      end
   end
end

local function vi_to_graph(vi, graph_name, attributes)
   local function safe(name) return graph_name.."_"..name:gsub(" ", "_"):gsub("([^a-zA-Z_])", function(c) return ("%2x"):format(string.byte(c)) end) end
   local cluster_name = (graph_name == "G") and "G" or ("cluster_" .. graph_name)
   local graph = { name = cluster_name, cindex = {}, iindex = {}, nindex = {}, nodes = {}, edges = {}, subgraphs = {}, attributes = attributes or {} }
   for i, node in pairs(vi.controls) do
      local name = safe(node.name)
      graph.nodes[name] = { name = name, display = node.name, attributes = { style="filled", fillcolor="white", shape = "none", margin="0", label = label_for_edge(node.name, "Control", "") } }
      graph.cindex[i-1] = name
   end
   for i, node in pairs(vi.indicators) do
      local name = safe(node.name)
      graph.nodes[name] = { name = name, display = node.name, attributes = { style="filled", fillcolor="white", shape = "none", margin="0", label = label_for_edge(node.name, "Indicator", "") } }
      graph.iindex[i-1] = name
   end
   for i, node in pairs(vi.nodes) do
      local name = safe(node.name)
      if node.type == "LvSubVI" then
         graph.subgraphs[name] = vi_to_graph(node.vi, name, {label = node.name, shape = "box"} )
         graph.nindex[i-1] = graph.subgraphs[name]
      elseif node.type == "LvWhile" or node.type == "LvFor" or node.type == "LvSequence" then
         graph.subgraphs[name] = vi_to_graph(node.vi, name, {label = node.type, shape = "Msquare"} )
         graph.nindex[i-1] = graph.subgraphs[name]
      elseif node.type == "LvCase" then
         local subgraphs = {}
         for _, subvi in ipairs(node.vis) do
            table.insert(subgraphs, vi_to_graph(subvi, name, {label = node.type, shape = "Msquare"} ))
         end
         graph.subgraphs[name] = subgraphs
         graph.nindex[i-1] = subgraphs
      elseif node.type == "LvFunction" then
         name = "Function_" .. name
         graph.nodes[name] = { name = name, attributes = { shape = "none", style="filled", fillcolor="white", margin="0", label = label_for_function(node.fname) } }
         graph.nindex[i-1] = name
      elseif node.type == "LvConstant" then
         graph.nodes[name] = { name = name, attributes = { shape = "box", label = node.name } }
         graph.nindex[i-1] = name
      elseif node.type == "LvFeedbackNode" then
         graph.nodes[name] = { name = name, attributes = { shape = "box", label = '"<-"', peripheries = 2 } }
         graph.nindex[i-1] = name
      end
   end
   local function decode_wire(wire, prefix)
      local nidx = tonumber(wire.nidx)
      local pidx = tonumber(wire.pidx)
      if wire.type == "LvC" then
         return graph.cindex[nidx]
      elseif wire.type == "LvI" then
         return graph.iindex[nidx]
      elseif wire.type == "LvN" then
         local decode_structure = function(n)
            if prefix == "out" then
               return n.iindex[pidx]
            else
               return n.cindex[pidx]
            end
         end
         return match_node(graph, graph.nindex[nidx]) {
            LvFunction = function(n)
               return n.name..":"..prefix..pidx
            end,
            LvSimpleNode = function(n)
               assert(pidx == 0)
               return n.name
            end,
            LvStructure = decode_structure,
            LvCase = function(n)
               return decode_structure(n[1])
            end,
         }
      end
   end
   for _, wire in pairs(vi.wires) do
      local src = decode_wire(wire.src, "out")
      local dst = decode_wire(wire.dst, "in")
      local probe = "probe_"..dst:gsub(":", "__")
      graph.nodes[probe] = { name = probe, attributes = { label = "", --[[ fixedsize = "true", width=0.6, ]] height=0.3, style = "rounded,filled", shape = "box" } }
      table.insert(graph.edges, {src, probe, attributes = { dir = "none" }})
      table.insert(graph.edges, {probe, dst})
   end
   return graph
end

local function write_graph(graph, cases, filename)
   local fd = io.open(filename, "w")

   local function safe_attr(v)
      if v:match("^\"") or v:match("^<") or v:match("^[A-Za-z0-9_]+$") then
         return v
      else
         return '"'..v..'"'
      end
   end

   local function try_attributes(attributes) 
      if attributes and next(attributes) then
         fd:write(" [")
         local out = {}
         for k, v in pairs(attributes) do
            table.insert(out, k.."="..safe_attr(tostring(v)))
         end
         fd:write(table.concat(out, ","))
         fd:write("]")
      end
   end

   local function write_nodes(nodes)
      for _, node in pairs(nodes) do
         fd:write(node.name)
         try_attributes(node.attributes)
         fd:write(";\n")
      end
   end
   
   local function write_edges(edges)
      for _, edge in ipairs(edges) do
         fd:write(edge[1].." -> "..edge[2])
         try_attributes(edge.attributes)
         fd:write(";\n")
      end
   end

   local function write_attributes(attributes)
      for k, v in pairs(attributes) do
         fd:write(k.."="..safe_attr(v)..";\n")
      end
   end

   local write_subgraph
   local function write_subgraphs(subgraphs)
      for _, subgraph in pairs(subgraphs) do
         if subgraph.name then
            write_subgraph(subgraph, "subgraph")
         else
            write_subgraph(cases[subgraph] or subgraph[1], "subgraph")
         end
      end
   end

   write_subgraph = function(graph, kind)
      fd:write(kind, " "..graph.name.." {\n")
      write_nodes(graph.nodes)
      write_edges(graph.edges)
      write_subgraphs(graph.subgraphs)
      write_attributes(graph.attributes)
      fd:write("}\n")
   end
   
   write_subgraph(graph, "digraph")
   fd:close()
end

local function update_probe(graph, probe, val)
   if not graph.nodes[probe] then
      return
   end
   local tbl = graph.nodes[probe].attributes
   if val == "Nothing" then
      val = ""
   end
   if val ~= "" then
      tbl.fillcolor = "yellow"
      tbl.label = show(val)
      tbl.fixedsize = #tbl.label < 10 and "true" or "false"
      tbl.value = val
   else
      tbl.fillcolor = "white"
   end
end

local function foreach_node(graph, fn)
   for _, node in pairs(graph.nodes) do
      fn(node)
   end
   for _, subgraph in pairs(graph.subgraphs) do
      if subgraph[1] then
         for _, case in ipairs(subgraph) do
            foreach_node(case, fn)
         end
      else
         foreach_node(subgraph, fn)
      end
   end
end

local function adjust_probes(graph, state, cases)
   for i, control_value in ipairs(state.control_values) do
      local node = graph.nodes[graph.cindex[i-1]]
      node.attributes.label = label_for_edge(node.display, "Control", control_value)
      node.attributes.fillcolor = "white"
      if node.value ~= control_value then
         node.attributes.fillcolor = "yellow"
      end
      node.value = control_value
   end
   for i, indicator_value in ipairs(state.indicator_values) do
      local node = graph.nodes[graph.iindex[i-1]]
      node.attributes.label = label_for_edge(node.display, "Indicator", indicator_value)
      local probe = "probe_" .. graph.iindex[i-1]
      node.attributes.fillcolor = "white"
      if show(graph.nodes[probe].attributes.value) ~= show(indicator_value) then
         node.attributes.fillcolor = "yellow"
         update_probe(graph, probe, indicator_value)
      else
         update_probe(graph, probe, "Nothing")
         graph.nodes[probe].attributes.value = indicator_value
      end
   end
   for i, node_state in ipairs(state.node_states) do
      local adjust_structure = function(node)
         for j, input_value in ipairs(node_state.inputs) do
            local probe = "probe_" .. node.cindex[j-1]
            if graph.nodes[probe] then
               update_probe(graph, probe, input_value)
            end
         end
         if next(node_state.cont) then
            adjust_probes(node, node_state.cont, cases)
         end
      end
      match_node(graph, graph.nindex[i-1]) {
         LvFunction = function(node)
            assert(type(node) == "table")
            assert(node.name)
            for j, input_value in ipairs(node_state.inputs) do
               local probe = "probe_" .. node.name .. "__in" .. (j-1)
               update_probe(graph, probe, input_value)
            end
            node.attributes.fillcolor = next(node_state.cont) and "green" or "white"
         end,
         LvSimpleNode = function(node)
            assert(type(node) == "table")
            assert(node.name)
            for _, input_value in ipairs(node_state.inputs) do
               local probe = "probe_" .. node.name
               update_probe(graph, probe, input_value)
            end
         end,
         LvStructure = adjust_structure,
         LvCase = function(node)
            local case
            if next(node_state.cont) then
               print(inspect(node_state.cont))
               case = tonumber(node_state.cont.control_values[1])
            else
               case = tonumber(node_state.inputs[1])
            end
            cases[node] = node[case + 1]
            adjust_structure(cases[node])
         end,
      }
   end
end

local vi 
local graph
local frame = 1

local function process_line(line)
   if line:match("^LvState") then
      local state = parse(state_grammar, line)
      local cases = {}
      foreach_node(graph, function(node)
         print(node.name)
         node.attributes.fillcolor = "white"
      end)
      adjust_probes(graph, state, cases)
      graph.attributes.label = "ts = "..state.ts
      write_graph(graph, cases, "graph/graph-"..("%07d"):format(frame)..".dot")
      frame = frame + 1
      return true
   elseif line:match("^LvVI") then
      vi = parse(vi_grammar, line)
      if not vi then
         os.exit(1)
      end
      graph = vi_to_graph(vi, "G")
      --print(inspect(graph))
      graph.attributes.rankdir = "LR"
      return true
   end
end

local last_line
for line in io.stdin:lines() do
   os.execute("mkdir -p graph")
   if process_line(line) then
      last_line = line
   end
end

for _ = 1, (tonumber(arg[1]) or 1) * 5 do
   process_line(last_line)
end
