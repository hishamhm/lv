
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

   Int <- { '-'? [0-9]+ }
   Double <- { posdouble } / '(' { '-' posdouble } ')'
   posdouble <- [0-9]+ '.' [0-9]+
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
           / {:type: 'LvFeedbackNode' :} ' (' {:value: LvValue     :} ')'
   
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
      os.exit(1)
--      print("Error at " .. line:sub(err))
      return nil
   end
   --print(inspect(object))
   return object
end

local functions = {
   ["+"] = { inputs = {"x", "y"}, outputs = {"="} },
   ["*"] = { inputs = {"x", "y"}, outputs = {"="} },
   ["<"] = { inputs = {"x", "y"}, outputs = {"="} },
   ["ArrayMax&Min"] = { inputs = {"in"}, outputs = {"max","min"} },
   ["Bundle"] = { inputs = {"a1","a2"}, outputs = {"aout"} },
   ["InsertIntoArray"] = { inputs = {"arr", "data"}, outputs = {"arr"} },
   ["RandomNumber"] = { inputs = {}, outputs = {"rnd"} },
   ["WaitUntilNextMs"] = { inputs = {"ms"}, outputs = {} },
}

local function label_for_function(name)
   if not functions[name] then
      error("Unknown function "..name)
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
   local out = { '<<table border="0" cellborder="1" cellspacing="0" cellpadding="4">' }
   table.insert(out, '<tr>')
   table.insert(out, '<td colspan="2">'..name:gsub("&", "&amp;"):gsub(">", "&gt;"):gsub("<", "&lt;")..'</td>')
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

local function vi_to_graph(vi, graph_name, attributes)
   local function safe(name) return graph_name.."_"..name:gsub(" ", "_"):gsub("([^a-zA-Z_])", function(c) return ("%2x"):format(string.byte(c)) end) end
   local cluster_name = (graph_name == "G") and "G" or ("cluster_" .. graph_name)
   local graph = { name = cluster_name, cindex = {}, iindex = {}, nindex = {}, nodes = {}, edges = {}, subgraphs = {}, attributes = attributes or {} }
   for i, node in pairs(vi.controls) do
      local name = safe(node.name)
      graph.nodes[name] = { name = name, attributes = { label = node.name, shape = "octagon" } }
      graph.cindex[i-1] = name
   end
   for i, node in pairs(vi.indicators) do
      local name = safe(node.name)
      graph.nodes[name] = { name = name, attributes = { label = node.name, shape = "doubleoctagon" } }
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
      elseif node.type == "LvFunction" then
         name = "Function_" .. name
         graph.nodes[name] = { name = name, attributes = { shape = "none", margin="0", label = label_for_function(node.fname) } }
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
         if type(graph.nindex[nidx]) == "string" then
            local name = graph.nindex[nidx]
            if name:match("^Function_") then
               return graph.nindex[nidx]..":"..prefix..pidx
            else
               assert(pidx == 0)
               return graph.nindex[nidx]
            end
         else
            if prefix == "out" then
               return graph.nindex[nidx].iindex[pidx]
            else
               return graph.nindex[nidx].cindex[pidx]
            end
         end
      end
   end
   for _, wire in pairs(vi.wires) do
      local src = decode_wire(wire.src, "out")
      local dst = decode_wire(wire.dst, "in")
      local probe = "probe_"..dst:gsub(":", "__")
      graph.nodes[probe] = { name = probe, attributes = { label = "?", fixedsize = "true", --[[ width=0.6, ]] height=0.3, style = "rounded", shape = "box" } }
      table.insert(graph.edges, {src, probe, attributes = { dir = "none" }})
      table.insert(graph.edges, {probe, dst})
   end
   return graph
end

local function write_graph(graph, filename)
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
         write_subgraph(subgraph, "subgraph")
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

local function update_probe(graph, probe, val)
   if not graph.nodes[probe] then
      return
   end
   local tbl = graph.nodes[probe].attributes
--   tbl.fillcolor = (tbl.value ~= val) and "yellow" or "white"
   tbl.label = show(val)
end

local function adjust_probes(graph, state)
   for i, control_value in ipairs(state.control_values) do
      graph.nodes[graph.cindex[i-1]].attributes.label = control_value
   end
   for i, indicator_value in ipairs(state.indicator_values) do
      local probe = "probe_" .. graph.iindex[i-1]
      update_probe(graph, probe, indicator_value)
   end
   for i, node_state in ipairs(state.node_states) do
      local node = graph.nindex[i-1]
      if type(node) == "string" then
         for j, input_value in ipairs(node_state.inputs) do
            local probe
            if node:match("^Function") then
               probe = "probe_" .. node .. "__in" .. (j-1)
            else
               probe = "probe_" .. node
            end
            update_probe(graph, probe, input_value)
         end
      else
         for j, input_value in ipairs(node_state.inputs) do
            local probe = "probe_" .. node.cindex[j-1]
            if graph.nodes[probe] then
               update_probe(graph, probe, input_value)
            end
         end
         if next(node_state.cont) then
            adjust_probes(node, node_state.cont)
         end
      end
   end
end

local vi 
local graph
local frame = 1

for line in io.stdin:lines() do
   os.execute("mkdir -p graph")
   if line:match("^LvState") then
      local state = parse(state_grammar, line)
      adjust_probes(graph, state)
      graph.attributes.label = "ts = "..state.ts
      write_graph(graph, "graph/graph-"..("%07d"):format(frame)..".dot")
      frame = frame + 1
   elseif line:match("^LvVI") then
      vi = parse(vi_grammar, line)
      if not vi then
         os.exit(1)
      end
      graph = vi_to_graph(vi, "G")
      --print(inspect(graph))
      graph.attributes.rankdir = "LR"
      graph.attributes.label = "ts = 0"
      write_graph(graph, "graph/graph-"..("%07d"):format(frame)..".dot")
      frame = frame + 1
   end
end
