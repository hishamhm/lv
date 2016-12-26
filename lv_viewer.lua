
local re = require("relabel")
local inspect = require("inspect")

local state_grammar = re.compile [[
   LvState <- 'LvState {' {|
      {:ts:               sTs :} ', '
      {:sched:            sSched :} ', '
      {:node_states:      sNodeStates :} ', '
      {:control_values:   sControlValues :} ', '
      {:indicator_values: sIndicatorValues :}
   |} '}'
   
   sTs              <- 'sTs = ' int
   sSched           <- 'sSched = ' LvNodeAddr_list
   sNodeStates      <- 'sNodeStates = ' Seq_LvNodeState
   sControlValues   <- 'sControlValues = ' Seq_Maybe_LvValue
   sIndicatorValues <- 'sIndicatorValues = ' Seq_Maybe_LvValue

   LvNodeAddr_list    <- '[' {| (LvNodeAddr    (',' LvNodeAddr)*    )? |} ']'
   LvNodeState_list   <- '[' {| (LvNodeState   (',' LvNodeState)*   )? |} ']'
   Maybe_LvValue_list <- '[' {| (Maybe_LvValue (',' Maybe_LvValue)* )? |} ']'
   LvValue_list       <- '[' {| (LvValue       (',' LvValue)*       )? |} ']'

   Seq_LvNodeState   <- 'fromList ' LvNodeState_list
   Seq_Maybe_LvValue <- 'fromList ' Maybe_LvValue_list

   LvNodeAddr <- { '{' {:type: LvType :} ' ' {:port: int :} '}' }
   LvType <- 'LvN' / 'LvC' / 'LvI'

   LvNodeState <- 'LvNodeState {' {|
      {:name:   nsName :} ', '
      {:cont:   nsCont :} ', '
      {:inlets: nsInlets :}
   |} '}'

   nsName <- 'nsName = ' string
   nsCont <- 'nsCont = ' Maybe_LvCont
   nsInlets <- 'nsInlets = ' Seq_Maybe_LvValue

   Maybe_LvCont  <- {| 'Nothing' / 'Just ' LvCont |}
   Maybe_LvValue <- 'Nothing' / 'Just (' LvValue ')'

   LvCont <- 'KFunction(' LvValue_list ')'
           / 'KState[' LvState ']'

   LvValue <- 'LvDBL ' double
            / 'LvI32 ' int
            / 'LvBoolean ' bool
            / 'LvString ' string

   int <- { '-'? [0-9]+ }
   double <- { posdouble / '(-' posdouble ')' }
   posdouble <- [0-9]+ '.' [0-9]+
   string <- '"' { [^"]* } '"'
   bool <- { 'True' / 'False' }
]]

--[=[
local vi_grammar = re.compile [[
]]
]=]

local function parse(grammar, line)
   local state, err, max = grammar:match(line)
   if not state then
      print(err, max)
      return
   end
   print(inspect(state))
end

for line in io.stdin:lines() do
   if line:match("^LvState") then
      parse(state_grammar, line)
   elseif line:match("^LvVI") then
--      parse(vi_grammar, line)
   end
end
