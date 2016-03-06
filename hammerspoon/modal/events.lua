local function transition(newNode)
  return {"transition", newNode}
end

local function exit()
  return {"exit", nil}
end

local function sequence(sequence)
  return {"sequence", sequence}
end

return { transition = transition, exit = exit, sequence = sequence}
