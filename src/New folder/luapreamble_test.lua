local math = math
local int_to_bool = function(v)
  return v ~= 0
end
local float_to_bool = function(v)
  return v ~= 0.0
end
local float_to_int = function(v)
  return math.floor(v)
end
local bool_to_int = function(v)
  if v == true then
    return 1
  else
    return 0
  end
end
local bool_to_float = function(v)
  if v == true then
    return 1.0
  else
    return 0.0
  end
end