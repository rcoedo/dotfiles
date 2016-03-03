local utils = {}
function utils.split(sString, sSeparator, nMax, bPlain)
  if type(nMax) == "boolean" then
    nMax, bPlain = bPlain, nMax
  end
  sSeparator = sSeparator or ""
  if type(sString) ~= "string" then
    error("sString parameter to hs.fnutils.split must be a string", 2) end
  if type(sSeparator) ~= "string" then
    error("sSeparator parameter to hs.fnutils.split must be a string", 2) end
  if type(nMax) ~= "number" and type(nMax) ~= "nil" then
    error("nMax parameter to hs.fnutils.split must be a number, if it is provided", 2) end
  if type(bPlain) ~= "boolean" and type(bPlain) ~= "nil" then
    error("bPlain parameter to hs.fnutils.split must be a boolean, if it is provided", 2) end
  if sSeparator == "" or maxSubStrings == 0 then return { sString } end -- degenerate cases
  local aRecord = {}
  if sString:len() > 0 then
    nMax = nMax or -1

    local nField, nStart = 1, 1
    local nFirst,nLast = sString:find(sSeparator, nStart, bPlain)
    while nFirst and nMax ~= 0 do
      aRecord[nField] = sString:sub(nStart, nFirst-1)
      nField = nField+1
      nStart = nLast+1
      nFirst,nLast = sString:find(sSeparator, nStart, bPlain)
      nMax = nMax-1
    end
    aRecord[nField] = sString:sub(nStart)
  end
  return aRecord
end
return utils
