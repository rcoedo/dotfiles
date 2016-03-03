require "busted"
local modalKey = require "modal_key"




describe("Test ModalKey", function()
  local m = modalKey.new("CMD-k")

  it("should parse shit", function()
    assert.are.equal("CMD-k", m.parseKey("CMD-k"))
  end)

  --it("should have lots of features", function()
    ---- deep check comparisons!
    --assert.are.same({ table = "great"}, { table = "great" })

    ---- or check by reference!
    --assert.are_not.equal({ table = "great"}, { table = "great"})

    --assert.true(1 == 1)
    --assert.falsy(nil)
    --assert.has.error(function() error("Wat") end, "Wat")
  --end)

  --it("should provide some shortcuts to common functions", function()
    --assert.are.unique({ { thing = 1 }, { thing = 2 }, { thing = 3 } })
  --end)

  --it("should have mocks and spies for functional tests", function()
    --local thing = require("thing_module")
    --spy.spy_on(thing, "greet")
    --thing.greet("Hi!")

    --assert.spy(thing.greet).was.called()
    --assert.spy(thing.greet).was.called_with("Hi!")
  --end)
end)
