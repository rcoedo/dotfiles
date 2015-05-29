function gridOp(op, g, win)
    if (op.x ~= null) then
        g.x = g.x + op.x
    end
    if (op.y ~= null) then
        g.y = g.y + op.y
    end
    if (op.w ~= null) then
        g.w = g.w + op.w
    end
    if (op.h ~= null) then
        g.h = g.h + op.h
    end
    hs.grid.set(win, g, win:screen())
end

function expandToRight(g, w)  gridOp({w = 1},         g, w) end
function expandToLeft(g, w)   gridOp({x = -1, w = 1}, g, w) end
function expandToBottom(g, w) gridOp({h = 1},         g, w) end
function expandToTop(g, w)    gridOp({y = -1, h = 1}, g, w) end

function shrinkToRight(g, w)  gridOp({x = 1, w = -1}, g, w) end
function shrinkToLeft(g, w)   gridOp({w = -1},        g, w) end
function shrinkToBottom(g, w) gridOp({y = 1, h = -1}, g, w) end
function shrinkToTop(g, w)    gridOp({h = -1}       , g, w) end

function smartResizeWindowRight()
    local win = hs.window.focusedWindow()
    local g = hs.grid.get(win)
    if ((g.x + g.w >= hs.grid.GRIDWIDTH) and (g.w > 1)) then
        shrinkToRight(g, win)
    elseif ((g.x + g.w < hs.grid.GRIDWIDTH) and (g.w < hs.grid.GRIDWIDTH)) then
        expandToRight(g, win)
    end
end

function smartResizeWindowLeft()
    local win = hs.window.focusedWindow()
    local g = hs.grid.get(win)
    if (g.x + g.w >= hs.grid.GRIDWIDTH) then
        if (g.x > 0) then
            expandToLeft(g, win)
        else
            shrinkToLeft(g, win)
        end
    else
        if (g.w > 1) then
            shrinkToLeft(g, win)
        elseif (g.x > 0) then
            expandToLeft(g, win)
        end
    end
end

function smartResizeWindowDown()
    local win = hs.window.focusedWindow()
    local g = hs.grid.get(win)
    if ((g.y + g.h >= hs.grid.GRIDHEIGHT) and (g.h > 1)) then
        shrinkToBottom(g, win)
    elseif ((g.y + g.h < hs.grid.GRIDHEIGHT) and (g.h < hs.grid.GRIDHEIGHT)) then
        expandToBottom(g, win)
    end
end

function smartResizeWindowUp()
    local win = hs.window.focusedWindow()
    local g = hs.grid.get(win)
    if (g.y + g.h >= hs.grid.GRIDHEIGHT) then
        if (g.y > 0) then
            expandToTop(g, win)
        else
            shrinkToTop(g, win)
        end
    else
        if (g.h > 1) then
            shrinkToTop(g, win)
        elseif (g.y > 0) then
            expandToTop(g, win)
        end
    end
end
