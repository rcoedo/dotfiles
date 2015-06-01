function gridOp(op, cell)
    if (op.x ~= null) then
        cell.x = cell.x + op.x
    end
    if (op.y ~= null) then
        cell.y = cell.y + op.y
    end
    if (op.w ~= null) then
        cell.w = cell.w + op.w
    end
    if (op.h ~= null) then
        cell.h = cell.h + op.h
    end
    return cell
end

function expandToRight(cell)  return gridOp({w = 1},         cell) end
function expandToLeft(cell)   return gridOp({x = -1, w = 1}, cell) end
function expandToBottom(cell) return gridOp({h = 1},         cell) end
function expandToTop(cell)    return gridOp({y = -1, h = 1}, cell) end

function shrinkToRight(cell)  return gridOp({x = 1, w = -1}, cell) end
function shrinkToLeft(cell)   return gridOp({w = -1},        cell) end
function shrinkToBottom(cell) return gridOp({y = 1, h = -1}, cell) end
function shrinkToTop(cell)    return gridOp({h = -1}       , cell) end

function smartResizeRight(cell)
    if ((cell.x + cell.w >= hs.grid.GRIDWIDTH) and (cell.w > 1)) then
        return shrinkToRight(cell)
    elseif ((cell.x + cell.w < hs.grid.GRIDWIDTH) and (cell.w < hs.grid.GRIDWIDTH)) then
        return expandToRight(cell)
    end
end

function smartResizeLeft(cell)
    if (cell.x + cell.w >= hs.grid.GRIDWIDTH) then
        if (cell.x > 0) then
            return expandToLeft(cell)
        else
            return shrinkToLeft(cell)
        end
    else
        if (cell.w > 1) then
            return shrinkToLeft(cell)
        elseif (cell.x > 0) then
            return expandToLeft(cell)
        end
    end
end

function smartResizeDown(cell)
    if ((cell.y + cell.h >= hs.grid.GRIDHEIGHT) and (cell.h > 1)) then
        return shrinkToBottom(cell)
    elseif ((cell.y + cell.h < hs.grid.GRIDHEIGHT) and (cell.h < hs.grid.GRIDHEIGHT)) then
        return expandToBottom(cell)
    end
end

function smartResizeUp(cell)
    if (cell.y + cell.h >= hs.grid.GRIDHEIGHT) then
        if (cell.y > 0) then
            return expandToTop(cell)
        else
            return shrinkToTop(cell)
        end
    else
        if (cell.h > 1) then
            return shrinkToTop(cell)
        elseif (cell.y > 0) then
            return expandToTop(cell)
        end
    end
end

function smartResizeWindowRight() hs.grid.adjustFocusedWindow(smartResizeRight) end
function smartResizeWindowLeft() hs.grid.adjustFocusedWindow(smartResizeLeft) end
function smartResizeWindowUp() hs.grid.adjustFocusedWindow(smartResizeUp) end
function smartResizeWindowDown() hs.grid.adjustFocusedWindow(smartResizeDown) end

function adjustGrid(h, w) hs.grid.adjustHeight(h) hs.grid.adjustWidth(w) end
function increaseGrid() adjustGrid(1, 1) end
function decreaseGrid() adjustGrid(-1, -1) end
