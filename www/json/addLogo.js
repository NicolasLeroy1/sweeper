$(document).on("contextmenu", ".minesweeper-grid", function(event) {
    event.preventDefault()
    })

function getCoords(el) {
    return {
    i: parseInt(el.attr("y"), 10),
    j: parseInt(el.attr("x"), 10),
    }
}
    
function getId(el) {
    return el.closest(".minesweeper-grid").data("input-id")
    }

$(document).on("click", ".minesweeper-grid.ongoing .hidden-cell", function(event) {
    const target = $(event.target)
    const id = getId(target)
    Shiny.setInputValue(id, {
    action: "playerTurn",
    ...getCoords(target),
    }, { priority: "event" })
    })

$(document).on("contextmenu", ".minesweeper-grid.ongoing .hidden-cell", function(event) {
    const target = $(event.target)
    const id = getId(target)
    Shiny.setInputValue(id, {
    action: "addFlag",
    ...getCoords(target),
    }, { priority: "event" })
    })
    