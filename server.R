library(shiny)
library(data.table)
library(igraph)


options(shiny.error=traceback)

#' Get values of multiple atrributes of multiple vertices
#' @param graph Graph
#' @param index Vertices to select
#' @param attrs Vector of attribute names
#' @return List of vectors of attribute values, one element per attribute
get.vertex.attributes <- function(graph, index=V(graph), attrs=list.vertex.attributes(graph)) {
    sapply(attrs,
           function(attr) get.vertex.attribute(graph, attr, index),
           simplify=F,
           USE.NAMES=T)
}

#' Get values of multiple atrributes of multiple edges
#' @param graph Graph
#' @param index Edges to select
#' @param attrs Vector of attribute names
#' @return List of vectors of attribute values, one element per attribute
get.edge.attributes <- function(graph, index=E(graph), attrs=list.edge.attributes(graph)) {
    sapply(attrs, 
           function(attr) get.edge.attribute(graph, attr, index),
           simplify=F,
           USE.NAMES=T)
}

#' Converts an igraph graph to a list of nodes and edges
#' @param module igraph module 
#' @return list of two elements: list of nodes and list of edges
graph2list <- function(module) {
    getNodeObject <- function(i) {
        return(c(
            list(index = i - 1),
            get.vertex.attributes(module, i)))
    }
    
    getEdgeObject <- function(i) {
        es <- get.edges(module, i)
        return(c(
            list(source=es[1]-1, target=es[2]-1),
            get.edge.attributes(module, i)))
    }
    
    graphObject <- list(
        nodes=lapply(V(module), getNodeObject),
        edges=lapply(E(module), getEdgeObject)
        )
    graphObject
}

renderGraph <- function(expr, env=parent.frame(), quoted=FALSE) {
    # Convert the expression + environment into a function
    func <- exprToFunction(expr, env, quoted)
    
    function() {
        val <- func()
        if (is.null(val)) {
            return(list(nodes=list(), links=list()));
        }
        for (a in list.vertex.attributes(val)) {
            if (!is.numeric(get.vertex.attribute(val, a))) {
                next
            }
            print(a)
            vs <- get.vertex.attribute(val, a)
            vs[which(vs == Inf)] <- 1e100
            vs[which(vs == -Inf)] <- -1e100
            val <- set.vertex.attribute(val, a, index=V(val), value=vs)
        }
        for (a in list.edge.attributes(val)) {
            if (!is.numeric(get.edge.attribute(val, a))) {
                next
            }
            print(a)
            vs <- get.edge.attribute(val, a)
            vs[which(vs == Inf)] <- 1e100
            vs[which(vs == -Inf)] <- -1e100
            val <- set.edge.attribute(val, a, index=E(val), value=vs)
        }
        graph2list(val)
    }
}

shinyServer(function(input, output, session) {
    
    longProcessStart <- function() {
        session$sendCustomMessage(type='showWaitMessage', list(value=T))
    }

    longProcessStop <- function() {
        session$sendCustomMessage(type='showWaitMessage', list(value=F))
    }

    graphInput <- reactive({
        longProcessStart()
        Sys.sleep(1)
        graph <- graph.ring(input$verticesNumber)
        V(graph)$label <- as.character(seq_len(input$verticesNumber))
        V(graph)$log2FC <- rnorm(input$verticesNumber)

        longProcessStop()
        graph
    })
    
    output$graph <- renderGraph({
        graphInput()
    })
    
})
