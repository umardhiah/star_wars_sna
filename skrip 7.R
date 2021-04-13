setwd("D:\\[2021] R Ladies\\SNA Star Wars")
# load convenient packages
library(dplyr)
library(stringr)
library(tidyr)

# read file line by line 
raw <- readLines("star_wars_7th.txt")

# create data frame
lines <- data_frame(raw = raw) 

# get rid of leading and trailing white spaces
# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
lines= mutate(lines,raw=trim(raw))

# get rid of the empty lines
lines2 <- filter(lines, raw != "")

# detect scenes: begin by EXT. or INT.
lines3 <-  mutate(lines2, is_scene = str_detect(raw, "T."),scene = cumsum(is_scene)) 

# drop lines that start with EXT. or INT.
lines4 <- filter(lines3,!is_scene)

# distinguish characters from what they say
lines5 <- separate(lines4, raw, c("speaker", "dialogue"), sep = ":", fill = "left",extra='drop')

# read in aliases (from Evelina's post)
aliases <- read.table('aliases2.csv',sep=',',header=T,colClasses = "character")
aliases$Alias
aliases$Name

# assign unique name to characters
# http://stackoverflow.com/questions/28593265/is-there-a-function-like-switch-which-works-inside-of-dplyrmutate
multipleReplace <- function(x, what, by) {
  stopifnot(length(what)==length(by))               
  ind <- match(x, what)
  ifelse(is.na(ind),x,by[ind])
}
lines6 <- mutate(lines5,speaker=multipleReplace(speaker,what=aliases$Alias,by=aliases$Name))

# read in actual names (from Evelina's post)
actual.names <- read.csv('characters3.csv',header=F,colClasses = "character")
actual.names <- c(as.matrix(actual.names))
# filter out non-characters
lines7 <- filter(lines6,speaker %in% actual.names)

# group by scene
lines8 <- group_by(lines7, scene, line = cumsum(!is.na(speaker))) 

lines9 <- summarize(lines8, speaker = speaker[1], dialogue = str_c(dialogue, collapse = " "))

# Count the lines-per-scene-per-character
# Turn the result into a binary speaker-by-scene matrix
by_speaker_scene <- count(lines9, scene, speaker)
by_speaker_scene

library(reshape2)
speaker_scene_matrix <-acast(by_speaker_scene , speaker ~ scene, fun.aggregate = length)
dim(speaker_scene_matrix)

norm <- speaker_scene_matrix / rowSums(speaker_scene_matrix)
h <- hclust(dist(norm, method = "manhattan"))
par(bg="white")
plot(h)

ordering <- h$labels[h$order]
ordering

scenes <-  filter(by_speaker_scene, n() > 1) # scenes with > 1 character
scenes2 <- ungroup(scenes)
scenes3 <- mutate(scenes2, scene = as.numeric(factor(scene)),
                  character = factor(speaker, levels = ordering))
library(ggplot2)
ggplot(scenes3, aes(scene, character)) +
  geom_point() +
  geom_path(aes(group = scene))
cooccur <- speaker_scene_matrix %*% t(speaker_scene_matrix)
heatmap(cooccur)
library(igraph)
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
plot(g, edge.width = E(g)$weight)
degree(g)
betweenness(g)

par(bg="lightblue")
plot(g, layout=layout.sphere,
     
     # === vertex
     vertex.color = "gold",          # Node color
     vertex.frame.color = "white",                 # Node border color
     vertex.shape="circle",                        # One of "none", "circle", "square", "csquare", "rectangle" "crectangle", "vrectangle", "pie", "raster", or "sphere"
     vertex.size=14,                               # Size of the node (default is 15)
     vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
     
     # === vertex label
     #vertex.label=LETTERS[1:10],                   # Character vector used to label the nodes
     vertex.label.color="black",
     vertex.label.family="Times",                  # Font family of the label (e.g."Times", "Helvetica")
     vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=1,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="white",                           # Edge color
     edge.width = E(g)$weight,                                 # Edge width, defaults to 1
     edge.arrow.size=1,                            # Arrow size, defaults to 1
     edge.arrow.width=1,                           # Arrow width, defaults to 1
     edge.lty="solid",                             # Line type, could be 0 or "blank", 1 or "solid", 2 or "dashed", 3 or "dotted", 4 or "dotdash", 5 or "longdash", 6 or "twodash"
     edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
)

#change color based on the dark/light side
# create vectors with characters in each side
dark_side <- c("GENERAL HUX", "KYLO REN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE",
                "REY","BB-8","FINN","POE")
other <- c("UNKAR", "JABBA")
#adjust node size
V(g)$size <- log(strength(g)) * 4 + 3
# node we'll create a new color variable as a node property
V(g)$color <- NA
V(g)$color[V(g)$name %in% dark_side] <- "red" #beaed4"
V(g)$color[V(g)$name %in% light_side] <- "gold" #7fc97f"
V(g)$color[V(g)$name %in% other] <- "grey70"
vertex_attr(g)

par(bg="#fdc086")
plot(g, layout=layout.sphere,
     
     # === vertex
     #vertex.color = "gold",          # Node color
     vertex.frame.color = "black",                 # Node border color
     vertex.shape="circle",                        # One of "none", "circle", "square", "csquare", "rectangle" "crectangle", "vrectangle", "pie", "raster", or "sphere"
     #vertex.size=14,                               # Size of the node (default is 15)
     vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
     
     # === vertex label
     #vertex.label=LETTERS[1:10],                   # Character vector used to label the nodes
     vertex.label.color="black",
     vertex.label.family="Times",                  # Font family of the label (e.g."Times", "Helvetica")
     vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=1,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="#ffff99",                           # Edge color
     edge.width = E(g)$weight,                                 # Edge width, defaults to 1
     edge.arrow.size=1,                            # Arrow size, defaults to 1
     edge.arrow.width=1,                           # Arrow width, defaults to 1
     edge.lty="solid",                             # Line type, could be 0 or "blank", 1 or "solid", 2 or "dashed", 3 or "dotted", 4 or "dotdash", 5 or "longdash", 6 or "twodash"
     edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
)

library(d3Network)
library(networkD3)
sg <- simplify(g)
df <- get.edgelist(g, names=TRUE)
df <- as.data.frame(df)
colnames(df) <- c('source', 'target')
df$value <- rep(1, nrow(df))
# get communities
fc <- fastgreedy.community(g)
com <- membership(fc)
node.info <- data.frame(name=names(com), group=as.vector(com))
links <- data.frame(source=match(df$source, node.info$name)-1,target=match(df$target, node.info$name)-1,value=df$value)

forceNetwork(Links = links, Nodes = node.info,Source = "source", Target = "target",Value = "value", NodeID = "name",Group = "group", opacity = 1, opacityNoHover=1)
