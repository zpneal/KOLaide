#' Pick key opinion leaders from a network given constraints
#'
#' @param network a unipartite unweighted network as an adjacency \code{matrix} or \code{igraph} object
#' @param tosource logical: edges point *toward* a source of information
#' @param goal string: goal for the KOL team (either \code{"diffusion"} or \code{"adoption"})
#' @param m integer: KOL team centrality parameter (\code{m == 1} is equivalent to simple degree centrality)
#' @param range vector: a vector of length 2 containing the minimum and maximum number of KOLs on a team
#' @param top numeric: restrict scope to the \code{top} nodes with the highest degree, closeness, or betweenness (useful for large networks)
#' @param include vector: names or indices of nodes that **must** be included on the KOL team
#' @param exclude vector: names or indices of nodes that **can not** be included on the KOL team
#' @param attribute string or vector: if \code{network} is an \code{igraph} object, the name of a node attribute. if \code{network} is an adjacency matrix, a vector containing a node attribute.
#' @param alpha numeric: parameter to control relative weight of breadth and diversity in overall evaluation of KOL teams (\eqn{0.5 \leq \alpha \leq 1})
#' @param beta numeric: parameter to control weight of team size in overall evaluation of KOL teams (\eqn{0 \leq \beta \leq 2})
#' @param file string: filename to write a sorted list of possible KOL teams as a CSV.
#'
#' @details
#' When seeking to diffuse a piece of information or encourage adoption of a behavior, it is often useful
#' to recruit the assistance of *key opinion leaders* (KOL) in a network. \code{pick_kols} facilitates selecting
#' members of a KOL team by returning a dataframe of possible teams. The selection of a KOL team often depends on
#' several factors, which this function summarizes as ABCDE:
#' * Availability - The availability of individuals to serve as a KOL. This can be controlled by the \code{include} and \code{exclude} parameters.
#' * Breadth - The fraction of non-KOLs that the KOL team can influence. When \code{goal=="diffusion"}, breadth is measured as the fraction of non-KOLs that a KOL team can reach in \code{m} steps (i.e., m-reach). When \code{goal=="adoption"}, breadth is measured as the fraction of non-KOLs that are directly connected to at least \code{m} KOLs (i.e., m-contact).
#' * Cost - The number of KOLs to be recruited and trained (i.e., team size).
#' * Diversity - The fraction of values of `attribute` represented on the KOL team.
#' * Evaluation - Potential KOL teams must be compared and evaluated in a way that balances these considerations.
#'
#' **Evaluating KOL Teams**
#'
#' Potential KOL teams are evaluated on the basis of breadth (B), Cost (C), and (if `attribute` is provided), Diversity (D)
#'    using \deqn{\frac{B}{C^\beta} \mbox{   or   } \frac{B^\alpha D^{1-\alpha}}{C^\beta}}
#'
#' The \eqn{\alpha} parameter can take values \eqn{0.5 < \alpha < 1} and controls the weight placed on breadth relative to diversity.
#'   Smaller values of \eqn{\alpha} place less weight on breadth and more weight on diversity, while larger values of \eqn{\alpha}
#'   place more weight on breadth and less weight on diversity. The default (\eqn{\alpha = 0.9}) places the majority of weight on
#'   the breadth of the network that KOL teams cover, while still considering the team's diversity (primarily as a tie-breaker).
#'
#' The \eqn{\beta} parameter can take values \eqn{0 < \beta < 2} and controls the cost of larger KOL team members. Smaller values of
#'    \eqn{\beta} imply decreasing marginal costs, while larger values of \eqn{\beta} imply increasing marginal costs. The default
#'    (\eqn{\beta = 0.9}) assumes that team members have a slight diminishing marginal cost (i.e. the cost of each additional
#'    team member is slightly smaller than the previous one).
#'
#' **Interpreting Edge Direction**
#'
#' If \code{network} is a directed network, then \code{tosource} controls how the direction of edges is interpreted:
#' * \code{tosource = TRUE} (default) - An edge i -> j is interpreted as "i gets information from j" or "i is influenced
#'   by j" (i.e., the edge points *toward* a source of information or influence). This type of data usually results from
#'   asking respondents to nominate the people from whom they seek advice. In this case, actors with high *in-degree* like
#'   *j* are generally better KOLs.
#' * \code{tosource = FALSE} - An edge i -> j is interpreted as "i sends information to j" or "i influences j" (i.e., the
#'   edge points *away* from a source of information or influence). This type of data usually results from asking respondents
#'   to report the people to whom they give advice. In this case, actors with high *out-degree* like *i* are generally better KOLs.
#'
#' @returns A sorted list containing a data frame of possible KOL teams with their characteristics, the \code{network}, \code{m}, \code{goal}, and (optionally) \code{attribute}
#' @export
#'
#' @examples
#' network <- igraph::sample_smallworld(1,26,2,.2)  #An example network
#' igraph::V(network)$name <- letters[1:26]  #Give the nodes names
#' igraph::V(network)$gender <- sample(c("M","F"),26,replace=TRUE)  #Give the nodes a "gender"
#' teams <- pick_kols(network,              #Find KOL teams in `network`
#'                    m = 2,
#'                    range = c(2,4),       #containing 2-4 members
#'                    attribute = "gender", #that are gender diverse
#'                    goal = "diffusion")   #and can help diffuse information
#' teams$teams[1:10,]  #Look at the top 10 teams

pick_kols <- function(network,
                      tosource = TRUE,
                      goal = "diffusion",
                      m = 1,
                      range = c(1,1),
                      top = NULL,
                      include = NULL,
                      exclude = NULL,
                      attribute = NULL,
                      alpha = 0.9,
                      beta = 0.9,
                      file = NULL) {

  #### Parameter Checks ####
  if (!methods::is(network,"matrix") & !methods::is(network,"igraph")) {stop("`network must be either a matrix or igraph object`")}
  if (length(range)!=2) {stop("`range` must be a numeric vector of length 2")}
    min <- min(range)
    max <- max(range)
    if (!is.numeric(min) | !is.numeric(max)) {stop("`range` must be specified using positive integers")}
    if (min%%1!=0 | min<1 | max%%1!=0 | max<1) {stop("`range` must be specified using positive integers")}
  if (!is.numeric(m)) {stop("`m` must be a positive integer")}
  if (m%%1!=0 | m<1) {stop("`m` must be a positive integer")}
  if (!is.null(top)) {
    if (!is.numeric(top)) {stop("`top` must be a positive integer")}
    if (top%%1!=0 | top<1) {stop("`top` must be a positive integer")}
  }
  if (!is.numeric(alpha)) {stop("`alpha` must be a numeric")}
  if (!is.numeric(alpha)) {stop("`beta` must be a numeric")}
  if (alpha<0.5 | alpha>1) {stop("`alpha` must be between 0.5 and 1")}
  if (beta<0 | beta>2) {stop("`beta` must be between 0 and 2")}

  #If `attribute` is supplied and `network` is an igraph object, ensure attribute is present and extract it
  if (!is.null(attribute) & methods::is(network,"igraph")) {
    if (!(attribute %in% igraph::vertex_attr_names(network))) {stop("this igraph network does not contain a node attribute named `attribute`")}
    attrib <- igraph::vertex_attr(network, attribute)
  }

  if (!is.null(attribute) & methods::is(network,"matrix")) {
    if (length(attribute)!=nrow(network)) {stop("the length of the node `attribute` must match the number of nodes in `network`")}
    attrib <- attribute
  }

  #Get adjacency matrix
  if (methods::is(network,"matrix")) {M <- network}
  if (methods::is(network,"igraph")) {M <- igraph::as_adjacency_matrix(network, sparse = FALSE)}
  if (tosource) {M <- t(M)}  #Reverse edge direction if necessary so that edges point in the direction of information/influence

  #Get igraph object
  if (methods::is(network,"matrix")) {
    net <- igraph::graph_from_adjacency_matrix(network)
    if (!is.null(attribute)) {net <- igraph::set_vertex_attr(graph = net, name = "attribute", value = attrib)}  #Add attribute, if supplied
    }
  if (methods::is(network,"igraph")) {net <- network}
  if (tosource) {net <- igraph::reverse_edges(net)}  #Reverse edge direction if necessary so that edges point in the direction of information/influence

  #If `include` is supplied as node indices
  if (!is.null(include)) {if (is.numeric(include)) {
    if (any(include%%1!=0) | any(include<0)) {stop("`include` must be a vector of node names or indices")}  #Check that indices are positive integers
    if (any(include > max(nrow(M)))) {stop("`include` contains node indices that do not exist in `network`")}  #Check that indices are in the network
  }}

  #If `exclude` is supplied as node indices
  if (!is.null(exclude)) {if (is.numeric(exclude)) {
    if (any(exclude%%1!=0) | any(exclude<0)) {stop("`exclude` must be a vector of node names or indices")}
    if (any(exclude > max(nrow(M)))) {stop("`exclude` contains node indices that do not exist in `network`")}
  }}

  #If `include` is supplied as node names
  if (!is.null(include)) {if (!(is.numeric(include))) {
    if (is.null(rownames(M))) {stop("`include` contains node names, but `network` does not contain node names")}  #Check that network has named nodes
    names <- rownames(M)  #Get node names
    if (!all(include %in% names)) {stop("`include` contains node names that are not present in `network`")}  #Check that node names are in network
    include <- which(names %in% include)  #Convert names to indices
  }}

  #If `exclude` is supplied as node names
  if (!is.null(exclude)) {if (!(is.numeric(exclude))) {
    if (is.null(rownames(M))) {stop("`exclude` contains node names, but `network` does not contain node names")}  #Check that network has named nodes
    names <- rownames(M)  #Get node names
    if (!all(exclude %in% names)) {stop("`exclude` contains node names that are not present in `network`")}  #Check that node names are in network
    exclude <- which(names %in% exclude)  #Convert names to indices
  }}

  #### Vector of eligible KOLs ####
  eligible <- c(1:nrow(M))  #Index of all network members

  eligible <- eligible[which(!(eligible %in% exclude))]  #Remove indices in `exclude`

  eligible <- eligible[which(!(eligible %in% which(rowSums(M) < 2)))]  #Exclude isolates and pendants

  #Restrict to central nodes
  if (!is.null(top)) {
    centralities <- data.frame(degree = rank(igraph::degree(net), ties.method = "random"),  #Compute node centrality, rank
                               close = rank(igraph::closeness(net), ties.method = "random"),
                               between = rank(igraph::betweenness(net), ties.method = "random"))
    centralities$top <- (centralities$degree > (nrow(centralities) - top)) |  #Find the `top` highest degree nodes
                        (centralities$close > (nrow(centralities) - top)) |   #...or `top` highest closeness nodes
                        (centralities$between > (nrow(centralities) - top))   #...or `top` highest betweenness nodes
    eligible <- eligible[which(eligible %in% which(centralities$top))]  #Exclude lower centrality nodes
  }

  #### List of KOL teams ####
  teams <- list()
  for (i in c(min:max)) {teams <- c(teams, utils::combn(eligible, i, simplify = FALSE))}  #Preliminary list
  if (!is.null(include)) {teams <- teams[which(lapply(teams, FUN = function(x) all(include %in% x))==TRUE)]}  #If `include` provided, remove teams missing required members
  if (length(teams)==0) {stop("There are no eligible KOL teams.")}

  #### Evaluate KOL teams ####
  #Compute m-reach (fraction of non-KOL nodes reachable by a KOL in up to m steps in adjacency matrix M)
  if (goal == "diffusion") {
    dist <- igraph::distances(net)
    mreach <- function(kols, m, dist) {
      if (length(kols)>1) {return(sum(apply(dist[kols,-kols],2,min)<=m) / (nrow(M) - length(kols)))}
      if (length(kols)==1) {return(sum(dist[kols,-kols]<=m) / (nrow(M) - length(kols)))}
      }
    breadth <- unlist(lapply(teams, FUN = function(x) mreach(x, m, dist)))
  }

  #Compute m-contact (fraction of non-KOL nodes directly connected to at least m KOLs in adjacency matrix M)
  if (goal == "adoption") {
    mcontact <- function(kols, m, M) {sum(colSums(M[kols,-kols])>=m) / (nrow(M) - length(kols))}
    breadth <- unlist(lapply(teams, FUN = function(x) mcontact(x, m, M)))
  }

  #If requested, compute team diversity
  if (!is.null(attribute)) {
    represented <- function(kols, attrib) {length(unique(attrib[kols])) / length(unique(attrib))}  #Fraction of node types represented on KOL team
    diversity <- unlist(lapply(teams, FUN = function(x) represented(x, attrib)))
  }

  #### Compile team list ####
  get_names <- function(x, M){
    if (is.null(rownames(M))) {return(paste(x, collapse=", "))}  #If no names, output indices
    if (!is.null(rownames(M))) {return(paste(rownames(M)[x], collapse=", "))}  #If names, output names
  }

  dat <- data.frame(team = unlist(lapply(teams, FUN = function(x) get_names(x, M))),
                    cost = unlist(lapply(teams, FUN = function(x) length(x))),
                    breadth = breadth)

  if (!is.null(attribute)) {dat$diversity <- diversity}  #If requested, include team diversity

  #### Assign teams an overall evaluation ####
  if (is.null(attribute)) {dat$evaluation <- dat$breadth / (dat$cost ^ beta)}
  if (!is.null(attribute)) {dat$evaluation <- ((dat$breadth ^ alpha) * (dat$diversity ^ (1-alpha))) / (dat$cost ^ beta)}

  #### Sort and restrict team list ####
  if (is.null(attribute)) {dat <- dat[order(-dat$evaluation, -dat$breadth),]}  #Sort by overall evaluation, then break ties by breadth
  if (!is.null(attribute)) {dat <- dat[order(-dat$evaluation, -dat$breadth, -dat$diversity),]}  #Sort by overall evaluation, then break ties by breadth, then by diversity
  rownames(dat) <- c(1:nrow(dat))  #Renumber rows

  #### Write team list ####
  if (!is.null(file)) {utils::write.csv(dat, paste0(file,".csv"), row.names = FALSE)}

  #### Build KOL object ####
  dat <- list(teams = dat,
              network = net,
              m = m,
              goal = goal)
  if (!is.null(attribute)) {
    if (length(attribute)==1) dat <- c(dat, attribute = attribute)
    if (length(attribute)>1) dat <- c(dat, attribute = "attribute")
    }

  class(dat) <- c("list", "KOL")
  return(dat)
}
