# cycling-survey-routes
Processing routes from cycling survey

`process.routes.R` does the following:

1 Filter network links to exclude motorways, unless specifically tagged as' bike’ or ‘walk’, and filter network nodes to those connected by the cyclable links

- Note that this would allow cycling on some links that aren’t actually tagged as ‘bike’ (which cyclists may in fact do!)
- Note that network is assumed to be one where all links are one way

2 Convert route strings in survey results to WKT, and then to an sf object

3 Extract the vertices of each string as points, and find the nearest node to each point

4 Eliminate any sections that return to a pre-used node as backtracking or a loop, except where start and end nodes are the same, or where manual inspection shows what looks like an obvious deliberate loop

5 For each segment between a pair of points, find the shortest route, and extract its nodes (vpath) and edges (epath)
- Note that this finds a directed route (cyclists cannot ride the wrong way, which some may in fact do)

6 Do another round of elimination of any sections that return to a pre-used node as backtracking or a loop

7 Write output

8 Produce check maps showing the survey and networked routes

9 Find routes that pass through 'stressful junctions'

10 Produce an expanded version of the output routes with network link details attached converts the WKT strings representing the digitised routes provided by the survey participants into sections of the network that correspond as closely as possible to the digitised routes.

## Bendigo branch
The `bendigo` branch of this repo was set up to process a Bendigo cycling survey.  The network used in Bendigo has simplified nodes, but unsimplified links, and so there are sometimes multiple paths (eg a road and a separated cycleway) between the same pair of nodes.  Whereas the Melbourne network for which the main branch was used had simplified links (in that example, the road and adjacent cycleway would be combined in a single link).

The main difference between the `bendigo` and `main` branches is that the `bendigo` branch weights the length of links in order to encourage matching, in circumstances where there are multiple adjacent paths, to links that are more likely to be used by cycling.  Specifically:
- links with any kind of 'cycleway' tag are weighted at length * 0.9 (so their use is preferred to the adjacent roadway), and
- links where 'is_cycle' is 0 (that is, non-cyclable links, such as footpaths) are weighted at length * 1.15 (so their use is not preferred to the adjacent roadway),

Other changes reflect other differences (for example, in attributes) between the Bendigo and Melbourne networks.

In the `bendigo` branch, `process routes.R` is set up with choice of city (Melbourne or Bendigo) as an option.  If 'Melbourne' is selected, the script runs in the same way as for the main branch.  If 'Bendigo' is selected, then the changes implemented for Bendigo will apply.

The `bendigo` branch also includes:
- `assess routes.R`, which assesses suitability of routes on a range of criteria (Hausdorff distance, directness ratio, same start/end points, and visual inspection), 
- `choice set.R`, which selects alternative routes for mode choice modelling, and
- `report.Rmd`, which produces a set of plots from the survey results.
