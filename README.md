# Party Vote Choice Experiment - Simulation Tool

## Introduction
This repository provides a shiny app that offers an easy-to-use simulation tool to calculate predicted vote probabilities for a hypothetical electoral contest between two political parties. Users can customize the parties' characteristics along multiple dimensions -- including several candidate characteristics, the ideological distance to a voter, and the parties' cohesiveness -- to simulate the effect of these factors on the relative voting probability in a direct two-party contest (assuming no abstention).

The results and the underlying statistical model are based on a conjoint experiment that was fielded in the [`German Internet Panel (GIP)`](https://www.uni-mannheim.de/gip/), a probability-based online survey of the German population, in 2020. For further questions, please feel free to contact one of the authors listed below.

## Usage
You can use this shiny app directly from within `R` by running:

```r
shiny::runGitHub("party-unity-conjoint", "pirmin-stoeckle", ref = "main")
``` 

## Authors
- [`Sebastian Juhl`](http://www.sebastianjuhl.com) (SFB 884; University of Mannheim)
- [`Roni Lehrer`](http://www.ronilehrer.com) (MZES; University of Mannheim)
- Pirmin Stöckle (SFB 884; University of Mannheim)
