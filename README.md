# Party Vote Choice Experiment - Simulation Tool

## Introduction
This repository provides a shiny app that offers an easy-to-use simulation tool to calculate predicted vote probabilities for a hypothetical electoral contest between two political parties. Users can customize the parties' characteristics along multiple dimensions -- including several candidate characteristics, the ideological distance to a voter, and the parties' cohesiveness -- to simulate the effect of these factors on the relative voting probability in a direct two-party contest (assuming no abstention).

The results and the underlying statistical model are based on a conjoint experiment that was fielded in Wave 44 (November 2019) of the [German Internet Panel (GIP)](https://www.uni-mannheim.de/gip/), a probability-based online survey of the German population. For further questions, please feel free to contact one of the authors listed below.

## Usage
You can use this shiny app directly from within `R` by running:

```r
shiny::runGitHub(repo="party-unity-conjoint"
                 ,username="pirmin-stoeckle"
                 ,ref="main")
``` 

## Authors
- [Sebastian Juhl](http://www.sebastianjuhl.com) (SFB 884; University of Mannheim)
- [Roni Lehrer](http://www.ronilehrer.com) (MZES; University of Mannheim)
- [Pirmin Stöckle](https://gess.uni-mannheim.de/doctoral-programs/social-and-behavioral-sciences-cdss/students/people/show/pirmin-stoeckle.html) (SFB 884 & CDSS; University of Mannheim)
